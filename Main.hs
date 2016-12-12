import Control.DeepSeq
import Data.Char
import Data.List
import System.Directory
import System.IO
import System.IO.Unsafe
import Text.Regex as R
import qualified Data.Map.Strict as M
import qualified Data.Text as T

-- Prevent laziness when reading from a file
parseFile :: FilePath -> IO String
parseFile fileName = withFile fileName ReadMode $
  \h -> do hSetEncoding h utf8_bom
           contents <- hGetContents h
           contents `deepseq` hClose h
           return contents

-- Contains the data to be used when parsing templates
type Config = M.Map String String

-- Parse a config from a file
parseConfig :: String -> Config
parseConfig = foldr addConfigValue M.empty . clean . lines
  where clean = filter (not . flip any ["#", ";", "", " "] . (==) . take 1)

-- Add a value to a config
addConfigValue :: String -> Config -> Config
addConfigValue raw config = M.insert key value config
  where (k, vs) = span (/= ' ') raw
        key = map toLower $ takeWhile (/= ':') k
        value = snd $ span (== ' ') vs

-- Return the value in a config, given key
getConfigValue :: Config -> String -> String
getConfigValue c k = M.findWithDefault "" k c

-- Finds the first mustache tag and returns it
mustache :: String -> (String, String, String)
mustache doc = case R.matchRegexAll regex doc of
  Nothing -> (doc, "", "")
  Just (before, match, after, _) -> (before, filter isAlpha match, after)
  where
    regex = R.mkRegex "{{[' ']?[A-Za-z]*[' ']?}}"

-- Finds the first #include tag and returns it
include :: String -> (String, String, String)
include doc = case R.matchRegexAll regex doc of
  Nothing -> (doc, "", "")
  Just (before, match, after, _) -> (before, filter (\c -> isAlpha c || isSeparator c) match, after)
  where
    regex = R.mkRegex "{{[' ']?#include[' '][A-Za-z]+[' ']?}}"

-- Recursively fetches all mustache tags in a document
-- and converts with tags from the config
compile :: Config -> String -> String
compile config "" = ""
compile config doc = before ++ value ++ (compile config after)
  where
    (before, match, after) = mustache doc
    value = getConfigValue config match

-- Include partials in template using #include
-- @TODO: offer a lamb to the unsafe god
prepare :: String -> IO String
prepare template = do

  let (before, match, after) = include template
  let partial = (words match) !! 1

  return $ if match == "" && after == ""
    then before ++ after 
    else before ++ (unsafePerformIO(parseFile ("src/partials/" ++ partial ++ ".html"))) ++ unsafePerformIO(prepare after)

hagen :: FilePath -> IO ()
hagen file = do

  let fileName = reverse $ takeWhile (/= '/') $ drop 1 $ dropWhile (/= '.') (reverse file)

  configFile <- parseFile file
  let config = parseConfig configFile

  let templateFile = getConfigValue config "template"
  template <- parseFile ("src/templates/" ++ templateFile ++ ".html")
  
  prepared <- prepare template
  let compiled = compile config prepared

  putStrLn $ "Writing " ++ fileName ++ ".html..."
  createDirectoryIfMissing True "public"
  writeFile ("public/" ++ fileName ++ ".html") compiled

main :: IO ()
main = do
  files <- listDirectory "src/pages"
  mapM (\file -> hagen $ "src/pages/" ++ file) files
  putStrLn "Done!"
