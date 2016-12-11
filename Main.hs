import Control.DeepSeq
import Data.Char
import System.Directory
import System.IO
import Text.Regex as R
import qualified Data.Map.Strict as M
import qualified Data.Text as T

-- Prevent laziness when reading from a file
parseData :: FilePath -> IO String
parseData fileName = withFile fileName ReadMode $
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

-- Recursively fetches all mustache tags in a document
-- and converts with tags from the config
transform :: Config -> String -> String
transform config "" = ""
transform config doc = before ++ value ++ (transform config after)
  where
    (before, match, after) = mustache doc
    value = getConfigValue config match

main :: IO ()
main = do

  putStrLn "Loading files..."
  configFile <- parseData "data.txt"
  htmlFile <- parseData "index.html"

  putStrLn "Transforming templates..."
  let config = parseConfig configFile
  let transformed = transform config htmlFile

  putStrLn "Writing new files..."
  createDirectoryIfMissing True "public"
  writeFile "public/output.html" transformed

  putStrLn "Done!"
