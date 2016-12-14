module Hagen (
  mustache,
  include,
  compile,
  prepare,
  hagen,
  templateDir,
  pagesDir,
  publicDir,
  partialDir,
  prop_mustache_regex
) where

import Config (Config, getConfigValue, parseConfig, parseFile)
import Control.DeepSeq
import Data.Char
import Data.List
import System.Directory
import System.IO
import System.IO.Unsafe
import Text.Regex as R
import qualified Data.Map.Strict as M
import qualified Data.Text as T

templateDir = "src/templates/"
pagesDir = "src/pages/"
publicDir = "public/"
partialDir = "src/partials/"

-- Finds the first mustache tag and returns it
mustache :: String -> (String, String, String)
mustache doc = case R.matchRegexAll regex doc of
  Nothing -> (doc, "", "")
  Just (before, match, after, _) -> (before, filter isAlpha match, after)
  where
    regex = R.mkRegex "{{[' ']?[A-Za-z]*[' ']?}}"

prop_mustache_regex doc = or [
    isInfixOf ("{{ " ++ match ++ " }}") doc,
    isInfixOf ("{{" ++ match ++ "}}") doc,
    isInfixOf ("{{ " ++ match ++ "}}") doc,
    isInfixOf ("{{" ++ match ++ " }}") doc,
    match == ""
  ]
  where
    (_,match,_) = mustache doc

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
    value = Config.getConfigValue config match

-- Include partials in template using #include
prepare :: String -> IO String
prepare template = do

  let (before, match, after) = include template
  let partial = (words match) !! 1

  return $ if match == "" && after == ""
    then before ++ after
    else before ++ (unsafePerformIO(Config.parseFile (partialDir ++ partial ++ ".html"))) ++ unsafePerformIO(prepare after)

hagen :: FilePath -> IO ()
hagen file = do

  let fileName = reverse $ takeWhile (/= '/') $ drop 1 $ dropWhile (/= '.') (reverse file)

  configFile <- parseFile file
  let config = Config.parseConfig configFile

  let templateFile = Config.getConfigValue config "template"
  template <- parseFile (templateDir ++ templateFile ++ ".html")

  prepared <- prepare template
  let compiled = compile config prepared

  putStrLn $ "Writing " ++ fileName ++ ".html..."
  createDirectoryIfMissing True publicDir
  writeFile (publicDir ++ fileName ++ ".html") compiled
