module Config (
  Config,
  parseConfig,
  parseFile,
  getConfigValue
) where

import Control.DeepSeq
import Data.Char
import Data.List
import System.Directory
import System.IO
import System.IO.Unsafe
import Text.Regex as R
import qualified Data.Map.Strict as M
import qualified Data.Text as T

-- Contains the data to be used when parsing templates
type Config = M.Map String String

-- Prevent laziness when reading from a file
parseFile :: FilePath -> IO String
parseFile fileName = withFile fileName ReadMode $
  \h -> do hSetEncoding h utf8_bom
           contents <- hGetContents h
           contents `deepseq` hClose h
           return contents

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
