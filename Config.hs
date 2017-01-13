module Config (
  Config,
  parseConfig,
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
import Test.QuickCheck

-- Contains the data to be used when parsing templates
type Config = M.Map String String

-- Parse a config from a file
parseConfig :: String -> Config
parseConfig = foldr addConfigValue M.empty . clean . lines
  where clean = filter (not . flip any ["#", ";", "", " "] . (==) . take 1)

-- A QuickCheck test to make sure that no comment is parsed as key/value
prop_parseConfig_comments :: String -> Bool
prop_parseConfig_comments s = null $ parseConfig s

-- Generates "commented" strings
commentGen :: Gen String
commentGen = do
  c <- elements ['#', ';']
  s <- filter (/= '\n') <$> arbitrary
  return $ c:s

-- Return the value in a config, given key
getConfigValue :: Config -> String -> String
getConfigValue c k = M.findWithDefault "" k c

extractKeyValue :: String -> (String, String)
extractKeyValue text = (key, value)
  where (k, vs) = span (/= ':') text
        key = map toLower k
        value = dropWhile isSeparator $ drop 1 vs

-- Add a value to a config
addConfigValue :: String -> Config -> Config
addConfigValue raw config = M.insert key value config
  where (key, value) = extractKeyValue raw
