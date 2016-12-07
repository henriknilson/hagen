import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as M
 
main :: IO ()
main = do
  f <- readFile "data.txt"
  let config = parseConfig f

  print $ M.findWithDefault "not found!" "title" config

type Config = M.Map String String

parseConfig :: String -> Config
parseConfig = foldr addConfigValue M.empty . clean . lines
  where clean = filter (not . flip any ["#", ";", "", " "] . (==) . take 1)
 
addConfigValue :: String -> Config -> Config
addConfigValue raw config = M.insert key value config
  where (k, vs) = span (/= ' ') raw
        key = map toLower $ takeWhile (/= ':') k
        value = snd $ span (== ' ') vs


