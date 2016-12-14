import Control.DeepSeq
import Data.Char
import Data.List
import System.Directory
import System.IO
import System.IO.Unsafe
import Text.Regex as R
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Hagen

main :: IO ()
main = do
  files <- listDirectory "src/pages"
  mapM (\file -> Hagen.hagen $ "src/pages/" ++ file) files
  putStrLn "Done!"
