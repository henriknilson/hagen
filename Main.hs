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
import Test.QuickCheck

main :: IO ()
main = do
  files <- listDirectory Hagen.pagesDir
  mapM (\file -> Hagen.hagen $ pagesDir ++ file) files
  putStrLn "Done!"
