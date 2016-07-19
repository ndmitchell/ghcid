
import System.Process.Extra

main = do
    system_ "cabal test"
