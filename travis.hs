
import System.Process.Extra

main = system_ "cabal test --show-details=streaming"
