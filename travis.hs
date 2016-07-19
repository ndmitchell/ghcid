
import System.Process.Extra
import System.Directory.Extra

main = do
    print =<< listFilesRecursive "dist"
    system_ "dist/build/ghcid_test/ghcid_test"
