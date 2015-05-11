
module Paths_ghcid where

import Data.Version
import Text.ParserCombinators.ReadP


version :: Version
-- can't write a literal Version value since in GHC 7.10 the versionsTag field is deprecated
version = head $ [v | (v,"") <- readP_to_S parseVersion "0.0"] ++ error "version, failed to parse"
