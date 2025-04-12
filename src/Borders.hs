module Borders where

import Types
import Depth
import System.IO.Unsafe

borderChars :: String
borderChars = "─│╭╮╰╯├┬┼┴┤"

ppHLine :: Doc
ppHLine = Doc (DCustom () $ \size o _ -> ([LFill "─" size], size)) $ fixedWidth 0

