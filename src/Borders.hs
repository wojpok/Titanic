module Borders where

import Types
import Depth
import System.IO.Unsafe

borderChars :: String
borderChars = "─│╭╮╰╯├┬┼┴┤"

ppHLine :: Doc
ppHLine = Doc (DCustom () $ \size o _ -> ([LFill "─" size], LString "─", size)) $ fixedWidth 0

ppVLine :: Doc
ppVLine = Doc (DCustom () $ \size o _ -> ([], LString " │ ", 3)) $ fixedWidth 3
