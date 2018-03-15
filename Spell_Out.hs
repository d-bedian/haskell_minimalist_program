module Spell_Out where

import Operations

--                               SPELL OUT

-- spell_out takes a PM and evaluates each PM it contains to determine the
-- string that is associated with it, then prints that string
spell_out :: PM -> String
spell_out Empty' = ""
spell_out (PM n l r)
          | sel n /= [] = error "Derivation does not converge"
          | n == LI { root = "the", cat = ["D"], infl = ["the"], sel = []} = spell_out l ++"the " ++ spell_out r
          | n == LI { root = "boy", cat = ["N","3"], infl = ["boy"], sel = []} = spell_out l ++ "boy " ++ spell_out r
          | n == LI { root = "die", cat = ["V"], infl = ["died"], sel = []} = spell_out l ++ "died " ++ spell_out r
          | otherwise                                     = spell_out l ++ "" ++ spell_out r
-- At some point we want a single case where the spell_out is root++morph(infl)
-- and irregular cases can be guards above that, or there can be two spell out
-- functions, one which outputs root++morph(infl) etc. and one which takes those
-- and outputs the real English word.

-- This can perhaps be done with folds or map

-- Also look at intercalate source code for better way to insert spaces between words in spell out.
