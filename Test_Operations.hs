module Test_Operations where

import Operations
import Spell_Out

--                              LEXICAL ENTRIES

-- sample lexical items to test the operations with. In the future, there should
-- be some f(n) for creating lexical items of a certain class (nouns, transitive
-- verbs, intransitive verbs, etc.)
the :: PM
the = pm_from_li LI { root = "the", cat = ["D"], infl = ["the"], sel = ["N"]}
boy :: PM
boy = pm_from_li LI { root = "boy", cat = ["N","3"], infl = ["boy"], sel = []}
died :: PM
died = pm_from_li LI { root = "die", cat = ["V"], infl = ["died"], sel = ["D"]}
t_past :: PM
t_past = pm_from_li LI { root = "", cat = ["T"], infl = ["D<"], sel = ["V"]}

test :: PM
test = merge t_past (merge died (merge the boy))
