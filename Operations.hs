-- A set of operations that are intended to derive grammatical sentences, and
-- a set of lexical items to test the operations. The operations are slightly
-- modified from the Minimalist program.

module Operations where

--                              DATA STRUCTURES

-- a Lexical Item (LI) consists of a root and lists of Category, Inflectional
-- and Selectional features, which each consist of lists of strings
data LI = LI { root :: String
             , cat :: [String]
             , infl :: [String]
             , sel :: [String]
             } deriving (Show, Eq)

-- a Phrase Marker (PM) consists of an empty phrase marker or a node containing
-- a LI and a left and right branch containing other phrase markers
data PM = Empty' | PM { node :: LI
                      , lbranch :: PM
                      , rbranch :: PM
                      } deriving (Show, Eq)

-- takes a LI and returns a phrase marker whose node is the lexical item and
-- whose subtrees are both empty
pm_from_li :: LI -> PM
pm_from_li x = PM {node = x, lbranch = Empty', rbranch = Empty'} 

--                              OPERATIONS

--                                 MERGE

-- takes a phrase marker and removes the selection features. Used in apply_merge
-- for the node of the lbranch in the newly constructed phrase marker
remove_sel :: PM -> PM
remove_sel x = PM { node = LI { root = root (node x), cat = cat (node x), infl = infl (node x), sel = []} , lbranch = lbranch x , rbranch = rbranch x }


-- apply_merge takes two PMs and applies the operation merge. The node will not
-- bear any inflectional features
apply_merge :: PM -> PM -> PM
apply_merge pm1 pm2 = PM { node = LI { root = x, cat = y, infl = [], sel = zs} 
                         , lbranch = remove_sel pm1
                         , rbranch = pm2
                         }
                         where x = root (node pm1)
                               y = cat (node pm1)
                               (z:zs) = sel (node pm1)


-- merge creates a new PM if the first selectional feature of the first PM
-- matches the first category feature of the second PM
merge :: PM -> PM -> PM
merge pm1 pm2
      | head (sel (node pm1)) == head (cat (node pm2)) = apply_merge pm1 pm2
      | otherwise = error "Phrase Markers could not be Merged"


--                                AGREE
-- The Agree operation acts on two syntactic objects in a phrase marker if one
-- has a valued CAT feature that matches an unvalued INFL feature on the other.
-- As a convention, all features will have the form of three characters and a
-- colon, with the following characters being the value of the feature, and I
-- use _ to denote that a feature is unmarked.




--                              MOVE_PHRASE

-- copy_pm creates a new phrase marker with the 

-- unpronounce :: LI -> PM -> PM
-- unpronounce x [] = [x]
-- unpronounce x (y:ys)
--             | x == y = x : ys
--             | otherwise = x : y : (unpronounce x ys)
--             --where y' = LI { cat = cat y, infl = [], sel = sel y}

-- apply_move_phrase :: PM -> PM -> PM
-- apply_move_phrase x [] = x
-- apply_move_phrase x (hd:tl)
--                   | head(cat hd) == "D" = hd : x ++ (unpronounce hd tl)
--                   | otherwise = (apply_move_phrase (x ++ [hd]) tl)

-- -- move_phrase drives movement of a DP into a subject poition
-- move_phrase :: PM -> PM
-- move_phrase [] = error "Phrase marker cannot be empty"
-- move_phrase (hd:next:tl)
--             | infl next == ["D<"] = apply_move_phrase [] tl 
--             | otherwise            = hd:tl
