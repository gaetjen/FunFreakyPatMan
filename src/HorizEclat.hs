module HorizEclat where

import FileIO
import HasseTree

main path file minSupport = do
    dataSet <- importData path file
    let frequents = horizEclat dataSet minSupport
    exportResult path file $ toString frequents
    --putStrLn $ show frequents

condDS :: [ItemSet] -> Item -> ([ItemSet], [ItemSet])
condDS [] _ = ([], [])
condDS (([]):dsT) condition = condDS dsT condition
condDS (dsH@(dsHH:dsHT):dsT) condition
    | dsHH == condition = ((dsHT:containCondRemoved), (dsHT:allCondRemoved))
    | dsHH /= condition = (containCondRemoved, (dsH:allCondRemoved)) where
        tailResult = condDS dsT condition
        containCondRemoved = fst tailResult
        allCondRemoved = snd tailResult

horizEclat :: [ItemSet] -> Int -> HasseTree
horizEclat ds minSupport = horizEclat' ds minSupport $ resetLeafCounts $ singletons ds minSupport

horizEclat' :: [ItemSet] -> Int -> HasseTree -> HasseTree
horizEclat' _ _ [] = []
horizEclat' is minSupport remaining@((HasseLeafNode itm n):remT)
    | sup < minSupport = sndSubproblem
    | sup >= minSupport = (HasseTreeNode itm sup fstSubproblem):sndSubproblem where
        sup = length containingRemoved
        containingRemoved = fst conditional
        allRemoved = snd conditional
        conditional = condDS is itm
        sndSubproblem = horizEclat' allRemoved minSupport remT
        fstSubproblem = horizEclat' containingRemoved minSupport remT

