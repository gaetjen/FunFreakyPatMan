module Apriori  where

import FileIO
import HasseTree

main path file minSupport = do
    dataSet <- importData path file
    let frequents = apriori dataSet minSupport
    exportResult path file $ toString frequents

apriori :: [ItemSet] -> Int -> HasseTree
apriori dataSet minSupport = apriori' dataSet (singletons dataSet minSupport) minSupport 1

apriori' :: [ItemSet] -> HasseTree -> Int -> Int -> HasseTree
-- apriori' _ [] _ _ = []
apriori' dataSet hasseTree minSupport k
    | k > (length hasseTree) = hasseTree
    | otherwise = apriori' dataSet postPruned minSupport (k + 1) where -- k = level of iteration
        postPruned = (posteriorPrune dataSet aprioriPruned minSupport (k + 1))
--         postPruned = aprioriPruned
        aprioriPruned = (genCandidates hasseTree hasseTree k [])

-- generates viable (i.e. according to a priori property) candidates
genCandidates :: HasseTree -> HasseTree -> Int -> ItemSet -> HasseTree
genCandidates [] _ _ _ = [] -- stop when we are at end of list
genCandidates (htH@(HasseLeafNode itm n):htT) reference k cand -- cand for candidate
    | k == 1 = ((HasseTreeNode itm n (nextLevel (reverse (itm:cand)) htT reference)):nextSameLine) -- front: node with old item and support counter, and pointer to newly generated child node
    | k > 1 = htH:nextSameLine where
        nextSameLine = (genCandidates htT reference k cand)
genCandidates ((HasseTreeNode itm n child):htT) reference k cand
    = (HasseTreeNode itm n (genCandidates child reference (k - 1) (itm:cand))):(genCandidates htT reference k cand)

nextLevel :: ItemSet -> HasseTree -> HasseTree -> HasseTree
nextLevel _ [] _ = []
nextLevel cand ((HasseLeafNode itm n):hlT) reference = -- hl for Hasse Leaves
    case any (True==) $ aprioriPrune (cand ++ [itm]) reference of
        True -> nextLevel cand hlT reference
        False -> (HasseLeafNode itm 0):(nextLevel cand hlT reference)

-- generates a list of Bool which indicate whether the supports of the subsets of a candidate itemSet are smaller than
-- the minimum support, based on a previously generated HasseTree
aprioriPrune :: ItemSet -> HasseTree -> [Bool]
aprioriPrune (itmSetH:itmSetT) reference = aprioriPrune' [] [itmSetH] itmSetT reference

aprioriPrune' :: ItemSet -> ItemSet -> ItemSet -> HasseTree -> [Bool]
aprioriPrune' _ _ [] _ = [False] -- this means we are omitting the last element, which is generated from an already frequent subset (neighbor)
aprioriPrune' prev omit tail@(tailH:tailT) reference = (notSupported (prev ++ tail) reference):(aprioriPrune' (prev ++ omit) [tailH] tailT reference)

-- gives True when the candidate is not in the HasseTree, i.e. the support is too small
notSupported :: ItemSet -> HasseTree -> Bool
notSupported cand [] = True
notSupported [lst] (htH:htT)
    | lst == item htH = False
    | lst > item htH = notSupported [lst] htT
notSupported (candH:_) (htH:_)
    | candH < (item htH) = True -- we've passed the point of no return, there is no way to still find the candidate in the tree
notSupported cand@(candH:_) ((HasseLeafNode itm _):htT)
    | candH == itm = True -- guaranteed that the Leaf Node is not on the last layer, i.e. supersets are infrequent, because of earlier pattern matching
    | candH > itm = notSupported cand htT -- otherwise iterate through to find the node on which to go down
notSupported cand@(candH:candT) ((HasseTreeNode itm _ nxtLevel):htT)
    | candH == itm = notSupported candT nxtLevel -- go down to the next level, omit first element of candidate
    | candH > itm = notSupported cand htT

posteriorPrune :: [ItemSet] -> HasseTree -> Int -> Int -> HasseTree
posteriorPrune dataSet hasseTree minSupport k = removeInfrequent minSupport (addDataSet dataSet hasseTree k) k

-- counts and adds all sets of size k
addDataSet :: [ItemSet] -> HasseTree -> Int -> HasseTree
addDataSet [] ht _ = ht
addDataSet (dsH:dsT) ht k = addDataSet dsT (addItemSet dsH ht k) k

addItemSet :: ItemSet -> HasseTree -> Int -> HasseTree
addItemSet [] ht _ = ht
addItemSet _ [] _ = []
addItemSet is@(isH:isT) ht@(htH:htT) 1
    | isH == item htH = (incCount htH):(addItemSet isT htT 1)
    | isH > item htH = htH:(addItemSet is htT 1)
    | isH < item htH = ht
addItemSet is (htH@(HasseLeafNode _ _):htT) k = htH:(addItemSet is htT k)
addItemSet is@(isH:isT) ht@(htH@(HasseTreeNode itm n child):htT) k
    | isH == item htH = (HasseTreeNode itm n (addItemSet isT child (k - 1))):(addItemSet isT htT k)
    | isH > item htH = htH:(addItemSet is htT k)
    | isH < item htH = ht

