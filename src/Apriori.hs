module Apriori  where
import FileIO
import Data.List

type ItemSet = [String]     -- an item set is a collection of items, which are identified with strings
type Item = String
-- a node in the Hasse diagram search tree contains an item, a counter for the support of the item set that is
-- constructed by traversing the tree to this item and a pointer to the list of child nodes
data HasseNode = HasseTreeNode Item Int HasseTree | HasseLeafNode Item Int deriving Show
type HasseTree = [HasseNode]

item :: HasseNode -> Item
item (HasseTreeNode itm _ _) = itm
item (HasseLeafNode itm _) = itm

count :: HasseNode -> Int
count (HasseTreeNode _ n _) = n
count (HasseLeafNode _ n) = n

child :: HasseNode -> HasseTree
child (HasseTreeNode _ _ c) = c

addCount :: Int -> HasseNode -> HasseNode
addCount a (HasseTreeNode itm n c)= HasseTreeNode itm (n + a) c
addCount a (HasseLeafNode itm n) = HasseLeafNode itm (n + a)

incCount = addCount 1

main path file minSupport = do
    dataSet <- importData path file
    let frequents = apriori dataSet minSupport
    putStrLn (show frequents)


apriori :: [ItemSet] -> Int -> HasseTree
apriori dataSet minSupport = apriori' dataSet (singletons dataSet minSupport) minSupport 2

apriori' :: [ItemSet] -> HasseTree -> Int -> Int -> HasseTree
apriori' _ [] _ _ = []
apriori' dataSet hasseTree minSupport k -- k = level of iteration
    | k > minSupport = hasseTree
    | otherwise = apriori' dataSet postPruned minSupport (k + 1) where
        postPruned = (posteriorPrune dataSet aprioriPruned minSupport k)
        aprioriPruned = (genCandidates hasseTree hasseTree minSupport (k - 1) [])

-- generates viable (i.e. according to a priori property) candidates
genCandidates :: HasseTree -> HasseTree -> Int -> Int -> ItemSet -> HasseTree
genCandidates [] _ _ _ _ = [] -- stop when we are at end of list
genCandidates (htH@(HasseLeafNode itm n):htT) reference minSupport k cand -- cand for candidate
    | k == 1 = ((HasseTreeNode itm n (nextLevel (reverse (itm:cand)) htT reference)):nextSameLine) -- front: node with old item and support counter, and pointer to newly generated child node
    | k > 1 = htH:nextSameLine where
        nextSameLine = (genCandidates htT reference minSupport k cand)
genCandidates ((HasseTreeNode itm n child):htT) reference minSupport k cand
    = (HasseTreeNode itm n (genCandidates child reference minSupport (k - 1) (itm:cand))):(genCandidates htT reference minSupport k cand)

nextLevel :: ItemSet -> HasseTree -> HasseTree -> HasseTree
nextLevel _ [] _ = []
nextLevel cand ((HasseLeafNode itm n):hlT) reference = -- hl for Hasse Leaves
    case any (==True) $ aprioriPrune (cand ++ [itm]) reference of
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
notSupported [lst] ((HasseLeafNode itm _):htT)
    | lst == itm = False
    | lst < itm = notSupported [lst] htT
notSupported (candH:_) (htH:_)
    | candH > (item htH) = True -- we've passed the point of no return, there is no way to still find the candidate in the tree
notSupported cand@(candH:_) ((HasseLeafNode itm _):htT)
    | candH == itm = True -- guaranteed that the Leaf Node is not on the last layer, i.e. supersets are infrequent, because of earlier pattern matching
    | candH < itm = notSupported cand htT -- otherwise iterate through to find the node on which to go down
notSupported cand@(candH:candT) ((HasseTreeNode itm _ nxtLevel):htT)
    | candH == itm = notSupported candT nxtLevel -- go down to the next level, omit first element of candidate
    | candH < itm = notSupported cand htT

posteriorPrune :: [ItemSet] -> HasseTree -> Int -> Int -> HasseTree
posteriorPrune dataSet hasseTree minSupport k = removeInfrequent minSupport (addDataSet dataSet hasseTree k) k

-- removes the infrequent item sets from the Hasse Tree
removeInfrequent :: Int -> HasseTree -> Int -> HasseTree
removeInfrequent _ [] _ = []
removeInfrequent minSupport ht 1 = filter (\x -> (count x) >= minSupport) ht
removeInfrequent minSupport (htH:htT) k = (removeInfrequent minSupport (child htH) (k - 1)) ++ (removeInfrequent minSupport htT k)

-- counts and adds all sets of size k
addDataSet :: [ItemSet] -> HasseTree -> Int -> HasseTree
addDataSet [] ht _ = ht
addDataSet (dsH:dsT) ht k = addDataSet dsT (addItemSet dsH ht k) k

addItemSet :: ItemSet -> HasseTree -> Int -> HasseTree
addItemSet [] ht _ = ht
addItemSet _ [] _ = []
addItemSet is@(isH:isT) ht@(htH:htT) 1
    | isH == item htH = (incCount htH):(addItemSet isT htT 1)
    | isH < item htH = htH:(addItemSet is htT 1)
    | isH > item htH = ht
addItemSet is (htH@(HasseLeafNode _ _):htT) k = htH:(addItemSet is htT k)
addItemSet is@(isH:isT) ht@(htH@(HasseTreeNode itm n child):htT) k
    | isH == item htH = (HasseTreeNode itm n (addItemSet isT child (k - 1))):(addItemSet isT htT k)
    | isH < item htH = htH:(addItemSet is htT k)
    | isH > item htH = ht

-- generates the top level of the hasse tree with all the singletons, filtered for minimum support and sorted lexicographically
singletons :: [ItemSet] -> Int -> HasseTree
singletons dataSet minSupport = sortOn item $ removeInfrequent minSupport (getSingletons dataSet) 1

-- count support for all singletons in the database
getSingletons :: [ItemSet] -> HasseTree
getSingletons is = getSingletons' is []

getSingletons' :: [ItemSet] -> HasseTree -> HasseTree
getSingletons' [] acc = acc
getSingletons' (isH:isT) acc = getSingletons' isT (addSetToSingletons isH acc)

-- adds/counts all the singletons in one transaction, adds them to a Hasse Tree
addSetToSingletons :: ItemSet -> HasseTree -> HasseTree
addSetToSingletons [] ht = ht  -- stop condition
addSetToSingletons (isH:isT) ht = addSetToSingletons isT (addItemToSingletons isH ht)

addItemToSingletons :: Item -> HasseTree -> HasseTree
addItemToSingletons itm [] = [HasseLeafNode itm 1] -- itm is the first occurence in the database, we append it with support 1 to the HasseTree
addItemToSingletons itm (htH@(HasseLeafNode singleton n):htT)
    | itm == singleton = (HasseLeafNode itm (n+1)):htT-- the item matches the element in the node, so increase counter
    | itm /= singleton = htH:(addItemToSingletons itm htT) -- otherwise go through rest of list




