module HasseTree where

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

toString :: HasseTree -> String
toString ht = toString' ht []

toString' :: HasseTree -> ItemSet -> String
toString' [] _ = ""
toString' ((HasseTreeNode itm n []):htT) prevSet = toString' ((HasseLeafNode itm n):htT) prevSet
toString' ((HasseLeafNode itm n):htT) prevSet = (unwords (reverse (itm:prevSet))) ++ " " ++ (show n) ++ "\n"++ (toString' htT prevSet)
toString' ((HasseTreeNode itm n child):htT) prevSet = (toString' child (itm:prevSet)) ++ (toString' htT prevSet)

-- removes the infrequent item sets from the Hasse Tree
removeInfrequent :: Int -> HasseTree -> Int -> HasseTree
removeInfrequent _ [] _ = []
removeInfrequent minSupport ht 1 = filter (\x -> (count x) >= minSupport) ht
removeInfrequent minSupport (htH@(HasseLeafNode _ _):htT) k = htH:(removeInfrequent minSupport htT k)
removeInfrequent minSupport ((HasseTreeNode itm n child):htT) k = [HasseTreeNode itm n (removeInfrequent minSupport child (k - 1))] ++ (removeInfrequent minSupport htT k)

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

resetLeafCounts :: HasseTree -> HasseTree
resetLeafCounts [] = []
resetLeafCounts ((HasseLeafNode itm n):tail) = (HasseLeafNode itm 0):(resetLeafCounts tail)
