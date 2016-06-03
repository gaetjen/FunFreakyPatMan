module Apriori  where
import FileIO
import Data.List

type ItemSet = [String]     -- an item set is a collection of items, which are identified with strings
type Item = String
-- a node in the Hasse diagram search tree contains an item set, a counter for the support of that item set and a
-- pointer to the list of child nodes
data HasseNode = HasseTreeNode ItemSet Int HasseTree | HasseLeafNode ItemSet Int deriving Show
type HasseTree = [HasseNode]

itemSet :: HasseNode -> ItemSet
itemSet (HasseTreeNode is _ _) = is
itemSet (HasseLeafNode is _) = is

count :: HasseNode -> Int
count (HasseTreeNode _ n _) = n
count (HasseLeafNode _ n) = n

main path file minSupport = do
    dataSet <- importData path file
    let singletons = sortOn itemSet $ filter (\x -> (count x) >= minSupport) $ getSingletons dataSet
    let frequents = apriori dataSet singletons minSupport
    putStrLn (show singletons)


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
addItemToSingletons itm [] = [HasseLeafNode [itm] 1] -- itm is the first occurence in the database, we append it with support 1 to the HasseTree
addItemToSingletons itm (htH@(HasseLeafNode [singleton] n):htT)
    | itm == singleton = (HasseLeafNode [itm] (n+1)):htT -- the item matches the element in the node, so increase counter
    | itm /= singleton = htH:(addItemToSingletons itm htT) -- otherwise go through rest of list




