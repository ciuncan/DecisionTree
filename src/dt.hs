module DT
       (Name, Value, Domain, ClassLabel, 
        Attribute, Database(Db, attributes, rows, classDomain),
        DecisionTree(DecisionNode, LeafNode),
        availableClasses, build_dt, classify, toRules, 
        read', readDatabase) where

import System
import System.IO
import Data.List
import Data.Function

type Name       = String
type Value      = String
type Domain     = [Value]
type ClassLabel = String
type Row        = [Value]
type Attribute  = (Name, Domain)

data Database     = Db { attributes :: [Attribute], 
                         rows :: [(Row, ClassLabel)], 
                         classDomain :: [ClassLabel] }
                    
data DecisionTree =  DecisionNode Attribute [(Value, DecisionTree)] 
                   | LeafNode ClassLabel 
                     deriving (Show)
                    
hasNoAttribute             :: Database -> Bool
hasNoAttribute (Db [] _ _) =  True
hasNoAttribute _           =  False

hasNoRow             :: Database -> Bool
hasNoRow (Db _ [] _) =  True
hasNoRow _           =  False

availableClasses :: Database -> [(ClassLabel, Int)]
availableClasses (Db _ rows _) = 
  map (\ls -> (head ls,length ls)) $ groupBy (==) $ sort $ map snd rows

sub_database :: Database -> Attribute -> Value -> Database
sub_database db (name,_) v =
  let
    attrs = attributes db
    ind = findIndex ((==name).fst) attrs
  in case ind of 
    Nothing -> Db [] [] []
    Just i  -> Db { attributes  = withoutIndex i attrs, 
                    rows        = map (\(r,lbl) -> (withoutIndex i r, lbl)) $ 
                                  filter (\(r,lbl) -> r !! i == v) $ rows db,
                    classDomain = classDomain db }
               
build_dt :: Database -> DecisionTree
build_dt db =
  let 
    classes = availableClasses db
    majorityClass cls = fst $ maximumBy (compare `on` snd) cls
  in if length classes == 1
     then LeafNode (fst $ head classes)
     else if hasNoAttribute db
          then LeafNode (majorityClass classes)
          else 
            let
              attrs = attributes db
              a@(name,domain) = best_splitting_criterion db
              otherAttributes = filter ((/=name) . fst) attrs
              createDecisionNode :: Domain -> DecisionTree -> DecisionTree
              createDecisionNode [] node                       = node
              createDecisionNode (v:rest) (DecisionNode _ lst) = 
                let
                  subDb = sub_database db a v
                in if hasNoRow subDb
                   then 
                     let
                       leaf = LeafNode (majorityClass classes)
                     in createDecisionNode rest (DecisionNode a ((v,leaf):lst))
                   else 
                     let
                       node = build_dt subDb
                     in createDecisionNode rest (DecisionNode a ((v,node):lst))
            in createDecisionNode domain (DecisionNode a [])
              
best_splitting_criterion :: Database -> Attribute
best_splitting_criterion db@(Db attrs rows cD) =
  let 
    gains = map (\a -> (a, (entropyGain db a))) attrs
  in fst $ minimumBy (compare `on` snd) gains

  
pLogP :: Float -> Float
pLogP p = if p == 0
          then 0
          else p * log p / log 2
               
reduceByPLogP :: Float -> Float -> Float  
reduceByPLogP = (+) `on` pLogP

entropy      :: Database -> Attribute -> Float
entropy (Db attrs rws cD) (name,_) =
  let
    Just ind = findIndex ((==name).fst) attrs
    column = sort $ map ((!! ind).fst) rws
    counts = map (\x-> length x) $ groupBy (==) column
    total = (fromIntegral $ sum counts) :: Float
    freqs = map (\x -> fromIntegral x / total) counts
  in sum $ map pLogP freqs

jointEntropy :: Database -> Attribute -> Float
jointEntropy (Db attrs rws cD) (name,_) =
  let 
    Just ind = findIndex ((==name) . fst) attrs
    colAndClass = sort $ map (\(r,lbl) -> (r !! ind,lbl)) rws
    counts = map (\x-> length x) $ groupBy (==) colAndClass
    total = (fromIntegral $ sum counts) :: Float
    freqs = map (\x -> fromIntegral x / total) counts
  in sum $ map pLogP freqs

entropyGain :: Database -> Attribute -> Float
entropyGain db a = entropy db a - jointEntropy db a

classify :: [Attribute] -> DecisionTree -> Row -> ClassLabel
classify _ (LeafNode lbl) _ = lbl
classify attrs (DecisionNode (name,_) children) row =
  classify newAttrs subtree newRow
  where
    Just subtree = lookup v children
    Just ind     = findIndex ((==name).fst) attrs
    v            = row !! ind
    newRow       = withoutIndex ind row
    newAttrs     = withoutIndex ind attrs
    
    
toRules :: DecisionTree -> [String]
toRules = toRules' ""

toRules' :: String -> DecisionTree -> [String]
toRules' current (LeafNode lbl) = 
  if null current
  then ["All are " ++ lbl]
  else
    let len = length current
        curr = take (len - 5) current
    in [curr ++ " then " ++ lbl]
toRules' current (DecisionNode (name,_) children) =
  let next v = if null current 
               then "if " ++ name ++ "=" ++ v ++ " and "
               else current ++ name ++ "=" ++ v ++ " and "
  in foldr1 (++) (map (\(v,tree) -> toRules' (next v) tree) children)
      
withoutIndex :: Int -> [a] -> [a]
withoutIndex i list = take i list ++ drop (i+1) list

instance Show Database where
  show db = "Attributes: \n" ++ 
            (showEachLine $ attributes db) ++ 
            "\nRows: \n" ++
            (showEachLine $ rows db) ++
            "\nClass labels: " ++ show (classDomain db)
            where
              showEachLine :: (Show a) => [a] -> String
              showEachLine list = unlines $ map show list
              
read' :: String -> Database
read' contents = 
  let 
    (frst:rest) = lines contents
    initialAttrs = (map (\w -> (w,[])) $ words frst) :: [Attribute]
  in readRows (Db initialAttrs [] []) rest
    

readDatabase :: FilePath -> IO Database
readDatabase filename = do
  contents <- readFile filename
  return $ read' contents

readRows :: Database -> [String] -> Database
readRows db []            = db
readRows db (r:remaining) =
  let
    ws = words r
    row = init ws
    lbl = last ws
    currentAttrs = attributes db
    currentRows = rows db
    currentClassDomain = classDomain db
    newAttrs = zipWith addToDomain currentAttrs row
      where
        addToDomain (name, domain) v = 
          (name, if elem v domain then domain else v:domain)
    newRows = (row, lbl):currentRows
    newClassDomain = if lbl `elem` currentClassDomain 
                     then currentClassDomain 
                     else lbl:currentClassDomain
  in readRows (Db newAttrs newRows newClassDomain) remaining

