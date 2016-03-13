module Main(main) where

import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List

type Vertex = Int
data Edge = Edge !Vertex !Vertex deriving (Eq, Ord, Show, Read)
data Graph = Graph !Int !(Map Vertex [Vertex]) deriving (Eq, Ord, Show, Read)
type Label = Int
type Labeling = Map Vertex Label

mkGraph :: Int -> Graph
mkGraph n = Graph n M.empty

getNeighbors :: Graph -> Vertex -> [Vertex]
getNeighbors (Graph _ edges) v =
  case M.lookup v edges of
    Just list -> list
    Nothing -> []

graphLen :: Graph -> Int
graphLen (Graph n _) = n

addEdge :: Graph -> Edge -> Graph
addEdge (Graph n edges) (Edge u v) = Graph n (M.alter (addNeighbor v) u edges)
  where
    addNeighbor :: Vertex -> Maybe [Vertex] -> Maybe [Vertex]
    addNeighbor x Nothing = Just [x]
    addNeighbor x (Just xs) = Just (x:xs)

addBiEdge :: Graph -> Edge -> Graph
addBiEdge graph (Edge u v) =
  let one = addEdge graph (Edge u v)
      two = addEdge one (Edge v u)
  in two

parseInput :: String -> Graph
parseInput input =
  let lines_ = lines input
      n = read $ head lines_
      edges = map parseEdge $ tail lines_
  in
   foldl' addBiEdge (mkGraph n) edges
  where
    parseEdge :: String -> Edge
    parseEdge line = case lex line of
      [(ulex, rest)] -> case lex rest of
        [(vlex, _)] -> Edge (read ulex) (read vlex)

sortUnique :: (Ord a, Eq a) => [a] -> [a]
sortUnique = unique . sort
  where
    unique [] = []
    unique (x:xs) = x : unique (dropWhile (== x) xs)

solve :: Graph -> [Labeling]
solve graph = sortUnique $ loop 0 M.empty
  where
    loop :: Int -> Labeling -> [Labeling]
    loop n lbl | n == graphLen graph = return lbl
    loop n lbl = do
      choice <- genChoices (n+1) lbl
      choice' <- filterChoice choice lbl
      let (vertex, label) = choice'
          lbl' = M.insert vertex label lbl
      loop (n+1) lbl'
    genChoices :: Label -> Labeling -> [(Vertex, Label)]
    genChoices label lbl = flip mapMaybe [0..(graphLen graph-1)] $ \v -> case M.lookup v lbl of
      Just _ -> Nothing
      Nothing -> Just (v, label)
    filterChoice :: (Vertex, Label) -> Labeling -> [(Vertex, Label)]
    filterChoice (vertex, label) lbl =
      let neighbors = getNeighbors graph vertex
          labels = mapMaybe (flip M.lookup lbl) neighbors
          allOk = all lblOk labels
          lblOk l = l /= label + 1 && l /= label - 1
      in if allOk then [(vertex, label)]
         else []

printOutput :: [Labeling] -> String
printOutput = concat . intersperse "====" . map printOne
  where
    printOne :: Labeling -> String
    printOne lbl = concat . flip map (sort $ M.toList lbl) $ \(k, v) ->
      show k ++ " " ++ show v ++ "\n"

main :: IO ()
main = interact $ printOutput . solve . parseInput
