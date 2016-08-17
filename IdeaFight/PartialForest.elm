-- a partially ordered forest
module IdeaFight.PartialForest exposing (Forest, empty, fromList, getNextPair, isEmpty)

type Node a = Node a (List (Node a))
type Forest a = Forest (List (Node a))

makeNode : a -> Node a
makeNode value = Node value []

fromList : List a -> Forest a
fromList values = Forest <| List.map makeNode values

empty : Forest a
empty = fromList []

isEmpty : Forest a -> Bool
isEmpty (Forest values) =
  case values of
    [] -> True
    _  -> False

getNextPair : Forest a -> (a, a)
getNextPair (Forest values) =
  case values of
    (Node a _) :: (Node b _) :: _ -> (a, b)
    _ -> Debug.crash "oh shit"
