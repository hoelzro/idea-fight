-- a partially ordered forest
module IdeaFight.PartialForest exposing (Forest, empty, fromList, getNextPair, isEmpty)

type Forest a = Forest (List a)

fromList : List a -> Forest a
fromList values = Forest values

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
    a :: b :: _ -> (a, b)
    _ -> Debug.crash "oh shit"
