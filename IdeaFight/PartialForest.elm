-- a partially ordered forest
module IdeaFight.PartialForest exposing (Forest, choose, drawForest, empty, fromList, getNextPair, isEmpty)

-- XXX DEBUG
import Html exposing (Html, li, ul, text)

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

drawNode : Node a -> Html msg
drawNode (Node value children) =
  li [] [ (text <| toString value), ul [] (List.map drawNode children) ]

drawForest : Forest a -> Html msg
drawForest (Forest nodes) =
  ul [] <| List.map drawNode nodes
