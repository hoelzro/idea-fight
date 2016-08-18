-- a partially ordered forest
module IdeaFight.PartialForest exposing (Forest, choose, drawForest, empty, fromList, getNextPair, isEmpty, topNCount)

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

reparentNode : Node a -> Node a -> Node a
reparentNode (Node value children) child = Node value (child :: children)

nodeValue : Node a -> a
nodeValue (Node value _) = value

drawNode : Node a -> Html msg
drawNode (Node value children) =
  li [] [ (text <| toString value), ul [] (List.map drawNode children) ]

drawForest : Forest a -> Html msg
drawForest (Forest nodes) =
  ul [] <| List.map drawNode nodes

choose : Forest a -> a -> Forest a
choose (Forest values) choice =
  case values of
    a :: b :: rest ->
      if choice == nodeValue a
        then Forest <| List.append rest [reparentNode a b]
        else if choice == nodeValue b then Forest <| List.append rest [ reparentNode b a ]
        else Debug.crash "You somehow made an impossible choice"
    _ -> Debug.crash "oh shit"

topNCount : Forest a -> Int
topNCount (Forest nodes) =
  let topNCountNodes = \nodes ->
      case nodes of
        [(Node _ children)] -> 1 + (topNCountNodes children)
        _ -> 0
  in topNCountNodes nodes
