-- a partially ordered forest
module IdeaFight.PartialForest exposing (Forest, choose, drawForest, empty, fromList, getNextPair, isEmpty, topN, topNCount)

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

getNextPairNodes : List (Node a) -> Maybe (a, a)
getNextPairNodes nodes =
  case nodes of
    (Node a _) :: (Node b _) :: _ -> Just (a, b)
    [(Node _ children)] -> getNextPairNodes children
    _ -> Nothing

getNextPair : Forest a -> Maybe (a, a)
getNextPair (Forest nodes) = getNextPairNodes nodes

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

chooseNodes : List (Node a) -> a -> List (Node a)
chooseNodes nodes choice =
  case nodes of
    a :: b :: rest ->
      if choice == nodeValue a
        then List.append rest [reparentNode a b]
        else if choice == nodeValue b then List.append rest [ reparentNode b a ]
        else Debug.crash "You somehow made an impossible choice"
    [(Node value children)] -> [Node value <| chooseNodes children choice]
    _ -> Debug.crash "You somehow called chooseNodes on a totally ordered forest"

choose : Forest a -> a -> Forest a
choose (Forest values) choice =
  Forest <| chooseNodes values choice

topNCount : Forest a -> Int
topNCount (Forest nodes) =
  let topNCountNodes = \nodes ->
      case nodes of
        [(Node _ children)] -> 1 + (topNCountNodes children)
        _ -> 0
  in topNCountNodes nodes

topNNodes : List (Node a) -> List a
topNNodes nodes =
  case nodes of
    [(Node value children)] -> value :: (topNNodes children)
    _ -> []

topN : Forest a -> List a
topN (Forest nodes) = topNNodes nodes
