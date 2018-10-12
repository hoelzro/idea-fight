-- A partially ordered forest.  See blog post linked to in README
-- for details.


module IdeaFight.PartialForest exposing (Forest, choose, fromList, getNextPair, topN)


type Node a
    = Node a (List (Node a))


type Forest a
    = Forest (List (Node a))


makeNode : a -> Node a
makeNode value =
    Node value []


fromList : List a -> Forest a
fromList values =
    Forest <| List.map makeNode values


getNextPairNodes : List (Node a) -> Maybe ( a, a )
getNextPairNodes nodes =
    case nodes of
        (Node a _) :: (Node b _) :: _ ->
            Just ( a, b )

        [ Node _ children ] ->
            getNextPairNodes children

        _ ->
            Nothing


getNextPair : Forest a -> Maybe ( a, a )
getNextPair (Forest nodes) =
    getNextPairNodes nodes


reparentNode : Node a -> Node a -> Node a
reparentNode (Node value children) child =
    Node value (child :: children)


nodeValue : Node a -> a
nodeValue (Node value _) =
    value


chooseNodes : List (Node a) -> a -> List (Node a)
chooseNodes nodes choice =
    case nodes of
        a :: b :: rest ->
            if choice == nodeValue a then
                List.append rest [ reparentNode a b ]

            else if choice == nodeValue b then
                List.append rest [ reparentNode b a ]

            else
                Debug.crash "You somehow made an impossible choice"

        [ Node value children ] ->
            [ Node value <| chooseNodes children choice ]

        _ ->
            Debug.crash "You somehow called chooseNodes on a totally ordered forest"


choose : Forest a -> a -> Forest a
choose (Forest values) choice =
    Forest <| chooseNodes values choice


topNNodes : List (Node a) -> List a
topNNodes nodes =
    case nodes of
        [ Node value children ] ->
            value :: topNNodes children

        _ ->
            []


topN : Forest a -> List a
topN (Forest nodes) =
    topNNodes nodes
