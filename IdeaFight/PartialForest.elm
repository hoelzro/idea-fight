-- A partially ordered forest.  See blog post linked to in README
-- for details.


module IdeaFight.PartialForest exposing (Forest, choose, decodeJSON, fromList, getNextPair, topN)

import Json.Decode as Decode


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
                List.append rest [ reparentNode a b ] -- This should be impossible!

        [ Node value children ] ->
            [ Node value <| chooseNodes children choice ]

        _ ->
            nodes -- This should be impossible!


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


decodeValue : Decode.Decoder String
decodeValue = Decode.field "value" Decode.string


decodeChildren : Decode.Decoder (List (Node String))
decodeChildren = Decode.field "children" <| Decode.list <| Decode.lazy <| \_ -> decodeNode


decodeNode : Decode.Decoder (Node String)
decodeNode = Decode.map2 Node decodeValue decodeChildren


decodeJSON : Decode.Decoder (Forest String)
decodeJSON = Decode.map Forest <| Decode.list decodeNode
