module IdeaFight.Compete exposing (Model, Msg, decodeModel, encodeModel, init, subscriptions, update, view)

import Browser.Events as Events
import Char
import Html exposing (Html, br, button, div, li, ol, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import IdeaFight.PartialForest as Forest
import IdeaFight.Shuffle as Shuffle
import Json.Decode as Decode
import Json.Encode as Encode
import Random
import String


type Model
    = Uninitialized
    | Initialized (Forest.Forest String)


type Msg
    = ShuffledContents (List String)
    | Choice String
    | NoOp


init : String -> ( Model, Cmd Msg )
init contents =
    let
        lines =
            String.lines <| String.trim contents
    in
    ( Uninitialized, Random.generate ShuffledContents <| Shuffle.shuffle lines )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Uninitialized ->
            case msg of
                ShuffledContents contents ->
                    ( Initialized <| Forest.fromList contents, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Initialized forest ->
            case msg of
                Choice choice ->
                    ( Initialized <| Forest.choose forest choice, Cmd.none )

                _ ->
                    ( model, Cmd.none )


decodeKeyPress : String -> String -> Decode.Decoder Msg
decodeKeyPress left right =
  Decode.map (keyToMsg left right) <| Decode.field "key" Decode.string

keyToMsg : String -> String -> String -> Msg
keyToMsg left right code =
    if code == "1" then
        Choice left

    else if code == "2" then
        Choice right

    else
        NoOp


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Uninitialized ->
            Sub.none

        Initialized forest ->
            case Forest.getNextPair forest of
                Just ( left, right ) ->
                    Events.onKeyPress <| decodeKeyPress left right

                Nothing ->
                    Sub.none


chooser : Forest.Forest String -> Html Msg
chooser forest =
    case Forest.getNextPair forest of
        Just ( lhs, rhs ) ->
            div []
                [ text "Which of these ideas do you like better?"
                , br [] []
                , button [ onClick <| Choice lhs, class "button-primary" ] [ text lhs ]
                , button [ onClick <| Choice rhs, class "button-primary" ] [ text rhs ]
                ]

        Nothing ->
            text "Your ideas are totally ordered!"


topValuesSoFar : Forest.Forest String -> Html Msg
topValuesSoFar forest =
    let
        topValues =
            Forest.topN forest
    in
    case topValues of
        [] ->
            text "We haven't found the best idea yet - keep choosing!"

        _ ->
            div [] [ text "Your best ideas:", ol [] <| List.map (\value -> li [] [ text value ]) topValues ]


view : Model -> Html Msg
view model =
    case model of
        Uninitialized ->
            text ""

        Initialized forest ->
            div []
                [ chooser forest
                , br [] []
                , topValuesSoFar forest
                ]

decodeModel : Decode.Decoder Model
decodeModel = Decode.field "nodes" <| Decode.map Initialized Forest.decodeJSON


encodeModel : Model -> List (String, Encode.Value)
encodeModel model =
  case model of
    Uninitialized -> []
    Initialized forest ->
      let encodedForest = Forest.encodeJSON forest
      in [("nodes", encodedForest)]
