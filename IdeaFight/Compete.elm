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

type Model idea
    = Uninitialized
    | Initialized (Forest.Forest idea)


type Msg idea
    = ShuffledContents (List idea)
    | Choice idea
    | NoOp

type alias Renderer idea = idea -> Html (Msg idea)

init : List idea -> ( Model idea, Cmd (Msg idea) )
init lines =
    ( Uninitialized, Random.generate ShuffledContents <| Shuffle.shuffle lines )


update : Msg idea -> Model idea -> ( Model idea, Cmd (Msg idea) )
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


decodeKeyPress : idea -> idea -> Decode.Decoder (Msg idea)
decodeKeyPress left right =
  Decode.map (keyToMsg left right) <| Decode.field "key" Decode.string

keyToMsg : idea -> idea -> String -> Msg idea
keyToMsg left right code =
    if code == "1" then
        Choice left

    else if code == "2" then
        Choice right

    else
        NoOp


subscriptions : Model idea -> Sub (Msg idea)
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


chooser : Renderer idea -> Forest.Forest idea -> Html (Msg idea)
chooser render forest =
    case Forest.getNextPair forest of
        Just ( lhs, rhs ) ->
            div []
                [ text "Which of these ideas do you like better?"
                , br [] []
                , button [ onClick <| Choice lhs, class "button-primary" ] [ render lhs ]
                , button [ onClick <| Choice rhs, class "button-primary" ] [ render rhs ]
                ]

        Nothing ->
            text "Your ideas are totally ordered!"


topValuesSoFar : Renderer idea -> Forest.Forest idea -> Html (Msg idea)
topValuesSoFar render forest =
    let
        topValues =
            Forest.topN forest
    in
    case topValues of
        [] ->
            text "We haven't found the best idea yet - keep choosing!"

        _ ->
            div [] [ text "Your best ideas:", ol [] <| List.map (\value -> li [] [ render value ]) topValues ]


view : Renderer idea -> Renderer idea -> Model idea -> Html (Msg idea)
view renderChoice renderTopValue model =
    case model of
        Uninitialized ->
            text ""

        Initialized forest ->
            div []
                [ chooser renderChoice forest
                , br [] []
                , topValuesSoFar renderTopValue forest
                ]

decodeModel : Decode.Decoder (Model String)
decodeModel = Decode.field "nodes" <| Decode.map Initialized Forest.decodeJSON


encodeModel : Model String -> List (String, Encode.Value)
encodeModel model =
  case model of
    Uninitialized -> []
    Initialized forest ->
      let encodedForest = Forest.encodeJSON forest
      in [("nodes", encodedForest)]
