module IdeaFight.Compete exposing (Model, Msg, init, subscriptions, update, view)

import Char
import Html exposing (Html, br, button, div, li, ol, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import IdeaFight.PartialForest as Forest
import IdeaFight.Shuffle as Shuffle
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
                    ( Initialized <| Forest.fromList [], Cmd.none ) -- This should be impossible!

        Initialized forest ->
            case msg of
                Choice choice ->
                    ( Initialized <| Forest.choose forest choice, Cmd.none )

                _ ->
                    ( Initialized <| Forest.fromList [], Cmd.none ) -- This should be impossible!


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

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
