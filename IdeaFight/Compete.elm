module IdeaFight.Compete exposing (Model, Msg, init, update, subscriptions, view)

import IdeaFight.PartialForest as Forest
import IdeaFight.Shuffle as Shuffle

import Html exposing (Html, br, button, div, li, ol, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

import Char
import Keyboard
import Random
import String

type Model a = Uninitialized | Initialized (Forest.Forest a)
type Msg a = ShuffledContents (List a) | Choice a | NoOp

init : String -> (Model String, Cmd (Msg String))
init contents =
  let lines = String.lines <| String.trim contents
  in (Uninitialized, Random.generate ShuffledContents <| Shuffle.shuffle lines)

update : Msg a -> Model a -> (Model a, Cmd (Msg a))
update msg model =
  case model of
    Uninitialized ->
      case msg of
        ShuffledContents contents -> (Initialized <| Forest.fromList contents, Cmd.none)
        _ -> Debug.crash "Somehow you got a non-initialization message on an uninitialized state"
    Initialized forest ->
      case msg of
        Choice choice -> (Initialized <| Forest.choose forest choice, Cmd.none)
        _ -> Debug.crash "Somehow you got an initialization message on an initialized state"

mapKeyPresses : a -> a -> Keyboard.KeyCode -> Msg a
mapKeyPresses left right code =
  if code == Char.toCode '1' then Choice left
  else if code == Char.toCode '2' then Choice right
  else NoOp

subscriptions : Model a -> Sub (Msg a)
subscriptions model =
  case model of
    Uninitialized -> Sub.none
    Initialized forest ->
      case Forest.getNextPair forest of
        Just (left, right) -> Keyboard.presses <| mapKeyPresses left right
        Nothing -> Sub.none

chooser : Forest.Forest a -> Html (Msg a)
chooser forest =
  case Forest.getNextPair forest of
    Just (lhs, rhs) -> div [] [
      text "Which of these ideas do you like better?",
      br [] [],
      button [onClick <| Choice lhs, class "button-primary"] [text <| toString lhs],
      button [onClick <| Choice rhs, class "button-primary"] [text <| toString rhs]
    ]
    Nothing -> text "Your ideas are totally ordered!"

topValuesSoFar : Forest.Forest a -> Html (Msg a)
topValuesSoFar forest =
  let topValues = Forest.topN forest
  in case topValues of
      []        -> text "We haven't found the best idea yet - keep choosing!"
      topValues -> div [] [ text "Your best ideas:", ol [] <| List.map (\value -> li [] [ text <| toString value ]) topValues ]

view : Model a -> Html (Msg a)
view model =
  case model of
    Uninitialized -> text ""
    Initialized forest ->
      div [] [
        chooser forest,
        br [] [],
        topValuesSoFar forest
      ]
