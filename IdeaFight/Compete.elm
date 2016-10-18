module IdeaFight.Compete exposing (IdeaFunctions, Model, Msg, init, update, subscriptions, view)

import IdeaFight.PartialForest as Forest
import IdeaFight.Shuffle as Shuffle

import Html exposing (Html, br, button, div, li, ol, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)

import Char
import Keyboard
import Random

type alias IdeaFunctions a = {
    renderChoice : a -> Html (Msg a),
    renderResult : a -> Html (Msg a)
  }

type Model a = Uninitialized (IdeaFunctions a) | Initialized (IdeaFunctions a) (Forest.Forest a)
type Msg a = ShuffledContents (List a) | Choice a | NoOp

init : IdeaFunctions a -> List a -> (Model a, Cmd (Msg a))
init funcs ideas = (Uninitialized funcs, Random.generate ShuffledContents <| Shuffle.shuffle ideas)

update : Msg a -> Model a -> (Model a, Cmd (Msg a))
update msg model =
  case model of
    Uninitialized funcs ->
      case msg of
        ShuffledContents contents -> (Initialized funcs <| Forest.fromList contents, Cmd.none)
        _ -> Debug.crash "Somehow you got a non-initialization message on an uninitialized state"
    Initialized funcs forest ->
      case msg of
        Choice choice -> (Initialized funcs <| Forest.choose forest choice, Cmd.none)
        _ -> Debug.crash "Somehow you got an initialization message on an initialized state"

mapKeyPresses : a -> a -> Keyboard.KeyCode -> Msg a
mapKeyPresses left right code =
  if code == Char.toCode '1' then Choice left
  else if code == Char.toCode '2' then Choice right
  else NoOp

subscriptions : Model a -> Sub (Msg a)
subscriptions model =
  case model of
    Uninitialized _ -> Sub.none
    Initialized _ forest ->
      case Forest.getNextPair forest of
        Just (left, right) -> Keyboard.presses <| mapKeyPresses left right
        Nothing -> Sub.none

chooser : IdeaFunctions a -> Forest.Forest a -> Html (Msg a)
chooser {renderChoice} forest =
  case Forest.getNextPair forest of
    Just (lhs, rhs) -> div [] [
      text "Which of these ideas do you like better?",
      br [] [],
      button [onClick <| Choice lhs, class "button-primary"] [renderChoice lhs],
      button [onClick <| Choice rhs, class "button-primary"] [renderChoice rhs]
    ]
    Nothing -> text "Your ideas are totally ordered!"

topValuesSoFar : IdeaFunctions a -> Forest.Forest a -> Html (Msg a)
topValuesSoFar {renderResult} forest =
  let topValues = Forest.topN forest
  in case topValues of
      []        -> text "We haven't found the best idea yet - keep choosing!"
      topValues -> div [] [ text "Your best ideas:", ol [] <| List.map (\value -> li [] [ renderResult value ]) topValues ]

view : Model a -> Html (Msg a)
view model =
  case model of
    Uninitialized _ -> text ""
    Initialized funcs forest ->
      div [] [
        chooser funcs forest,
        br [] [],
        topValuesSoFar funcs forest
      ]
