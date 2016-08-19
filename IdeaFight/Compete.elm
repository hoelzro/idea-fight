module IdeaFight.Compete exposing (Model, Msg, init, update, subscriptions, view)

import IdeaFight.PartialForest as Forest
import IdeaFight.Shuffle as Shuffle

import Html.App as App
import Html exposing (Html, br, button, div, li, text, ul)
import Html.Events exposing (onClick)

import Random
import String

type alias Model = Forest.Forest String
type Msg = ShuffledContents (List String) | Choice String

init : String -> (Model, Cmd Msg)
init contents =
  let lines = String.lines contents
  in (Forest.empty, Random.generate ShuffledContents <| Shuffle.shuffle lines)

update : Msg -> Model -> (Model, Cmd Msg)
update msg forest =
  case msg of
    ShuffledContents contents -> (Forest.fromList contents, Cmd.none)
    Choice choice -> (Forest.choose forest choice, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

chooser : Forest.Forest String -> Html Msg
chooser forest =
  if Forest.isEmpty forest then
    div [] []
  else
    case Forest.getNextPair forest of
      Just (lhs, rhs) -> div [] [
        text "Which of these ideas do you like better?",
        br [] [],
        button [onClick <| Choice lhs] [text lhs],
        button [onClick <| Choice rhs] [text rhs]
      ]
      Nothing -> text "The forest is totally ordered!"

topValuesSoFar : Forest.Forest String -> Html Msg
topValuesSoFar forest =
  let topValues = Forest.topN forest
  in ul [] <| List.map (\value -> li [] [ text value ]) topValues

view : Model -> Html Msg
view forest =
  div [] [
    chooser forest,
    br [] [],
    topValuesSoFar forest
  ]

main : Program Never
main = App.program {
    init = init "one\ntwo\nthree\nfour\nfive\nsix\nseven\neight",
    update = update,
    subscriptions = subscriptions,
    view = view
  }
