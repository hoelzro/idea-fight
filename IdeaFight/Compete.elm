module IdeaFight.Compete exposing (Model, Msg, init, update, subscriptions, view)

import IdeaFight.PartialForest as Forest
import IdeaFight.Shuffle as Shuffle

import Html.App as App
import Html exposing (Html, br, button, div, text)

import Random
import String

type alias Model = (Forest.Forest String, Int)
type Msg = ShuffledContents (List String)

init : String -> (Model, Cmd Msg)
init contents =
  let lines = String.lines contents
  in ((Forest.empty, 3), Random.generate ShuffledContents <| Shuffle.shuffle lines)

update : Msg -> Model -> (Model, Cmd Msg)
update msg (_, numTop) =
  case msg of
    ShuffledContents contents -> ((Forest.fromList contents, numTop), Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

view : Model -> Html Msg
view (forest, numTop) =
  if Forest.isEmpty forest then
    div [] []
  else
    let (lhs, rhs) = Forest.getNextPair forest
    in div [] [
        text "Which of these ideas do you like better?",
        br [] [],
        button [] [text lhs],
        button [] [text rhs]
      ]

main : Program Never
main = App.program {
    init = init "one\ntwo\nthree\nfour\nfive\nsix\nseven\neight",
    update = update,
    subscriptions = subscriptions,
    view = view
  }
