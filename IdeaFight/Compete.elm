module IdeaFight.Compete exposing (Model, Msg, init, update, subscriptions, view)

import IdeaFight.PartialForest as Forest
import IdeaFight.Shuffle as Shuffle

import Html.App as App
import Html exposing (Html, text)

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
view model = text <| toString model

main : Program Never
main = App.program {
    init = init "one\ntwo\nthree\nfour\nfive\nsix\nseven\neight",
    update = update,
    subscriptions = subscriptions,
    view = view
  }
