import Html.App as App
import Html exposing (Html, br, button, div, text)
import Html.Events exposing (onClick)

import IdeaFight.PartialForest as Forest
import IdeaFight.Shuffle as Shuffle

import Random
import Time

type alias Model = Forest.Forest Int
type Msg = ShuffleMe (List Int) | Advance

stepForest : Forest.Forest comparable -> Forest.Forest comparable
stepForest forest =
  case Forest.getNextPair forest of
    Just (a, b) -> Forest.choose forest <| max a b
    Nothing -> forest

init : (Model, Cmd Msg)
init = (Forest.empty, Random.generate ShuffleMe (Shuffle.shuffle [1..8]))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ShuffleMe values -> (Forest.fromList values, Cmd.none)
    Advance -> (stepForest model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

view : Model -> Html Msg
view model = div [] [
    button [ onClick Advance ] [ text "Advance" ],
    br [] [],
    text <| toString <| Forest.topNCount model,
    br [] [],
    Forest.drawForest model
  ]

main : Program Never
main = App.program {
    init = init,
    update = update,
    subscriptions = subscriptions,
    view = view
  }
