import Html.App as App
import Html exposing (Html, br, div, text)

import IdeaFight.PartialForest as Forest
import IdeaFight.Shuffle as Shuffle

import Random
import Time

type alias Model = Forest.Forest Int
type Msg = ShuffleMe (List Int) | Tick

stepForest : Forest.Forest comparable -> Forest.Forest comparable
stepForest forest =
  let (a, b) = Forest.getNextPair forest
  in Forest.choose forest <| max a b

init : (Model, Cmd Msg)
init = (Forest.empty, Random.generate ShuffleMe (Shuffle.shuffle [1..8]))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ShuffleMe values -> (Forest.fromList values, Cmd.none)
    Tick -> (stepForest model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ = Time.every (Time.second * 5) <| always Tick

view : Model -> Html Msg
view model = div [] [
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
