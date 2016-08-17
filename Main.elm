import Html.App as App
import Html exposing (Html)

import IdeaFight.LandingPage as LandingPage

type Model = LandingPageModel LandingPage.Model
type Msg   = LandingPageMsg LandingPage.Msg

mapTEA : (modela -> modelb) -> (msga -> msgb) -> (modela, Cmd msga) -> (modelb, Cmd msgb)
mapTEA modelTransform msgTransform (oldModel, oldCmd) =
  (modelTransform oldModel, Cmd.map msgTransform oldCmd)

init : (Model, Cmd Msg)
init = mapTEA LandingPageModel LandingPageMsg <| LandingPage.init

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let _ = Debug.log "model" model in
  case (msg, model) of
    (LandingPageMsg msg, LandingPageModel model) -> mapTEA LandingPageModel LandingPageMsg <| LandingPage.update msg model

subscriptions : Model -> Sub Msg
subscriptions model =
  case model of
    LandingPageModel model -> Sub.map LandingPageMsg <| LandingPage.subscriptions model

view : Model -> Html Msg
view model =
  case model of
    LandingPageModel model -> App.map LandingPageMsg <| LandingPage.view model

main : Program Never
main = App.program {
    init = init,
    update = update,
    subscriptions = subscriptions,
    view = view
  }
