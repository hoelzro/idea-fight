module IdeaFight.LandingPage exposing (Model, Msg, init, update, subscriptions, view)

import Html.App as App
import Html exposing (Html, br, button, div, h1, hr, text, textarea)
import Html.Events exposing (onClick, onInput)

type alias Model = (String, Bool)
type Msg = UpdateContents String | Continue

init : (Model, Cmd Msg)
init = (("", False), Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case (msg, model) of
    (UpdateContents newContent, _) -> ((newContent, False), Cmd.none)
    (Continue, (contents, _)) -> ((contents, True), Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

inputs : Html Msg
inputs = div [] [
    textarea [ onInput UpdateContents ] [],
    br [] [],
    button [ onClick Continue ] [ text "Continue >" ]
  ]

view : Model -> Html Msg
view model = div [] [
    h1 [] [text "Idea Fight"],
    hr [] [],
    inputs
  ]
