module IdeaFight.Compete exposing (Model, Msg, init, update, subscriptions, view)

import Html.App as App
import Html exposing (Html, text)

type alias Model = ()
type alias Msg = ()

init : (Model, Cmd Msg)
init = ((), Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

view : Model -> Html Msg
view model = text "compete!"
