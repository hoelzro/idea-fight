module IdeaFight.Compete exposing (Model, Msg, init, update, subscriptions, view)

import Html.App as App
import Html exposing (Html, text)

import String

type alias Model = List String
type alias Msg = ()

init : String -> (Model, Cmd Msg)
init contents = (String.split "\n" contents, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = (model, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

view : Model -> Html Msg
view model = text <| toString model
