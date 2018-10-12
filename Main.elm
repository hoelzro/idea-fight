module Main exposing (Model(..), Msg(..), init, main, mapTEA, subscriptions, switchSubAppsIfNeeded, update, view)

import Html exposing (Html)
import IdeaFight.Compete as Compete
import IdeaFight.LandingPage as LandingPage


type Model
    = LandingPageModel LandingPage.Model
    | CompeteModel Compete.Model


type Msg
    = LandingPageMsg LandingPage.Msg
    | CompeteMsg Compete.Msg


mapTEA : (modela -> modelb) -> (msga -> msgb) -> ( modela, Cmd msga ) -> ( modelb, Cmd msgb )
mapTEA modelTransform msgTransform ( oldModel, oldCmd ) =
    ( modelTransform oldModel, Cmd.map msgTransform oldCmd )


init : ( Model, Cmd Msg )
init =
    mapTEA LandingPageModel LandingPageMsg <| LandingPage.init


switchSubAppsIfNeeded : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
switchSubAppsIfNeeded ( model, cmd ) =
    case model of
        LandingPageModel ( contents, True ) ->
            mapTEA CompeteModel CompeteMsg <| Compete.init contents

        _ ->
            ( model, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LandingPageMsg msg, LandingPageModel model ) ->
            switchSubAppsIfNeeded <| mapTEA LandingPageModel LandingPageMsg <| LandingPage.update msg model

        ( CompeteMsg msg, CompeteModel model ) ->
            mapTEA CompeteModel CompeteMsg <| Compete.update msg model

        ( _, _ ) ->
            Debug.crash "Model-Message mismatch"


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        LandingPageModel model ->
            Sub.map LandingPageMsg <| LandingPage.subscriptions model

        CompeteModel model ->
            Sub.map CompeteMsg <| Compete.subscriptions model


view : Model -> Html Msg
view model =
    case model of
        LandingPageModel model ->
            Html.map LandingPageMsg <| LandingPage.view model

        CompeteModel model ->
            Html.map CompeteMsg <| Compete.view model


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
