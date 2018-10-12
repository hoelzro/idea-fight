module Main exposing (Model(..), Msg(..), init, main, mapTEA, subscriptions, switchSubAppsIfNeeded, update, view)

import Browser
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
        ( LandingPageMsg landing_msg, LandingPageModel landing_model ) ->
            switchSubAppsIfNeeded <| mapTEA LandingPageModel LandingPageMsg <| LandingPage.update landing_msg landing_model

        ( CompeteMsg compete_msg, CompeteModel compete_model ) ->
            mapTEA CompeteModel CompeteMsg <| Compete.update compete_msg compete_model

        ( _, _ ) ->
            (model, Cmd.none) -- This should be impossible!


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        LandingPageModel landing_model ->
            Sub.map LandingPageMsg <| LandingPage.subscriptions landing_model

        CompeteModel compete_model ->
            Sub.map CompeteMsg <| Compete.subscriptions compete_model


view : Model -> Html Msg
view model =
    case model of
        LandingPageModel landing_model ->
            Html.map LandingPageMsg <| LandingPage.view landing_model

        CompeteModel compete_model ->
            Html.map CompeteMsg <| Compete.view compete_model


main : Program () Model Msg
main =
  Browser.element
        { init = always init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
