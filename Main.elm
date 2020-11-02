module Main exposing (Model(..), Msg(..), init, main, mapTEA, subscriptions, switchSubAppsIfNeeded, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import File exposing (File)
import File.Select as Select
import Json.Decode as Decode
import Json.Encode as Encode
import Task

import IdeaFight.Compete as Compete
import IdeaFight.LandingPage as LandingPage


type Model
    = LandingPageModel LandingPage.Model
    | CompeteModel Compete.Model


type Msg
    = LandingPageMsg LandingPage.Msg
    | CompeteMsg Compete.Msg
    | PerformImportMsg
    | FileSelectedForImportMsg File
    | FileLoadedMsg String


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

ifType : String -> Decode.Decoder a -> Decode.Decoder a
ifType expectedType successDecoder = Decode.field "__type__" Decode.string |> Decode.andThen (\gotType -> if gotType == expectedType then successDecoder else Decode.fail "type didn't match")

decodeLandingPageModel : Decode.Decoder Model
decodeLandingPageModel = ifType "landing_page" LandingPage.decodeModel |> Decode.map LandingPageModel

decodeCompeteModel : Decode.Decoder Model
decodeCompeteModel = ifType "compete" Compete.decodeModel |> Decode.map CompeteModel

-- XXX map the error
decodeModel : String -> Result Decode.Error Model
decodeModel =
  let modelDecoder = Decode.oneOf [ decodeLandingPageModel, decodeCompeteModel ]
  in Decode.decodeString modelDecoder

encodeModel : Model -> Encode.Value
encodeModel model =
  case model of
    LandingPageModel landing_model -> Encode.object <| ("__type__", Encode.string "landing_page") :: LandingPage.encodeModel landing_model
    CompeteModel compete_model -> Encode.object <| ("__type__", Encode.string "compete") :: Compete.encodeModel compete_model

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LandingPageMsg landing_msg, LandingPageModel landing_model ) ->
            switchSubAppsIfNeeded <| mapTEA LandingPageModel LandingPageMsg <| LandingPage.update landing_msg landing_model

        ( CompeteMsg compete_msg, CompeteModel compete_model ) ->
            mapTEA CompeteModel CompeteMsg <| Compete.update compete_msg compete_model

        ( PerformImportMsg, _ ) ->
          ( model, Select.file ["text/json"] FileSelectedForImportMsg )

        ( FileSelectedForImportMsg file, _ ) ->
          ( model, Task.perform FileLoadedMsg <| File.toString file)

        ( FileLoadedMsg content, _ ) ->
          case decodeModel content of
            Ok newModel -> (newModel, Cmd.none)
            Err err -> let _ = Debug.log "got error: " (Decode.errorToString err) in (model, Cmd.none) -- XXX handle me properly

        ( _, _ ) ->
            (model, Cmd.none) -- This should be impossible!


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        LandingPageModel landing_model ->
            Sub.map LandingPageMsg <| LandingPage.subscriptions landing_model

        CompeteModel compete_model ->
            Sub.map CompeteMsg <| Compete.subscriptions compete_model

importButton : Html Msg
importButton = button [ onClick PerformImportMsg, class "button-primary" ] [ text "Import" ]

view : Model -> Html Msg
view model =
    case model of
        LandingPageModel landing_model ->
            let inner = Html.map LandingPageMsg <| LandingPage.view landing_model
            in div [] [
              inner
            , importButton
            ]

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
