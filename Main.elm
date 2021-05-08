port module Main exposing (Model(..), Msg(..), init, main, mapTEA, subscriptions, switchSubAppsIfNeeded, update, view)

import Browser
import Html exposing (Html, a, button, div, img, nav, p, text)
import Html.Attributes exposing (alt, attribute, class, href, src, style, target)
import Html.Events exposing (onClick)
import File exposing (File)
import File.Download as Download
import File.Select as Select
import Json.Decode as Decode
import Json.Encode as Encode
import Task

import IdeaFight.Compete as Compete
import IdeaFight.LandingPage as LandingPage


type Model
    = LandingPageModel LandingPage.Model
    | CompeteModel (Compete.Model String)
    | LoadOldState Model


type Msg
    = LandingPageMsg LandingPage.Msg
    | CompeteMsg (Compete.Msg String)
    | PerformImportMsg
    | PerformExportMsg
    | FileSelectedForImportMsg File
    | FileLoadedMsg String
    | LoadOldModelMsg
    | IgnoreOldModelMsg


mapTEA : (modela -> modelb) -> (msga -> msgb) -> ( modela, Cmd msga ) -> ( modelb, Cmd msgb )
mapTEA modelTransform msgTransform ( oldModel, oldCmd ) =
    ( modelTransform oldModel, Cmd.map msgTransform oldCmd )


init : Maybe String -> ( Model, Cmd Msg )
init previousSessionState =
    case previousSessionState of
      Nothing -> mapTEA LandingPageModel LandingPageMsg <| LandingPage.init
      Just serializedState -> case decodeModel serializedState of
        Ok state -> (LoadOldState state, Cmd.none)
        Err _ -> mapTEA LandingPageModel LandingPageMsg <| LandingPage.init -- XXX inform the user?

switchSubAppsIfNeeded : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
switchSubAppsIfNeeded ( model, cmd ) =
    case model of
        LandingPageModel ( contents, True ) ->
            mapTEA CompeteModel CompeteMsg <| Compete.init <| String.lines <| String.trim contents

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
    LoadOldState innerModel -> encodeModel innerModel

update_ : Msg -> Model -> ( Model, Cmd Msg )
update_ msg model =
    case ( msg, model ) of
        ( LandingPageMsg landing_msg, LandingPageModel landing_model ) ->
            switchSubAppsIfNeeded <| mapTEA LandingPageModel LandingPageMsg <| LandingPage.update landing_msg landing_model

        ( CompeteMsg compete_msg, CompeteModel compete_model ) ->
            mapTEA CompeteModel CompeteMsg <| Compete.update compete_msg compete_model

        ( PerformImportMsg, _ ) ->
          ( model, Select.file ["text/json"] FileSelectedForImportMsg )

        ( PerformExportMsg, _ ) ->
          let serializedModel = Encode.encode 0 <| encodeModel model
              downloadCmd = Download.string "idea-fight.json" "application/json" serializedModel
          in ( model, downloadCmd )

        ( FileSelectedForImportMsg file, _ ) ->
          ( model, Task.perform FileLoadedMsg <| File.toString file)

        ( FileLoadedMsg content, _ ) ->
          case decodeModel content of
            Ok newModel -> (newModel, Cmd.none)
            Err err -> let _ = Debug.log "got error: " (Decode.errorToString err) in (model, Cmd.none) -- XXX handle me properly

        ( LoadOldModelMsg, LoadOldState oldModel) ->
          ( oldModel, Cmd.none )

        ( IgnoreOldModelMsg, _) ->
          init Nothing

        ( _, _ ) ->
            (model, Cmd.none) -- This should be impossible!


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let (newModel, newMsg) = update_ msg model
      serializedModel = Encode.encode 0 <| encodeModel newModel
      saveMsg = saveState serializedModel
  in (newModel, Cmd.batch [newMsg, saveMsg])

subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        LandingPageModel landing_model ->
            Sub.map LandingPageMsg <| LandingPage.subscriptions landing_model

        CompeteModel compete_model ->
            Sub.map CompeteMsg <| Compete.subscriptions compete_model

        LoadOldState old_model ->
          Sub.none

importButton : Html Msg
importButton = button [ onClick PerformImportMsg, class "button-primary" ] [ text "Import" ]


exportButton : Html Msg
exportButton = button [ onClick PerformExportMsg, class "button-primary" ] [ text "Export" ]


repoLocation : String
repoLocation = "https://github.com/hoelzro/idea-fight"


ribbonImageLocation : String
ribbonImageLocation = "https://camo.githubusercontent.com/e7bbb0521b397edbd5fe43e7f760759336b5e05f/68747470733a2f2f73332e616d617a6f6e6177732e636f6d2f6769746875622f726962626f6e732f666f726b6d655f72696768745f677265656e5f3030373230302e706e67"


canonicalRibbonImageLocation : String
canonicalRibbonImageLocation = "https://s3.amazonaws.com/github/ribbons/forkme_right_green_007200.png"


view : Model -> Html Msg
view model =
    let
        navbar = nav [] [ importButton, exportButton ]
        ribbonImage = img [style "position" "absolute", style "top" "0", style "right" "0", style "border" "0", src ribbonImageLocation, alt "Fork me on GitHub", attribute "data-canonical-src" canonicalRibbonImageLocation ] []
        ribbonAnchor = a [href repoLocation, target "_blank" ] [ribbonImage]
        innerView = case model of
          LandingPageModel landing_model ->
              let inner = Html.map LandingPageMsg <| LandingPage.view landing_model
              in div [] [ inner ]

          CompeteModel compete_model ->
              let inner = Html.map CompeteMsg <| Compete.view compete_model
              in div [] [ inner ]
          LoadOldState oldModel ->
            let msg = p [] [text "It seems you have returned after an unfinished session; would you like to restore the previous session's state?"]
                loadButton = button [ onClick LoadOldModelMsg, class "button-primary" ] [text "Yes"]
                ignoreButton = button [ onClick IgnoreOldModelMsg, class "button-primary" ] [text "No"]
              in div [] [ msg, loadButton, ignoreButton ]
        oneHalfColumnDiv = div [class "one-half", class "column", style "margin-top" "25px"] [innerView]
        rowDiv = div [class "row"] [oneHalfColumnDiv]
        containerDiv = div [class "container"] [rowDiv]
    in div [] [ navbar, ribbonAnchor , containerDiv ]

port saveState : String -> Cmd msg

main : Program (Maybe String) Model Msg
main =
  Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
