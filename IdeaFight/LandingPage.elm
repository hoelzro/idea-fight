module IdeaFight.LandingPage exposing (Model, Msg, decodeModel, encodeModel, init, subscriptions, update, view)

import Html exposing (Html, a, br, button, div, form, h1, h4, hr, label, p, text, textarea)
import Html.Attributes exposing (class, for, href, name, target)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode
import Json.Encode as Encode


type alias Model =
    ( String, Bool )


type Msg
    = UpdateContents String
    | Continue


init : ( Model, Cmd Msg )
init =
    ( ( "", False ), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( UpdateContents newContent, _ ) ->
            ( ( newContent, False ), Cmd.none )

        ( Continue, ( contents, _ ) ) ->
            ( ( contents, True ), Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


inputForm : String -> Html Msg
inputForm contents =
    div []
        [ label [ for "idea-list" ] [ text "Enter one idea per line:" ]
        , textarea [ onInput UpdateContents, class "u-full-width", name "idea-list" ] [ text contents ]
        , button [ onClick Continue, class "button-primary" ] [ text "Continue >" ]
        ]


descriptionParagraph : Html Msg
descriptionParagraph =
    p []
        [ text "This is a web application for helping you decide the priorities of your ideas. Here's a "
        , a [ href "http://hoelz.ro/blog/idea-fight", target "_blank" ] [ text "blog post" ]
        , text " explaining the why and how."
        ]


view : Model -> Html Msg
view (contents, _) =
    div []
        [ h1 [] [ text "Idea Fight!" ]
        , h4 [] [ text "What's This?" ]
        , descriptionParagraph
        , inputForm contents
        ]

decodeModel : Decode.Decoder Model
decodeModel = Decode.field "content" <| Decode.map2 Tuple.pair Decode.string <| Decode.succeed False


encodeModel : Model -> List (String, Encode.Value)
encodeModel (contents, _) =
  let encodedContents = Encode.string contents
  in [("content", encodedContents)]
