-- XXX you need at least three ideas (or whatever N is)
module IdeaFight.LandingPage exposing (Model, Msg, init, update, subscriptions, view)

import Html.App as App
import Html exposing (Html, a, br, button, div, form, h1, h4, hr, label, p, text, textarea)
import Html.Attributes exposing (class, href, for, name)
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

inputForm : Html Msg
inputForm =
  div [] [
    label [ for "idea-list" ] [ text "Enter one idea per line:" ],
    textarea [ onInput UpdateContents, class "u-full-width", name "idea-list" ] [],
    button [ onClick Continue, class "button-primary" ] [ text "Continue >" ]
  ]

descriptionParagraph : Html Msg
descriptionParagraph = p [] [
    text "This is a web application for helping you decide the priorities of your ideas. Here's a ",
    a [ href "http://hoelz.ro/blog/idea-fight" ] [ text "blog post" ],
    text " explaining the why and how."
  ]

view : Model -> Html Msg
view model = div [] [
    h1 [] [text "Idea Fight!"],
    h4 [] [text "What's This?"],
    descriptionParagraph,
    inputForm
  ]
