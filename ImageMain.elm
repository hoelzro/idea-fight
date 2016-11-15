import Html exposing (Html, img)
import Html.Attributes as Attributes

import IdeaFight.Compete as Compete

type alias Image = String

makeImage : Image -> Html msg
makeImage src =
  img [ Attributes.src src ] []

imageIdeaFunctions : Compete.IdeaFunctions Image
imageIdeaFunctions = {
    renderChoice = makeImage,
    renderResult = makeImage
  }

type Model = CompeteModel (Compete.Model Image)
type Msg = CompeteMsg (Compete.Msg Image)

images : List Image
images = [
    "http://localhost:5000/1.jpg",
    "http://localhost:5000/2.jpg",
    "http://localhost:5000/3.jpg",
    "http://localhost:5000/4.jpg",
    "http://localhost:5000/5.jpg",
    "http://localhost:5000/6.jpg",
    "http://localhost:5000/7.jpg",
    "http://localhost:5000/8.jpg"
  ]

init : (Model, Cmd Msg)
init =
    let (innerModel, cmd) = Compete.init imageIdeaFunctions images
    in (CompeteModel innerModel, Cmd.map CompeteMsg cmd)

update : Msg -> Model -> (Model, Cmd Msg)
update (CompeteMsg innerMsg) (CompeteModel innerModel) =
  let (newInnerModel, cmd) =  Compete.update innerMsg innerModel
  in (CompeteModel newInnerModel, Cmd.map CompeteMsg cmd)

subscriptions : Model -> Sub Msg
subscriptions (CompeteModel innerModel) =
  Sub.map CompeteMsg <| Compete.subscriptions innerModel

view : Model -> Html Msg
view (CompeteModel innerModel) =
  Html.map CompeteMsg <| Compete.view innerModel

main : Program Never Model Msg
main = Html.program {
    init = init,
    update = update,
    subscriptions = subscriptions,
    view = view
  }
