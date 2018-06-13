import Html exposing (Html, text, div)
import Navigation exposing (Location)
import UrlParser exposing (parseHash, s, oneOf, top, map)


main =
  Navigation.program Path
  { view = view
  , update = update
  , init = init
  , subscriptions = (\_->Sub.none)
  }

-- MSG --
type Msg
  = Path Location

-- ROUTE --
type Route
  = Home
  | Rooms
  | NotFound

matchRoute : Location -> Route
matchRoute location =
  case (parseHash matcher location) of
    Just route -> route
    Nothing    -> NotFound

matcher =
  oneOf
    [ map Home top
    , map Rooms (s "/rooms")
    ]

-- MODEL --
type alias Model =
  { route : Route
  }

-- INIT --
init : Location -> (Model, Cmd msg)
init location =
  update (Path location) (Model Home)


-- VIEW --
view : Model -> Html msg
view model =
  div [ ] [ text "hello" ]


-- UPDATE --
update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
  case msg of
    _-> ( model, Cmd.none )

