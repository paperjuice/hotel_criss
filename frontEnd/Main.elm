import Html exposing (Html, text, div, img, button, span)
import Html.Attributes exposing (class, src, style)
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
    , map Rooms (s "rooms")
    ]

-- MODEL --
type Language = Ro | En
type alias Model =
  { route : Route
  , language: Language
  }

-- INIT --
init : Location -> (Model, Cmd msg)
init location =
  update (Path location) (Model Home Ro)


-- VIEW --
view : Model -> Html msg
view model =
  case model.route of
    Home     -> viewHome model
    Rooms    -> div [ ] [ text "Rooms" ]
    NotFound -> div [ ] [ text "Nope :(" ]


-- UPDATE --
update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
  case msg of
    Path location ->
      let
          newRoute = matchRoute (Debug.log "loc" location)
      in
          ( { model | route = newRoute }, Cmd.none )


---------------
-- HOME VIEW --
---------------
viewHome : Model -> Html msg
viewHome model =
  div [ class "home" ]
      [ header model
      ]

header : Model -> Html msg
header model =
  div [ class "header" ]
      [ div [ class "hotel-criss" ] [ text "Hotel Criss" ]
      , img [ class "hotel-stars", src "icons/hotel-star.svg" ] [ ]
      , button [ class "book" ] [ text "Rezerva" ]
      , div [ class "language" ] (languageColor model.language)
      ]

languageColor : Language -> List (Html msg)
languageColor language =
  case language of
    Ro ->
      languageButtons "#4d4d4d" "white"

    En ->
      languageButtons "white" "#4d4d4d"

languageButtons : String -> String -> List (Html msg)
languageButtons colorRo colorEn =
      [ button [ style [ ("color", colorRo) ] ] [ text "Ro" ]
      , span [ ] [ text "/" ]
      , button [ style [ ("color", colorEn) ] ] [ text "En" ]
      ]

