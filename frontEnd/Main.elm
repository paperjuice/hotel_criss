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


-- UPDATE --
update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
  case msg of
    Path location ->
      let
          newRoute = matchRoute (Debug.log "loc" location)
      in
          ( { model | route = newRoute }, Cmd.none )


-- VIEW --
view : Model -> Html msg
view model =
  case model.route of
    Home     -> viewHome model
    Rooms    -> div [ ] [ text "Rooms kfalsdjfkjsd" ]
    NotFound -> div [ ] [ text "Nope :(" ]


---------------
-- HOME VIEW --
---------------
viewHome : Model -> Html msg
viewHome model =
  div [ class "home" ]
      [ header model
      , description
      , container
      , contact
      , footer
      ]

header : Model -> Html msg
header model =
  div [ class "header" ]
      [ div [ class "hotel-criss" ] [ text "Hotel Criss" ]
      , img [ class "hotel-stars", src "icons/hotel-star.svg" ] [ ]
      , button [ class "book" ] [ text "Rezerva" ]
      , div [ class "language" ] (languageColor model.language)
      ]

description : Html msg
description =
  div [ class "description" ]
      [ div [ class "text" ]
            [ text "Aici putem pune  o scurta descriere despre Bucuresti, putin despre istoricul hotelului, unde este situtat in capitala, cateva vorbe despre numarul de camere si conditiil excelente pe care le ofere." ]
      , img [ class "break", src "icons/break.svg" ] [ ]
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

container : Html msg
container =
  div [ class "frame" ]
      [ roomItem
      , restaurantItem
      , conferenceItem
      , offerItem
      ]

roomItem : Html msg
roomItem =
  div [ class "container" ]
      [ div [ class "left" ]
            [ div [ class "picture" ]
                  [ img [ class "room-block"
                        , src "https://i.imgur.com/bMYJQje.png"
                        ] [ ]
                  ]
            , descriptionItem "Camere" "Laudam camere nitel, spunem cate sunt in total, si cateva vorbe despre cum e fiecare si ca preturile sunt accesible."
            ]
      , div [ class "right" ]
            [ img [ class "room-right"
                  , src "https://i.imgur.com/ttSkgoA.jpg"
                  ] [ ]
            ]
      , div [ class "bottom" ]
            [ img [ class "room-bot"
                  , src "https://i.imgur.com/69P9RmO.jpg"
                  ] [ ]
            ]
      ]

restaurantItem : Html msg
restaurantItem =
  div [ class "container" ]
      [ div [ class "right" ]
            [ img [ class "rest-right"
                  , src "https://i.imgur.com/pFpC8Pp.jpg"
                  ] [ ]
            ]
      , div [ class "left" ]
            [ div [ class "picture" ]
                  [ img [ class "rest-left"
                        , src "https://i.imgur.com/1GGzLwD.jpg"
                        ] [ ]
                  ]
            , descriptionItem "Restaurant" "Spunem despre mancarea pe care o ofera hotelul si contextul in care o poate servi (evenimente)."
            ]
      , div [ class "bottom" ]
            [ img [ class "rest-bot"
                  , src "https://i.imgur.com/l3YZ8Qe.jpg"
                  ] [ ]
            ]
      ]

conferenceItem : Html msg
conferenceItem =
  div [ class "container" ]
      [ div [ class "left" ]
            [ div [ class "picture" ]
                  [ img [ class "conf-left"
                        , src "https://i.imgur.com/XS6uWXP.jpg"
                        ] [ ]
                  ]
            , descriptionItem "Centru de conferinte" "Ceva despre fatul ca hotelul ofera spatii pentru conferinte."
            ]
      , div [ class "right" ]
            [ img [ class "conf-right"
                  , src "https://i.imgur.com/D9hXKgc.jpg"
                  ] [ ]
            ]
      , div [ class "bottom" ]
            [ img [ class "conf-bot"
                  , src "https://i.imgur.com/8TBYOGM.jpg"
                  ] [ ]
            ]
      ]

offerItem : Html msg
offerItem =
  div [ class "container" ]
      [ div [ class "right" ]
            [ img [ class "offer-right"
                  , src "https://i.imgur.com/lSW3WK2.jpg"
                  ] [ ]
            ]
      , div [ class "left" ]
            [ div [ class "picture" ]
                  [ img [ class "offer-left"
                        , src "https://i.imgur.com/V24BLy2.jpg"
                        ] [ ]
                  ]
            , descriptionItem "Oferte" "Hotelul organizeaza diferite eveniment in functie de perioada anului."
            ]
      , div [ class "bottom" ]
            [ img [ class "offer-bot"
                  , src "https://i.imgur.com/dINiFcv.jpg"
                  ] [ ]
            ]
      ]

contact =
  div [ class "contact" ]
      [ div [ class "title" ] [ text "Contact" ]
      , img [ src "https://i.imgur.com/p8dr1yh.png"] [ ]
      , detailButton
      ]

descriptionItem : String -> String -> Html msg
descriptionItem title description=
  div [ class "text" ]
      [ div [ class "title" ] [ text title ]
      , div [ class "description" ]
            [ text  description ]
      , detailButton
      ]

detailButton : Html msg
detailButton =
  div [ class "detail-button" ] [ text "Detalii" ]


footer : Html msg
footer =
  div [ class "footer" ]
      [
      ]
