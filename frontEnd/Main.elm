import Html exposing (Html, text, div, img, button, span, iframe)
import Html.Events exposing (onClick)
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

googleMaps = "https://www.google.com/maps/embed?pb=!1m18!1m12!1m3!1d2848.939794849981!2d25.987970515523305!3d44.43439717910221!2m3!1f0!2f0!3f0!3m2!1i1024!2i768!4f13.1!3m3!1m2!1s0x40b200e31c40b727%3A0x18dec04ac0ffa6f2!2sBulevardul+Iuliu+Maniu+484%2C+Bucure%C8%99ti%2C+Romania!5e0!3m2!1sen!2sse!4v1529339983765"

-- TRANSLATION --
roomsRo = "Camere"
roomsEn = "Rooms"
roomsDescRo = "Laudam camere nitel, spunem cate sunt in total, si cateva vorbe despre cum e fiecare si ca preturile sunt accesible."
roomsDescEn = "We talk about how awesome the rooms are, how many are in total and perhaps a few words regarding the accessible prices we offer."

restRo = "Restaurant"
restEn = "Restaurant"
restDescRo = "Spunem despre mancarea pe care o ofera hotelul si contextul in care o poate servi (evenimente)."
restDescEn = "We should say a few words about the food and the context in which this is server, donno."

confRo = "Centru de conferinte"
confEn = "Conference center"
confDescRo = "Ceva despre fatul ca hotelul ofera spatii pentru conferinte."
confDescEn = "We offer spatious conference rooms up to 180 seats."

offerRo = "Oferte"
offerEn = "Special offers"
offerDescRo = "Hotelul organizeaza diferite eveniment in functie de perioada anului."
offerDescEn = "Over the year we organise various events in which we invite you to take part."


-- MSG --
type Msg
  = Path Location
  | Language


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
init : Location -> (Model, Cmd Msg)
init location =
  update (Path location) (Model Home Ro)


-- UPDATE --
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Path location ->
      let
          newRoute = matchRoute (Debug.log "loc" location)
      in
          ( { model | route = newRoute }, Cmd.none )

    Language ->
      let
          newLang =
            case model.language of
              Ro -> En
              En -> Ro
      in
          ( { model | language = newLang }, Cmd.none )


-- VIEW --
view : Model -> Html Msg
view model =
  case model.route of
    Home     -> viewHome model
    Rooms    -> div [ ] [ text "Rooms kfalsdjfkjsd" ]
    NotFound -> div [ ] [ text "Nope :(" ]


---------------
-- HOME VIEW --
---------------
viewHome : Model -> Html Msg
viewHome model =
  div [ class "home" ]
      [ header model
      , description model
      , container model
      , contact model
      , footer
      ]

header : Model -> Html Msg
header model =
  div [ class "header" ]
      [ div [ class "hotel-criss" ] [ text "Hotel Criss" ]
      , img [ class "hotel-stars", src "icons/hotel-star.svg" ] [ ]
      , button [ class "book" ]
               [ text (properLanguageText model.language "Rezerva" "Book") ]
      , div [ class "language", onClick Language ] (languageColor model.language)
      ]

description : Model -> Html Msg
description model =
  div [ class "description" ]
      [ div [ class "text" ]
            [ text (properLanguageText model.language "Aici putem pune  o scurta descriere despre Bucuresti, putin despre istoricul hotelului, unde este situtat in capitala, cateva vorbe despre numarul de camere si conditiil excelente pe care le ofere." "This is the exact version of the Romanian text but obviously in English.") ]
      , img [ class "break", src "icons/break.svg" ] [ ]
      ]

languageColor : Language -> List (Html Msg)
languageColor language =
  case language of
    Ro ->
      languageButtons "#4d4d4d" "white"

    En ->
      languageButtons "white" "#4d4d4d"

languageButtons : String -> String -> List (Html Msg)
languageButtons colorRo colorEn =
      [ button [ style [ ("color", colorRo) ] ] [ text "Ro" ]
      , span [ ] [ text "/" ]
      , button [ style [ ("color", colorEn) ] ] [ text "En" ]
      ]

container : Model -> Html Msg
container model =
  div [ class "frame" ]
      [ roomItem model
      , restaurantItem model
      , conferenceItem model
      , offerItem model
      ]

roomItem : Model -> Html Msg
roomItem model =
  div [ class "container" ]
      [ div [ class "left" ]
            [ div [ class "picture" ]
                  [ img [ class "room-block"
                        , src "https://i.imgur.com/bMYJQje.png"
                        ] [ ]
                  ]
            , descriptionItem
              model
              (properLanguageText model.language roomsRo roomsEn)
              (properLanguageText model.language roomsDescRo roomsDescEn)
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

restaurantItem : Model -> Html Msg
restaurantItem model =
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
            , descriptionItem
              model
              ( properLanguageText model.language restRo restEn )
              ( properLanguageText model.language restDescRo restDescEn )
            ]
      , div [ class "bottom" ]
            [ img [ class "rest-bot"
                  , src "https://i.imgur.com/l3YZ8Qe.jpg"
                  ] [ ]
            ]
      ]

conferenceItem : Model -> Html Msg
conferenceItem model =
  div [ class "container" ]
      [ div [ class "left" ]
            [ div [ class "picture" ]
                  [ img [ class "conf-left"
                        , src "https://i.imgur.com/XS6uWXP.jpg"
                        ] [ ]
                  ]
            , descriptionItem
              model
              ( properLanguageText model.language confRo confEn )
              ( properLanguageText model.language confDescRo confDescEn )
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

offerItem : Model -> Html Msg
offerItem model =
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
            , descriptionItem
              model
              ( properLanguageText model.language offerRo offerEn )
              ( properLanguageText model.language offerDescRo offerDescEn )

            ]
      , div [ class "bottom" ]
            [ img [ class "offer-bot"
                  , src "https://i.imgur.com/dINiFcv.jpg"
                  ] [ ]
            ]
      ]

contact : Model -> Html Msg
contact model =
  div [ class "contact" ]
      [ img [ src "icons/break.svg" ] [ ]
      , div [ class "title" ]
            [ text (properLanguageText model.language "Contactati-ne!" "Get in touch!")  ]
      , div [ class "description" ]
            [ text (properLanguageText
                    model.language
                    "Suntem mai mult ca fericiti sa stam la dispozitia dumneavoastra." 
                    "We are more than happy to assist you with any inquiries."
                    ) ]

      , div [ class "contacts" ]
            [ div [ class "phone" ]
                  [ div [ class "text" ]
                        [ text (properLanguageText
                                model.language
                                "Numar de telefon"
                                "Phone number"
                                ) ]
                  , div [ class "number" ]
                        [ text "021 317 53 19" ]
                  ]
            , div [ class "email" ]
                  [ div [ class "text" ] [ text "E-mail" ]
                  , div [ class "address" ] [ text "rezervari@hotelcriss.ro" ]
                  ]
            ]
      , iframe [ src googleMaps ] [ ]
      , detailButton model
      ]

descriptionItem : Model -> String -> String -> Html Msg
descriptionItem model title description=
  div [ class "text" ]
      [ div [ class "title" ] [ text title ]
      , div [ class "description" ]
            [ text  description ]
      , detailButton model
      ]

detailButton : Model -> Html Msg
detailButton model =
  div [ class "detail-button" ]
      [ text (properLanguageText model.language "Detalii" "Details") ]


footer : Html Msg
footer =
  div [ class "footer" ]
      [
      ]


properLanguageText : Language -> String -> String -> String
properLanguageText language textRo textEn =
  case language of
    En -> textEn
    Ro -> textRo

