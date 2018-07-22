import Html exposing (Html, text, div, img, button, span, iframe, a, span, p, input)
import Html.Events exposing (onClick)
import Html.Attributes exposing (class, src, style, href, type_, placeholder)
import Navigation exposing (Location)
import UrlParser exposing (parseHash, s, oneOf, top, map)
import Dom.Scroll exposing (toTop)
import Task exposing (attempt)

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
roomsDescRo = "Relaxati-va in confortul oferit de camerele noastre cu pat dublu matrimonial amenajate cu mobila de cea mai buna calitate. Decorate in culori subtile camerele noastre superioare sunt mobilate elegant si va ofera multiple facilitati. Aici veti gasi tot  ceea ce aveti nevoie pentru a face afaceri si pentru relaxare."
roomsDescEn = "We talk about how awesome the rooms are, how many are in total and perhaps a few words regarding the accessible prices we offer."

restRo = "Restaurant"
restEn = "Restaurant"
restDescRo = "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet nec, vulputate eget, arcu."
restDescEn = "We should say a few words about the food and the context in which this is server, donno."

confRo = "Centru de conferinte"
confEn = "Conference center"
confDescRo = "Ceva despre fatul ca hotelul ofera spatii pentru conferinte."
confDescEn = "We offer spatious conference rooms up to 180 seats."

offerRo = "Oferte"
offerEn = "Special offers"
offerDescRo = "Hotelul organizeaza diferite eveniment in functie de perioada anului."
offerDescEn = "Over the year we organise various events in which we invite you to take part."

dummyText = "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. "


-- MSG --
type Msg
  = Path Location
  | Language
  | GoHome
  | RoomsDetails
  | RestaurantDetails
  | ConferenceDetails
  | PhGalleryDetails
  | OffersDetails
  | ContactDetails
  | DoNothing

type Bool = True | False


-- ROUTE --
type Route
  = Home
  | Rooms
  | Restaurant
  | Conference
  | Offers
  | Contact
  | PhGallery
  | RoomCM_3
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
    , map Rooms (s "camere")
    , map Contact (s "contact")
    , map Restaurant (s "restaurant")
    , map Conference (s "sali-conferinta")
    , map PhGallery (s "galerie-foto")
    , map RoomCM_3 ( s "rooms/camera_matrimoniala_3s")
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

    -- TODO: Path-ul sa se schimbe in functie de language. Eg. camere/rooms
    GoHome ->
      ( { model | route = Home }
      , Navigation.newUrl("#/")
      )

    RoomsDetails ->
      ( { model | route = Rooms }
      , Cmd.batch
        [ navNewUrl model "#/camere" "#/rooms"
        , cmdToTop
        ]
      )

    RestaurantDetails ->
      ( { model | route = Restaurant }
      , Cmd.batch
        [ navNewUrl model "#/restaurant" "#/restaurant"
        , cmdToTop
        ]
      )

    ConferenceDetails ->
      ( { model | route = Conference }
      , Cmd.batch
        [ navNewUrl model "#/sali-conferinta" "#/conference-rooms"
        , cmdToTop
        ]
      )

    OffersDetails ->
      ( { model | route = Offers }
      , Cmd.batch
        [ navNewUrl model "#/oferte" "#/offers"
        , cmdToTop
        ]
      )

    ContactDetails ->
      ( { model | route = Contact }
      , Cmd.batch
        [ navNewUrl model "#/contacte" "#/contacts"
        , cmdToTop
        ]
      )

    PhGalleryDetails ->
      ( { model | route = PhGallery }
      , Cmd.batch
        [ navNewUrl model "#/galerie-foto" "#/photo-gallery"
        , cmdToTop
        ]
      )

    DoNothing ->
      ( model, Cmd.none)


cmdToTop : Cmd Msg
cmdToTop =
  toTop "main-body"
  |> attempt (\_ -> DoNothing)

navNewUrl : Model -> String -> String -> Cmd msg
navNewUrl model roPath enPath =
  Navigation.newUrl ( properLanguageText model.language roPath enPath )


-- VIEW --
view : Model -> Html Msg
view model =
  div [ ]
      [ header (Debug.log " he" model)
      , ( case model.route of
              Home       -> viewHome model
              Rooms      -> roomPage
              Restaurant -> restaurantPage
              Conference -> conferencePage
              Offers     -> div [ ] [ text "Offers" ]
              Contact    -> div [ ] [ text "Contacts" ]
              PhGallery  -> photoGalleryPage
              RoomCM_3  -> camera_matrimoniala_3
              NotFound   -> div [ ] [ text "Nope :(" ]
        )
      , subscribe
      , footer
      ]


---------------
-- HOME VIEW --
---------------
viewHome : Model -> Html Msg
viewHome model =
  div [ class "home" ]
      [ intro
      , introText model
      , roomItem model
      , restaurantItem model
      , conferenceItem model
      , contact model
      ]

header : Model -> Html Msg
header model =
  div [ class "header" ]
      [ button [ class "home", onClick GoHome ] [ text "HC" ]
      , div [ class "left-cont" ]
            [ button [ class "rooms", onClick RoomsDetails ] [ text "Camere" ]
            , button [ class "conference", onClick ConferenceDetails ]
                     [ text "Sali Conferinta" ]
            , button [ class "restaurant", onClick RestaurantDetails ]
                     [ text "Restaurant" ]
            ]
      , button [ class "book" ]
               [ text (properLanguageText model.language "Rezerva" "Book") ]
      , div [ class "right-cont" ]
            [ button [ class "offers" ] [ text "Offerte" ]
            , button [ class "photos", onClick PhGalleryDetails ] [ text "Galerie Photo" ]
            , button [ class "contacts" ] [ text "Contacte" ]
            ]
      , div [ class "language", onClick Language ] (languageColor model.language)
      ]


intro : Html msg
intro =
  div [ class "intro" ]
      [ img [ class "bg-image"
            , src "https://i.imgur.com/zx2HzOC.jpg" ]  [ ]
      , div [ class "title" ]   [ text "Hotel Criss" ]
      , img [ class "stars", src "icons/hotel-star.svg" ]       [ ]
      , div [ class "tagline" ]     [ text "Here we put the tagline" ]
      , div [ class "description" ] [ text "Here we have a short description" ]
      , img [ class "scroll", src "icons/scroll.svg" ] [ ]
      ]


introText : Model -> Html Msg
introText model =
  div [ class "intro-text-container" ]
      [ div [ class "intro-text" ]
            [ span [ class "title" ] [ text "Hotel Criss " ],
              text (properLanguageText model.language "aici putem pune  o scurta descriere despre Bucuresti, putin despre istoricul hotelului, unde este situtat in capitala, cateva vorbe despre numarul de camere si conditiil excelente pe care le ofere." "This is the exact version of the Romanian text but obviously in English.")
            ]
      ]

-- ROOM --
roomItem : Model -> Html Msg
roomItem model =
  div [ class "room-item" ]
      [ div [ class "title" ]
            [ text "Camere" ]
      , div [ class "description" ]
            [ text roomsDescRo ]
      , div [ class "picture-container" ]
            [ div [ class "picture" ]
                  [ div [ class "title" ] [ text "Camera matrimoniala/twin  ★ ★ ★ "]
                  , img [ src "https://i.imgur.com/DjV2Tk0.jpg" ] [ ]
                  , div [ class "text" ] [ text "Rezerva acum!" ]
                  ]
            , div [ class "picture" ]
                  [ div [ class "title" ] [ text "Camera matrimoniala/twin Superioara  ★ ★ ★ "]
                  , img [ src "https://i.imgur.com/2ZCWb0g.jpg" ] [ ]
                  , div [ class "text" ] [ text "Rezerva acum!" ]
                  ]
             , div [ class "picture" ]
                  [ div [ class "title" ] [ text "Camera matrimoniala/twin  ★ ★ ★ ★ "]
                  , img [ src "https://i.imgur.com/S2QS5Xv.jpg" ] [ ]
                  , div [ class "text" ] [ text "Rezerva acum!" ]
                  ]
             , div [ class "picture" ]
                  [ div [ class "title" ] [ text "Apartament  ★ ★ ★ ★ "]
                  , img [ src "https://i.imgur.com/Ea1kShC.jpg" ] [ ]
                  , div [ class "text" ] [ text "Rezerva acum!" ]
                  ]
             ]
        , div [ class "space" ] [ ]
      ]




camera_matrimoniala_3 : Html msg
camera_matrimoniala_3 =
  div [ class "cm_3" ]
      [
      ]
restaurantItem : Model -> Html Msg
restaurantItem model =
  div [ class "restaurant-item" ]
      [ div [ class "picture-container" ]
            [ img [ class "one", src "https://i.imgur.com/pFpC8Pp.jpg" ] [ ]
            ]
      , div [ class "text-container" ]
            [ div [ class "title" ]
                  [ text restRo ]
            , div [ class "description" ]
                  [ text restDescRo]
            , detailButton RestaurantDetails model
            ]
      ]



conferenceItem : Model -> Html Msg
conferenceItem model =
  div [ class "conference-item" ]
      [ div [ class "top" ]
            [ div [ class "text-container" ]
                  [ div [ class "title" ]       [ text "Sali de Conferinta" ]
                  , div [ class "description" ] [ text confDescRo ]
                  , detailButton ConferenceDetails model
                  ]
            , div [ class "picture-container" ]
                  [ img [ src "https://i.imgur.com/8TBYOGM.jpg" ] [ ]
                  ]
            ]
      , div [ class "bottom" ]
            [ div [ class "left-picture-container" ]
                  [ img [ src "https://i.imgur.com/D9hXKgc.jpg" ] [ ]
                  ]
            , div [ class "right-picture-container" ]
                  [ img [ src "https://i.imgur.com/eOw3LHX.jpg" ] [ ]
                  ]
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
      ]

languageColor : Language -> List (Html Msg)
languageColor language =
  case language of
    Ro ->
      languageButtons "white" "#4d4d4d"

    En ->
      languageButtons "#4d4d4d" "white"

languageButtons : String -> String -> List (Html Msg)
languageButtons colorRo colorEn =
      [ button [ style [ ("color", colorRo) ] ] [ text "Ro" ]
      , span [ ] [ text "/" ]
      , button [ style [ ("color", colorEn) ] ] [ text "En" ]
      ]

descriptionItem : Msg -> Model -> String -> String -> Html Msg
descriptionItem msg model title description=
  div [ class "text" ]
      [ div [ class "title" ] [ text title ]
      , div [ class "description" ]
            [ text  description ]
      , (detailButton msg model)
      ]


detailButton : Msg -> Model -> Html Msg
detailButton msg model =
  button [ class "detail-button", onClick msg ]
         [ text (properLanguageText model.language "Detalii" "Details") ]


subscribe : Html msg
subscribe =
  div [ class "subscribe" ]
      [ div [ class "text" ] [ text "Aboneaza-te pentru oferte speciale!" ]
      , div [ class "input" ]
            [ input [ type_ "text", placeholder "Adresa de e-mail" ] [ ]
            , div [ class "button-container" ]
                  [ button [ ] [ text "Aboneaza-te" ]
                  ]
            ]
      ]

footer : Html Msg
footer =
  div [ class "footer" ] [
        div [ class "info" ]
            [ a [ href "https://www.facebook.com/hotelcrissbucuresti/" ]
                [ img [ src "icons/fb.svg" ] [ ]
                ]
            , img [ src "icons/mail.svg" ] [ ]
            ]
      , div [ class "copyright" ]
            [ div [ class "description" ] [ text "Copyright 2009 - 2018 CrossLine SRL." ]
            , div [ class "terms" ] [ text "Terms and Condition" ]
            , div [ class "anpc" ] [ text "ANPC" ]
            , div [ class "author" ] [ text "Autor: Dragos Dumitru" ]
            ]
      ]


-- ROOM PAGE --
roomPage : Html msg
roomPage =
  div [ class "room-page" ]
      [ div [ class "checkin" ]
            [ div [ class "room-name" ] [ text "Camera matrimoniala/twin ***" ]
            , div [ class "room-price" ] [ text "200.0Lei X 4 nopti" ]
            , div [ class "room-price-total" ] [ text "Total: 800.0Lei" ]
            , div [ class "period" ] [ text "05.10.2018 - 09.10.2018" ]
            , div [ class "non-ref" ] [ text "Non Refundable!" ]
            , div [ class "pay" ] [ text "Checkout" ]
            ]
      , div [ class "room-page-item" ]
            [ div [ class "page-title" ] [ text "Camere" ]
            , div [ class "title" ] [ text "Camera matrimoniala/twin ***"]
            , div [ class "price" ] [ text "200.0 LEI/noapte" ]
            , div [ class "desc" ] [ text dummyText ]
            , div [ class "icon-title" ] [ text "Detalii camera" ]
            , div [ class "details" ]
                  [ div [ class "icon-cont" ]
                        [ img [ src "icons/_size.svg" ] [ ]
                        , div [ class "desc" ] [ text "29 m2" ]
                        ]
                  , div [ class "icon-cont" ]
                        [ img [ src "icons/_avatar.svg" ] [ ]
                        , div [ class "desc" ] [ text "2 people" ]
                        ]
                  , div [ class "icon-cont" ]
                        [ img [ src "icons/_bed.svg" ] [ ]
                        , div [ class "desc" ] [ text "Double bed" ]
                        ]
                  ]
            , div [ class "amenities-title" ] [ text "Facilitati"]
            , div [ class "amenities" ]
                  [ div [ class "icon-cont" ]
                        [ img [ src "icons/_tv.svg" ] [ ]
                        , div [ class "desc" ] [ text "TV" ]
                        ]
                  , div [ class "icon-cont" ]
                        [ img [ src "icons/_hairdryer.svg" ] [ ]
                        , div [ class "desc" ] [ text "Uscator de par" ]
                        ]
                  , div [ class "icon-cont" ]
                        [ img [ src "icons/_minibar.svg" ] [ ]
                        , div [ class "desc" ] [ text "Mini bar" ]
                        ]
                  , div [ class "icon-cont" ]
                        [ img [ src "icons/_phone.svg" ] [ ]
                        , div [ class "desc" ] [ text "Telefon" ]
                        ]
                  , div [ class "icon-cont" ]
                        [ img [ src "icons/_room-service.svg" ] [ ]
                        , div [ class "desc" ] [ text "Room service" ]
                        ]
                  , div [ class "icon-cont" ]
                        [ img [ src "icons/_shower.svg" ] [ ]
                        , div [ class "desc" ] [ text "Dus" ]
                        ]
                  , div [ class "icon-cont" ]
                        [ img [ src "icons/_Wardrobe.svg" ] [ ]
                        , div [ class "desc" ] [ text "Spatii de depozitare" ]
                        ]
                  , div [ class "icon-cont" ]
                        [ img [ src "icons/_wifi.svg" ] [ ]
                        , div [ class "desc" ] [ text "Wi-Fi de mare viteza" ]
                        ]
                  ]
            , div [ class "picture-cont"]
                  [ div [ ] [ img [ src "https://i.imgur.com/ttSkgoA.jpg" ] [ ] ]
                  , div [ ] [ img [ src "https://i.imgur.com/bMYJQje.png" ] [ ] ]
                  ]
            , div [ class "select" ]
                  [ div [ class "select-button" ] [text "Selecteaza" ]
                  ]
            ]
      , img [ class "break", src "icons/break.svg" ] []
      ]

-- RESTAURANT PAGE --
restaurantPage : Html msg
restaurantPage =
  div [ class "restaurant-page" ]
      [ div [ class "main" ]
            [ div [ class "title" ] [ text "Restaurant" ]
            , div [ class "desc" ]
                  [ text "Va oferim tarife atractive pentru meniurile alese!" ]
            , div [ class "break" ]
                  [ img [ src "icons/break.svg" ] [ ]
                  ]
            , div [ class "bg-image" ]
                  [ img [ src "https://i.imgur.com/PxcyO9U.jpg" ] [ ]
                  ]
            ]
      , div [ class "intro-container" ]
            [ div [ class "desc" ]
                  [ text "Hotelul va pune la dispozitie cele 3 restaurante de 140, 80 si respectiv 50 locuri alaturi de terasa pe timpul verii."
                  ]
            ]
      , div [ class "container-two" ]
            [ div [ class "text" ]
                  [ text "Organizam mese festive, nunti si botezuri, evenimente cu stil intr-o ambianta rafinata si cu meniuri variate conform dorintelor Dvs."
                  ]
            , div [ class "image" ]
                  [ img [ src "https://i.imgur.com/l3YZ8Qe.jpg" ] [ ]
                  ]
            ]
      , div [ class "container-three" ]
            [ div [ class "text" ]
                  [ text "Gratuit: ⋆ aranjarea si decorarea festiva a meselor si scaunelor cu huse si funde din organza ⋆ un apartament pentru noaptea nuntii ⋆ parcare pentru invitatii Dvs."
                  ]
            ]
      , div [ class "container-four" ]
            [ div [ class "picture" ]
                  [ img [ src "https://i.imgur.com/1GGzLwD.jpg" ] [ ]
                  , img [ src "https://i.imgur.com/1GGzLwD.jpg" ] [ ]
                  , img [ src "https://i.imgur.com/1GGzLwD.jpg" ] [ ]
                  ]
            ]
      , div [ class "container-five" ]
            [ div [ class "text" ]
                  [ text "Va asteptam cu drag!"
                  ]
            ]
      ]

-- CONFERENCE PAGE --
conferencePage : Html msg
conferencePage =
  div [ class "conference-page" ]
      [ div [ class "title" ] [ text "Sali de conferinta" ]
      , div [ class "description-one" ]
            [ text "Pentru societatile interesate de organizarea de conferinte si training-uri punem la dispozitie 4 sali de conferinta cu spatii generoase si lumina naturala avand capacitati diferite de 250 mp, 100 mp si 55 mp si 35 mp."
            ]
      , div [ class "description-two" ]
            [ text "Putem organiza coffee break-uri si mese festive pentru orice tip de eveniment business iar pe perioada desfasurarii acestora beneficiati de asistenta din partea unui coordonator."
            ]
      , div [ class "halls" ]
            [ div [ class "title" ] [ text "Dotari" ]
            , div [ class "offer-container" ]
                  [ div [ class "left" ]
                        [ div [ ] [ text " ★ Wi-Fi gratis" ]
                        , div [ ] [ text " ★ Ecran de proiectie" ]
                        , div [ ] [ text " ★ Video-proiector " ]
                        , div [ ] [ text " ★ Flipchart" ]
                        ]
                  , div [ class "right" ]
                        [ div [ ] [ text " ★ Consumabile" ]
                        , div [ ] [ text " ★ Climatizare" ]
                        , div [ ] [ text " ★ Foaier pentru servire coffee break" ]
                        , div [ ] [ text " ★ Mese si scaune aranjate conform solicitarilor dumneavoastre" ]
                        ]
                  ]
            ]
      , div [ class "break" ] [ img [ src "icons/break.svg" ] [ ] ]
      , div [ class "theater" ]
            [ div [ class "title" ]
                  [ p [ ] [ text "Classroom" ]
                  ]
            , div [ class "capacity" ] [ text "Capacitate" ]
            , div [ class "content" ]
                  [ div [ ] [ text "★ 100 persoane | 250m2" ]
                  , div [ ] [ text "★  40 persoane | 100m2" ]
                  , div [ ] [ text "★  26 persoane |  55m2" ]
                  , div [ ] [ text "★  20 persoane |  35m2" ]
                  ]
            , div [ class "pictures" ]
                  [ img  [ src "https://i.imgur.com/UhFb4jv.jpg" ] [ ]
                  ]
            ]
      , div [ class "theater" ]
            [ div [ class "title" ]
                  [ p [ ] [ text "Boardroom" ]
                  ]
            , div [ class "capacity" ] [ text "Capacitate" ]
            , div [ class "content" ]
                  [ div [ ] [ text "★ 100 persoane | 250m2" ]
                  , div [ ] [ text "★  40 persoane | 100m2" ]
                  , div [ ] [ text "★  26 persoane |  55m2" ]
                  , div [ ] [ text "★  20 persoane |  35m2" ]
                  ]
            , div [ class "pictures" ]
                  [ img  [ src "https://i.imgur.com/jaYhQzl.jpg" ] [ ]
                  ]
            ]
      , div [ class "theater" ]
            [ div [ class "title" ]
                  [ p [ ] [ text "U-Shape" ]
                  ]
            , div [ class "capacity" ] [ text "Capacitate" ]
            , div [ class "content" ]
                  [ div [ ] [ text "★ 100 persoane | 250m2" ]
                  , div [ ] [ text "★  40 persoane | 100m2" ]
                  , div [ ] [ text "★  26 persoane |  55m2" ]
                  , div [ ] [ text "★  20 persoane |  35m2" ]
                  ]
            , div [ class "pictures" ]
                  [ img  [ src "https://i.imgur.com/4IwInDU.jpg" ] [ ]
                  ]
            ]
      , div [ class "theater" ]
            [ div [ class "title" ]
                  [ p [ ] [ text "Classroom" ]
                  ]
            , div [ class "capacity" ] [ text "Capacitate" ]
            , div [ class "content" ]
                  [ div [ ] [ text "★ 100 persoane | 250m2" ]
                  , div [ ] [ text "★  40 persoane | 100m2" ]
                  , div [ ] [ text "★  26 persoane |  55m2" ]
                  , div [ ] [ text "★  20 persoane |  35m2" ]
                  ]
            , div [ class "pictures" ]
                  [ img  [ src "https://i.imgur.com/UzMPQVG.jpg" ] [ ]
                  ]
            ]
      ]

-- GALERIE PHOTO PAGE --
photoGalleryPage : Html msg
photoGalleryPage =
  div [ class "photo-page" ]
      [ div [ class "title" ] [ text "Galerie Foto" ]
      , img [ class "break", src "icons/break.svg" ] [ ]
      , div [ class "container-one" ]
            [ div [ class "box-one" ]
                  [ img [ src "https://i.imgur.com/D7AeU8X.jpg" ] [ ]
                  ]
            , div [ class "box-two" ]
                  [ img [ src "https://i.imgur.com/EiPy9K9.jpg" ] [ ]
                  ]
            ]
      , div [ class "container-two" ]
            [ div [ class "box-one" ]
                  [ img [ src "https://i.imgur.com/t1nLs6w.jpg" ] [ ]
                  ]
            , div [ class "box-two" ]
                  [ img [ src "https://i.imgur.com/n7MHJwT.jpg" ] [ ]
                  ]
            , div [ class "box-three" ]
                  [ img [ src "https://i.imgur.com/Dt4RqAB.jpg" ] [ ]
                  ]
            ]
      , div [ class "container-three" ]
            [ div [ class "box-one" ]
                  [ img [ src "https://i.imgur.com/nIs4jlo.jpg" ] [ ]
                  ]
            ]
      , div [ class "container-one" ]
            [ div [ class "box-two" ]
                  [ img [ src "https://i.imgur.com/ARrqPrL.jpg" ] [ ]
                  ]
            , div [ class "box-one" ]
                  [ img [ src "https://i.imgur.com/KUecB7c.jpg" ] [ ]
                  ]
            ]
      , div [ class "container-two" ]
            [ div [ class "box-one" ]
                  [ img [ src "https://i.imgur.com/zzgoeSB.jpg" ] [ ]
                  ]
            , div [ class "box-two" ]
                  [ img [ src "https://i.imgur.com/GyM26GW.jpg" ] [ ]
                  ]
            , div [ class "box-three" ]
                  [ img [ src "https://i.imgur.com/UzMPQVG.jpg" ] [ ]
                  ]
            ]
      , div [ class "container-three" ]
            [ div [ class "box-one" ]
                  [ img [ src "https://i.imgur.com/l3YZ8Qe.jpg" ] [ ]
                  ]
            ]
      , div [ class "end" ] [ ]
      ]


properLanguageText : Language -> String -> String -> String
properLanguageText language textRo textEn =
  case language of
    En -> textEn
    Ro -> textRo

