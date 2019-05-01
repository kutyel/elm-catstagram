module Main exposing (main)

import Browser exposing (Document, UrlRequest(..), application)
import Browser.Navigation exposing (Key, load, pushUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error, expectJson, get)
import Json.Decode exposing (Decoder, field, list, string)
import Url exposing (Url)



-- MAIN


main : Program String Model Msg
main =
    application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = Navigate
        }



-- MODEL


type Route
    = Route Key Url


type Model
    = Failure
    | Loading
    | Success (List String) -- TODO: Success Route (List String)


init : String -> Url -> Key -> ( Model, Cmd Msg )
init token url key =
    ( Loading, getPosts token )



-- UPDATE


type Msg
    = Navigate UrlRequest
    | UrlChanged Url
    | GotPosts (Result Error (List String))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Navigate urlRequest ->
            case urlRequest of
                Internal url ->
                    -- case model of
                    -- TODO: Success (Route key _) _ ->
                    --     ( model, pushUrl key (Url.toString url) )
                    -- _ ->
                    ( model, Cmd.none )

                External href ->
                    ( model, load href )

        UrlChanged url ->
            -- case model of
            -- TODO: Success (Route key _) str ->
            --     ( Success (Route key url) str, Cmd.none )
            -- _ ->
            ( model, Cmd.none )

        GotPosts result ->
            case result of
                Ok posts ->
                    ( Success posts, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Elm Catstagram"
    , body =
        [ h1 []
            [ a [ href "/" ] [ text "Catstagram" ] ]
        , case model of
            Failure ->
                h2 [] [ text "An error occured :(" ]

            Loading ->
                viewSpinner

            Success posts ->
                ul []
                    (List.map viewLink posts)
        ]
    }


viewSpinner : Html msg
viewSpinner =
    div [ class "spinner" ]
        [ div [] []
        , div [] []
        , div [] []
        , div [] []
        ]


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]



-- HTTP


getPosts : String -> Cmd Msg
getPosts token =
    get
        { url = "https://api.instagram.com/v1/users/self/media/recent/?access_token=" ++ token
        , expect = expectJson GotPosts postDecoder
        }


postDecoder : Decoder (List String)
postDecoder =
    field "data" (list (field "link" string))
