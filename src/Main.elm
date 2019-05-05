module Main exposing (main)

import Browser exposing (Document, UrlRequest(..), application)
import Browser.Navigation exposing (Key, load, pushUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Http exposing (Error, expectJson, get)
import Json.Decode exposing (Decoder, bool, field, int, list, map6, string)
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


type alias Post =
    { id : String
    , caption : String
    , images : String
    , likes : Int
    , comments : Int
    , user_has_liked : Bool
    }


type Route
    = Route Key Url


type Model
    = Failure
    | Loading
    | Detail Post -- TODO: Comments : List String
    | Home (List Post)


init : String -> Url -> Key -> ( Model, Cmd Msg )
init token url key =
    ( Loading, getPosts token )



-- UPDATE


type Msg
    = Like Post Bool
    | UrlChanged Url
    | Navigate UrlRequest
    | FetchedPosts (Result Error (List Post))


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

        Like post liked ->
            case model of
                Detail _ ->
                    ( Detail (updatePost post liked)
                    , Cmd.none
                    )

                Home posts ->
                    ( Home
                        (List.map
                            (\x ->
                                if x == post then
                                    updatePost post liked

                                else
                                    x
                            )
                            posts
                        )
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        FetchedPosts result ->
            case result of
                Ok posts ->
                    ( Home posts, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )


updatePost : Post -> Bool -> Post
updatePost post liked =
    { post
        | user_has_liked = liked
        , likes =
            post.likes
                + (if liked then
                    1

                   else
                    -1
                  )
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Document Msg
view model =
    { title = "🐈 Elm Catstagram"
    , body =
        [ h1 []
            [ a [ href "/" ] [ text "Catstagram" ] ]
        , case model of
            Failure ->
                h2 [] [ text "An error occured :(" ]

            Loading ->
                viewSpinner

            Detail post ->
                div [ class "single-photo" ]
                    [ viewPost post
                    ]

            Home posts ->
                Keyed.node "div"
                    [ class "photo-grid" ]
                    (List.map viewKeyedPost posts)
        ]
    }


viewSpinner : Html Msg
viewSpinner =
    div [ class "spinner" ]
        [ div [] []
        , div [] []
        , div [] []
        , div [] []
        ]


viewKeyedPost : Post -> ( String, Html Msg )
viewKeyedPost ({ id } as post) =
    ( id, lazy viewPost post )


viewPost : Post -> Html Msg
viewPost ({ id, caption, comments, images, user_has_liked, likes } as post) =
    figure [ class "grid-figure" ]
        [ div [ class "grid-photo-wrap" ]
            [ a [ href ("/view/" ++ id) ]
                [ img [ src images, alt id, class "grid-photo" ] []
                ]
            ]
        , figcaption []
            [ p [] [ text caption ]
            , div [ class "control-buttons" ]
                [ button
                    [ onClick (Like post (not user_has_liked))
                    , class
                        (if user_has_liked then
                            "liked"

                         else
                            ""
                        )
                    ]
                    [ text ("♥ " ++ String.fromInt likes) ]
                , a [ class "button", href ("/view/" ++ id) ]
                    [ span
                        [ class "comment-count" ]
                        [ span [ class "speech-bubble" ]
                            [ text (String.fromInt comments) ]
                        ]
                    ]
                ]
            ]
        ]



-- HTTP


getPosts : String -> Cmd Msg
getPosts token =
    get
        { url = "https://api.instagram.com/v1/users/self/media/recent/?access_token=" ++ token
        , expect = expectJson FetchedPosts postDecoder
        }


postDecoder : Decoder (List Post)
postDecoder =
    field "data"
        (list
            (map6 Post
                (field "id" string)
                (field "caption" (field "text" string))
                (field "images" (field "standard_resolution" (field "url" string)))
                (field "likes" (field "count" int))
                (field "comments" (field "count" int))
                (field "user_has_liked" bool)
            )
        )
