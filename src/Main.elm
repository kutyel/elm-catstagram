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
import Url.Parser as P exposing ((</>), Parser, map, oneOf, s)



-- ROUTE


type Route
    = Root
    | View String


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Root P.top
        , map View (s "view" </> P.string)
        ]


fromUrl : Url -> Route
fromUrl url =
    Maybe.withDefault Root (P.parse parser url)


path : Route -> String
path route =
    case route of
        Root ->
            "/"

        View postId ->
            "/view/" ++ postId



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


type PageState a
    = Failure
    | Loading
    | Success a


type Model
    = Home (PageState (List Post))
    | Detail (PageState Post)


init : String -> Url -> Key -> ( Model, Cmd Msg )
init token url key =
    ( Home Loading, getPosts token )



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
                    -- TODO: ( model, pushUrl key (Url.toString url) )
                    ( model, Cmd.none )

                External href ->
                    ( model, load href )

        UrlChanged url ->
            -- TODO: fromUrl url
            ( model, Cmd.none )

        Like post liked ->
            case model of
                Home (Success posts) ->
                    ( Home
                        (Success
                            (List.map
                                (\p ->
                                    if p == post then
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

                                    else
                                        p
                                )
                                posts
                            )
                        )
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        FetchedPosts result ->
            case result of
                Ok posts ->
                    ( Home (Success posts), Cmd.none )

                Err _ ->
                    ( Home Failure, Cmd.none )



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
            Home Failure ->
                h2 [] [ text "An error occured :(" ]

            Detail (Success post) ->
                div [ class "single-photo" ]
                    [ viewPost post
                    ]

            Home (Success posts) ->
                Keyed.node "div"
                    [ class "photo-grid" ]
                    (List.map viewKeyedPost posts)

            _ ->
                viewSpinner
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
