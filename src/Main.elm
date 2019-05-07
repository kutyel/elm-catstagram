module Main exposing (main)

import Browser exposing (Document, UrlRequest(..), application)
import Browser.Navigation exposing (Key, load, pushUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Http exposing (Error, expectJson, get)
import Json.Decode exposing (Decoder, bool, field, int, list, string, succeed)
import Json.Decode.Pipeline exposing (required)
import List.Extra exposing (find)
import Regex exposing (Regex, fromString, never)
import Url exposing (Url)
import Url.Parser as P exposing ((</>), Parser, map, oneOf, parse, s, top)



-- MAIN


main : Program String Model Msg
main =
    application
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- ROUTE


type Route
    = Home
    | Detail String


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Home top
        , map Detail (s "view" </> P.string)
        ]


fromUrl : Url -> Route
fromUrl url =
    Maybe.withDefault Home (parse parser url)


path : Route -> String
path route =
    case route of
        Home ->
            "/"

        Detail postId ->
            "/view/" ++ postId



-- MODEL


type alias Post =
    { id : String
    , caption : String
    , images : String
    , likes : Int
    , comments : Int
    , user_has_liked : Bool
    }


type Model
    = Failure
    | Loading
    | Success Route (List Post)


init : String -> Url -> Key -> ( Model, Cmd Msg )
init token url key =
    ( Loading, getPosts token )



-- UPDATE


type Msg
    = Like Post Bool
    | UrlChanged Url
    | LinkClicked UrlRequest
    | FetchedPosts (Result Error (List Post))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Internal url ->
                    -- TODO:  ( model, pushUrl key (Url.toString url) )
                    ( model, Cmd.none )

                External href ->
                    ( model, load href )

        ( UrlChanged url, Success _ posts ) ->
            ( Success (fromUrl url) posts, Cmd.none )

        ( Like post liked, Success _ posts ) ->
            ( Success
                Home
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
            , Cmd.none
            )

        ( FetchedPosts result, _ ) ->
            case result of
                Ok posts ->
                    ( Success Home posts, Cmd.none )

                Err _ ->
                    ( Failure, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "🐈 Elm Catstagram"
    , body =
        [ h1 []
            [ a [ href "/" ] [ text "Catstagram" ] ]
        , case model of
            Loading ->
                viewSpinner

            Failure ->
                h2 [] [ text "An error occured :(" ]

            Success route posts ->
                case route of
                    Home ->
                        Keyed.node "div"
                            [ class "photo-grid" ]
                            (List.map viewKeyedPost posts)

                    Detail postId ->
                        div [ class "single-photo" ]
                            [ case find (\{ id } -> id == postId) posts of
                                Nothing ->
                                    text "Could not find any post! :("

                                Just post ->
                                    viewPost post
                            ]
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
            [ p [] (replaceHashtags caption) -- TODO: fix the rest of the description
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



-- HASHTAG


hashtag : Regex
hashtag =
    Maybe.withDefault never <| fromString "#(\\w+)"


replaceHashtags : String -> List (Html Msg)
replaceHashtags str =
    List.map (\{ match } -> viewHashtag match) (Regex.find hashtag str)


viewHashtag : String -> Html Msg
viewHashtag str =
    -- TODO: fix the link to Instagram #hashtag page
    a [ href ("https://www.instagram.com/explore/tags/" ++ str) ] [ text (str ++ " ") ]



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
            (succeed Post
                |> required "id" string
                |> required "caption" (field "text" string)
                |> required "images" (field "standard_resolution" (field "url" string))
                |> required "likes" (field "count" int)
                |> required "comments" (field "count" int)
                |> required "user_has_liked" bool
            )
        )
