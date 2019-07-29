module Main exposing (main)

import Browser exposing (Document, UrlRequest(..), application)
import Browser.Navigation exposing (Key, load, pushUrl)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy)
import Http exposing (Error, expectJson, get)
import Json.Decode exposing (Decoder, bool, field, int, list, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, required)
import List.Extra exposing (find)
import Prng.Uuid as Uuid
import Random.Pcg.Extended exposing (Seed, initialSeed, step)
import Url exposing (Url)
import Url.Parser as P exposing ((</>), Parser, map, oneOf, parse, s, top)



-- MAIN


main : Program Flags Model Msg
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



-- MODEL


type alias Post =
    { id : String
    , caption : String
    , images : String
    , likes : Int
    , numComments : Int
    , liked : Bool
    , tags : List String
    , comments : List Comment
    }


type alias Comment =
    { id : String
    , from : String
    , txt : String
    }


type alias Flags =
    ( String, Int, List Int )


type alias FormState =
    { author : String
    , comment : String
    }


type Posts
    = Failure
    | Loading
    | Success (List Post)


type alias Model =
    { key : Key
    , seed : Seed
    , posts : Posts
    , route : Route
    , apiKey : String
    , form : FormState
    }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init ( apiKey, sd, seedExtension ) url key =
    let
        route =
            fromUrl url

        seed =
            initialSeed sd seedExtension

        form =
            FormState "" ""
    in
    ( Model key seed Loading route apiKey form, getPosts apiKey )



-- UPDATE


type Msg
    = Like Post Bool
    | UrlChanged Url
    | ChangeAuthor String
    | ChangeComment String
    | LinkClicked UrlRequest
    | RemoveComment Post String
    | AddComment Post FormState
    | FetchedPosts (Result Error (List Post))
    | FetchedComments String (Result Error (List Comment))


updatePost : Model -> String -> (Post -> Post) -> ( Model, Cmd Msg )
updatePost model postId f =
    case model.posts of
        Success posts ->
            ( { model
                | posts =
                    Success
                        (List.map
                            (\({ id } as post) ->
                                if id == postId then
                                    f post

                                else
                                    post
                            )
                            posts
                        )
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeAuthor author ->
            let
                form =
                    model.form
            in
            ( { model | form = { form | author = author } }, Cmd.none )

        ChangeComment comment ->
            let
                form =
                    model.form
            in
            ( { model | form = { form | comment = comment } }, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model, pushUrl model.key (Url.toString url) )

                External href ->
                    ( model, load href )

        UrlChanged url ->
            case fromUrl url of
                Home ->
                    ( { model | route = Home }, Cmd.none )

                Detail postId ->
                    case model.posts of
                        Success posts ->
                            ( { model | route = Detail postId }
                            , case find (.id >> (==) postId) posts of
                                Nothing ->
                                    Cmd.none

                                Just { comments } ->
                                    case comments of
                                        [] ->
                                            getComments model.apiKey postId

                                        _ ->
                                            Cmd.none
                            )

                        _ ->
                            ( model, Cmd.none )

        FetchedPosts result ->
            case result of
                Ok posts ->
                    ( { model | posts = Success posts }, Cmd.none )

                Err _ ->
                    ( { model | posts = Failure }, Cmd.none )

        FetchedComments postId result ->
            case result of
                Ok comments ->
                    updatePost model
                        postId
                        (\post ->
                            { post
                                | comments = comments
                            }
                        )

                Err _ ->
                    ( { model | posts = Failure }, Cmd.none )

        Like { id } liked ->
            updatePost model
                id
                (\post ->
                    { post
                        | liked = liked
                        , likes =
                            post.likes
                                + (if liked then
                                    1

                                   else
                                    -1
                                  )
                    }
                )

        RemoveComment ({ id } as post) commentId ->
            updatePost model
                id
                (\{ comments } ->
                    { post
                        | comments = List.filter (.id >> (/=) commentId) comments
                    }
                )

        AddComment post { author, comment } ->
            case model.posts of
                Success posts ->
                    let
                        ( newUuid, newSeed ) =
                            step Uuid.generator model.seed

                        uuid =
                            Uuid.toString newUuid
                    in
                    ( { model
                        | seed = newSeed
                        , form = FormState "" ""
                        , posts =
                            Success
                                (List.map
                                    (\({ comments } as p) ->
                                        if p == post then
                                            { post
                                                | comments = Comment uuid author comment :: comments
                                            }

                                        else
                                            p
                                    )
                                    posts
                                )
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    { title = "🐈 Elm Catstagram"
    , body =
        [ h1 []
            [ a [ href "/" ] [ text "Catstagram" ] ]
        , case model.posts of
            Loading ->
                viewSpinner

            Failure ->
                h2 [] [ text "An error occured :(" ]

            Success posts ->
                case model.route of
                    Home ->
                        Keyed.node "div"
                            [ class "photo-grid" ]
                            (List.map viewKeyedPost posts)

                    Detail postId ->
                        div [ class "single-photo" ]
                            (case find (.id >> (==) postId) posts of
                                Nothing ->
                                    [ text "Could not find any post! :(" ]

                                Just post ->
                                    [ viewPost post
                                    , viewComments model.form post
                                    ]
                            )
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
viewPost ({ id, caption, numComments, images, liked, likes, tags, comments } as post) =
    figure [ class "grid-figure" ]
        [ div [ class "grid-photo-wrap" ]
            [ a [ href ("/view/" ++ id) ]
                [ img [ src images, alt id, class "grid-photo" ] []
                ]
            ]
        , figcaption []
            [ p []
                (text
                    (Maybe.withDefault "" <| List.head <| String.split "#" caption)
                    :: List.map viewHashtag tags
                )
            , div [ class "control-buttons" ]
                [ button
                    [ onClick (Like post (not liked))
                    , class
                        (if liked then
                            "liked"

                         else
                            ""
                        )
                    ]
                    [ text ("♥ " ++ String.fromInt likes) ]
                , a [ class "button", href ("/view/" ++ id) ]
                    [ span
                        [ class "comment-count" ]
                        [ span [ class "speech-bubble" ] []
                        , text
                            (String.fromInt
                                (case comments of
                                    [] ->
                                        numComments

                                    xs ->
                                        List.length xs
                                )
                            )
                        ]
                    ]
                ]
            ]
        ]


viewHashtag : String -> Html Msg
viewHashtag str =
    a [ href ("https://www.instagram.com/explore/tags/" ++ str) ] [ text ("#" ++ str ++ " ") ]


viewComments : FormState -> Post -> Html Msg
viewComments ({ author, comment } as form) ({ comments } as post) =
    div [ class "comments" ]
        [ div [ class "comments-list" ]
            (List.map
                (\{ id, txt, from } ->
                    div [ class "comment" ]
                        [ p []
                            [ strong [] [ text from ]
                            , text txt
                            , button [ class "remove-comment", onClick (RemoveComment post id) ] [ text "✖" ]
                            ]
                        ]
                )
                comments
            )
        , Html.form [ class "comment-form", onSubmit (AddComment post form) ]
            [ input [ type_ "text", placeholder "author", value author, onInput ChangeAuthor ] []
            , input [ type_ "text", placeholder "comment", value comment, onInput ChangeComment ] []
            , input [ type_ "submit", hidden True ] []
            ]
        ]



-- HTTP


getPosts : String -> Cmd Msg
getPosts token =
    get
        { url = "https://api.instagram.com/v1/users/self/media/recent/?access_token=" ++ token
        , expect = expectJson FetchedPosts postDecoder
        }


getComments : String -> String -> Cmd Msg
getComments token postId =
    get
        { url = "https://api.instagram.com/v1/media/" ++ postId ++ "/comments?access_token=" ++ token
        , expect = expectJson (FetchedComments postId) commentDecoder
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
                |> required "tags" (list string)
                |> hardcoded []
            )
        )


commentDecoder : Decoder (List Comment)
commentDecoder =
    field "data"
        (list
            (succeed Comment
                |> required "id" string
                |> required "from" (field "username" string)
                |> required "text" string
            )
        )
