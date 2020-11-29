module Picshare exposing (main)

import Html exposing (..)
import Html.Attributes exposing (class, placeholder, src, type_, disabled, value)
import Html.Events exposing (onClick, onInput, onSubmit)
import Browser
import Array exposing (Array)

type alias Model =
    { url : String
    , caption : String
    , liked : Bool
    , comments : List String
    , newComment : String
    }

baseUrl : String
baseUrl =
    "https://programming-elm.com/"

initialModel : Model
initialModel =
    { url = baseUrl ++ "2.jpg"
    , caption = "Snow Fox"
    , liked = False
    , comments = ["So cute!"]
    , newComment = ""
    }

type Msg
    = ToggleLike
    | UpdateComment String
    | SaveComment

update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleLike ->
            { model | liked = not model.liked }
        UpdateComment comment ->
            { model | newComment = comment}
        SaveComment ->
            saveNewComment model

saveNewComment : Model -> Model
saveNewComment model =
    let
        comment =
            String.trim model.newComment
    in
    case comment of
        "" ->
            model
        _ ->
            { model | comments = model.comments ++ [ comment ], newComment = ""}

viewDetailedPhoto : Model -> Html Msg
viewDetailedPhoto model =
    div [ class "detailed-photo" ]
        [ img [ src model.url ] []
        , div [ class "photo-info" ]
              [ viewLoveButton model
              , h2 [ class "caption" ] [ text model.caption ] 
              , viewComments model
              ]
        ]

viewLoveButton : Model -> Html Msg
viewLoveButton model =
    let
        buttonClass =
            if model.liked then
                "fa-heart"
            else
                "fa-heart-o"
    in 
    div [ class "like-button" ]
        [ i
            [ class "fa fa-2x" 
            , class buttonClass
            , onClick ToggleLike
            ]
            []
        ]

viewComments : Model -> Html Msg
viewComments model =
    div []
        [ viewCommentList model.comments
        , form [ class "new-comment", onSubmit SaveComment ]
               [ input
                    [ type_ "text" 
                    , placeholder "Add a comment..."
                    , value model.newComment
                    , onInput UpdateComment
                    ]
                    []
                , button 
                    [ disabled (String.isEmpty model.newComment) ]
                    [ text "Save" ]
               ]
        ]

viewCommentList : List String -> Html Msg
viewCommentList comments =
    case comments of
        [] ->
            text ""
        _ ->
            div [ class "components" ]
                [ ul []
                     (List.map viewComment comments)
                ]

viewComment : String -> Html Msg
viewComment comment =
    li []
        [ strong [] [ text "Comment: " ]
        , text comment
        ]

view : Model -> Html Msg
view model =
    div []
        [ div [ class "header" ]
            [ h1 [] [ text "Picshare" ] ]
        , div [ class "content-flow" ]
            [ viewDetailedPhoto model ]
        ]

main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }