module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as Html
import List
import Elmoji.Html exposing (textWith, replaceWithEmojiOne, replaceWithTwemoji)


main : Program Never
main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    String


init : Model
init =
    ""



-- UPDATE


type Msg
    = InputChanged String


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputChanged msg ->
            msg



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ textarea [ onInput InputChanged ] []
        , h3 [] [ text "Twemoji:" ]
        , div [] (twext model)
        , h3 [] [ text "EmojiOne:" ]
        , div [] (eotext model)
        ]


twext : String -> List (Html Msg)
twext =
    textWith replaceWithTwemoji

eotext : String -> List (Html Msg)
eotext =
    textWith replaceWithEmojiOne
