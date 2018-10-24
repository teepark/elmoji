module Main exposing (main)

import Browser
import Emoji exposing (replaceWithTwemoji, textWith, text_)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import String


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
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
update (InputChanged msg) model =
    msg



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ textarea [ onInput InputChanged ] []
        , h3 [] [ text "Twemoji:" ]
        , div [] (twext model)
        , h3 [] [ text "EmojiOne:" ]
        , div [] [ text_ model ]
        , h3 [] [ text "Custom:" ]
        , div [] (customtext model)
        ]


twext : String -> List (Html Msg)
twext =
    textWith replaceWithTwemoji


mapEmoji : List String -> Html a
mapEmoji codePoints =
    text ("(I'm code " ++ String.join "-" codePoints ++ ")")


customtext : String -> List (Html Msg)
customtext =
    textWith mapEmoji
