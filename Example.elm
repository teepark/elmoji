module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import String
import Elmoji exposing (text_, textWith, replaceWithTwemoji)


main : Program Never Model Msg
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
        , div [] [ text_ model ]
        , h3 [] [ text "Custom:" ]
        , div [] (customtext model)
        ]


twext : String -> List (Html Msg)
twext =
    textWith replaceWithTwemoji


mapEmoji : List String -> Html a
mapEmoji codePoints =
    text ("(I'm code " ++ (String.join "-" codePoints) ++ ")")


customtext : String -> List (Html Msg)
customtext =
    textWith mapEmoji
