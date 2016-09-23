--TODO: narrow down the exported names


module Elmoji exposing (..)

import Char
import Html exposing (..)
import Html.Events exposing (..)
import Html.App as Html
import List
import String
import Elmoji.Valid exposing (member, store, splitPrefix)
import Elmoji.Hex exposing (dump)


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
        [ input
            [ onInput InputChanged ]
            []
        , div []
            (div [] [ text <| toString <| member model store ]
                :: (List.map
                        (\c ->
                            div [] [ c |> Char.toCode |> dump |> toString |> text ]
                        )
                        (String.toList model)
                   )
            )
        ]


type Chunk
    = StringChunk String
    | CodeChunk String


type String'
    = String' (List Chunk)


deref : String' -> List Chunk
deref s =
    let
        ( String' l ) =
            s
    in
        l
