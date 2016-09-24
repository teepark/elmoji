import Html exposing (..)
import Html.Events exposing (..)
import Html.App as Html
import List
import Elmoji
import Elmoji.Internal.Hex exposing (dump)


main : Program Never
main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }



-- MODEL


type alias Model =
    Elmoji.String'


init : Model
init =
    Elmoji.String' <| [ Elmoji.StringChunk "" ]



-- UPDATE


type Msg
    = InputChanged String


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputChanged msg ->
            Elmoji.parse msg



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput InputChanged ] []
        , div []
            [ text <| toString model ]
        ]


viewChunk : Elmoji.Chunk -> Html Msg
viewChunk chunk =
    case chunk of
        Elmoji.StringChunk string ->
            div []
                [ text "StringChunk: "
                , pre []
                    [ text string ]
                ]

        Elmoji.CodeChunk codes ->
            div []
                [ text "CodeChunk: "
                , pre []
                    ( List.map
                        (\i ->
                            pre [] [ text ("0x" ++ toString (dump i)) ]
                        )
                        codes
                    )
                ]
