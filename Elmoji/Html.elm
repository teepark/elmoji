module Elmoji.Html exposing (textWith, emojiOne)

import Elmoji exposing (..)
import Elmoji.Internal.Hex exposing (dump)
import Html exposing (..)
import Html.Attributes exposing (..)
import List
import String


textWith : (List String -> Html a) -> String -> List (Html a)
textWith replacer body =
    let
        ( String' chunks ) =
            parse body
    in
        List.map
            (\chunk ->
                case chunk of
                    StringChunk s ->
                        text s

                    CodeChunk codepts ->
                        codepts
                            |> List.map dump
                            |> replacer
            )
            chunks


e1Base : String
e1Base =
    "https://cdnjs.cloudflare.com/ajax/libs/emojione/2.2.6/assets/png/"


emojiOne : (List String -> Html a)
emojiOne codepts =
    img [ src (e1Base ++ (List.intersperse "-" codepts |> String.join "") ++ ".png")
        , style [("height", "1em")]
        ]
        []
