module Elmoji.Html exposing (..)

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


urlWithBase : String -> List String -> String
urlWithBase base codepts =
    base ++ (List.intersperse "-" codepts |> String.join "") ++ ".png"


emojiOneBaseUrl : String
emojiOneBaseUrl =
    "https://cdnjs.cloudflare.com/ajax/libs/emojione/2.2.6/assets/png/"


replaceWithEmojiOne : (List String -> Html a)
replaceWithEmojiOne codepts =
    img [ src <| urlWithBase emojiOneBaseUrl codepts
        , style [("height", "1em")]
        ]
        []
