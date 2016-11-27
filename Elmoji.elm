module Elmoji exposing (text_, textWith, replaceWithEmojiOne, replaceWithTwemoji)

{-| This library is for conveniently supporting
[emoji](http://unicode.org/emoji/charts/full-emoji-list.html) in Elm
applications.

There is a high-level drop-in replacement for `Html.text` which has to make
some extra assumptions about the app, and customizable mapping over emojis.

# The high level
@docs text_

# Customizable
@docs textWith, replaceWithEmojiOne, replaceWithTwemoji
-}

import Elmoji.Internal.Parse exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import List
import String


{-| Convert a String with unicode emoji characters into an Html element
containing the text with `<img>` tags replacing the emojis.

This function produces a `<span class='elmoji'>` containing the text, replacing
emojis with `<img class='elmoji-img elmoji-one'>` tags pointing to CDN-hosted
[EmojiOne](http://emojione.com/).

    div [] [ text' "Live long and prosper 🖖" ]
-}
text_ : String -> Html a
text_ =
    textWith replaceWithEmojiOne >> span [ class "elmoji" ]


{-| Create a customized emoji converter. The function argument maps emoji
(identified by the lowercase hex-encoded unicode code point sequence) to
Html nodes.

    mapEmoji : List String -> Html a
    mapEmoji codePoints =
        text ("(I'm code " ++ (String.join "-" codePoints) ++ ")")

    div []
        ( textWith mapEmoji "here's a penguin:🐧" )
-}
textWith : (List String -> Html a) -> String -> List (Html a)
textWith replacer body =
    let
        (String_ chunks) =
            parse body
    in
        List.map
            (\chunk ->
                case chunk of
                    StringChunk s ->
                        text s

                    CodeChunk codepts ->
                        replacer codepts
            )
            chunks


{-| Turn an emoji unicode sequence into an `<img>` pointing at
[EmojiOne](http://emojione.com/), with classes `elmoji-img` and `elmoji-one`.

    -- this is the definition of text' from this module.
    text' : String -> Html a
    text' =
        textWith replaceWithEmojiOne >> span [ class "elmoji" ]
-}
replaceWithEmojiOne : List String -> Html a
replaceWithEmojiOne codepts =
    img
        [ src <| urlWithBase emojiOneBaseUrl codepts
        , class "elmoji-img elmoji-one"
        ]
        []


{-| Convert an emoji unicode sequence into a
[Twemoji](http://twitter.github.io/twemoji/) `<img>` tag. It will have CSS
classes `elmoji-img` and `elmoji-twem`.

    -- build your own Html.text drop-in replacement
    text' : String -> Html a
    text' body =
        span [] ( textWith replaceWithTwemoji body )
-}
replaceWithTwemoji : List String -> Html a
replaceWithTwemoji codepts =
    img
        [ src <| urlWithBase twemojiBaseUrl codepts
        , class "elmoji-img elmoji-twem"
        ]
        []


urlWithBase : String -> List String -> String
urlWithBase base codepts =
    base ++ (List.intersperse "-" codepts |> String.join "") ++ ".png"


emojiOneBaseUrl : String
emojiOneBaseUrl =
    "https://cdnjs.cloudflare.com/ajax/libs/emojione/2.2.6/assets/png/"


twemojiBaseUrl : String
twemojiBaseUrl =
    "https://twemoji.maxcdn.com/2/72x72/"
