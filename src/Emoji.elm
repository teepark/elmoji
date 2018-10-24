module Emoji exposing (replaceWithEmojiOne, replaceWithTwemoji, textWith, text_)

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

import Emoji.Internal.Parse exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import List
import String


{-| Convert a String with unicode emoji characters into an Html element
containing the text with `<img>` tags replacing the emojis.

This function produces a `<span class='elm-emoji'>` containing the text, replacing
emojis with `<img class='elm-emoji-img elm-emoji-one'>` tags pointing to CDN-hosted
[EmojiOne](http://emojione.com/).

    div [] [ text' "Live long and prosper 🖖" ]

-}
text_ : String -> Html a
text_ =
    textWith replaceWithEmojiOne >> span [ class "elm-emoji" ]


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
[EmojiOne](http://emojione.com/), with classes `elm-emoji-img` and `elm-emoji-one`.

    text' : String -> Html a
    text' =
        textWith replaceWithEmojiOne >> span [ class "elm-emoji" ]

-}
replaceWithEmojiOne : List String -> Html a
replaceWithEmojiOne codepts =
    img
        [ src <| urlWithBase emojiOneBaseUrl codepts
        , class "elm-emoji-img elm-emoji-one"
        ]
        []


{-| Convert an emoji unicode sequence into a
[Twemoji](http://twitter.github.io/twemoji/) `<img>` tag. It will have CSS
classes `elm-emoji-img` and `elm-emoji-twem`.

    text' : String -> Html a
    text' body =
        span [] (textWith replaceWithTwemoji body)

-}
replaceWithTwemoji : List String -> Html a
replaceWithTwemoji codepts =
    img
        [ src <| urlWithBase twemojiBaseUrl codepts
        , class "elm-emoji-img elm-emoji-twem"
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
