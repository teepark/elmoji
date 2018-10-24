module Emoji.Internal.Parse exposing (Chunk(..), String_(..), parse)

import Dict
import Emoji.Internal.Valid exposing (Store(..), longest, store)
import List
import String


type Chunk
    = StringChunk String
    | CodeChunk (List String)


type String_
    = String_ (List Chunk)


parse : String -> String_
parse string =
    let
        string_ =
            parse_ "" [] string
    in
    String_ <| List.reverse string_


parse_ : String -> List Chunk -> String -> List Chunk
parse_ buf accum string =
    case ( string, buf ) of
        ( "", "" ) ->
            accum

        ( "", _ ) ->
            StringChunk (String.reverse buf) :: accum

        _ ->
            case splitPrefix string of
                ( ( 0, _ ), _ ) ->
                    case String.uncons string of
                        Nothing ->
                            accum

                        Just ( c, rest ) ->
                            parse_ (String.cons c buf) accum rest

                ( ( matchLen, matchCodes ), remaining ) ->
                    let
                        nextAccum =
                            if buf == "" then
                                accum

                            else
                                StringChunk (String.reverse buf) :: accum
                    in
                    parse_ "" (CodeChunk matchCodes :: nextAccum) remaining


dropLeft : Int -> String -> String
dropLeft n string =
    if n < 1 then
        string

    else
        string
            |> String.uncons
            |> Maybe.map (Tuple.second >> dropLeft (n - 1))
            |> Maybe.withDefault string


splitPrefix : String -> ( ( Int, List String ), String )
splitPrefix string =
    let
        ( len, code ) =
            findPrefix ( 0, [] ) 0 string store
    in
    ( ( len, code )
    , dropLeft len string
    )


findPrefix : ( Int, List String ) -> Int -> String -> Store -> ( Int, List String )
findPrefix lastFound count string store =
    if count > longest then
        lastFound

    else
        let
            (Store foundCode children) =
                store

            bestMatch =
                Maybe.withDefault
                    lastFound
                    (Maybe.map
                        (\code -> ( count, code ))
                        foundCode
                    )
        in
        case String.uncons string of
            Nothing ->
                bestMatch

            Just ( char, rest ) ->
                case Dict.get char children of
                    Nothing ->
                        bestMatch

                    Just childStore ->
                        findPrefix
                            bestMatch
                            (count + 1)
                            rest
                            childStore



{- findPrefix Maybes:

         |   no child   |    child     |
   ------+--------------+--------------+-
         |              |              |
   no    |    return    | recurse with |
   match |  last match  |  last match  | <- "last match"
         |              |              |
   ------+--------------+--------------+-
         |              |              |
   match |    return    | recurse with |
         |  new match   |  "savepoint" | <- "new match"
         |              |              |
   ------+--------------+--------------+-
         |      ^       |      ^       |
             "return"      "recurse"

   (also the String.uncons, but we need the char
   to test for a child so it's not independent)
-}
