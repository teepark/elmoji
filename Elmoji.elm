module Elmoji exposing (String'(..), Chunk(..), parse)

import Dict
import List
import String
import Elmoji.Internal.Valid exposing (Store(..), store, longest)


type Chunk
    = StringChunk String
    | CodeChunk (List Int)


type String'
    = String' (List Chunk)


parse : String -> String'
parse string =
    let
        string' =
            parse' "" [] string
    in
        String' <| List.reverse string'


parse' : String -> List Chunk -> String -> List Chunk
parse' buf accum string =
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
                            parse' (String.cons c buf) accum rest

                ( ( matchLen, matchCodes ), remaining ) ->
                    let
                        accum =
                            if buf == "" then
                                accum
                            else
                                (StringChunk (String.reverse buf)) :: accum
                    in
                        parse' "" ((CodeChunk matchCodes) :: accum) remaining


splitPrefix : String -> ( ( Int, List Int ), String )
splitPrefix string =
    let
        ( len, code ) =
            findPrefix ( 0, [] ) 0 string store
    in
        ( ( len, code )
        , String.dropLeft len string
        )


findPrefix : ( Int, List Int ) -> Int -> String -> Store -> ( Int, List Int )
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
