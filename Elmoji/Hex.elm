module Elmoji.Hex exposing (dump)

import Array
import Char
import List
import Maybe
import String


chars : Array.Array Char
chars =
    String.toList "0123456789abcdef" |> Array.fromList


dump : Int -> String
dump =
    String.reverse << dump'


dump' : Int -> String
dump' n =
    let
        c =
            digit (n % 16)
    in
        case n // 16 of
            0 ->
                String.fromList [ c ]

            rest ->
                String.cons c <| dump' rest


digit : Int -> Char
digit =
    Maybe.withDefault 'Z' << (flip Array.get) chars



{- UNUSED


   parse : String -> Result String Int
   parse string =
       if string == "" then
           Err "the empty string is not a valid hex integer"
       else
           string |> String.reverse |> parse'


   parse' : String -> Result String Int
   parse' string =
       case String.uncons string of
           Nothing ->
               Ok 0

           Just ( c, rest ) ->
               Result.map2
                   (\ci resti ->
                       ci + resti * 16
                   )
                   (charToInt c)
                   (parse' rest)


   charToInt : Char -> Result String Int
   charToInt c =
       let
           zero =
               Char.toCode '0'

           a =
               Char.toCode 'a'

           ccode =
               Char.toCode c
       in
           if zero <= ccode && ccode <= zero + 9 then
               Result.Ok (ccode - zero)
           else if a <= ccode && ccode <= a + 5 then
               Result.Ok (ccode - a + 10)
           else
               Result.Err ("invalid hex char: '" ++ String.fromList [ c ] ++ "'")


-}
