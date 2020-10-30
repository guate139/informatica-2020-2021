module Programa exposing (..)

import Svg exposing (svg, line)
import Svg.Attributes exposing (width, height, viewBox, x1, y1, x2, y2, stroke)
import Html exposing (div, text, button)
import Html.Events exposing (onClick)
import Browser

import FractalArbol exposing (crearLineasA, arbol)
import FractalKoch exposing (crearLineasK, snowflake)
import FractalSpinski exposing (crearLineasS, sierpinski)

type Modelo = Modelo String Float Int

type Accion =
    CambiarFractal String
    | AgregarIter
    | RestarIter
    | AumentarLong
    | DisminuirLong

inicial = Modelo "Snowflake" 200 5

actualizador accion (Modelo nombre long n) =
    case accion of
    CambiarFractal nuevoNom -> Modelo nuevoNom long n
    AgregarIter -> Modelo nombre long (n+1)
    RestarIter -> Modelo nombre long (n-1)
    AumentarLong -> Modelo nombre (long+10) n
    DisminuirLong -> Modelo nombre (long-10) n

vista (Modelo nombre long n) =
    div [] [
        div [] [
            button [onClick (CambiarFractal "Snowflake")] [text "Snowflake"],
            button [onClick (CambiarFractal "Arbol")] [text "Arbol"],
            button [onClick (CambiarFractal "Sierpinski")] [text "Sierpinski"],
            button [onClick AgregarIter] [text "+ Iter"],
            button [onClick RestarIter] [text "- Iter"],
            button [onClick AumentarLong] [text "+ Longitud"],
            button [onClick DisminuirLong] [text "- Longitud"]
        ],
        if nombre == "Arbol" then
            svg
            [
                width "500",
                height "500",
                viewBox "-250 0 500 500"
            ]
            (crearLineasA (arbol long n))
        else if nombre == "Snowflake" then
            svg
            [
                width "500",
                height "500",
                viewBox "0 -75 250 250"
            ]
            (crearLineasK (snowflake long n))
        else if nombre == "Sierpinski" then
            svg
            [
                width "500",
                height "500",
                viewBox "0 0 500 500"
            ]
            (crearLineasS (sierpinski long n))
        else
            text "¡Ese fractal es inválido!"
    ]

main = Browser.sandbox {
        init = inicial,
        view = vista,
        update = actualizador
    }
