module FractalSpinski exposing (..)

import Svg exposing (svg, line)
import Svg.Attributes exposing (width, height, viewBox, x1, y1, x2, y2, stroke)
import Html exposing (div)
import Browser

sierpinski long n =
    let

        p1 = (1, long * sin (pi/3))
        p2 = (1 + long, long * sin (pi/3))
        p3 = (1 + long * cos (pi/3), 0)
    in

        sierpinskiAux n p1 p2 p3

sierpinskiAux n (x1,y1) (x2,y2) (x3,y3) =
    let

        x12 = x1 + (x2 - x1)/2
        y12 = y1 + (y2 - y1)/2

        x23 = x2 + (x3 - x2)/2
        y23 = y2 + (y3 - y2)/2

        x13 = x1 + (x3 - x1)/2
        y13 = y1 + (y3 - y1)/2
    in
        if n<=0 then

            [
                [(x1,y1),(x2,y2),(x3,y3)]
            ]
        else

            sierpinskiAux (n-1) (x1,y1) (x12,y12) (x13,y13) ++

            sierpinskiAux (n-1) (x2,y2) (x12,y12) (x23,y23) ++

            sierpinskiAux (n-1) (x3,y3) (x13,y13) (x23,y23)


type Modelo = Modelo Float Int

type Accion = Nada

inicial = Modelo 400 5

actualizador accion modelo = modelo

vista (Modelo long it) =
    div [] [
        svg
        [
            width "500",
            height "500",
            viewBox "0 0 500 500"
        ]
        (crearLineasS (sierpinski long it))
    ]

crearLineasS triangs =
    crearLineasAux triangs []

crearLineasAux triangs lineas =
    case triangs of

    [(a,b), (c,d), (e,f)] :: resto ->
        let

            linea1 = line [
                    x1 (String.fromFloat a), y1 (String.fromFloat b),
                    x2 (String.fromFloat c), y2 (String.fromFloat d),
                    stroke "black"
                ] []

            linea2 = line [
                    x1 (String.fromFloat c), y1 (String.fromFloat d),
                    x2 (String.fromFloat e), y2 (String.fromFloat f),
                    stroke "black"
                ] []

            linea3 = line [
                    x1 (String.fromFloat e), y1 (String.fromFloat f),
                    x2 (String.fromFloat a), y2 (String.fromFloat b),
                    stroke "black"
                ] []

            nuevoLineas = linea1 :: linea2 :: linea3 :: lineas
        in

            crearLineasAux resto nuevoLineas
    _ -> lineas

main = Browser.sandbox {
        init = inicial,
        update = actualizador,
        view = vista
    }
