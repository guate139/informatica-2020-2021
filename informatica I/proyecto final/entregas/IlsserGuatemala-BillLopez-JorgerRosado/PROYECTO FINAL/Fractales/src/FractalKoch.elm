module FractalKoch exposing (..)

import Svg exposing (svg, line)
import Svg.Attributes exposing (width, height, viewBox, x1, y1, x2, y2, stroke)
import Html exposing (div)
import Browser

punto1 (xi, yi) (xf, yf) =
    let
        x1 = xi + (xf-xi)/3
        y1 = yi + (yf-yi)/3
    in
        (x1, y1)

punto3 (xi, yi) (xf, yf) =
    let
        x3 = xf - (xf-xi)/3
        y3 = yf - (yf-yi)/3
    in
        (x3, y3)

punto2 (x1, y1) (x3, y3) =
    let
        x2 = (x1 + x3) * cos (pi/3) - (y3 - y1) * sin (pi/3)
        y2 = (y1 + y3) * cos (pi/3) + (x3 - x1) * sin (pi/3)
    in
        (x2, y2)

curvaKoch (xi, yi) (xf, yf) n =
    let

        inic = (xi, yi)
        fin = (xf, yf)
        p1 = punto1 inic fin
        p3 = punto3 inic fin
        p2 = punto2 p1 p3
    in
        if n <= 0 then
            [inic, fin]
        else if n == 1 then

            [inic, p1, p2, p3, fin]
        else
            curvaKoch inic p1 (n-1) ++
            curvaKoch p1 p2 (n-1) ++
            curvaKoch p2 p3 (n-1) ++
            curvaKoch p3 fin (n-1)

snowflake long n =
    let
        p1 = (1, 0)
        p2 = (1 + long, 0)
        p3 = (1 + long * cos (pi/3), long * sin (pi/3))
    in
        curvaKoch p1 p3 n ++
        curvaKoch p3 p2 n ++
        curvaKoch p2 p1 n


type Modelo = Modelo Float Int

type Accion = Nada

inicial = Modelo 200 5

actualizador accion modelo = modelo

vista (Modelo long it) =
    div [] [
        svg
        [
            width "500",
            height "500",
            viewBox "0 -75 250 250"
        ]
        (crearLineasK (snowflake long it))
    ]

crearLineasK coords =
    crearLineasAux coords []

crearLineasAux coords lineas =
    case coords of
    (a,b) :: (c,d) :: resto ->
        let
            linea = line [
                    x1 (String.fromFloat a), y1 (String.fromFloat b),
                    x2 (String.fromFloat c), y2 (String.fromFloat d),
                    stroke "black"
                ] []
            nuevoLineas = linea :: lineas
        in
            crearLineasAux ((c,d) :: resto) nuevoLineas
    _ -> lineas

main = Browser.sandbox {
        init = inicial,
        update = actualizador,
        view = vista
    }
