module FractalArbol exposing (..)

import Svg exposing (svg, line)
import Svg.Attributes exposing (width, height, viewBox, x1, y1, x2, y2, stroke)
import Html exposing (div)
import Browser

arbol long n =
    arbolAux (long*2/3) n (0,500) (0, 500-long) (pi/2)

arbolAux long n (xi,yi) (xf,yf) ang =
    let
        ang1 = ang - pi/4
        x1 = xf + long * cos (ang1)
        y1 = yf - long * sin (ang1)

        ang2 = ang + pi/4
        x2 = xf + long * cos (ang2)
        y2 = yf - long * sin (ang2)
    in
        if n <= 0 then

            [
                [(xi,yi), (xf,yf)]
            ]
        else

            [
                [(xi,yi), (xf,yf)]
            ] ++
            arbolAux (long*2/3) (n-1) (xf,yf) (x1,y1) ang1 ++
            arbolAux (long*2/3) (n-1) (xf,yf) (x2,y2) ang2


type Modelo = Modelo Float Int

type Accion = Nada

inicial = Modelo 100 12

actualizador accion modelo = modelo

vista (Modelo long it) =
    div [] [
        svg
        [
            width "500",
            height "500",
            viewBox "-250 0 500 500"
        ]
        (crearLineasA (arbol long it))
    ]

crearLineasA lista =
    crearLineasAux lista []

crearLineasAux lista lineas =
    case lista of
    [(a,b), (c,d)] :: resto ->
        let
            linea = line [
                    x1 (String.fromFloat a), y1 (String.fromFloat b),
                    x2 (String.fromFloat c), y2 (String.fromFloat d),
                    stroke "black"
                ] []
            nuevoLineas = linea :: lineas
        in
            crearLineasAux resto nuevoLineas
    _ -> lineas

main = Browser.sandbox {
        init = inicial,
        update = actualizador,
        view = vista
    }
