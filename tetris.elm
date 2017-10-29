module Main exposing (main)

import AnimationFrame
import Html exposing (Html)
import Html.Attributes exposing (width, height, style)
import Math.Matrix4 as Mat4 exposing (..)
import Math.Vector3 as Vec3 exposing (vec3, Vec3)
import Math.Vector2 as Vec2 exposing (Vec2)
import Time exposing (Time)
import WebGL exposing (Mesh, Shader)
import Keyboard exposing (downs, KeyCode)


type alias Game =
    { player : Player
    , dropTimer : Float
    }


type alias Player =
    { position : Vec3
    , mesh : Mesh Vertex
    }


mesh : Mesh Vertex
mesh =
    let
        sz =
            25
    in
        WebGL.triangles
            (gridMesh ( 0, 0 ) t)


t : List (List Int)
t =
    [ [ 1, 1, 1 ]
    , [ 0, 1, 0 ]
    ]


board : List (List Int)
board =
    [ [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    , [ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 ]
    ]


initGame =
    { player = Player (Vec3.vec3 6 0 0) mesh
    , dropTimer = 0
    }


main =
    Html.program
        { init = ( initGame, Cmd.none )
        , view = view
        , subscriptions =
            (\model ->
                Sub.batch
                    [ AnimationFrame.diffs Frame
                    , Keyboard.downs KeyDown
                    ]
            )
        , update = update
        }


type Msg
    = Frame Time
    | KeyDown KeyCode


update : Msg -> Game -> ( Game, Cmd Msg )
update msg ({ player, dropTimer } as game) =
    case msg of
        Frame time ->
            let
                diff =
                    Time.inMilliseconds time
            in
                if (dropTimer >= 2000) then
                    ( { game | player = { player | position = (Vec3.add (Vec3.vec3 0 1 0) player.position) }, dropTimer = 0 }, Cmd.none )
                else
                    ( { game | dropTimer = dropTimer + diff }, Cmd.none )

        KeyDown keyCode ->
            if keyCode == 40 then
                ( dropPiece game, Cmd.none )
            else
                ( game, Cmd.none )


view : Game -> Html msg
view ({ player } as game) =
    WebGL.toHtml
        [ width 400
        , height 500
        , style [ ( "display", "block" ) ]
        ]
        [ WebGL.entity
            vertexShader
            fragmentShader
            player.mesh
            { perspective = Mat4.mul perspective (Mat4.makeTranslate (Vec3.scale 25 player.position)) }
        , WebGL.entity
            vertexShader
            fragmentShader
            (WebGL.triangles (gridMesh ( 0, 0 ) board))
            { perspective = perspective }
        ]


perspective : Mat4
perspective =
    (Mat4.makeOrtho2D 0 400 500 0)


type alias Vertex =
    { position : Vec3
    , color : Vec3
    }


dropPiece : Game -> Game
dropPiece ({ player } as game) =
    { game | player = { player | position = Vec3.add game.player.position (Vec3.vec3 0 1 0) } }


moveVertex : ( Float, Float ) -> Vertex -> Vertex
moveVertex ( x, y ) ({ position, color } as vertex) =
    let
        m =
            Mat4.makeTranslate3 x y 0
    in
        Vertex (Mat4.transform m position) color


moveTriangle : ( Float, Float ) -> ( Vertex, Vertex, Vertex ) -> ( Vertex, Vertex, Vertex )
moveTriangle ( x, y ) ( a, b, c ) =
    ( moveVertex ( x, y ) a
    , moveVertex ( x, y ) b
    , moveVertex ( x, y ) c
    )


square : Float -> ( Float, Float ) -> List ( Vertex, Vertex, Vertex )
square sz ( x, y ) =
    [ ( Vertex (vec3 0 0 0) (vec3 1 0 0)
      , Vertex (vec3 sz 0 0) (vec3 1.0 0 0)
      , Vertex (vec3 sz sz 0) (vec3 1 0 0)
      )
    , ( Vertex (vec3 0 0 0) (vec3 1 0 0)
      , Vertex (vec3 0 sz 0) (vec3 1 0 0)
      , Vertex (vec3 sz sz 0) (vec3 1 0 0)
      )
    ]
        |> List.map (moveTriangle ( x, y ))


gridMesh : ( Float, Float ) -> List (List Int) -> List ( Vertex, Vertex, Vertex )
gridMesh ( x, y ) g =
    List.indexedMap
        (\row colValues ->
            (List.indexedMap
                (\col value ->
                    if value == 1 then
                        Just (square 25 ( x + (toFloat col) * 25, y + (toFloat row) * 25 ))
                    else
                        Nothing
                )
                colValues
            )
                |> List.map (Maybe.withDefault [])
                |> List.concat
        )
        g
        |> List.concat



-- Shaders


type alias Uniforms =
    { perspective : Mat4 }


vertexShader : Shader Vertex Uniforms { vcolor : Vec3 }
vertexShader =
    [glsl|
        attribute vec3 position;
        attribute vec3 color;
        uniform mat4 perspective;
        varying vec3 vcolor;
        void main () {
            gl_Position = perspective * vec4(position, 1.0);
            vcolor = color;
        }
    |]


fragmentShader : Shader {} Uniforms { vcolor : Vec3 }
fragmentShader =
    [glsl|
        precision mediump float;
        varying vec3 vcolor;
        void main () {
            gl_FragColor = vec4(vcolor, 1.0);
        }
    |]
