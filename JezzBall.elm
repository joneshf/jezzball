import Keyboard
import Mouse
import Window

-- Utils

{-
    We need three parts to this
    1 Models
        This includes:
        A Inputs
            i   Keyboard
                a (N)ew Game
                b (P)ause
                c (H)elp
            ii  Mouse
                a left spit out lines
                b rotate cursor
            iii Clock
            iv  ETC
        B The actual game
            i   Cursor
            ii  Balls
            iii Score
            iv  ETC
    2 Updates
    3 Views
-}

-- Models

-- Inputs

-- Keyboard
data KeyInput = KeyInput Bool Bool Bool Bool

defaultKeyInput : KeyInput
defaultKeyInput = KeyInput False False False False

-- We care about the space, N, P, and H keys
keyInput : Signal KeyInput
keyInput = KeyInput <~ Keyboard.space
                     ~ Keyboard.isDown 78 -- N
                     ~ Keyboard.isDown 80 -- P
                     ~ Keyboard.isDown 72 -- H

-- Mouse

-- We need to know when the user clicks.
data MouseInput = MouseInput Bool (Int, Int) (Int, Int)

defaultMouseInput : MouseInput
defaultMouseInput = MouseInput False (0, 0) (0, 0)

mouseInput : Signal MouseInput
mouseInput = MouseInput <~ Mouse.isDown ~ Mouse.position ~ Window.dimensions

-- Clock

-- Set the target frame rate.
delta : Signal Time
delta = inSeconds <~ fps 50

-- Combine all the inputs.
data Input = Input Float KeyInput MouseInput

input : Signal Input
input = sampleOn delta (Input <~ delta ~ keyInput ~ mouseInput)

-- Game inputs

data Cursor    = Cursor (Float, Float) Bool
data Ball      = Ball (Float, Float) (Float, Float)
data Balls     = Balls [Ball]
data Direction = U | D | L | R
data TempWall  = TempWall (Float, Float) (Float, Float) Direction
data TempWalls = TempWalls [TempWall]
data Cover     = Cover (Float, Float) (Float, Float)
data Covers    = Covers [Cover]
data Score     = Score Int
data Lives     = Lives Int
data State     = Level Int | BetweenLevels | Pause

-- Everything we need to know about a particular round.
data GameState = GameState State Lives Score Covers TempWalls Balls Cursor

-- Static game stuff.
gameWidth : number
gameWidth = 640

gameHeight : number
gameHeight = 480

halfWidth : number
halfWidth = gameWidth / 2

halfHeight : number
halfHeight = gameHeight / 2

-- Create four covers for the outside of the playing area.
defaultCovers : [Cover]
defaultCovers = [ Cover (-halfWidth, gameHeight) (halfWidth, halfHeight)
                , Cover (-halfWidth, -halfHeight) (halfWidth, -gameHeight)
                , Cover (-gameWidth, halfHeight) (-halfWidth, -halfHeight)
                , Cover (halfWidth, halfHeight) (gameWidth, -halfHeight)
                ]

defaultGame : GameState
defaultGame = GameState BetweenLevels
                        (Lives 3)
                        (Score 0)
                        (Covers defaultCovers)
                        (TempWalls [])
                        (Balls [ Ball (0, 50) (-100, -100)
                               , Ball (-25, 30) (100, 100)
                               ])
                        (Cursor (0, 0) True)

-- Updates

-- Cursor helpers.
toFloat2 : (Int, Int) -> (Float, Float)
toFloat2 (x, y) = (toFloat x, toFloat y)

newPos : (Int, Int) -> (Int, Int) -> (Float, Float)
newPos (x, y) (w, h) = (toFloat x - (toFloat w /  2), toFloat h / 2 - toFloat y)

-- Ball helpers
makePos : number -> number
makePos n = if n > 0 then n else (-n)

makeNeg : number -> number
makeNeg n = if n < 0 then n else (-n)

within : Float -> Float -> Float -> Bool
within ep n x = n - ep < x && x < n + ep

stepVel : Float -> Bool -> Bool -> Float
stepVel v l u = if l then makePos v else if u then makeNeg v else v

lowCol : Float -> Float -> Bool
lowCol n m = n < 7 - m

highCol : Float -> Float -> Bool
highCol n m = n > m - 7

lowColX : Float -> (Float, Float) -> Bool
lowColX x (start, stop) = start + 7 < x && stop + 7 < x

highColX : Float -> (Float, Float) -> Bool
highColX x (start, stop) = x < start - 7 && x < stop - 7

lowColY : Float -> (Float, Float) -> Bool
lowColY y (start, stop) = y < start - 7 && y <  stop - 7

highColY : Float -> (Float, Float) -> Bool
highColY y (start, stop) = y > stop + 7

between : Float -> Float -> Float -> Bool
between n x x' = x <= n && n <= x' || x' <= n && n <= x'

betweenX : (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
betweenX (n, _) (x, _) (x', _) = between n x x'

betweenY : (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
betweenY (_, n) (_, x) (_, x') = between n x x'

hitCoverX : (Float, Float) -> [Cover] -> Bool
hitCoverX pos covs = case covs of
    []                     -> False
    (Cover start stop)::cs -> betweenX pos start stop || hitCoverX pos cs

hitCoverY : (Float, Float) -> [Cover] -> Bool
hitCoverY pos covs = case covs of
    []                     -> False
    (Cover start stop)::cs -> betweenY pos start stop || hitCoverY pos cs

starts : Cover -> (Float, Float)
starts (Cover start _) = start

stops : Cover -> (Float, Float)
stops (Cover _ stop) = stop

stepBall : Float -> [Cover] -> Ball -> Ball
stepBall d cov (Ball (x, y) (vx, vy)) =
    let
        startXs = map (fst . starts) cov
        stopXs  = map (fst . stops) cov
        startYs = map (snd . starts) cov
        stopYs  = map (snd . stops) cov
        xPairs = zip startXs stopXs
        yPairs = zip startYs stopYs
        lowColXs  = any (lowCol x)  <| map (fst . starts) cov
        highColXs = any (highCol x) <| map (fst . stops)  cov
        lowColYs  = any (lowCol y)  <| map (snd . starts) cov
        highColYs = any (highCol y) <| map (snd . stops)  cov
        vx' = stepVel vx lowColXs highColXs
        vy' = stepVel vy lowColYs highColYs
        x' = x + vx' * d
        y' = y + vy' * d
    in (Ball (x', y') (vx', vy'))


wallInc : Float
wallInc = 5

stepWall : TempWall -> TempWall
stepWall (TempWall start (x, y) dir) =
    case dir of
        U -> TempWall start (x, y) U
        D -> TempWall start (x, y - wallInc) D
        L -> TempWall start (x, y) L
        R -> TempWall start (x - wallInc, y) R

stepGame : Input -> GameState -> GameState
stepGame (Input d (KeyInput space n p h) (MouseInput clk pos win))
         (GameState state lives score (Covers cov) (TempWalls tws) (Balls balls) (Cursor _ vert)) =
    let balls' = case state of
            (Level n)     -> map (stepBall d cov) balls
            BetweenLevels -> balls
        state' = case state of
            -- Should update level if everything is good.
            (Level n)     -> state
            BetweenLevels -> if n then Level 1 else state
        pos' = newPos pos win
        vert' = if space then not vert else vert
        cursor' = Cursor pos' vert'
        newWalls = if clk && state /= BetweenLevels && tws == [] then
                if vert' then [TempWall pos' pos' U, TempWall pos' pos' D]
                else [TempWall pos' pos' L, TempWall pos' pos' R]
            else []
        tws' = map stepWall tws
        tws'' = tws' ++ newWalls
    in GameState state' lives score (Covers cov) (TempWalls tws'') (Balls balls') cursor'

gameState : Signal GameState
gameState = foldp stepGame defaultGame input

-- View

bgGreen : Color
bgGreen = rgb 60 100 60

bgBlue : Color
bgBlue = rgb 60 60 100

bgGray : Color
bgGray = rgb 230 210 180

fgGreen : Color
fgGreen = rgb 160 200 160

fgBlue : Color
fgBlue = rgb 160 160 200

fgGray : Color
fgGray = rgb 0 0 0

txt : Color -> (Text -> Text) -> String -> Element
txt col f = text . f . monospace . Text.color col . toText

txtBlue : String -> Element
txtBlue = txt fgBlue id

txtGreen : String -> Element
txtGreen = txt fgGreen id

txtGray : String -> Element
txtGray = txt fgGray id

msg : String
msg = "Press N to begin"

make : Color -> Shape -> (Float, Float) -> Form
make col shape pos = shape |> filled col |> move pos

makeCursor : Bool -> (Float, Float) -> Form
makeCursor vert =
    let shape = if vert then rect 15 30 else rect 30 15
    in make gray shape

makeBall : Ball -> Form
makeBall (Ball pos vel) = make white (circle 10) pos

makeWall : TempWall -> Form
makeWall (TempWall ((x, y) as pos) ((x', y') as pos') dir) = case dir of
    U -> make black (rect 15 (abs (y' - y))) pos'
    D -> make black (rect 15 (abs (y' - y))) pos
    L -> make black (rect (abs (x' - x)) 15) pos'
    R -> make black (rect (abs (x' - x)) 15) pos

makeCover : Cover -> Form
makeCover (Cover (x, y) (x', y')) =
    make black (rect (x' - x) (y - y')) ((x - x') / 2 + x', (y - y') / 2 + y')

-- Draw everything.
display : (Int, Int) -> GameState -> Element
display (w, h) (GameState state (Lives l) (Score s) (Covers covs) (TempWalls tws) (Balls balls) (Cursor pos vert)) =
    layers
        [ container w h middle <| collage (gameWidth + 80) (gameHeight + 80) <|
            [ filled bgGray (rect (gameWidth + 80) (gameHeight + 80))
            , rect gameWidth gameHeight |> filled bgBlue
            ]
        , container w h middle <| collage gameWidth gameHeight <|
            map makeWall tws ++ map makeBall balls ++ map makeCover covs ++ [makeCursor vert pos]
        , container w h middle <| collage gameWidth (gameHeight + 80) <|
            [ move (0, gameHeight / 2 + 20) <| toForm <| flow right
                [ txtGray <| "Lives: " ++ (show l) ++ "  Score: " ++ (show s)
                ]
            ]
        , container w h middle <| flow right
            [ if state == BetweenLevels then txtBlue msg else spacer 1 1
            ]
        ]

main : Signal Element
main = display <~ Window.dimensions ~ gameState
