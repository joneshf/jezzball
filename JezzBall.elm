import Keyboard
import Mouse
import Window

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
keyInput = lift4 KeyInput Keyboard.space
                          (Keyboard.isDown 78) -- N
                          (Keyboard.isDown 80) -- P
                          (Keyboard.isDown 72) -- H

-- Mouse

-- We need to know when the user clicks.
data MouseInput = MouseInput Bool (Int, Int) (Int, Int)

defaultMouseInput : MouseInput
defaultMouseInput = MouseInput False (0, 0) (0, 0)

mouseInput : Signal MouseInput
mouseInput = MouseInput <~ Mouse.isClicked ~ Mouse.position ~ Window.dimensions

-- Clock

-- Set the target frame rate.
delta : Signal Time
delta = lift inSeconds (fps 50)

-- Combine all the inputs.
data Input = Input Float KeyInput MouseInput

input : Signal Input
input = sampleOn delta (Input <~ delta ~ keyInput ~ mouseInput)

-- Game inputs

data Cursor = Cursor (Float, Float)
data Ball   = Ball (Float, Float) (Float, Float)
data Balls  = Balls [Ball]
data Cover  = Cover Float
data Score  = Score Int
data State  = Level Int | BetweenLevels

-- Everything we need to know about a particular round.
data GameState = GameState State Score Cover Balls Cursor

-- Static game stuff.
gameWidth : number
gameWidth = 640

gameHeight : number
gameHeight = 480

halfWidth : number
halfWidth = gameWidth / 2

halfHeight : number
halfHeight = gameHeight / 2

defaultGame : GameState
defaultGame = GameState BetweenLevels
                        (Score 0)
                        (Cover 0)
                        (Balls [Ball (0, 0) (150, 150)])
                        (Cursor (0, 0))

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

stepBall : Float -> Ball -> Ball
stepBall d (Ball (x, y) (vx, vy)) =
    let vx' = stepVel vx (x < 7 - halfWidth) (x > halfWidth - 7)
        vy' = stepVel vy (y < 7 - halfHeight) (y > halfHeight - 7)
        x' = x + vx' * d
        y' = y + vy' * d
    in (Ball (x', y') (vx', vy'))

stepGame : Input -> GameState -> GameState
stepGame (Input d (KeyInput space n p h) (MouseInput clk pos win))
         (GameState state score cov (Balls balls) (Cursor c)) =
    let balls' = case state of
            (Level n)     -> map (stepBall d) balls
            BetweenLevels -> balls
        state' = case state of
            -- Should update level if everything is good.
            (Level n)     -> state
            BetweenLevels -> if n then Level 1 else state
        pos' = newPos pos win
        cursor' = Cursor pos'
    in GameState state' score cov (Balls balls') cursor'

gameState : Signal GameState
gameState = foldp stepGame defaultGame input

-- View

bg : Color
bg = rgb 60 100 60

textGreen : Color
textGreen = rgb 160 200 160

txt : (Text -> Text) -> String -> Element
txt f = text . f . monospace . Text.color textGreen . toText

msg : String
msg = "Press N to begin"

make : (Float, Float) -> Shape -> Form
make (x, y) shape = shape |> filled white
                          |> move (x, y)

makeBall : Ball -> Form
makeBall (Ball pos vel) = circle 7 |> filled white
                          |> move pos

-- Draw everything.
display : (Int, Int) -> GameState -> Element
display (w, h) (GameState state score cover (Balls balls) (Cursor c)) =
    container w h middle <| collage gameWidth gameHeight <|
        [ rect gameWidth gameHeight |> filled bg
        , rect 10 20 |> make c
        , toForm <| if state == BetweenLevels then txt id msg else spacer 1 1
        ] ++ map makeBall balls

main : Signal Element
main = lift2 display Window.dimensions gameState
