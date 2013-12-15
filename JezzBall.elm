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

defaultKeyInput = KeyInput False False False False

-- We care about the space, N, P, and H keys
keyInput = lift4 KeyInput Keyboard.space
                          (Keyboard.isDown 78) -- N
                          (Keyboard.isDown 80) -- P
                          (Keyboard.isDown 72) -- H

-- Mouse

-- We need to know when the user clicks.
data MouseInput = MouseInput Bool

defaultMouseInput = MouseInput False

mouseInput = lift MouseInput Mouse.isClicked

-- Clock

-- Set the target frame rate.
delta = lift inSeconds (fps 50)

-- Combine all the inputs.
data Input = Input Float KeyInput MouseInput

input = sampleOn delta (lift3 Input delta keyInput mouseInput)

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
gameWidth = 640
gameHeight = 480
halfWidth = gameWidth / 2
halfHeight = gameHeight / 2

defaultGame = GameState BetweenLevels
                        (Score 0)
                        (Cover 0)
                        (Balls [Ball (0, 0) (150, 150)])
                        (Cursor (0, 0))

-- Updates

-- Cursor movement.

-- Ball helpers
makePos n = if n > 0 then n else (-n)
makeNeg n = if n < 0 then n else (-n)
within ep n x = n - ep < x && x < n + ep

stepVel v l u = if l then makePos v else if u then makeNeg v else v

stepBall d (Ball (x, y) (vx, vy)) =
    let vx' = stepVel vx (x < 7 - halfWidth) (x > halfWidth - 7)
        vy' = stepVel vy (y < 7 - halfHeight) (y > halfHeight - 7)
        x' = x + vx' * d
        y' = y + vy' * d
    in (Ball (x', y') (vx', vy'))

stepGame (Input d (KeyInput space n p h) (MouseInput clk))
         (GameState state score cov (Balls balls) cursor) =
    let balls' = case state of
            (Level n)     -> map (stepBall d) balls
            BetweenLevels -> balls
        state' = case state of
            -- Should update level if everything is good.
            (Level n)     -> state
            BetweenLevels -> if n then Level 1 else state
    in GameState state' score cov (Balls balls') cursor

gameState = foldp stepGame defaultGame input

-- View

scoreBoard w inPlay = "Press N to begin"

textGreen = rgb 160 200 160

txt f = text . f . monospace . Text.color textGreen . toText

msg = "Press N to begin"

make (x, y) shape = shape |> filled white
                          |> move (x, y)

makeBall (Ball pos vel) = circle 7 |> filled white
                          |> move pos

bg = rgb 60 100 60

-- Draw everything.
display (w, h) (GameState state score cover (Balls balls) (Cursor c)) =
    container w h middle <| collage gameWidth gameHeight <|
        [ rect gameWidth gameHeight |> filled bg
        , rect 10 20 |> make c
        , toForm <| if state == BetweenLevels then txt id msg else spacer 1 1
        ] ++ map makeBall balls

main = lift2 display Window.dimensions gameState
