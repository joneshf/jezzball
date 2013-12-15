module JezzBall where

import Keyboard
import Mouse

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
data Score  = Score Int
data State  = Level Int | BetweenLevels

-- Everything we need to know about a particular round.
data GameState = GameState State Score Balls Cursor

-- Static game stuff.
gameWidth = 640
gameHeight = 480
halfWidth = gameWidth / 2
halfHeight = gameHeight / 2

defaultGame = GameState BetweenLevels
                        (Score 0)
                        (Balls [Ball (halfWidth, halfHeight) (150, 150)])
                        (Cursor (halfWidth, halfHeight))
