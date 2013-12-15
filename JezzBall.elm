module JezzBall where

-- Set the target frame rate.
delta = lift inSeconds $ fps 50

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
data KeyInput = KeyInput Bool Bool Bool Bool

defaultKeyInput = KeyInput False False False False

-- We care about the space, N, P, and H keys
keyInput = lift4 KeyInput Keyboard.space
                          (Keyboard.isDown 78) -- N
                          (Keyboard.isDown 80) -- P
                          (Keyboard.isDown 72) -- H

-- We need to know when the user clicks.
data MouseInput = MouseInput Bool

defaultMouseInput = MouseInput False

mouseInput = Mouse.isClicked

-- Combine all the inputs.
data Input = Input Float KeyInput MouseInput

input = delta `sampleOn` (lift3 delta keyInput mouseInput)
