{-# LANGUAGE OverloadedStrings #-}

import ThreeUpdate

bolaY :: Double -> Double -> Double
bolaY f s = (abs (sin (f * s))) + 0.5

update :: Double -> Double -> Double -> ThreeAnimation ()
update frame mouseX mouseY = let speed = 0.2
                             in do
                                setPosition "bola" (vector (2 * mouseX) (bolaY frame speed) (-2 * mouseY))

main = do
       export "updates" $ createUpdateFunction update
