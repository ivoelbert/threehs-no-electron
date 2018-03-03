{-# LANGUAGE OverloadedStrings #-}

import ThreeUpdate

bolaY :: Double -> Double -> Double
bolaY f s = (abs (sin (f * s))) + 0.5

update :: Double -> Double -> Double -> ThreeAnimation ()
update frame mouseX mouseY = let speed = 0.2
                             in do
                                newAnimation
                                setPosition "dirLight" (vector (-3) 3 5)
                                setPosition "camara" (vector 0 3 5)
                                setUp "camara" (vector 0 1 0)
                                lookAt "camara" (vector 0 0 0)
                                setPosition "mesa" (vector 0 (-0.5) 0)
                                setPosition "bola" (vector (2 * mouseX) (bolaY frame speed) (-2 * mouseY))

main = do
       export "updates" $ createUpdateFunction update
