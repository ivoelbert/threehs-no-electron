{-# LANGUAGE OverloadedStrings #-}

import ThreeUpdate

update :: Double -> Double -> Double -> ThreeAnimation ()
update frame mouseX mouseY = do
                             newAnimation
                             setPosition "dirLight" (vector (-3) 3 5)
                             setPosition "camara" (vector 0 3 5)
                             setUp "camara" (vector 0 1 0)
                             lookAt "camara" (vector 0 0 0)
                             setPosition "mesa" (vector 0 (-0.5) 0)
                             setPosition "bola" (vector (2 * mouseX) 0.5 (-2 * mouseY))

main = do
       export "updates" $ createUpdateFunction update
