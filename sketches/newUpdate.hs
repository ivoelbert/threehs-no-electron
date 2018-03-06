{-# LANGUAGE OverloadedStrings #-}
import ThreeUpdate

myFirstUpdate :: Double -> Double -> Double -> ThreeAnimation ()

myFirstUpdate frame mouseX mouseY = do
                                    newAnimation
                                    rotateOnAxis "cube" (vector 0 1 0) 0.05

main = do
       export "updates" $ createUpdateFunction myFirstUpdate
