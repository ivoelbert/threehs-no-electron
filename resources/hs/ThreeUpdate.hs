module ThreeUpdate (
  setPosition,
  translateOnAxis,
  applyQuaternion,
  rotateOnAxis,
  setUp,
  lookAt,
  setScale,
  applyMatrix,
  createUpdateFunction,

  module ThreeTypes,
  module Haste.Foreign,
  module Haste.Prim
) where

{-# LANGUAGE OverloadedStrings #-}

import ThreeTypes

import Haste.Foreign
import Haste.JSON
import Haste.JSString
import Haste.Prim (toJSStr)

import Data.Set


-- Transform to JSON
matrixToJson :: Matrix4 -> JSON
matrixToJson xss = Arr (Prelude.map (\x -> Num x) (Prelude.concat xss))

propertyToJson :: ObjTransform -> JSON
propertyToJson (SetPosition vect) =
  Dict [(pack "type", (Str (pack "setPosition"))), (pack "x", Num (vx vect)), (pack "y", Num (vy vect)), (pack "z", Num (vz vect))]
propertyToJson (TranslateOnAxis vect dist) =
  Dict [(pack "type", (Str (pack "translateOnAxis"))), (pack "x", Num (vx vect)), (pack "y", Num (vy vect)), (pack "z", Num (vz vect)), (pack("dist"), Num dist)]
propertyToJson (ApplyQuaternion quat) =
  Dict [(pack "type", (Str (pack "applyQuaternion"))), (pack "x", Num (qx quat)), (pack "y", Num (qy quat)), (pack "z", Num (qz quat)), (pack("w"), Num (qw quat))]
propertyToJson (RotateOnAxis vect radians) =
  Dict [(pack "type", (Str (pack "rotateOnAxis"))), (pack "x", Num (vx vect)), (pack "y", Num (vy vect)), (pack "z", Num (vz vect)), (pack("radians"), Num radians)]
propertyToJson (SetUp vect) =
  Dict [(pack "type", (Str (pack "setUp"))), (pack "x", Num (vx vect)), (pack "y", Num (vy vect)), (pack "z", Num (vz vect))]
propertyToJson (LookAt vect) =
  Dict [(pack "type", (Str (pack "lookAt"))), (pack "x", Num (vx vect)), (pack "y", Num (vy vect)), (pack "z", Num (vz vect))]
propertyToJson (SetScale vect) =
  Dict [(pack "type", (Str (pack "setScale"))), (pack "x", Num (vx vect)), (pack "y", Num (vy vect)), (pack "z", Num (vz vect))]
propertyToJson (ApplyMatrix xss) =
  Dict [(pack "type", (Str (pack "applyMatrix"))), (pack "matrix", matrixToJson xss)]

tupleToJson :: (String, [ObjTransform]) -> (JSString, JSON)
tupleToJson (s, obj) = let jsonArr = Prelude.map propertyToJson (Prelude.reverse obj) in (pack s, Arr jsonArr)

listToJson :: [(String, [ObjTransform])] -> JSON
listToJson xs = Dict (Prelude.map tupleToJson xs)


-- Interface:
createUpdateFunction :: (Double -> Double -> Double -> ThreeAnimation ()) -> (Double -> Double -> Double -> JSString)
createUpdateFunction f = (\frame mX mY -> let list = Prelude.snd (runThreeAnimation (f frame mX mY)) in (encodeJSON (listToJson list)))


-- Transformations
addTransform :: String -> ObjTransform -> ThreeAnimation ()
addTransform name transf = Anim ( (), [(name, [transf])])

setPosition :: String -> Vector3 -> ThreeAnimation ()
setPosition name pos = addTransform name (SetPosition pos)

translateOnAxis :: String -> Vector3 -> Double -> ThreeAnimation ()
translateOnAxis name dir dist = addTransform name (TranslateOnAxis dir dist)

applyQuaternion :: String -> Quaternion -> ThreeAnimation ()
applyQuaternion name quat = addTransform name (ApplyQuaternion quat)

rotateOnAxis :: String -> Vector3 -> Double -> ThreeAnimation ()
rotateOnAxis name axis angle = addTransform name (RotateOnAxis axis angle)

setUp :: String -> Vector3 -> ThreeAnimation ()
setUp name up = addTransform name (SetUp up)

lookAt :: String -> Vector3 -> ThreeAnimation ()
lookAt name pos = addTransform name (LookAt pos)

setScale :: String -> Vector3 -> ThreeAnimation ()
setScale name scl = addTransform name (SetScale scl)

applyMatrix :: String -> Matrix4 -> ThreeAnimation ()
applyMatrix name mat = addTransform name (ApplyMatrix mat)
