module ThreeTypes where

import Control.Applicative
import Control.Monad (liftM, ap)


type Matrix4 = [[Double]]

identity4 :: Matrix4
identity4 = [[1, 0, 0, 0],
             [0, 1, 0, 0],
             [0, 0, 1, 0],
             [0, 0, 0, 1]]

data Quaternion = Quaternion {
                               qx :: Double,
                               qy :: Double,
                               qz :: Double,
                               qw :: Double
                             } deriving Show -- LTA gimbal lock

identityQ :: Quaternion
identityQ = Quaternion {qx = 0, qy = 0, qz = 0, qw = 1}

quaternion :: Double -> Double -> Double -> Double -> Quaternion
quaternion x y z w = Quaternion {qx = x, qy = y, qz = z, qw = w}

data Vector3 = Vector3 { vx :: Double,
                         vy :: Double,
                         vz :: Double
                       } deriving Show

vec0 :: Vector3
vec0 = Vector3 {vx = 0, vy = 0, vz = 0}

vector :: Double -> Double -> Double -> Vector3
vector x y z = Vector3 {vx = x, vy = y, vz = z}


data Color = RGB Double Double Double | HEX String deriving Show

rgb :: Double -> Double -> Double -> Color
rgb r g b = RGB r g b

hex :: String -> Color
hex h = HEX h


data ObjTransform = SetPosition Vector3 --Setea la posicion del objeto
                  | TranslateOnAxis Vector3 Double --Traslada el objeto cierta distancia por un vector
                  | ApplyQuaternion Quaternion --Aplica la rotacion representada por el cuaternion
                  | RotateOnAxis Vector3 Double --Rota el objeto ciertos radianes con respecto a un vector
                  | SetUp Vector3 --Setea el vector "up", util para LookAt
                  | LookAt Vector3 --Rota un objeto para que "mire" hacia un punto.
                  | SetScale Vector3 --Setea la escala del objeto en los ejes x y z
                  | ApplyMatrix Matrix4 deriving Show --Premultiplica la matriz de transformacion del objeto por la indicada



{---------------------------------------------------------------------------------------------------}
{-- Representa un frame de animacion en el que a un objeto se le aplican ciertas transformaciones --}
{---------------------------------------------------------------------------------------------------}

newtype ThreeAnimation a = Anim (a, [(String, [ObjTransform])]) deriving Show

runThreeAnimation :: ThreeAnimation a -> (a, [(String, [ObjTransform])])
runThreeAnimation (Anim p) = p

instance Functor ThreeAnimation where
  fmap = liftM

instance Applicative ThreeAnimation where
  pure  = return
  (<*>) = ap

instance Monad ThreeAnimation where
  return x = Anim (x, [])
  Anim (x, xs) >>= f = let Anim (x', xs') = f x
                        in Anim (x', joinTransforms xs' xs)


joinTransforms :: [(String, [ObjTransform])] -> [(String, [ObjTransform])] -> [(String, [ObjTransform])]
joinTransforms [] ts = ts
joinTransforms (x:xs) ts = joinTransforms xs (joinTransform x ts)

joinTransform :: (String, [ObjTransform]) -> [(String, [ObjTransform])] -> [(String, [ObjTransform])]
joinTransform (name, transf) [] = [(name, transf)]
joinTransform (name, transf) ((n, ts):xs) = if name == n
                                           then (n, transf ++ ts):xs
                                           else (n, ts):(joinTransform (name, transf) xs)


{-----------------------------------------------------------------------------------------------------}
{-- Representa una escena. Una lista de objetos con un nombre para poder actualizar en la animacion --}
{-----------------------------------------------------------------------------------------------------}
data Object3D = ThreeMesh Mesh | ThreeLight Light | ThreeCamera Camera deriving Show

newtype ThreeScene a = Scene (a, [(String, Object3D, [ObjTransform])]) deriving Show

runThreeScene :: ThreeScene a -> (a, [(String, Object3D, [ObjTransform])])
runThreeScene (Scene p) = p

getSceneList :: ThreeScene a -> [(String, Object3D, [ObjTransform])]
getSceneList scn = snd (runThreeScene scn)

instance Functor ThreeScene where
  fmap = liftM

instance Applicative ThreeScene where
  pure  = return
  (<*>) = ap

instance Monad ThreeScene where
  return x = Scene (x, [])
  Scene (x, xs) >>= f = let Scene (x', xs') = f x
                        in Scene (x', xs' ++ xs)


-- Geometries
data Geometry = BoxGeometry { width :: Double,
                              height :: Double,
                              depth :: Double
                            }
              | ConeGeometry { radius :: Double,
                               height :: Double,
                               radialSegments :: Int
                             }
              | CylinderGeometry { radiusTop :: Double,
                                   radiusBottom :: Double,
                                   height :: Double,
                                   radialSegments :: Int
                                 }
              | SphereGeometry { radius :: Double,
                                 widthSegments :: Int,
                                 heightSegments :: Int
                               }
              | TorusGeometry { radius :: Double,
                                tube :: Double,
                                radialSegments :: Int,
                                tubularSegments :: Int
                              } deriving Show


-- Materials
data Material = StandardMaterial { materialColor :: Color,
                                   emissive :: Color,
                                   roughness :: Double,
                                   metalness :: Double
                                 } deriving Show


-- Mesh
data Mesh = Mesh Geometry Material deriving Show


-- Lights
data Light = AmbientLight { lightColor :: Color,
                            intensity :: Double
                          }
           | DirectionalLight {
                                lightColor :: Color,
                                intensity :: Double,
                                target :: Vector3
                              }
           | PointLight {
                          lightColor :: Color,
                          intensity :: Double,
                          distance :: Double,
                          decay :: Double
                        } deriving Show


-- Cameras
data Camera = PerspectiveCamera {
                                  fov :: Double,
                                  near :: Double,
                                  far :: Double
                                }
            | OrthographicCamera {
                                   orthoWidth :: Double,
                                   near :: Double,
                                   far :: Double
                                 } deriving Show
