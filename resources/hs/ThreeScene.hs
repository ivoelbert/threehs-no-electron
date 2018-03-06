module ThreeScene (
  -- Scene
  addToScene,

  -- Geometries
  boxGeometry,
  coneGeometry,
  cylinderGeometry,
  sphereGeometry,
  torusGeometry,

  -- Material
  standardMaterial,

  -- Mesh
  createMesh,

  -- Lights
  ambientLight,
  directionalLight,
  pointLight,

  -- Cameras
  perspectiveCamera,
  orthographicCamera,

  -- Transformations
  setPosition,
  translateOnAxis,
  applyQuaternion,
  rotateOnAxis,
  setUp,
  lookAt,
  setScale,
  applyMatrix,

  module ThreeTypes,
  module PrettyPrinter
) where


import ThreeTypes
import PrettyPrinter


-- Scene
addToScene :: String -> Object3D -> [ObjTransform] -> ThreeScene ()
addToScene name obj transf = Scene ((), [(name, obj, transf)])


-- Geometries
createGeometry :: Geometry -> ThreeScene Geometry
createGeometry geom = return geom

boxGeometry :: Double -> Double -> Double -> ThreeScene Geometry
boxGeometry w h d = createGeometry (BoxGeometry {width = w, height = h, depth = d})

coneGeometry :: Double -> Double -> Int -> ThreeScene Geometry
coneGeometry r h rs = createGeometry (ConeGeometry {radius = r, height = h, radialSegments = rs})

cylinderGeometry :: Double -> Double -> Double -> Int -> ThreeScene Geometry
cylinderGeometry rt rb h rs = createGeometry (CylinderGeometry {radiusTop = rt, radiusBottom = rb, height = h, radialSegments = rs})

sphereGeometry :: Double -> Int -> Int -> ThreeScene Geometry
sphereGeometry r ws hs = createGeometry (SphereGeometry {radius = r, widthSegments = ws, heightSegments = hs})

torusGeometry :: Double -> Double -> Int -> Int -> ThreeScene Geometry
torusGeometry r t rs ts = createGeometry (TorusGeometry {radius = r, tube = t, radialSegments = rs, tubularSegments = ts})


-- Materials
createMaterial :: Material -> ThreeScene Material
createMaterial mat = return mat

standardMaterial :: Color -> Color -> Double -> Double -> ThreeScene Material
standardMaterial c e r m = createMaterial (StandardMaterial {materialColor = c, emissive = e, roughness = r, metalness = m})


-- Mesh
createMesh :: Geometry -> Material -> ThreeScene Object3D
createMesh geom mat = return (ThreeMesh (Mesh geom mat))


-- Lights
createLight :: Light -> ThreeScene Object3D
createLight light = return (ThreeLight light)

ambientLight :: Color -> Double -> ThreeScene Object3D
ambientLight c i = createLight (AmbientLight {lightColor = c, intensity = i})

directionalLight :: Color -> Double -> Vector3 -> ThreeScene Object3D
directionalLight c i t = createLight (DirectionalLight {lightColor = c, intensity = i, target = t})

pointLight :: Color -> Double -> Double -> Double -> ThreeScene Object3D
pointLight c i di de = createLight (PointLight {lightColor = c, intensity = i, distance = di, decay = de})


-- Cameras
createCamera :: Camera -> ThreeScene Object3D
createCamera camera = return (ThreeCamera camera)

perspectiveCamera :: Double -> Double -> Double -> ThreeScene Object3D
perspectiveCamera fo n fa = createCamera (PerspectiveCamera {fov = fo, near = n, far = fa})

orthographicCamera :: Double -> Double -> Double -> ThreeScene Object3D
orthographicCamera w n f = createCamera (OrthographicCamera {orthoWidth = w, near = n, far = f})


-- Transformations

setPosition :: Vector3 -> ThreeScene ObjTransform
setPosition pos = return (SetPosition pos)

translateOnAxis :: Vector3 -> Double -> ThreeScene ObjTransform
translateOnAxis dir dist = return (TranslateOnAxis dir dist)

applyQuaternion :: Quaternion -> ThreeScene ObjTransform
applyQuaternion quat = return (ApplyQuaternion quat)

rotateOnAxis :: Vector3 -> Double -> ThreeScene ObjTransform
rotateOnAxis axis angle = return (RotateOnAxis axis angle)

setUp :: Vector3 -> ThreeScene ObjTransform
setUp up = return (SetUp up)

lookAt :: Vector3 -> ThreeScene ObjTransform
lookAt pos = return (LookAt pos)

setScale :: Vector3 -> ThreeScene ObjTransform
setScale scl = return (SetScale scl)

applyMatrix :: Matrix4 -> ThreeScene ObjTransform
applyMatrix mat = return (ApplyMatrix mat)
