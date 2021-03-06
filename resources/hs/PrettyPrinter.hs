module PrettyPrinter (
  printScene
) where

import Text.PrettyPrint.HughesPJ
import ThreeTypes
import Data.List

printScene :: ThreeScene () -> IO ()
printScene scn = let pp = checkIntegrity (getSceneList scn)
                  in case pp of
                       Left err -> putStr err
                       Right str -> do putStr "success!\n"
                                       prelude <- readFile "../js/threePrelude.js"
                                       epilogue <- readFile "../js/threeEpilogue.js"
                                       writeFile "../js/SKETCH.js" $ prelude ++ str ++ epilogue


-- check scene's integrity
checkIntegrity :: [(String, Object3D, [ObjTransform])] -> Either String String
checkIntegrity sceneList = let cameras = countCameras sceneList
                           in if cameras == 1
                              then case duplicates sceneList of
                                     Just name -> Left $ "Name \"" ++ name ++ "\" corresponds to multiple objects!\n"
                                     Nothing -> Right $ render $ pp sceneList
                              else if cameras < 1
                                   then Left "No camera to render the scene!\n"
                                   else Left "You can't have multiple cameras in the same scene!\n"

duplicates :: [(String, Object3D, [ObjTransform])] -> Maybe String
duplicates list = findDuplicate (Prelude.map fst3 list)

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a

count :: Eq a => a -> [a] -> Int
count e [] = 0
count e (a:xs) = (count e xs +) $ if a == e then 1 else 0

findDuplicate :: [String] -> Maybe String
findDuplicate [] = Nothing
findDuplicate (x:xs) = if count x xs > 0 then Just x else findDuplicate xs

countCameras :: [(String, Object3D, [ObjTransform])] -> Int
countCameras [] = 0
countCameras ((_, ThreeCamera _, _):xs) = countCameras xs + 1
countCameras (_:xs) = countCameras xs


-- Print a scene
pp :: [(String, Object3D, [ObjTransform])] -> Doc
pp [] = empty
pp (x:xs) = printObj x <>
            text "\n" <>
            pp xs

printObj :: (String, Object3D, [ObjTransform]) -> Doc
printObj (name, ThreeMesh mesh, transf) = printMesh name mesh transf
printObj (name, ThreeLight light, transf) = printLight name light transf
printObj (name, ThreeCamera camera, transf) = printCamera name camera transf


-- Print transformations
printTransf :: String -> [ObjTransform] -> Doc
printTransf vName transf = let transfDocs = (Prelude.map (ppTransf vName) transf)
                           in Prelude.foldl (<>) empty transfDocs

ppTransf :: String -> ObjTransform -> Doc
ppTransf vName (SetPosition pos) =
    text (vName ++ ".position.set( " ++ (show (vx pos)) ++ ", " ++ (show (vy pos)) ++ ", " ++ (show (vz pos)) ++ " );\n")
ppTransf vName (TranslateOnAxis ax dist) =
    text ("let " ++ vName ++ "Axis = new THREE.Vector3( " ++ (show (vx ax)) ++ ", " ++ (show (vy ax)) ++ ", " ++ (show (vz ax)) ++ " );\n") <>
    text (vName ++ ".translateOnAxis( " ++ vName ++ "Axis, " ++ (show dist) ++ " );\n")
ppTransf vName (ApplyQuaternion quat) =
    text ("let " ++ vName ++ "Quat = new THREE.Quaternion( " ++ (show (qx quat)) ++ ", " ++ (show (qy quat)) ++ ", " ++ (show (qz quat)) ++ ", " ++ (show (qw quat)) ++ " );\n") <>
    text (vName ++ ".applyQuaternion( " ++ vName ++ "Quat );\n")
ppTransf vName (RotateOnAxis ax ang) =
    text ("let " ++ vName ++ "Axis = new THREE.Vector3( " ++ (show (vx ax)) ++ ", " ++ (show (vy ax)) ++ ", " ++ (show (vz ax)) ++ " );\n") <>
    text (vName ++ ".rotateOnAxis( " ++ vName ++ "Axis, " ++ (show ang) ++ " );\n")
ppTransf vName (SetUp vec) =
    text (vName ++ ".up.set( " ++ (show (vx vec)) ++ ", " ++ (show (vy vec)) ++ ", " ++ (show (vz vec)) ++ " );\n")
ppTransf vName (LookAt vec) =
    text (vName ++ ".lookAt( " ++ (show (vx vec)) ++ ", " ++ (show (vy vec)) ++ ", " ++ (show (vz vec)) ++ " );\n")
ppTransf vName (SetScale vec) =
    text (vName ++ ".scale.set( " ++ (show (vx vec)) ++ ", " ++ (show (vy vec)) ++ ", " ++ (show (vz vec)) ++ " );\n")
ppTransf vName (ApplyMatrix mat) =
    text ("let " ++ vName ++ "Mat = new THREE.Matrix4().fromArray( " ++ (ppArr (Prelude.concat mat)) ++ " );\n") <>
    text (vName ++ ".applyMatrix( " ++ vName ++ "Mat );\n")

ppArr :: [Double] -> String
ppArr xs = "[" ++ (ppArr' xs)

ppArr' :: [Double] -> String
ppArr' [] = "]"
ppArr' (x:[]) = (show x) ++ "]"
ppArr' (x:xs) = (show x) ++ ", " ++ (ppArr' xs)


-- Print Mesh
printMesh :: String -> Mesh -> [ObjTransform] -> Doc
printMesh name (Mesh geom mat) transf = let varName = name ++ "Mesh"
                                       in (printGeometry name geom) <>
                                          text "\n" <>
                                          (printMaterial name mat) <>
                                          text "\n" <>
                                          text ("let " ++ varName ++ " = new THREE.Mesh( " ++
                                          name ++ "Geometry, " ++
                                          name ++ "Material );") <>
                                          text "\n" <>
                                          text (varName ++ ".name = \"" ++ name ++ "\";\n") <>
                                          text ("scene.add( " ++ varName ++ " );\n") <>
                                          printTransf varName transf


-- Print Geometries
printGeometry :: String -> Geometry -> Doc
--let nameGeometry = new THREE.BoxBufferGeometry( width, height, depth );
printGeometry name (BoxGeometry {width = w, height = h, depth = d}) =
    text ("let " ++ name ++ "Geometry = new THREE.BoxBufferGeometry( " ++
    (show w) ++ ", " ++
    (show h) ++ ", " ++
    (show d) ++
    " );")

--let nameGeometry = new THREE.ConeBufferGeometry( radius, height, radialSegments );
printGeometry name (ConeGeometry {radius = r, height = h, radialSegments = rs}) =
    text ("let " ++ name ++ "Geometry = new THREE.ConeBufferGeometry( " ++
    (show r) ++ ", " ++
    (show h) ++ ", " ++
    (show rs) ++
    " );")

--let nameGeometry = new THREE.CylinderBufferGeometry( radiusTop, radiusBottom, height, radialSegments );
printGeometry name (CylinderGeometry {radiusTop = rt, radiusBottom = rb, height = h, radialSegments = rs}) =
    text ("let " ++ name ++ "Geometry = new THREE.CylinderBufferGeometry( " ++
    (show rt) ++ ", " ++
    (show rb) ++ ", " ++
    (show h) ++ ", " ++
    (show rs) ++
    " );")

--let nameGeometry = new THREE.SphereBufferGeometry( radius, widthSegments, heightSegments );
printGeometry name (SphereGeometry {radius = r, widthSegments = ws, heightSegments = hs}) =
    text ("let " ++ name ++ "Geometry = new THREE.SphereBufferGeometry( " ++
    (show r) ++ ", " ++
    (show ws) ++ ", " ++
    (show hs) ++
    " );")

--let nameGeometry = new THREE.TorusBufferGeometry( radius, tube, radialSegments, tubularSegments );
printGeometry name (TorusGeometry {radius = r, tube = t, radialSegments = rs, tubularSegments = ts}) =
    text ("let " ++ name ++ "Geometry = new THREE.TorusBufferGeometry( " ++
    (show r) ++ ", " ++
    (show t) ++ ", " ++
    (show rs) ++ ", " ++
    (show ts) ++
    " );")


-- Print Materials
printMaterial :: String -> Material -> Doc
{--
let nameMaterial = new THREE.MeshStandardMaterial( {
                                                        color: materialColor,
                                                        emissive: emissive,
                                                        roughness: roughness,
                                                        metalness: metalness
                                                   } );
--}
printMaterial name (StandardMaterial {materialColor = c, emissive = e, roughness = r, metalness = m}) =
    text ("let " ++ name ++ "Material = new THREE.MeshStandardMaterial( {\n") <>
    text "    color: " <> (printColor c) <> text ",\n" <>
    text "    emissive: " <> (printColor e) <> text ",\n" <>
    text ("    roughness: " ++ (show r) ++ ",\n") <>
    text ("    metalness: " ++ (show m) ++ "\n") <>
    text ("} );")


printColor :: Color -> Doc
printColor (RGB r g b) = text ("new THREE.Color(" ++ (show r) ++ ", " ++ (show g) ++ ", " ++ (show b) ++ ")")
printColor (HEX s) = text ("0x" ++ s)


-- Print lights
printLight :: String -> Light -> [ObjTransform] -> Doc
{--
let nameLight = new THREE.AmbientLight( c, i );
scene.add( amblight );
--}
printLight name (AmbientLight {lightColor = c, intensity = i}) transf =
    let varName = name ++ "Light"
    in text ("let " ++ varName ++ " = new THREE.AmbientLight( ") <>
       printColor c <>
       text (", " ++ (show i) ++ " );\n") <>
       text (varName ++ ".name = \"" ++ name ++ "\";\n") <>
       text ("scene.add( " ++ varName ++ " );\n") <>
       printTransf varName transf

printLight name (DirectionalLight {lightColor = c, intensity = i, target = t}) transf =
    let varName = name ++ "Light"
        targetName = name ++ "Target"
        in text ("let " ++ varName ++ " = new THREE.DirectionalLight( ") <>
           printColor c <>
           text (", " ++ (show i) ++ " );\n") <>
           text (varName ++ ".name = \"" ++ name ++ "\";\n") <>
           text ("scene.add( " ++ varName ++ " );\n\n") <>
           text ("let " ++ targetName ++ " = new THREE.Object3D();\n") <>
           text (targetName ++ ".position.set( " ++ (show (vx t)) ++ ", " ++ (show (vy t)) ++ ", " ++ (show (vy t)) ++ " );\n") <>
           text ("scene.add( " ++ targetName ++ " );\n") <>
           text (varName ++ ".target = " ++ targetName ++ ";\n") <>
           printTransf varName transf

printLight name (PointLight {lightColor = c, intensity = i, distance = dist, decay = dec}) transf =
    let varName = name ++ "Light"
    in text ("let " ++ varName ++ " = new THREE.PointLight( ") <>
       printColor c <>
       text (", " ++ (show i)) <>
       text (", " ++ (show dist)) <>
       text (", " ++ (show dec) ++ " );\n") <>
       text (varName ++ ".name = " ++ name ++ ";\n") <>
       text ("scene.add( " ++ varName ++ " );\n") <>
       printTransf varName transf


-- Print cameras
printCamera :: String -> Camera -> [ObjTransform] -> Doc
{--
let nameCamera = new THREE.PerspectiveCamera( fov, window.innerWidth/window.innerHeight, near, far );
--}
printCamera name (PerspectiveCamera {fov = fo, near = n, far = fa}) transf =
    text ("let camera = new THREE.PerspectiveCamera( ") <>
    text (show fo) <>
    text (", window.innerWidth/window.innerHeight") <>
    text (", " ++ (show n)) <>
    text (", " ++ (show fa) ++ " );\n") <>
    text ("camera.name = \"" ++ name ++ "\";\n") <>
    text ("scene.add( camera );\n") <>
    printTransf "camera" transf

{--
let nameCamera = new THREE.OrthographicCamera( width / - 2, width / 2, height / 2, height / - 2, 1, 1000 );
--}
printCamera name (OrthographicCamera {orthoWidth = w, near = n, far = f}) transf =
    text ("let orthoHeight = " ++ (show w) ++ " * window.innerHeight/window.innerWidth;\n") <>
    text ("let camera = new THREE.OrthographicCamera( ") <>
    text ((show w) ++ " / - 2, " ++ (show w) ++ " / 2, orthoHeight / 2, orthoHeight / - 2") <>
    text (", " ++ (show n)) <>
    text (", " ++ (show f) ++ " );\n") <>
    text ("camera.name = \"" ++ name ++ "\";\n") <>
    text ("scene.add( camera );\n") <>
    printTransf "camera" transf
