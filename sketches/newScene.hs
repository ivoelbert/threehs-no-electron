import ThreeScene

myFirstScene :: ThreeScene ()

myFirstScene = do newScene
                  cam <- perspectiveCamera 75 0.1 100
                  camPosition <- setPosition (vector 0 2 3)
                  camUp <- setUp (vector 0 1 0)
                  camLookAt <- lookAt (vector 0 0 0)
                  addToScene "camera" cam [camPosition, camUp, camLookAt]
                  light <- ambientLight (hex "fff7d1") 1
                  addToScene "light" light []
                  cubeGeom <- boxGeometry 1 1 1
                  cubeMat <- standardMaterial (hex "e56244") (hex "000000") 1 0.4
                  cubeMesh <- createMesh cubeGeom cubeMat
                  addToScene "cube" cubeMesh []

main :: IO ()
main = do printScene myFirstScene
