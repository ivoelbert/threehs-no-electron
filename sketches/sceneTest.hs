import ThreeScene

scene :: ThreeScene ()
scene = do
        newScene
        cam <- perspectiveCamera 75 0.1 100
        camPosition <- setPosition (vector 0 3 5)
        camSetUp <- setUp (vector 0 1 0)
        camLookAt <- lookAt (vector 0 0 0)
        addToScene "camara" cam [camPosition, camSetUp, camLookAt]
        luz1 <- directionalLight (hex "ffffff") 1 vec0
        luz2 <- ambientLight (hex "f9e0a9") 0.5
        dirLightPos <- setPosition (vector (-3) 3 5)
        addToScene "dirLight" luz1 [dirLightPos]
        addToScene "ambLight" luz2 []
        tableGeom <- boxGeometry 5 1 5
        tableMat <- standardMaterial (hex "47aa68") (hex "000000") 0.8 0.1
        tableMesh <- createMesh tableGeom tableMat
        tablePos <- setPosition (vector 0 (-0.5) 0)
        addToScene "mesa" tableMesh [tablePos]
        bolaGeom <- sphereGeometry 0.5 32 32
        bolaMat <- standardMaterial (hex "ffffff") (hex "111111") 1 0.5
        bolaMesh <- createMesh bolaGeom bolaMat
        addToScene "bola" bolaMesh []

main :: IO ()
main = do printScene scene
