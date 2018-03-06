# Documentation

What you can do in your scene and update will be specified here

# Scene

A scene is basically a set of *objects* with a name and some initial transformations. The mentioned *objects* can be *meshes*, *lights* or a *camera*. One and only one *camera* is needed to render your scene, and at least a *light* is recommended to shine on your *meshes*

```haskell
addToScene :: String -> Object3D -> [ObjTransform] -> ThreeScene ()

addToScene name obj transf
```
Adds the object *obj* with name *name* to the scene, with initial transformations *transf*


## Mesh

A *mesh* is an object you can see in your scene. As stated, it is needed to specify the geometry of the mesh (where are the vertices in 3D space) and a material (how its surface looks)

```haskell
createMesh :: Geometry -> Material -> ThreeScene Object3D

createMesh geometry material
```
Creates a mesh with specified *geometry* and *material*


### geometries

Geometries specify a set of vertices in 3D space, together with a material they are the two necessary components of a *mesh* (a visible object in your scene)

```haskell
boxGeometry :: Double -> Double -> Double -> ThreeScene Geometry

boxGeometry width height depth
```
Creates a box geometry with specified *width*, *height* and *depth*
See [BoxBufferGeometry](https://threejs.org/docs/index.html#api/geometries/BoxBufferGeometry), ignore widthSegments, heightSegments and depthSegments as they are defaulted


```haskell
coneGeometry :: Double -> Double -> Int -> ThreeScene Geometry

coneGeometry radius height radialSegments
```
Creates a cone geometry with specified *radius*, *height* and *radialSegments*
See [ConeBufferGeometry](https://threejs.org/docs/index.html#api/geometries/ConeBufferGeometry), ignore heightSegments, openEnded, thetaStart and thetaLength as they are defaulted


```haskell
cylinderGeometry :: Double -> Double -> Double -> Int -> ThreeScene Geometry

cylinderGeometry radiusTop radiusBottom height radialSegments
```
Creates a cylinder geometry with specified *radiusTop*, *radiusBottom*, *height* and *radialSegments*
See [CylinderBufferGeometry](https://threejs.org/docs/index.html#api/geometries/CylinderBufferGeometry), ignore heightSegments, openEnded, thetaStart and thetaLength as they are defaulted


```haskell
sphereGeometry :: Double -> Int -> Int -> ThreeScene Geometry

sphereGeometry radius widthSegments heightSegments
```
Creates a sphere geometry with specified *radius*, *widthSegments* and *heightSegments*
See [SphereBufferGeometry](https://threejs.org/docs/index.html#api/geometries/SphereBufferGeometry), ignore phiStart, phiLength, thetaStart and thetaLength as they are defaulted


```haskell
torusGeometry :: Double -> Double -> Int -> Int -> ThreeScene Geometry

torusGeometry radius tube radialSegments tubularSegments
```
Creates a sphere geometry with specified *radius*, *tube*, *radialSegments* and *tubularSegments*
See [TorusBufferGeometry](https://threejs.org/docs/index.html#api/geometries/TorusBufferGeometry), ignore arc as it is defaulted


### Materials

Materials specify how a mesh with a specific geometry will look. That is, what color it will be, what solid color will it emit, how rough and metalic it will be (both of which describe how light will bounce off it)

```haskell
standardMaterial :: Color -> Color -> Double -> Double -> ThreeScene Material

standardMaterial color emissive roughness metalness
```
Creates a standard material with specified *color*, *emissive*, *roughness* and *metalness*
See [MeshStandardMaterial](https://threejs.org/docs/index.html#api/materials/MeshStandardMaterial), now only those 4 parameters are supported, everything else is defaulted.


## Lights

Lights to shine on your meshes. Kind of straightforward...

```haskell
ambientLight :: Color -> Double -> ThreeScene Object3D

ambientLight color intensity
```
Creates an ambient light with specified *color* and *intensity*
See [AmbientLight](https://threejs.org/docs/index.html#api/lights/AmbientLight)


```haskell
directionalLight :: Color -> Double -> Vector3 -> ThreeScene Object3D

directionalLight color intensity target
```
Creates a directional light with specified *color*, *intensity* and *target*
See [DirectionalLight](https://threejs.org/docs/index.html#api/lights/DirectionalLight)


```haskell
pointLight :: Color -> Double -> Double -> Double -> ThreeScene Object3D

pointLight color intensity distance decay
```
Creates a point light with specified *color*, *intensity*, *distance* and *decay*
See [PointLight](https://threejs.org/docs/index.html#api/lights/PointLight)


## Cameras

A camera is needed to render your scene. You can create either a perspective camera or an orthographic one.

```haskell
perspectiveCamera :: Double -> Double -> Double -> ThreeScene Object3D

perspectiveCamera fov near far
```
Creates a perspective camera with specified *fov*, *near* and *far*
See [PerspectiveCamera](https://threejs.org/docs/index.html#api/cameras/PerspectiveCamera), ignore *aspect* as it will be set automatically.


```haskell
orthographicCamera :: Double -> Double -> Double -> ThreeScene Object3D

orthographicCamera width near far
```
Creates an orthographic camera with specified *width*, *near*, and *far*
See [OrthographicCamera](https://threejs.org/docs/index.html#api/cameras/OrthographicCamera), and consider: *left* and *right* are set to *-width/2* and *width/2* respectively. *top* and *bottom* are calculated using your window aspect ratio and your specified width.


## Transformations

When adding an *object* to your scene, you can specify a list of initial transformations to be applied to it.

```haskell
setPosition :: Vector3 -> ThreeScene ObjTransform

setPosition pos
```
Sets an object's position in 3D space.


```haskell
translateOnAxis :: Vector3 -> Double -> ThreeScene ObjTransform

translateOnAxis direction distance
```
Translates an object a certain *distance* in a *direction* (in local space)


```haskell
applyQuaternion :: Quaternion -> ThreeScene ObjTransform

applyQuaternion quaternion
```
Applies the rotation described by a *quaternion* to an object.


```haskell
rotateOnAxis :: Vector3 -> Double -> ThreeScene ObjTransform

rotateOnAxis axis angle
```
Rotates an object around a certain *axis* (in local space) about *angle* radians


```haskell
setUp :: Vector3 -> ThreeScene ObjTransform

setUp up
```
Sets an object's up vector. This is usually needed to use *lookAt*.


```haskell
lookAt :: Vector3 -> ThreeScene ObjTransform

lookAt pos
```
Aligns an object so its +z axis points towards *pos* (in world space). It is recommended to set the object's up vector to determine the orientation of the object after looking at a something.


```haskell
setScale :: Vector3 -> ThreeScene ObjTransform

setScale scl
```
Sets an object's scale along its local *x*, *y* and *z* axes


```haskell
applyMatrix :: Matrix4 -> ThreeScene ObjTransform

applyMatrix matrix
```
Applies a *matrix* to an object. That is, the object's local transformation matrix is premultiplied by *matrix*


## Vectors, matrices and other useful thingies

### Vector3

```haskell
vector :: Double -> Double -> Double -> Vector3

vector x y z
```
Creates a Vector3 with *x*, *y* and *z* components


```haskell
vec0 :: Vector3
```
Just a vector (0, 0, 0)


### Quaternion

```haskell
quaternion :: Double -> Double -> Double -> Double -> Quaternion

quaternion x y z w
```
Creates a Quaternion with *x*, *y*, *z* and *w* components


```haskell
identityQ :: Quaternion
```
Just a quaternion (0, 0, 0, 1)


### Matrix4

A matrix is specified as follows:

```haskell
[[1, 0, 0, 0],
 [0, 1, 0, 0],
 [0, 0, 1, 0],
 [0, 0, 0, 1]]
```
This is (of course) the identity matrix.


```haskell
identity4 :: Matrix4
```
is a compact way of writing the identity matrix, too.


### Color

```haskell
rgb :: Double -> Double -> Double -> Color
rgb red green blue
```
Describes a color by its *red*, *green* and *blue* values (*rgb 1 0 0* for full red, for example).


```haskell
hex :: String -> Color
```
Describes a color by its hexadecimal representation (*"0000ff"* for full green, for example).
