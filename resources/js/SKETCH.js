var scene = new THREE.Scene();

var renderer = new THREE.WebGLRenderer( { preserveDrawingBuffer: true, antialias: true } );
renderer.setSize( window.innerWidth, window.innerHeight );
document.body.appendChild( renderer.domElement );

////////////////////////////////////////////////////////
// BEGIN PRINTED FROM HASKELL
////////////////////////////////////////////////////////
 
let cubeGeometry = new THREE.BoxBufferGeometry( 1.0, 1.0, 1.0 );
let cubeMaterial = new THREE.MeshStandardMaterial( {
    color: 0xe56244,
    emissive: 0x000000,
    roughness: 1.0,
    metalness: 0.4
} );
let cubeMesh = new THREE.Mesh( cubeGeometry, cubeMaterial );
cubeMesh.name = "cube";
scene.add( cubeMesh );

let lightLight = new THREE.AmbientLight( 0xfff7d1, 1.0 );
lightLight.name = "light";
scene.add( lightLight );

let camera = new THREE.PerspectiveCamera( 75.0, window.innerWidth/window.innerHeight, 0.1, 100.0 );
camera.name = "camera";
scene.add( camera );
camera.position.set( 0.0, 2.0, 3.0 );
camera.up.set( 0.0, 1.0, 0.0 );
camera.lookAt( 0.0, 0.0, 0.0 );

////////////////////////////////////////////////////////
// END PRINTED FROM HASKELL
////////////////////////////////////////////////////////

var animate = function () {

  requestNextFrame( animate );
  handleUpdates(Haste.updates(frameCount, mouseX, mouseY));
	renderer.render(scene, camera);

};

setFrameRate(30);
animate();
