
var frameRate = 60;
var frameCount = 0;
var mouseX = 0;
var mouseY = 0;

////////////////////////////////
// Handle mouse interaction
////////////////////////////////
document.addEventListener('mousemove', onDocumentMouseMove, false);
window.addEventListener('resize', onWindowResize, false);

function onDocumentMouseMove(event) {
    event.preventDefault();
    mouseX = (event.clientX / window.innerWidth) * 2 - 1;
    mouseY = -(event.clientY / window.innerHeight) * 2 + 1;
}

function onWindowResize() {
    camera.aspect = window.innerWidth / window.innerHeight;
    camera.updateProjectionMatrix();
    renderer.setSize(window.innerWidth, window.innerHeight);
}
////////////////////////////////
// End handle mouse interaction
////////////////////////////////


////////////////////////////////
// Handle frame stuff
////////////////////////////////
function requestNextFrame( fun )
{
  setTimeout( function() {
      requestAnimationFrame( fun );
      frameCount++;
  }, 1000 / frameRate );
}

function setFrameRate( n )
{
  frameRate = n;
}
////////////////////////////////
// End handle frame stuff
////////////////////////////////


////////////////////////////////
// Handle object updates
////////////////////////////////

function handleUpdates(hObjectS)
{
  /*
    hObject es un JSON de la forma:
    "objName" : [{"type": "tipo", "param1": ..., ...}, ...]
  */
  let hObject = JSON.parse(hObjectS);
  let keys = Object.keys(hObject);

  for (let k = 0; k < keys.length; k++) {
    let key = keys[k];
    let objToUpdate = scene.getObjectByName(key);

    let updates = hObject[key];
    for(let i = 0; i < updates.length; i++)
    {
      let update = updates[i]
      switch(update.type)
      {
        case "setPosition":
        let sPos = new THREE.Vector3(update.x, update.y, update.z);
        objToUpdate.position.copy(sPos);
        break;

        case "translateOnAxis":
        let tAxis = new THREE.Vector3(update.x, update.y, update.z).normalize();
        let dist = update.dist;
        objToUpdate.translateOnAxis(tAxis, dist);
        break;

        case "applyQuaternion":
        let quat = new THREE.Quaternion(update.x, update.y, update.z, update.w);
        objToUpdate.applyQuaternion(quat);
        break;

        case "rotateOnAxis":
        let rAxis = new THREE.Vector3(update.x, update.y, update.z).normalize();
        let angle = update.radians;
        objToUpdate.rotateOnAxis(rAxis, angle);
        break;

        case "setUp":
        let up = new THREE.Vector3(update.x, update.y, update.z).normalize();
        objToUpdate.up.copy(up);
        break;

        case "lookAt":
        let lPos = new THREE.Vector3(update.x, update.y, update.z);
        objToUpdate.lookAt(lPos);
        break;

        case "setScale":
        let scl = new THREE.Vector3(update.x, update.y, update.z);
        objToUpdate.scale.copy(scl);
        break;

        case "applyMatrix":
        let arr = update.matrix;
        let mat = new THREE.Matrix4().fromArray(arr);
        objToUpdate.applyMatrix(mat);
        break;
      }
    }
  }

  return undefined;
}

////////////////////////////////
// End handle object updates
////////////////////////////////
