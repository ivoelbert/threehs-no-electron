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
