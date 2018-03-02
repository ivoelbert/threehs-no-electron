echo "Setting up..."
cd resources/hs
./sceneToRun
cp update.js ../js/update.js
echo "Cleaning up..."
rm *.o
rm *.hi
rm *.jsmod
rm update.js
rm scene.hs
rm update.hs
rm sceneTo*
echo "ENDED - Please check for errors above."
