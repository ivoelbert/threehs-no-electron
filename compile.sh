echo "Compiling..."
rm resources/js/SKETCH.js
rm resources/js/update.js
cp $1 resources/hs/scene.hs
cp $2 resources/hs/update.hs
cd resources/hs
ghc scene.hs -o sceneToRun
hastec '--start=$HASTE_MAIN();' update.hs
echo "ENDED - Please check for errors above."
