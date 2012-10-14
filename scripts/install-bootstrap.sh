#!/bin/bash

echo "Downloading Bootstrap..."
wget -q twitter.github.com/bootstrap/assets/bootstrap.zip
echo "Unzipping Bootstrap..."
unzip -q bootstrap.zip
echo "Moving Bootstrap files..."
cd bootstrap
mv css/* ../css/
mv js/* ../js/
mv img/* ../img/
cd ..
echo "Cleaning up..."
rm bootstrap.zip
rm -r bootstrap
echo "Done."
