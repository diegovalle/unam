#! /bin/sh
cd ../graphs/
FILES=*.svg
mkdir -p png
for f in $FILES
do
    convert "$f" png/"$f".png
done
