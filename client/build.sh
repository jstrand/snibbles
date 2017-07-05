#/bin/sh

mkdir out

elm-make Snibbles.elm --output snibbles.js

cp snibbles.html styles.css snibbles.js out/

mv out/snibbles.html out/index.html

