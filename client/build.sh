#/bin/sh

mkdir out

elm-make Snibbles.elm --output snibbles.js --yes

cp snibbles.html styles.css snibbles.js out/

mv out/snibbles.html out/index.html

