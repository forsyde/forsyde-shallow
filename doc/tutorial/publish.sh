#!/bin/bash

# Script to synch (publish) the tutorial www version with the darcs version
make tutorial.html
make tutorial.pdf
make html-chunks
cp *.html *.css tutorial.pdf ../www/files/tutorial/
cp figures/*.svg figures/*.png ../www/files/tutorial/figures
tar -cvzf ForSyDe_tutorial_src.tgz ForSyDe_tutorial_src/*.hs
mv ForSyDe_tutorial_src.tgz ../www/files/tutorial/
darcs add ../www/files/tutorial/*.html
darcs add ../www/files/tutorial/figures/*{.svg,.png}
darcs record -am "Doc: www: synched tutorial" ../www/files/tutorial/