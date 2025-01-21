#!/usr/bin/env -S sh
# -*- Mode:Shell-script; Coding:us-ascii-unix; fill-column:158 -*-

for f in ~/world/my_prog/finance/FortranFinance/monte_carlo/*.png; do
  for siz in full 800x ; do # 1024x 512x 256x 128x
    fn=$(basename $f | sed "s/.png$/_$siz.png/")
    ffn="pics/$fn"
    if [ "$f" -nt "$ffn" ]; then
      if [ $siz = 'full' ]; then
        echo cp $f $ffn
        cp $f $ffn
      else
        echo magick $f -resize ${siz} $ffn
        magick $f -resize ${siz} $ffn
      fi
    else
      echo SKIP $f $ffn
    fi
  done
done
