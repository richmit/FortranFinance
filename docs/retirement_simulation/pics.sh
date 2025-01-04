#!/usr/bin/env -S sh
# -*- Mode:Shell-script; Coding:us-ascii-unix; fill-column:158 -*-

if [ -z "$1" ]; then
  echo "Need tag for pics"
  exit
fi

for f in ~/world/my_prog/finance/FortranFinance/retirement_simulation/*.png; do
  if echo $f | grep -E '/(paidsource|paidunpaid|savings)\.png$' >/dev/null; then
    typ='fix'
  else
    typ='mc'
  fi
  for siz in full 1024x 800x 512x 256x 128x; do
    fn=$(basename $f | sed "s/.png$/_${1}_${typ}_$siz.png/")
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




