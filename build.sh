#!/bin/bash

function make() {
  elm-make "$1"
  cat <<HEAD > "$2"
<!DOCTYPE HTML>
<html><head><meta charset="UTF-8"><title>Main</title><style>html, head, body { padding:0; margin:0; }
html,body { height: 100%; margin: 0px; }
</style></head><body><script>
HEAD
  cat elm.js >> "$2"
  cat <<FOOT >> "$2"
</script><script>var runningElmModule = Elm.fullscreen(Elm.Main);</script></body></html>
FOOT
  rm elm.js
  echo "Successfully generated $2"
}

function mmake() {
  make "$1.elm" "$1.html"
}

mmake FlipDemo
mmake BoxesDemo
mmake MoveDemo
mmake FancyMoveDemo
