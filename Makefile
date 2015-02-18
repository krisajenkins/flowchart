all: flowchart.js

flowchart.js: Main.elm
	elm-make $< --output=$@
