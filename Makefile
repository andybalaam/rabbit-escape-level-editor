
all:
	mkdir -p dist
	elm make \
		src/Main.elm \
		--output=dist/rabbit-escape-level-editor.js

clean:
	rm -rf dist
