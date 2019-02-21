IMAGES_ORIG := $(wildcard images/*)
IMAGES := $(subst images,www/images,$(IMAGES_ORIG))

all: compile test

www:
	mkdir -p www

www/images/%: images/% www/images
	cp $< $@

www/images: www
	mkdir -p $@

www/%.html: %.html www
	cp $< $@

www/style.css: style.css www
	cp $< $@

images: ${IMAGES}

index: www/index.html www/index-view.html www/style.css

# TODO https://elm-lang.org/0.19.0/optimize

compile: index images
	elm make \
		src/Main.elm \
		--output=www/level-editor.js

upload: compile
	rsync -r --delete \
		./www/ \
		dreamhost:artificialworlds.net/rabbit-escape/level-editor/

run:
	elm reactor

test:
	elm-test

setup:
	echo "sudo apt install nodejs npm"
	echo "sudo npm install --unsafe-perm=true --global elm"
	echo "sudo npm install --unsafe-perm=true --global elm-test"

clean:
	rm -rf www
