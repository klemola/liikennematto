COMPRESS_ARGS='pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe'

build:
	@echo ">> Setup"
	@mkdir -p ./build
	@rm -rf ./build/**

	@echo ">> Building"
	elm make --optimize src/Main.elm --output=./build/liikennematto.js
	terser --compress $(COMPRESS_ARGS) ./build/liikennematto.js --output ./build/liikennematto.min.js

	@echo ">> Copying files"
	cp -r ./src/assets ./build
	# create preload links for all assets and insert them to the index.html file
	ls ./build/assets | awk '{print "<link rel=\"preload\" as=\"image\" href=\"/assets/"$$0"\" />"}' | sed -e '/^.*replaceme/{r /dev/stdin' -e 'd;}' ./src/index.template.html > ./build/index.html

	@echo ">> Creating an archive"
	cd build && zip -rq matto.zip ./* -x "liikennematto.js" "liikennematto-debug.js"
	@echo ">> Done!"

check:
	@mkdir -p ./build
	clear
	elm make src/Main.elm --output=./build/liikennematto-debug.js

dev:
	elm reactor

serve:
	python3 -m http.server --directory ./build

test:
	clear
	elm-test


.PHONY: build check dev serve test
