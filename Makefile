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
	cp ./src/index.template.html ./build/index.html

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

benchmark:
	mkdir -p "build/benchmarks"
	elm make benchmarks/RoadNetworkBenchmark.elm --optimize --output build/benchmarks/road_network.html
	elm make benchmarks/TrafficBenchmark.elm --optimize --output build/benchmarks/traffic.html
	elm make benchmarks/SplineBenchmark.elm --optimize --output build/benchmarks/splines.html
	elm make benchmarks/PathfindingAndRouteBenchmark.elm --optimize --output build/benchmarks/pathfinding_route.html
	elm make benchmarks/GameLoopBenchmark.elm --optimize --output build/benchmarks/game_loop.html
	elm make benchmarks/RenderBenchmark.elm --optimize --output build/benchmarks/render.html

devtools:
	elm make devtools/RenderFixture.elm --output build/render_fixture.html
	elm make devtools/LotsGallery.elm --output build/lots_debug.html
	elm make devtools/PathfindingVisualization.elm --output build/pathfinding_visualization.html
	elm make devtools/RouteVisualization.elm --output build/route_visualization.html

review:
	elm-review --fix

stats:
	cloc --exclude-dir=Data,Vendor,assets src

.PHONY: build check dev serve test benchmark devtools review stats
