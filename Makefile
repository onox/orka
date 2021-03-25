CFLAGS ?= -O2 -march=native

GLFW_LIBS := $(strip $(shell pkgconf --libs glfw3))
SIMD := $(shell ((gcc $(CFLAGS) -dN -E - < /dev/null | grep -q "AVX2") && echo "AVX2") || echo "AVX")

X_GLFW_LIBS := -XORKA_GLFW_GLFW_LIBS="$(GLFW_LIBS)"
X_SIMD := -XORKA_SIMD_EXT="$(SIMD)"
SCENARIO_VARS = $(X_GLFW_LIBS) $(X_SIMD)

GPRBUILD = nice gprbuild -dm -p $(SCENARIO_VARS)
GPRCLEAN = gprclean -q $(SCENARIO_VARS)

.PHONY: build examples tools tests coverage docs clean

build:
	cd orka && alr build
	cd orka_glfw && alr build $(SCENARIO_VARS)
#	cd orka_plugin_sdl && alr build
	cd orka_plugin_archives && alr build
	cd orka_plugin_gltf && alr build
	cd orka_plugin_terrain && alr build
	cd orka_plugin_atmosphere && alr build

build_test:
	$(GPRBUILD) -P tests/unit/tests.gpr -XMode=coverage -cargs -O0 -march=native

examples: build
	cd orka_examples && alr build $(SCENARIO_VARS)

tools: build
	cd orka_tools && alr build $(SCENARIO_VARS)

tests: build_test
	./tests/unit/bin/run_unit_tests

coverage:
	mkdir -p tests/cov
	lcov -q -c -d tests/unit/build/obj -o tests/cov/unit.info
	lcov -q -c -d build/obj/tests -o tests/cov/unit.info
	lcov -q -r tests/cov/unit.info */adainclude/* -o tests/cov/unit.info
	lcov -q -r tests/cov/unit.info */tests/unit/* -o tests/cov/unit.info
	genhtml -q --ignore-errors source -o tests/cov/html tests/cov/unit.info
	lcov -l tests/cov/unit.info

docs:
	docker run --rm -it -p 8000:8000 -v ${PWD}:/docs:ro -u $(shell id -u):$(shell id -g) squidfunk/mkdocs-material

clean:
	cd orka_egl && alr clean
	cd orka_types && alr clean
	cd orka_simd && alr clean
	cd orka_transforms && alr clean
	cd orka && alr clean
	cd orka_glfw && alr clean
	cd orka_tools && alr clean
	cd orka_examples && alr clean
	cd orka_plugin_sdl && alr clean
	cd orka_plugin_archives && alr clean
	cd orka_plugin_gltf && alr clean
	cd orka_plugin_terrain && alr clean
	cd orka_plugin_atmosphere && alr clean

#	$(GPRCLEAN) -P tests/unit/tests.gpr
#	rm -rf bin build tests/unit/build tests/unit/bin tests/cov
