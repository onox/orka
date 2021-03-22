CFLAGS ?= -O2 -march=native

WINDOWING_BACKEND := egl

GLFW_LIBS := $(strip $(shell pkgconf --libs glfw3))
SIMD := $(shell ((gcc $(CFLAGS) -dN -E - < /dev/null | grep -q "AVX2") && echo "AVX2") || echo "AVX")

X_WINDOWING_SYSTEM := -XWindowing_System=$(WINDOWING_BACKEND)
X_GLFW_LIBS := -XORKA_GLFW_GLFW_LIBS="$(GLFW_LIBS)"
X_SIMD := -XORKA_SIMD_EXT="$(SIMD)"
SCENARIO_VARS = $(X_WINDOWING_SYSTEM) $(X_GLFW_LIBS) $(X_SIMD)

GPRBUILD = nice gprbuild -dm -p $(SCENARIO_VARS)
GPRCLEAN = gprclean -q $(SCENARIO_VARS)

.PHONY: build examples tools tests coverage docs clean

ORKA_PATHS = ../orka:../orka_egl:../orka_simd:../orka_transforms:../orka_types

ORKA_GLFW_PATHS = $(ORKA_PATHS):../orka_glfw

build:
	cd orka_glfw && GPR_PROJECT_PATH="$(ORKA_PATHS):`pwd`" alr build
#	cd orka_plugin_sdl && GPR_PROJECT_PATH="$(ORKA_PATHS):`pwd`" alr build
	cd orka_plugin_archives && GPR_PROJECT_PATH="$(ORKA_PATHS):`pwd`" alr build
	cd orka_plugin_gltf && GPR_PROJECT_PATH="$(ORKA_PATHS):`pwd`" alr build
	cd orka_plugin_terrain && GPR_PROJECT_PATH="$(ORKA_PATHS):`pwd`" alr build
	cd orka_plugin_atmosphere && GPR_PROJECT_PATH="$(ORKA_PATHS):`pwd`" alr build

build_test:
	$(GPRBUILD) -P tests/unit/tests.gpr -XMode=coverage -cargs -O0 -march=native

examples: build
	$(GPRBUILD) -P tools/examples.gpr -cargs $(CFLAGS)

tools: build
	cd orka_tools && GPR_PROJECT_PATH="$(ORKA_GLFW_PATHS):`pwd`" alr build $(SCENARIO_VARS)

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

	cd orka_tools && GPR_PROJECT_PATH="$(ORKA_GLFW_PATHS):`pwd`" alr clean
	cd orka_glfw && GPR_PROJECT_PATH="$(ORKA_PATHS):`pwd`" alr clean
	cd orka_plugin_sdl && GPR_PROJECT_PATH="$(ORKA_PATHS):`pwd`" alr clean
	cd orka_plugin_archives && GPR_PROJECT_PATH="$(ORKA_PATHS):`pwd`" alr clean
	cd orka_plugin_gltf && GPR_PROJECT_PATH="$(ORKA_PATHS):`pwd`" alr clean
	cd orka_plugin_terrain && GPR_PROJECT_PATH="$(ORKA_PATHS):`pwd`" alr clean
	cd orka_plugin_atmosphere && GPR_PROJECT_PATH="$(ORKA_PATHS):`pwd`" alr clean

#	$(GPRCLEAN) -P tests/unit/tests.gpr
#	$(GPRCLEAN) -P tools/examples.gpr
#	rm -rf bin build tests/unit/build tests/unit/bin tests/cov
