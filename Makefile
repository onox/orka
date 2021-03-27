GLFW_LIBS := $(strip $(shell pkgconf --libs glfw3))
SIMD := $(shell ((gcc -march=native -dN -E - < /dev/null | grep -q "AVX2") && echo "AVX2") || echo "AVX")

X_GLFW_LIBS := -XORKA_GLFW_GLFW_LIBS="$(GLFW_LIBS)"
X_SIMD := -XORKA_SIMD_EXT="$(SIMD)"
SCENARIO_VARS = $(X_GLFW_LIBS) $(X_SIMD)

COVERAGE_VARS = -XORKA_TYPES_BUILD_MODE=coverage -XORKA_SIMD_BUILD_MODE=coverage  -XORKA_TRANSFORMS_BUILD_MODE=coverage -XORKA_BUILD_MODE=coverage

.PHONY: build examples tools tests coverage docs clean

build:
	cd orka && alr build
	cd orka_glfw && alr build $(SCENARIO_VARS)
#	cd orka_plugin_sdl && alr build
	cd orka_plugin_archives && alr build
	cd orka_plugin_gltf && alr build
	cd orka_plugin_terrain && alr build
	cd orka_plugin_atmosphere && alr build

examples:
	cd examples && alr build $(SCENARIO_VARS)

tools:
	cd orka_tools && alr build $(SCENARIO_VARS)

tests:
	cd tests && alr build $(SCENARIO_VARS) $(COVERAGE_VARS)
	cd tests && alr	run

coverage: tests
	mkdir -p tests/cov
	lcov -q -c -d orka_types/build/obj -d orka_simd/build/obj -d orka_transforms/build/obj -d orka/build/obj -o tests/cov/unit.info
	lcov -q -r tests/cov/unit.info */adainclude/* -o tests/cov/unit.info
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
#	cd orka_plugin_sdl && alr clean
	cd orka_plugin_archives && alr clean
	cd orka_plugin_gltf && alr clean
	cd orka_plugin_terrain && alr clean
	cd orka_plugin_atmosphere && alr clean
	cd examples && alr clean
	cd tests && alr clean
	rm -rf orka*/build examples/build tests/build orka*/config examples/config tests/config tests/cov
