SIMD := $(shell ((gcc -march=native -dN -E - < /dev/null | grep -q "AVX2") && echo "AVX2") || echo "AVX")

SCENARIO_VARS = -XORKA_SIMD_EXT="$(SIMD)"

COVERAGE_VARS = -XORKA_BUILD_MODE=coverage

DEBUG_VARS = -XORKA_BUILD_MODE=debug -XORKA_DEBUG_SYMBOLS=enabled

.PHONY: build examples tools tests coverage docs clean

build:
	cd orka_egl && alr build
	cd orka_types && alr build $(SCENARIO_VARS)
	cd orka_simd && alr build $(SCENARIO_VARS)
	cd orka_transforms && alr build $(SCENARIO_VARS)
	cd orka_numerics && alr build $(SCENARIO_VARS)
	cd orka && alr build $(SCENARIO_VARS)
#	cd orka_plugin_sdl && alr build $(SCENARIO_VARS)
	cd orka_plugin_archives && alr build $(SCENARIO_VARS)
	cd orka_plugin_gltf && alr build $(SCENARIO_VARS)
	cd orka_plugin_terrain && alr build $(SCENARIO_VARS)
	cd orka_plugin_atmosphere && alr build $(SCENARIO_VARS)

examples:
	cd examples && alr build $(SCENARIO_VARS)

tools:
	cd orka_tools && alr build $(SCENARIO_VARS)

tests:
	cd tests && alr build $(SCENARIO_VARS) $(COVERAGE_VARS)
	cd tests && alr run -s

coverage:
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
	cd orka_numerics && alr clean
	cd orka && alr clean
	cd orka_tools && alr clean
#	cd orka_plugin_sdl && alr clean
	cd orka_plugin_archives && alr clean
	cd orka_plugin_gltf && alr clean
	cd orka_plugin_terrain && alr clean
	cd orka_plugin_atmosphere && alr clean
	cd examples && alr clean
	cd tests && alr clean
	rm -rf orka*/build examples/build tests/build orka*/config examples/config tests/config tests/cov
