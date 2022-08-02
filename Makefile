ALR_CLEAN = alr clean -- -p
ALR_BUILD = alr build --validation

.PHONY: build examples tools tests coverage docs clean

build:
	cd orka_egl && $(ALR_BUILD)
	cd orka_types && $(ALR_BUILD)
	cd orka_simd && $(ALR_BUILD)
	cd orka_transforms && $(ALR_BUILD)
	cd orka_numerics && $(ALR_BUILD)
	cd orka && $(ALR_BUILD)
#	cd orka_plugin_sdl && $(ALR_BUILD)
	cd orka_plugin_archives && $(ALR_BUILD)
	cd orka_plugin_gltf && $(ALR_BUILD)
	cd orka_plugin_terrain && $(ALR_BUILD)
	cd orka_plugin_atmosphere && $(ALR_BUILD)

examples:
	cd examples && $(ALR_BUILD)

tools:
	cd orka_tools && $(ALR_BUILD)

tests:
	cd tests && $(ALR_BUILD)
	cd tests && alr run -s

coverage:
	mkdir -p tests/cov
	lcov -q -c -d orka_types/build/obj -d orka_simd/build/obj -d orka_transforms/build/obj -d orka_numerics/build/obj -d orka/build/obj -o tests/cov/unit.info
	lcov -q -r tests/cov/unit.info */adainclude/* -o tests/cov/unit.info
	genhtml -q --ignore-errors source -o tests/cov/html tests/cov/unit.info
	lcov -l tests/cov/unit.info

docs:
	docker run --rm -it -p 8000:8000 -v ${PWD}:/docs:ro -u $(shell id -u):$(shell id -g) squidfunk/mkdocs-material

clean:
	cd orka_egl && $(ALR_CLEAN)
	cd orka_types && $(ALR_CLEAN)
	cd orka_simd && $(ALR_CLEAN)
	cd orka_transforms && $(ALR_CLEAN)
	cd orka_numerics && $(ALR_CLEAN)
	cd orka && $(ALR_CLEAN)
	cd orka_tools && $(ALR_CLEAN)
#	cd orka_plugin_sdl && $(ALR_CLEAN)
	cd orka_plugin_archives && $(ALR_CLEAN)
	cd orka_plugin_gltf && $(ALR_CLEAN)
	cd orka_plugin_terrain && $(ALR_CLEAN)
	cd orka_plugin_atmosphere && $(ALR_CLEAN)
	cd examples && $(ALR_CLEAN)
	cd tests && $(ALR_CLEAN)
	rm -rf orka*/build examples/build tests/build orka*/config examples/config tests/config tests/cov
