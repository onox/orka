SIMD := $(shell ((gcc -march=native -dN -E - < /dev/null | grep -q "AVX2") && echo "AVX2") || echo "AVX")

SCENARIO_VARS = -XORKA_SIMD_EXT="$(SIMD)"

.PHONY: build debug run clean trace dump

all: run

build:
	alr build $(SCENARIO_VARS)
run: build
	cd example && GALLIUM_HUD=".c35frametime" MESA_LOADER_DRIVER_OVERRIDE=iris ./build/bin/example
verbose: build
	cd example && WAYLAND_DEBUG=1 GALLIUM_HUD=".c35frametime" MESA_LOADER_DRIVER_OVERRIDE=iris ./build/bin/example
debug: build
	cd example && gdb ./build/bin/example
trace: build
	cd example && WAYLAND_DEBUG=1 apitrace trace --api=egl ./build/bin/example
dump:
	apitrace dump example.trace
clean:
	alr clean
	rm -rf build example/build
