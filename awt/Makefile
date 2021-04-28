.PHONY: build debug run clean trace dump

all: run

build:
	alr build
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
