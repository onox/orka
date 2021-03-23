.PHONY: build debug run clean trace dump

all: run

build:
	alr build
run: build
	cd test && GALLIUM_HUD=".c35frametime" MESA_LOADER_DRIVER_OVERRIDE=iris ./bin/test
verbose: build
	cd test && WAYLAND_DEBUG=1 GALLIUM_HUD=".c35frametime" MESA_LOADER_DRIVER_OVERRIDE=iris ./bin/test
debug: build
	cd test && gdb ./bin/test
trace: build
	cd test && WAYLAND_DEBUG=1 apitrace trace --api=egl ./bin/test
dump:
	apitrace dump test.trace
clean:
	alr clean
	rm -rf build test/build
	rmdir test/bin
