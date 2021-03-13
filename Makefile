CFLAGS ?= -O2 -march=native

WINDOWING_BACKEND := egl
LIBRARY_TYPE ?= relocatable
MODE ?= development

GLFW_LIBS := $(strip $(shell pkgconf --libs glfw3))
SIMD := $(shell ((gcc $(CFLAGS) -dN -E - < /dev/null | grep -q "AVX2") && echo "AVX2") || echo "AVX")

X_WINDOWING_SYSTEM := -XWindowing_System=$(WINDOWING_BACKEND)
X_LIBRARY_TYPE := -XLibrary_Type=$(LIBRARY_TYPE)
X_GLFW_LIBS := -XGLFW_Libs="$(GLFW_LIBS)"
X_SIMD := -XORKA_SIMD_EXT="$(SIMD)"
SCENARIO_VARS = $(X_WINDOWING_SYSTEM) $(X_LIBRARY_TYPE) $(X_GLFW_LIBS) $(X_SIMD)

GPRBUILD = nice gprbuild -dm -p $(SCENARIO_VARS)
GPRCLEAN = gprclean -q $(SCENARIO_VARS)
GPRINSTALL = gprinstall -q $(SCENARIO_VARS)

PREFIX ?= /usr

includedir = $(PREFIX)/include
gprdir     = $(PREFIX)/share/gpr
libdir     = $(PREFIX)/lib
alidir     = $(libdir)

installcmd = $(GPRINSTALL) -p \
	--sources-subdir=$(includedir) \
	--project-subdir=$(gprdir) \
	--lib-subdir=$(libdir) \
	--ali-subdir=$(alidir) \
	--prefix=$(PREFIX)

.PHONY: build examples tools tests coverage docs clean install uninstall

build:
	cd orka_egl && alr build
	$(GPRBUILD) -P tools/orka-glfw.gpr -XMode=$(MODE) -cargs $(CFLAGS)

build_test:
	$(GPRBUILD) -P tests/unit/tests.gpr -XMode=coverage -cargs -O0 -march=native

examples: build
	$(GPRBUILD) -P tools/examples.gpr -XMode=$(MODE) -cargs $(CFLAGS)

tools: build
	$(GPRBUILD) -P tools/tools.gpr -XMode=$(MODE) -cargs $(CFLAGS)

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
	$(GPRCLEAN) -r -P tools/orka-glfw.gpr
	$(GPRCLEAN) -P tests/unit/tests.gpr
	$(GPRCLEAN) -P tools/examples.gpr
	$(GPRCLEAN) -P tools/tools.gpr
	rm -rf bin build tests/unit/build tests/unit/bin tests/cov

install:
	$(installcmd) -f --install-name='orka' -P tools/orka.gpr
	$(installcmd) -f --install-name='orka-glfw' -P tools/orka-glfw.gpr

uninstall:
	$(installcmd) --uninstall --install-name='orka-glfw' -P tools/orka-glfw.gpr
	$(installcmd) --uninstall --install-name='orka' -P tools/orka.gpr
