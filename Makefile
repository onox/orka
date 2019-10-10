WINDOWING_BACKEND := egl

LIBRARY_TYPE ?= relocatable
MODE ?= development

CFLAGS  ?= -O2 -march=native
LDFLAGS ?= -Wl,-z,relro -Wl,-z,now

X_WINDOWING_SYSTEM := -XWindowing_System=$(WINDOWING_BACKEND)
X_LIBRARY_TYPE := -XLibrary_Type=$(LIBRARY_TYPE)

GPRBUILD = gprbuild -dm -p $(X_WINDOWING_SYSTEM) $(X_LIBRARY_TYPE)
GPRCLEAN = gprclean -q $(X_WINDOWING_SYSTEM)
GPRINSTALL = gprinstall -q $(X_WINDOWING_SYSTEM)

PREFIX ?= /usr

includedir = $(PREFIX)/include
gprdir     = $(PREFIX)/share/gpr
libdir     = $(PREFIX)/lib
alidir     = $(libdir)

.PHONY: build examples tools test coverage docs clean install

build:
	$(GPRBUILD) -P orka-glfw.gpr -XMode=$(MODE) -cargs $(CFLAGS) -largs $(LDFLAGS)

build_test:
	$(GPRBUILD) -P test/unit/orka/unit_tests.gpr -XMode=coverage -cargs -O0 -march=native -largs $(LDFLAGS)

examples: build
	$(GPRBUILD) -P examples.gpr -XMode=$(MODE) -cargs $(CFLAGS) -largs $(LDFLAGS)

tools: build
	$(GPRBUILD) -P tools.gpr -XMode=$(MODE) -cargs $(CFLAGS) -largs $(LDFLAGS)

test: build_test
	./test/unit/orka/bin/run_unit_tests

coverage:
	mkdir -p test/cov
	lcov -q -c -d test/unit/orka/obj -o test/cov/unit.info
	lcov -q -c -d obj/orka -o test/cov/unit.info
	lcov -q -r test/cov/unit.info */adainclude/* -o test/cov/unit.info
	lcov -q -r test/cov/unit.info */test/unit/* -o test/cov/unit.info
	genhtml -q --ignore-errors source -o test/cov/html test/cov/unit.info
	lcov -l test/cov/unit.info

docs:
	docker run --rm -it -p 8000:8000 -v ${PWD}:/docs:ro -u $(shell id -u):$(shell id -g) squidfunk/mkdocs-material

clean:
	$(GPRCLEAN) -r -P orka-glfw.gpr
	$(GPRCLEAN) -P test/unit/orka/unit_tests.gpr
	$(GPRCLEAN) -P examples.gpr
	$(GPRCLEAN) -P tools.gpr
	rm -rf bin lib obj test/unit/orka/obj test/unit/orka/bin test/cov

install:
	$(GPRINSTALL) --relocate-build-tree -p --install-name='orka' \
		--sources-subdir=$(includedir) \
		--project-subdir=$(gprdir) \
		--lib-subdir=$(libdir) \
		--ali-subdir=$(alidir) \
		--prefix=$(PREFIX) -P orka-lib.gpr
	$(GPRINSTALL) --relocate-build-tree -p --install-name='orka-glfw' \
		--sources-subdir=$(includedir) \
		--project-subdir=$(gprdir) \
		--lib-subdir=$(libdir) \
		--ali-subdir=$(alidir) \
		--prefix=$(PREFIX) -P orka-glfw.gpr
