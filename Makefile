WINDOWING_BACKEND := windows
UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  WINDOWING_BACKEND := egl
endif

LIBRARY_TYPE ?= relocatable
MODE ?= development

GNAT_FLAGS ?= -dm
CFLAGS  ?= -O2 -march=native
LDFLAGS ?= -Wl,-z,relro -Wl,-z,now

X_WINDOWING_SYSTEM := -XWindowing_System=$(WINDOWING_BACKEND)
X_LIBRARY_TYPE := -XLibrary_Type=$(LIBRARY_TYPE)
X_MODE = -XMode=$(MODE)
X_COMPILER_FLAGS = -XCompiler_Flags="${CFLAGS}"

GPRBUILD = gprbuild $(GNAT_FLAGS) -p $(X_WINDOWING_SYSTEM) $(X_LIBRARY_TYPE) $(X_COMPILER_FLAGS) $(X_MODE)
GPRCLEAN = gprclean -q $(X_WINDOWING_SYSTEM)
GPRINSTALL = gprinstall -q $(X_WINDOWING_SYSTEM)

PREFIX ?= /usr

includedir = $(PREFIX)/include
gprdir     = $(PREFIX)/share/gpr
libdir     = $(PREFIX)/lib
alidir     = $(libdir)

.PHONY: build examples tools test run_unit_tests clean install

build:
	$(GPRBUILD) -P orka-glfw.gpr -largs $(LDFLAGS)

examples:
	$(GPRBUILD) -P examples.gpr -largs $(LDFLAGS)

tools:
	$(GPRBUILD) -P tools.gpr -largs $(LDFLAGS)

test:
	$(GPRBUILD) -P test/unit/orka/unit_tests.gpr

run_unit_tests:
	./test/unit/orka/bin/run_unit_tests

clean:
	$(GPRCLEAN) -r -P orka-glfw.gpr
	$(GPRCLEAN) -P test/unit/orka/unit_tests.gpr
	$(GPRCLEAN) -P examples.gpr
	$(GPRCLEAN) -P tools.gpr
	rmdir lib/glfw
	rmdir lib/orka
	rmdir lib
	rmdir obj/glfw
	rmdir obj/orka
	rmdir obj
	rmdir bin

install:
	$(GPRINSTALL) --relocate-build-tree -p --install-name='orka' \
		--sources-subdir=$(includedir) \
		--project-subdir=$(gprdir) \
		--lib-subdir=$(libdir) \
		--ali-subdir=$(alidir) \
		--prefix=$(PREFIX) -P orka.gpr
	$(GPRINSTALL) --relocate-build-tree -p --install-name='orka-glfw' \
		--sources-subdir=$(includedir) \
		--project-subdir=$(gprdir) \
		--lib-subdir=$(libdir) \
		--ali-subdir=$(alidir) \
		--prefix=$(PREFIX) -P orka-glfw.gpr
