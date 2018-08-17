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

SRC_DIR = src
LIB_DIR = lib

PREFIX ?= /usr

includedir = $(PREFIX)/include
gprdir     = $(PREFIX)/share/gpr
libdir     = $(PREFIX)/lib
alidir     = $(libdir)

build_src:
	$(GPRBUILD) -P orka-glfw.gpr -largs $(LDFLAGS)

build_examples:
	$(GPRBUILD) -P examples.gpr -largs $(LDFLAGS)

build_unit_tests:
	$(GPRBUILD) -P test/unit/orka/unit_tests.gpr

clean_unit_tests:
	$(GPRCLEAN) -P test/unit/orka/unit_tests.gpr
	rmdir test/unit/orka/obj
	rmdir test/unit/orka/bin

run_unit_tests:
	./test/unit/orka/bin/run_unit_tests

clean_src:
	$(GPRCLEAN) -r -P orka-glfw.gpr
	rmdir lib/glfw
	rmdir lib/orka
	rmdir lib
	rmdir obj/glfw
	rmdir obj/orka
	rmdir obj

clean_examples:
	$(GPRCLEAN) -P examples.gpr
	rmdir bin

all: build

build: build_src

examples: build_examples

test: build_unit_tests

clean: clean_examples clean_unit_tests clean_src


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
