WINDOWING_BACKEND := windows
UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  WINDOWING_BACKEND := x11
endif

WINDOWING_SYSTEM := -XWindowing_System=${WINDOWING_BACKEND}
LIBRARY_TYPE ?= relocatable
MODE ?= development

GNAT_FLAGS ?= -dm
CFLAGS  ?= -O2 -march=native
LDFLAGS ?= -Wl,-z,relro -Wl,-z,now

GPRBUILD = gprbuild $(GNAT_FLAGS) -p ${WINDOWING_SYSTEM} -XLibrary_Type=${LIBRARY_TYPE} -XCompiler_Flags="${CFLAGS}" -XMode=${MODE}
GPRCLEAN = gprclean -q ${WINDOWING_SYSTEM}

build_src:
	$(GPRBUILD) -P root.gpr -largs ${LDFLAGS}

build_examples:
	$(GPRBUILD) -P examples.gpr -largs ${LDFLAGS}

build_unit_tests:
	$(GPRBUILD) -P test/unit/orka/unit_tests.gpr

clean_unit_tests:
	$(GPRCLEAN) -P test/unit/orka/unit_tests.gpr
	rmdir test/unit/orka/obj
	rmdir test/unit/orka/bin

run_unit_tests:
	./test/unit/orka/bin/run_unit_tests

clean_src:
	$(GPRCLEAN) -r -P root.gpr
	rmdir lib/{glfw,orka}
	rmdir lib
	rmdir obj/{glfw,orka}
	rmdir obj

clean_examples:
	$(GPRCLEAN) -P examples.gpr
	rmdir bin

all: build

build: build_src

examples: build_examples

test: build_unit_tests

clean: clean_examples clean_unit_tests clean_src
