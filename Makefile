WINDOWING_BACKEND := windows
UNAME := $(shell uname)
ifeq ($(UNAME), Darwin)
  WINDOWING_BACKEND := quartz
endif
ifeq ($(UNAME), Linux)
  WINDOWING_BACKEND := x11
endif

WINDOWING_SYSTEM := -XWindowing_System=${WINDOWING_BACKEND}
LIBRARY_TYPE ?= relocatable
MODE ?= release

GNAT_FLAGS ?=
CFLAGS  ?= -O2 -march=native
LDFLAGS ?= -Wl,-z,relro -Wl,-z,now

GPRBUILD = gprbuild $(GNAT_FLAGS) -p ${WINDOWING_SYSTEM} -XLibrary_Type=${LIBRARY_TYPE} -XCompiler_Flags="${CFLAGS}" -XMode=${MODE}
GPRCLEAN = gprclean -q ${WINDOWING_SYSTEM}

build_src:
	$(GPRBUILD) -P root.gpr -largs ${LDFLAGS}

build_examples:
	$(GPRBUILD) -P examples.gpr -largs ${LDFLAGS}

clean_src:
	$(GPRCLEAN) -r -P root.gpr
	rmdir lib/{glfw,orka,opengl}
	rmdir lib
	rmdir obj/{glfw,orka,opengl}
	rmdir obj

clean_examples:
	$(GPRCLEAN) -P examples.gpr
	rmdir bin

all: build

build: build_src

examples: build_examples

clean: clean_examples clean_src
