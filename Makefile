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

GNAT_FLAGS ?=
CFLAGS  ?= -O2 -march=native
LDFLAGS ?= -Wl,-z,relro -Wl,-z,now

GPRBUILD = gprbuild $(GNAT_FLAGS) -p ${WINDOWING_SYSTEM} -XCompiler_Flags="${CFLAGS}"
GPRCLEAN = gprclean -q ${WINDOWING_SYSTEM}

build_src:
	$(GPRBUILD) -P opengl-glfw.gpr -largs ${LDFLAGS}
	$(GPRBUILD) -P orka.gpr -largs ${LDFLAGS}

build_test:
	$(GPRBUILD) -P opengl_test.gpr -largs ${LDFLAGS}
	$(GPRBUILD) -P glfw_test.gpr -largs ${LDFLAGS}
	$(GPRBUILD) -P orka_test.gpr -largs ${LDFLAGS}

clean_src:
	$(GPRCLEAN) -r -P opengl-glfw.gpr
	$(GPRCLEAN) -r -P orka.gpr
	rmdir lib/{glfw,orka,opengl}
	rmdir obj/{glfw,orka,opengl}
	rmdir lib obj

clean_test:
	$(GPRCLEAN) -P orka_test.gpr
	$(GPRCLEAN) -P glfw_test.gpr
	$(GPRCLEAN) -P opengl_test.gpr
	rmdir bin

all: build

build: build_src

test: build_test

clean: clean_test clean_src
