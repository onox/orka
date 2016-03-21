WINDOWING_BACKEND := windows
UNAME := $(shell uname)
ifeq ($(UNAME), Darwin)
  WINDOWING_BACKEND := quartz
endif
ifeq ($(UNAME), Linux)
  WINDOWING_BACKEND := x11
endif

WINDOWING_SYSTEM := -XWindowing_System=${WINDOWING_BACKEND}
LIBRARY_TYPE ?= static

GNAT_FLAGS ?=
GPRBUILD = gprbuild $(GNAT_FLAGS) -p ${WINDOWING_SYSTEM}
GPRCLEAN = gprclean -q ${WINDOWING_SYSTEM}

build_src:
	$(GPRBUILD) -P opengl-glfw.gpr

build_tests:
	$(GPRBUILD) -P opengl_test.gpr
	$(GPRBUILD) -P glfw_test.gpr

clean_src:
	$(GPRCLEAN) -r -P opengl-glfw.gpr
	rmdir bin lib obj

clean_tests:
	$(GPRCLEAN) -P glfw_test.gpr
	$(GPRCLEAN) -P opengl_test.gpr

all: build

build: build_src

test: build_tests

clean: clean_tests clean_src
