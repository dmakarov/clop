CXX      ?= g++
CXXFLAGS := -std=c++11 -O3 -Wall
UNAME    := $(shell uname)

# OPENCL
ifeq ($(UNAME), Linux)
  OPENCL_LIB := -lOpenCL -lrt
endif
ifeq ($(UNAME), Darwin)
  OPENCL_LIB := -framework OpenCL
endif

all: nw

nw: main.cpp
	$(CXX) $(CXXFLAGS) -o $@ $< $(OPENCL_LIB)

clean:
	@rm -rf nw
