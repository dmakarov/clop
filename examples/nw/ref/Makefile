CXX      ?= g++
CXXFLAGS := -std=c++11 -O3 -Wall -Werror -I../../include
UNAME    := $(shell uname)
COMMON_HEADERS = ../../include/common.hpp

# OPENCL
ifeq ($(UNAME), Linux)
  OPENCL_LIB := -lOpenCL -lrt
endif
ifeq ($(UNAME), Darwin)
  OPENCL_LIB := -framework OpenCL
endif

all: nw

nw: main.cpp $(COMMON_HEADERS) Makefile
	$(CXX) $(CXXFLAGS) -o $@ $< $(OPENCL_LIB)

clean:
	@rm -rf nw
