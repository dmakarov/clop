CXX      ?= g++
CXXFLAGS := -std=c++11 -O3 -Wall -Werror -I../../include
UNAME    := $(shell uname)

# OPENCL
ifeq ($(UNAME), Linux)
  OPENCL_LIB := -lOpenCL -lrt
endif
ifeq ($(UNAME), Darwin)
  OPENCL_LIB := -framework OpenCL
endif

all: algebra

algebra: main.cpp ../../include/common.hpp Makefile
	$(CXX) $(CXXFLAGS) -o $@ $< $(OPENCL_LIB)

clean:
	@rm -rf algebra
