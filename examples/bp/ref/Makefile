CXX      := g++
CXXFLAGS := -I../../include -O3 --std=c++11 -Wall

# OPENCL
UNAME := $(shell uname)
ifeq ($(UNAME), Linux)
  OPENCL_LIB := -lOpenCL -lrt
  DOCL_LIB := -L-lOpenCL
endif
ifeq ($(UNAME), Darwin)
  OPENCL_LIB := -framework OpenCL
  DOCL_LIB := -L-framework -LOpenCL
endif

all: bp

bp: app.cpp ../../include/common.hpp Makefile
	$(CXX) $(CXXFLAGS) -o $@ $< $(OPENCL_LIB)

clean:
	@rm -rf bp app.o
