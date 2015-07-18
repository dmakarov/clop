CXX      := g++
CXXFLAGS := -O3 -Wall -std=c++14

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

all: lud

lud: app.cpp
	$(CXX) $(CXXFLAGS) -o $@ $< $(OPENCL_LIB)

clean:
	@rm -rf lud app.o
