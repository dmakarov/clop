CXX      := g++
CXXFLAGS := -O3 -Wall

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

all: algebra

algebra: app.cpp
	$(CXX) $(CXXFLAGS) -o $@ $< $(OPENCL_LIB)

clean:
	@rm -rf algebra app.o
