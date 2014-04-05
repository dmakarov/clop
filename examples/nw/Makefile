DC       := ldc2
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

all: nwc nwd

nwc: nw.cpp
	$(CXX) $(CXXFLAGS) -o $@ $< $(OPENCL_LIB)

nwd: nw.d ../lib/libcl4d.a
	$(DC) $< -d -release -O -of$@ -I../cl4d $(DOCL_LIB) -L-L../lib -L-lcl4d

clean:
	@rm -rf nwc nwd nwd.o
