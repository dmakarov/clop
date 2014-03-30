DC       := ../ldc/build/bin/ldmd2
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

#nw: nw.cpp
#	$(CXX) $(CXXFLAGS) -o $@ $< $(OPENCL_LIB)

nw: nw.d lib/libcl4d.a
	$(DC) $< -d -release -O -of$@ -Icl4d $(DOCL_LIB) -L-Llib -L-lcl4d

lib/libcl4d.a: cl4d/opencl/*.d cl4d/opencl/c/cl.d cl4d/opencl/c/cl_ext.d cl4d/opencl/c/cl_gl.d cl4d/opencl/c/cl_gl_ext.d cl4d/opencl/c/cl_platform.d cl4d/opencl/c/opencl.d
	$(DC) -d -release -O -Icl4d -lib -of$@ $^

clean:
	@rm -rf nw nw.o
