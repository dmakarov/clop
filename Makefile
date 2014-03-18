DC       := dmd
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
	$(DC) $< -of$@ -Icl4d $(DOCL_LIB) -L-Llib -L-lcl4d

lib/libcl4d.a: cl4d/opencl/*.d cl4d/opencl/c/cl.d cl4d/opencl/c/opencl.d
	$(DC) -Icl4d -lib -of$@ $^

clean:
	@rm -rf nw nw.o
