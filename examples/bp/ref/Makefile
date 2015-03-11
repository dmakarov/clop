include ../../common/make.config

backprop: backprop.cpp backprop.h $(COMMON_HEADERS)
	$(C++) $< -o $@ $(CFLAGS) $(OPENCL_INC) $(OPENCL_LIB)

clean:
	@rm -rf backprop $(COMMON_OBJECTS)
