include ../../common/make.config

pathfinder: pathfinder.cpp $(COMMON_HEADERS)
	$(C++) $< -o $@ $(CFLAGS) $(OPENCL_INC) $(OPENCL_LIB)

clean:
	@rm -rf pathfinder *\# $(COMMON_OBJECTS)
