module clop.rt.clid.device;
import derelict.opencl.cl;

class Device {
public:
	cl_device_type getType() const
	{
		cl_device_type ret;
		cl_int ok = clGetDeviceInfo(_id, CL_DEVICE_TYPE, sizeof(cl_device_type), &ret, NULL);
		checkValid(ok);
		return ret;
	}


	cl_device_id getId() const
	{
		return _id;
	}


	bool isGPU() const
	{
		return getType() == CL_DEVICE_TYPE_GPU;
	}

	bool isCPU() const
	{
		return getType() == CL_DEVICE_TYPE_CPU;
	}

	bool isAPU() const
	{
		return getType() == CL_DEVICE_TYPE_ACCELERATOR;
	}


	void describe() const
	{
		writeln(stringInfoToBuffer("DEVICE_NAME", CL_DEVICE_NAME));
		writeln(stringInfoToBuffer("DEVICE_VENDOR", CL_DEVICE_VENDOR));
		writeln(stringInfoToBuffer("DEVICE_VERSION", CL_DEVICE_VERSION));
		writeln(stringInfoToBuffer("DRIVER_VERSION", CL_DRIVER_VERSION));
		writeln(infoToBuffer!cl_uint("DEVICE_MAX_COMPUTE_UNITS", CL_DEVICE_MAX_COMPUTE_UNITS));
		writeln(infoToBuffer!cl_uint("DEVICE_MAX_CLOCK_FREQUENCY", CL_DEVICE_MAX_CLOCK_FREQUENCY));
		writeln(infoToBuffer!cl_ulong("DEVICE_GLOBAL_MEM_SIZE", CL_DEVICE_GLOBAL_MEM_SIZE));
		writeln(infoToBuffer!cl_ulong("DEVICE_MAX_MEM_ALLOC_SIZE", CL_DEVICE_MAX_MEM_ALLOC_SIZE));
		writeln(infoToBuffer!cl_ulong("CL_DEVICE_MAX_WORK_GROUP_SIZE", CL_DEVICE_MAX_WORK_GROUP_SIZE));


		cl_device_fp_config prec;
		checkValid(clGetDeviceInfo(_id, CL_DEVICE_DOUBLE_FP_CONFIG, sizeof(cl_device_fp_config), &prec, NULL));
		writeln(" CL_DEVICE_DOUBLE_FP_CONFIG = %d\n", ( (CL_FP_FMA | CL_FP_ROUND_TO_NEAREST | CL_FP_ROUND_TO_ZERO | CL_FP_ROUND_TO_INF | CL_FP_INF_NAN | CL_FP_DENORM) == prec ));
	}

	bool checkValid(const cl_int ret) const
	{
		if(CL_INVALID_DEVICE == ret) {
			writeln("CL_INVALID_DEVICE");
			assert(ret == CL_SUCCESS);
			return false;
		}

		if(CL_INVALID_VALUE == ret) {
			writeln("CL_INVALID_VALUE");
			assert(ret == CL_SUCCESS);
			return false;
		}

		assert(ret == CL_SUCCESS);
		return ret == CL_SUCCESS;
	}


	void toString(const string infoName, cl_device_info info) const
	{
		static const int MAX_BUFFER_SIZE = 2048;
		char[MAX_BUFFER_SIZE] buffer;
		checkValid(clGetDeviceInfo(_id, info, sizeof(buffer), buffer, NULL));
		return + infoName + " = " + buffer + "\n";
	}



	string toStringWithType(T)(const string infoName, cl_device_info info) const
	{
		T buf;
		checkValid(clGetDeviceInfo(getId(), info, sizeof(buf), buf, NULL));
		return infoName + " = " + buf + "\n";
	}

	this(const cl_device_id id)
	{
		_id = id;
	}

	this()
	{
		_id = 0;
	}

	~this()
	{
		clReleaseDevice(_id);
	}
	
	private
	cl_device_id _id;
}
