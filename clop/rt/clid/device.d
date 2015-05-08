module clop.rt.clid.device;

import std.stdio;
import std.conv;
import std.string;
import derelict.opencl.cl;

class Device {
	public {
		cl_device_type getType() 
		{
			cl_device_type ret;

			ulong[] empty;
			cl_int ok = clGetDeviceInfo(_id, CL_DEVICE_TYPE, cl_device_type.sizeof, &ret, empty.ptr);
			checkValid(ok);
			return ret;
		}


		cl_device_id getId() 
		{
			return _id;
		}


		bool isGPU() 
		{
			return getType() == CL_DEVICE_TYPE_GPU;
		}

		bool isCPU() 
		{
			return getType() == CL_DEVICE_TYPE_CPU;
		}

		bool isAPU() 
		{
			return getType() == CL_DEVICE_TYPE_ACCELERATOR;
		}


		void describe() 
		{
			writeln(info2String("DEVICE_NAME", CL_DEVICE_NAME));
			writeln(info2String("DEVICE_VENDOR", CL_DEVICE_VENDOR));
			writeln(info2String("DEVICE_VERSION", CL_DEVICE_VERSION));
			writeln(info2String("DRIVER_VERSION", CL_DRIVER_VERSION));
			writeln(info2StringWithType!cl_uint("DEVICE_MAX_COMPUTE_UNITS", CL_DEVICE_MAX_COMPUTE_UNITS));
			writeln(info2StringWithType!cl_uint("DEVICE_MAX_CLOCK_FREQUENCY", CL_DEVICE_MAX_CLOCK_FREQUENCY));
			writeln(info2StringWithType!cl_ulong("DEVICE_GLOBAL_MEM_SIZE", CL_DEVICE_GLOBAL_MEM_SIZE));
			writeln(info2StringWithType!cl_ulong("DEVICE_MAX_MEM_ALLOC_SIZE", CL_DEVICE_MAX_MEM_ALLOC_SIZE));
			writeln(info2StringWithType!cl_ulong("CL_DEVICE_MAX_WORK_GROUP_SIZE", CL_DEVICE_MAX_WORK_GROUP_SIZE));


			cl_device_fp_config prec;
			checkValid(clGetDeviceInfo(_id, CL_DEVICE_DOUBLE_FP_CONFIG, cl_device_fp_config.sizeof, &prec, null));
			writeln("CL_DEVICE_DOUBLE_FP_CONFIG = ", ( (CL_FP_FMA | CL_FP_ROUND_TO_NEAREST | CL_FP_ROUND_TO_ZERO | CL_FP_ROUND_TO_INF | CL_FP_INF_NAN | CL_FP_DENORM) == prec ));
		}

		bool checkValid( cl_int ret) 
		{
			if(CL_INVALID_DEVICE == ret) {
				writeln("CL_INVALID_DEVICE");
				assert(ret == CL_SUCCESS);
				return false;
			}

			if(CL_INVALID_VALUE == ret) {
				writeln("CL_INVALID_VALUE");
			//assert(ret == CL_SUCCESS);
			return false;
		}

		assert(ret == CL_SUCCESS);
		return ret == CL_SUCCESS;
	}


	string info2String(string infoName, cl_device_info info) 
	{
		static const int MAX_BUFFER_SIZE = 2048;
		char[] buffer= new char[MAX_BUFFER_SIZE];
		ulong[] empty;
		if(!checkValid(clGetDeviceInfo(_id, info, buffer.sizeof, cast(void *)buffer.ptr, empty.ptr))) {
			return infoName ~ " = " ~ "[Error]";
		}
		string str = text(fromStringz(cast(char *)buffer.ptr));
		return infoName ~ " = " ~ str;
	}



	string info2StringWithType(T)(string infoName, cl_device_info info) 
	{
		T buf;
		ulong[] empty;
		checkValid(clGetDeviceInfo(getId(), info, buf.sizeof, cast(void *)&buf, empty.ptr));
		return infoName ~ " = " ~ text(buf);
	}

	this( cl_device_id id)
	{
		_id = id;
	}

	this()
	{
		_id = null;
	}

	~this()
	{
		clReleaseDevice(_id);
	}
}

private {
	cl_device_id _id;
}
}
