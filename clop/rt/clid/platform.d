module clop.rt.clid.platform;

public import std.algorithm;
import std.exception;
import std.stdio;
import std.string;
import std.container.array;
import derelict.opencl.cl;

import clop.rt.clid.queue;
import clop.rt.clid.context;
import clop.rt.clid.arglist;
import clop.rt.clid.device;
import clop.rt.clid.clerror;

class Platform  {
	public

	bool GetIDs(cl_platform_id[] platforms, cl_uint maxPlatforms = 1)
	{
		DerelictCL.load();
		cl_uint numPlatforms;
		platforms.length = maxPlatforms;
		cl_int status = clGetPlatformIDs(maxPlatforms, platforms.ptr, &numPlatforms);
		platforms.length = numPlatforms;
		return status == CL_SUCCESS;
	}

	this()
	{
		GetIDs(_platforms);

		cl_uint numDevices;
		CLError ret = new CLError(clGetDeviceIDs(_platforms[0], CL_DEVICE_TYPE_ALL, 0, null, &numDevices));
		assert(ret.success());
		assert(numDevices > 0);

		cl_device_id[] temp = new cl_device_id[numDevices];

		ret = new CLError(clGetDeviceIDs(_platforms[0], cast(uint)CL_DEVICE_TYPE_ALL, cast(uint)temp.length, cast(void **)temp, &numDevices));
		assert(ret.success());

		_devices.reserve(temp.length);

		foreach(cl_device_id it; temp) {
			_devices.insertBack(new Device(it));
		}
	}

	Device cpu()
	{
		return device(CL_DEVICE_TYPE_CPU);
	}

	Device device(cl_device_type type) 
	{
		cl_device_id dId;
		cl_uint numDevices;
		CLError ret = new CLError(clGetDeviceIDs(_platforms[0], type, 1, &dId, &numDevices));
		return new Device(dId);
	}

	Array!Device devices() 
	{
		return _devices;
	}

	size_t nDevices()
	{
		return _devices.length;
	}

	static Platform GetDefault()
	{
		static Platform def = new Platform();
		return def;
	}

	private
	Array!Device _devices;
	cl_platform_id[] _platforms;
}


