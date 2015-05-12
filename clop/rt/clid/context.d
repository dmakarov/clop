module clop.rt.clid.context;
import std.array;
import std.container.array;
import std.stdio;
import std.exception;
import std.format;

import derelict.opencl.cl;
import clop.rt.clid.queue;
import clop.rt.clid.platform;
import clop.rt.clid.device;
import clop.rt.clid.clerror;
import clop.rt.clid.settings;


class Context {
	public {

		static Context GetDefault()
		{
			static Context def = null;
			if(def is null) {
				writeln("Creating context singleton");
				def = new Context();

				Settings s = Settings.Instance();
				Device device = null;

				writeln(s.deviceType());

				switch(s.deviceType()) {
					case "cpu" : 
					{
						device = Platform.GetDefault().cpu();
						break;
					}

					case "gpu":
					{
						device = Platform.GetDefault().gpu();
						break;
					}

					default: 
					{
						break;
					}
				}

				if(device is null) {
					writeln("[Error] invalid context");
					return null;
				}

				device.describe();
				def.initialize(device);
			} 

			return def;
		}

		Array!Queue queues()
		{
			return _queues;
		}

		Queue queue(size_t index) 
		{
			return _queues[index];
		}

		bool initialize(Device device)
		{
			Array!Device vec = [device];
			return initialize(vec);
		}

		bool initialize(Array!Device devices) 
		{
			DerelictCL.load();
			cl_int err = 0;

			cl_device_id[] devIds;
			devIds.length = devices.length;
			
			int i = 0;
			foreach(Device it; devices) {
				devIds[i++] = it.getId();
			}
			
			long[] empty;
			_context = clCreateContext(null, cast(uint)devIds.length, &devIds[0], null, null, &err);

			CLError ret = new CLError(err);
			if(!ret.success())
				return false;

			_queues.length = devices.length;

			bool ok = true;
			cl_ulong nDevices = devices.length;
			for(i = 0; i < nDevices; ++i) {
				_queues[i] = new Queue(clCreateCommandQueue(_context,  devIds[i], 0, &err));
				ok &= ret.check(err);
			}

			_devices = devices;

			_initialized = true;
			return ok;
		}

		Array!Device devices()
		{
			return _devices;
		}
		
		Device device(size_t index) 
		{
			return _devices[index];
		}

		cl_context implementation() 
		{
			return _context;
		}

		this() 
		{
			_initialized = false;
		}

		~this()
		{
			if(!_initialized) return;

			cl_int ret = clReleaseContext(_context);
			assert(ret == CL_SUCCESS);
		}
	}

	private {
		bool _initialized;
		cl_context _context;
		Array!Queue _queues;
		Array!Device _devices;
	}
}

