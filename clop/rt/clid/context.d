module clop.rt.clid.context;

import std.container.array;
import std.stdio;
import std.exception;
import std.format;

import derelict.opencl.cl;
import clop.rt.clid.queue;
import clop.rt.clid.platform;
import clop.rt.clid.device;

class Context {
	public

	static Context GetDefault()
	{
		static Context def = null;
		if(def is null) {
			writeln("Creating context singleton");
			def = new Context();

			
			//def = cutk.make_shared<Context>();
			//const cutk.String devType = Clipp.Instance().settings().get("device_type").toString();
			//cutk.shared_ptr<Device> device;
			//if(devType.stdString() == "gpu")  {
			//	device = Platform.GetDefault().device(CL_DEVICE_TYPE_GPU);
			//} else {
				device = Platform.GetDefault().device(CL_DEVICE_TYPE_CPU);
			//}
			
			device.describe();
			def.initialize(device);
			
		} 
		return def;
	}

	Array!Queue queues()
	{
		return _queues;
	}

	Array!Queue queues() const
	{
		return _queues;
	}

	Queue queue(const size_t index) 
	{
		return _queues[index];
	}

	bool initialize(const Array!Device devices);

	bool initialize(const Device device);

	Array!Device devices()
	{
		return _devices;
	}

	Device device(const size_t index)
	{
		return _devices[index];
	}

	Array!Device devices() const
	{
		return _devices;
	}

	Device device(const size_t index) const
	{
		return _devices[index];
	}

	cl_context implementation() const
	{
		return _context;
	}

	this() {}
	~this();

	private
	cl_context _context;
	Array!Queue _queues;
	Array!Device _devices;
}

