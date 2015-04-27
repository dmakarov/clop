module clop.rt.clid.queue;

import std.array;
import derelict.opencl.cl;
import clop.rt.clid.kernel;


class Queue {
	public
	
	this(const cl_command_queue queue);
	

	~this()
	{
		release();
	}

	bool enqueue(Kernel kernel)
	{
		size_t * gws = NULL;
		size_t * lws = NULL;

		if(!kernel.localWorkSize().empty()) {
			lws = &kernel.localWorkSize()[0];
		}

		if(!kernel.globalWorkSize().empty()) {
			gws = &kernel.globalWorkSize()[0];
		}

		Error err = clEnqueueNDRangeKernel(_queue,
			kernel.implementation(),
			kernel.nWorkDims(),
			NULL,
			gws,
			lws,
			0, NULL, NULL);

		return err.success();
	}

	bool enqueue(Kernel kernel, cl_event[] eventsToWait, ref cl_event event);
	bool finish();


	cl_command_queue implementation() const {
		return _queue;
	}

	static Queue GetDefault()
	{
		static Queue instance = null;
		if(instance is null) {
			writeln("Creating Queue instance");
			instance = new Queue();
		}

		return instance;
	}


	void release()
	{
		clReleaseCommandQueue(_queue);
	}

	private
	cl_command_queue _queue;
}



