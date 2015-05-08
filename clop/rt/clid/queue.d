module clop.rt.clid.queue;

import std.array;
import std.stdio;
import derelict.opencl.cl;
import clop.rt.clid.kernel;
import clop.rt.clid.clerror;
import clop.rt.clid.context;


class Queue {
	public {

		this(cl_command_queue queue)
		{
			_queue = queue;
		}


		~this()
		{
			release();
		}

		bool enqueue(Kernel kernel)
		{
			cl_ulong[] gws = null;
			cl_ulong[] lws = null;

			if(!kernel.localWorkSize().empty) {
				lws = kernel.localWorkSize();
			}

			if(!kernel.globalWorkSize().empty) {
				gws = kernel.globalWorkSize();
			}


			cl_ulong[] empty0;
			void[] empty1;
			void[][] empty2;

			cl_event event;
			CLError err = new CLError( 
				clEnqueueNDRangeKernel(
					_queue,
					kernel.implementation(),
					cast(cl_uint) kernel.nWorkDims(),
					null,
					gws.ptr,
					lws.ptr,
					0, 
					null, 
					&event) 
				);

			return err.success();
		}

		bool finish()
		{
			return CLError.Check(clFinish(_queue));
		}


		cl_command_queue implementation() {
			return _queue;
		}

		static Queue GetDefault()
		{
			static Queue instance = null;
			if(instance is null) {
				writeln("Creating Queue instance");
				instance =  Context.GetDefault().queue(0);
			}

			return instance;
		}


		void release()
		{
			clReleaseCommandQueue(_queue);
		} 
	}

	private {
		cl_command_queue _queue;
	}
}



