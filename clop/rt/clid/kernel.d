
module clop.rt.clid.kernel;

import derelict.opencl.cl;
import clop.rt.clid.queue;
import clop.rt.clid.context;
import clop.rt.clid.arglist;


class Kernel {
	public {
		bool call(ArgList args, Queue queue = Queue.GetDefault())
		{
			bool ok = args.commit(_context, queue);
			if(!ok) return false;
			
			ok = args.setTo(this);
			if(!ok) return false;

			return queue.enqueue(this);
		}

		cl_ulong getPrefferedWorkGroupSizeMultiple(cl_device_id devId = Context.GetDefault().device(0).getId() )
		{
			cl_ulong wgSize;
			ulong[] empty;
			clGetKernelWorkGroupInfo(this.implementation(), devId, cast(int)CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE, ulong.sizeof, cast(void *)&wgSize, empty.ptr);
			return wgSize;
		}

		void setGlobalWorkSize(ulong globalWorkSize0)
		{
			_globalWorkSize.length = 1;
			_globalWorkSize[0] = globalWorkSize0;
		}

		void setLocalWorkSize(ulong localWorkSize0)
		{
			_localWorkSize.length = 1;
			_localWorkSize[0] = localWorkSize0;

		}

		void setGlobalWorkSize(ulong globalWorkSize0,ulong globalWorkSize1)
		{
			_globalWorkSize.length = 2;
			_globalWorkSize[0] = globalWorkSize0;
			_globalWorkSize[1] = globalWorkSize1;
		}

		void setLocalWorkSize(ulong localWorkSize0, ulong localWorkSize1)
		{	
			_localWorkSize.length = 2;
			_localWorkSize[0] = localWorkSize0;
			_localWorkSize[1] = localWorkSize1;
		}

		void setGlobalWorkSize(ulong globalWorkSize0,ulong globalWorkSize1, ulong globalWorkSize2)
		{
			_globalWorkSize.length = 3;
			_globalWorkSize[0] = globalWorkSize0;
			_globalWorkSize[1] = globalWorkSize1;
			_globalWorkSize[2] = globalWorkSize2;
		}

		void setLocalWorkSize(ulong localWorkSize0, ulong localWorkSize1, ulong localWorkSize2)
		{
			_localWorkSize.length = 3;
			_localWorkSize[0] = localWorkSize0;
			_localWorkSize[1] = localWorkSize1;
			_localWorkSize[2] = localWorkSize2;
		}

		ulong nWorkDims() 
		{
			assert(_localWorkSize.length == 0 || _localWorkSize.length == _globalWorkSize.length);
			return _globalWorkSize.length;
		}

		~this()
		{
			clReleaseKernel(_kernel);
		}
		

		this(cl_kernel kernel)
		{
			_kernel = kernel;
			_localWorkSize = null;
			_globalWorkSize = null;
		}

		ulong[] localWorkSize()
		{
			return _localWorkSize;
		}

		ulong[] globalWorkSize()
		{
			return _globalWorkSize;
		}

		cl_kernel implementation()
		{
			return _kernel;
		}

		void setContext(Context context)
		{
			_context = context;
		}

		bool call(Args...)(Args args)
		{
			ArgList al;
			if(!fillList(al, 0, args)) return false;
			if(!call(al)) return false;
			if(!al.updateHost()) return false;
			return Queue.GetDefault().finish();
		}
	}

	private {
		cl_kernel _kernel;
		ulong[] _localWorkSize;
		ulong[] _globalWorkSize;
		Context _context;


		IMemory makeMemory(Arg)(Arg arg) 	
		{
			return MakeMemory(arg);
		}

		bool fillList(Arg)(ArgList al, ulong index, Arg arg)
		{
			auto mem = makeMemory(arg);
			if(!mem.commit(_context)) return false;
			al.arg(index, mem);	
			return true;
		}

		bool fillList(First, Rest...)(ArgList al, ulong index, First first, Rest rest)
		{
			auto mem = makeMemory(first);
			if(!mem.commit(_context)) return false;
			al.arg(index, mem);
			return fillList(al, index+1, rest);
		}
	}
}








