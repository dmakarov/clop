
module clop.rt.clid.kernel;

import derelict.opencl.cl;
import clop.rt.clid.queue;
import clop.rt.clid.context;
import clop.rt.clid.arglist;


class Kernel {
public
	bool call(ArgList args, Queue queue = Queue.GetDefault())
	{

	}

	cl_ulong getPrefferedWorkGroupSizeMultiple(const cl_device_id devId = Context.GetDefault().device(0).getId() )
	{
		cl_ulong wgSize;
		clGetKernelWorkGroupInfo(this.implementation(), devId, CL_KERNEL_PREFERRED_WORK_GROUP_SIZE_MULTIPLE, sizeof(size_t), wgSize, NULL);
		return wgSize;
	}

	void setGlobalWorkSize(const size_t globalWorkSize0);
	void setLocalWorkSize(const size_t localWorkGroups0);

	void setGlobalWorkSize(const size_t globalWorkSize0,const size_t globalWorkSize1);
	void setLocalWorkSize(const size_t localWorkGroups0, const size_t localWorkGroups1);

	void setGlobalWorkSize(const size_t globalWorkSize0,const size_t globalWorkSize1, const size_t globalWorkSize2);
	void setLocalWorkSize(const size_t localWorkGroups0, const size_t localWorkGroups1, const size_t localWorkGroups2);


	size_t nWorkDims() const;

	this(const cl_kernel kernel)
	{
		_kernel = kernel;
		 _localWorkSize = null;
		 _globalWorkSize = null;
	}

	~this();

	size_t[] localWorkSize()
	{
		return _localWorkSize;
	}

	size_t[] globalWorkSize()
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


private
	cl_kernel _kernel;
	size_t[] _localWorkSize;
	size_t[] _globalWorkSize;
	Context _context;


	IMemory makeMemory(Arg)(Arg arg) const
	{
		return MakeMemory(arg);
	}

	bool fillList(Arg)(ArgList al, const size_t index, Arg arg)
	{
		auto mem = makeMemory(arg);
		if(!mem.commit(_context)) return false;
		al.arg(index, mem);	
		return true;
	}

	bool fillList(First, Rest...)(ArgList al, const size_t index, First first, Rest rest)
	{
		auto mem = makeMemory(first);
		if(!mem.commit(_context)) return false;
		al.arg(index, mem);
		return fillList(al, index+1, rest);
	}
}








