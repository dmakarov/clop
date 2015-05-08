module clop.rt.clid.arraymemory;


import std.container.array;
import std.stdio;

import clop.rt.clid.imemory;

class ArrayMemory(T) : IMemory {
	public
	cl_mem implementation()
	{
		return _mem;
	}

	void init()
	{
		_isInitialized = false;
		_isFinalized = false;
		_ptr = null;
		_size = 0;
		_flags = CL_MEM_USE_HOST_PTR | CL_MEM_READ_WRITE;
		_mappedPtr = null;
	}

	this()
	{
		init();
	}

	this(T[] ptr, size_t size, cl_mem_flags flags)
	{
		init();
		_ptr = ptr;
		_size = size;
		_flags = flags;
	}

	void setMemFlags(cl_mem_flags flags)
	{
		_flags = flags;
	}

	void setReadOnly()
	{
		setMemFlags(CL_MEM_USE_HOST_PTR | CL_MEM_READ_ONLY);
	}

	void setWriteOnly()
	{
		setMemFlags(CL_MEM_USE_HOST_PTR | CL_MEM_WRITE_ONLY);
	}

	void setReadWrite()
	{
		setMemFlags(CL_MEM_USE_HOST_PTR | CL_MEM_READ_WRITE);
	}

	T * ptr() { return _ptr; }
	size_t size() { return _size; }

	~this()
	{
		clear();
	}

	void clear()
	{
		if(_isInitialized) {
			clReleaseMemObject(_mem);
			_isInitialized = false;
		}
	}

	bool initialize(Context context = Context.GetDefault())
	{
		if(_isInitialized) return true;


		cl_int err;
		_mem = clCreateBuffer(context.implementation(), _flags, sizeof(T) * _size, cast(void *) _ptr, &err);
		_isInitialized = CLError.Check(err);
		return _isInitialized;
	}

	bool updateDevice(Queue queue = Queue.GetDefault())
	{
		CUTK_ASSERT(_isInitialized);
		_isFinalized = fromHostToDevice(queue);
		return _isFinalized;
	}

	bool updateHost(Queue queue = Queue.GetDefault()) 
	{
		CUTK_ASSERT(_isInitialized);
		return fromDeviceToHost(queue);
	}

	bool finalize(Queue queue = Queue.GetDefault())
	{
		CUTK_ASSERT(_isInitialized);
		if(_isFinalized) return true;
		_isFinalized = fromHostToDevice(queue);
		return _isFinalized;
	}

	bool commit(Context context = Context.GetDefault(), Queue queue = Queue.GetDefault()) 
	{
		if(_isFinalized) return true;

		if(!initialize(context)) return false;
		return finalize(queue);
	}

	void setPtr(T * ptr)
	{
		_ptr = ptr;
	}

	void setSize(size_t size)
	{
		_size = size;
	}

	private
	bool _isInitialized;
	bool _isFinalized;
	T[] _ptr;
	size_t _size;
	cl_mem _mem;
	cl_mem_flags _flags;
	void * _mappedPtr;

	bool fromHostToDevice(Queue queue = Queue.GetDefault())
	{
		alias CLType!T.Type DType; 

		if(_flags & CL_MEM_WRITE_ONLY) {
			return true;
		}

		cl_int err;
		DType *clBuffer = cast(DType *) clEnqueueMapBuffer(queue.implementation(),
			_mem,
			CL_TRUE,
			CL_MAP_WRITE,
			0,
			_size * sizeof(DType),
			0, NULL, NULL, &err);

		if(!CLError.Check(err)) {
			return false;
		}

		clEnqueueUnmapMemObject(queue.implementation(), _mem, clBuffer, 0, NULL, NULL); 
			clFinish(queue.implementation());  //just to make sure
			return true;
		}

		bool fromDeviceToHost(Queue queue = Queue.GetDefault())
		{
			alias CLType!T.Type DType; 

			if(_flags & CL_MEM_READ_ONLY) {
				return true;
			}

			cl_int err;
			DType *clBuffer = cast(DType *) clEnqueueMapBuffer(queue.implementation(),
				_mem,
				CL_TRUE,
				CL_MAP_READ,
				0,
				_size * sizeof(DType),
				0, NULL, NULL, &err );

			if(!CLError.Check(err)) {
				return false;
			}

			clEnqueueUnmapMemObject(queue.implementation(), _mem, clBuffer, 0, NULL, NULL); 
			clFinish(queue.implementation());  //just to make sure
			return true;
		}
	}

