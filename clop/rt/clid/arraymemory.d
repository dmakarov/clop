module clop.rt.clid.arraymemory;
import std.container.array;
import std.stdio;

import derelict.opencl.cl;

import clop.rt.clid.imemory;
import clop.rt.clid.context;
import clop.rt.clid.queue;
import clop.rt.clid.type;
import clop.rt.clid.clerror;

class ArrayMemory(T) : IMemory {
	public {
		override {
			cl_mem implementation()
			{
				return _mem;
			}

			bool initialize(Context context = Context.GetDefault())
			{
				if(_isInitialized) return true;


				cl_int err;
				_mem = clCreateBuffer(context.implementation(), _flags, T.sizeof * _size, cast(void *) _ptr, &err);
				_isInitialized = CLError.Check(err);
				return _isInitialized;
			}

			bool updateDevice(Queue queue = Queue.GetDefault())
			{
				assert(_isInitialized);
				_isFinalized = fromHostToDevice(queue);
				return _isFinalized;
			}

			bool updateHost(Queue queue = Queue.GetDefault()) 
			{
				assert(_isInitialized);
				return fromDeviceToHost(queue);
			}

			bool finalize(Queue queue = Queue.GetDefault())
			{
				assert(_isInitialized);
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

			size_t size() { return _size; }
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

		T * ptr() { return _ptr.ptr; }
		

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

		
		void setPtr(T[] ptr)
		{
			_ptr = ptr;
		}

		void setSize(size_t size)
		{
			_size = size;
		}
	}

	private  {
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
				_size * DType.sizeof,
				0, null, null, &err);

			if(!CLError.Check(err)) {
				return false;
			}

			clEnqueueUnmapMemObject(queue.implementation(), _mem, clBuffer, 0, null, null); 
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
				_size * DType.sizeof,
				0, null, null, &err );

			if(!CLError.Check(err)) {
				return false;
			}

			clEnqueueUnmapMemObject(queue.implementation(), _mem, clBuffer, 0, null, null); 
			clFinish(queue.implementation());  //just to make sure
			return true;
		}
	}

}
