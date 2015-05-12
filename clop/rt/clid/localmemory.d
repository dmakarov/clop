module clop.rt.clid.localmemory;

import std.stdio;
import derelict.opencl.cl;

import clop.rt.clid.context;
import clop.rt.clid.queue;
import clop.rt.clid.imemory;



class LocalMemory(T) : IMemory {
	public {
		size_t size() const  { return _size; }

		ref cl_mem implementation()
		{
			static cl_mem * ptr = NULL;
			return *ptr;
		}

		bool initialize(Context context = Context.GetDefault())
		{
			return true;
		}

		bool updateDevice(Queue queue = Queue.GetDefault())
		{
			return true;
		}

		bool updateHost(Queue queue = Queue.GetDefault())
		{
			return true;
		}

		bool finalize(Queue queue = Queue.GetDefault())
		{
			return true;
		}

		bool commit(Context context = Context.GetDefault(), Queue queue = Queue.GetDefault())
		{
			return true;
		}

		this(const size_t size)	
		{
			_size = size;
		}
		
		void clear() {}
		
		~this()
		{
			clear();
		}
		
		size_t sizeOfMemory() const
		{
			return _size*T.sizeof;
		}
		
		void * pointer()
		{
			return cast(void *) null;
		}
	}

	private {
		size_t _size;
		cl_mem _mem;
	}
};


LocalMemory!T MakeLocal(T)(const size_t nElements)
{
	return LocalMemory!T(nElements);
}

