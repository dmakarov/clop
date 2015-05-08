module clop.rt.clid.memory;

import std.container.array;
import std.stdio;

import derelict.opencl.cl;
import clop.rt.clid.imemory;

class Memory(T) : IMemory {
	public {
		size_t size()  { return 1; }
		//cl_mem implementation() { return *((cl_mem *) &_hostValue); }

		this(T hostValue)
		{
			_hostValue = hostValue;
		}

		bool initialize(Context context = Context.GetDefault())
		{
			return true;
		}

		bool updateHost(Queue queue = Queue.GetDefault()) 
		{
			return true;
		}

		bool updateDevice(Queue queue = Queue.GetDefault()) 
		{
			return true;
		}


		bool finalize(Queue queue = Queue.GetDefault()) 
		{
			return true;
		}

		bool commit(Context context = Context.GetDefault(), Queue queue = Queue.GetDefault())
		{
			return finalize(queue);
		}
	}

	private {
		T _hostValue;
	}
}


class Memory(A : Array!T, T) : ArrayMemory!T {
	public {
		this()
		{}

		this(cl_ulong size)
		{
			_hostData = new Array!T(size);
		}

		Array!T hostData()
		{
			return _hostData;
		}

		bool initialize(Context context = Context.GetDefault())
		{
			setPtr(&_hostData[0]);
			setSize(_hostData.size());
			return super.initialize(context);
		}

		bool finalize(Queue queue = Queue.GetDefault()) 
		{
			return super.finalize(queue);
		}
	}

	private {
		std.vector!T _hostData;
	}
}


class Memory(A : Array!T, T) : ArrayMemory!T {
	public {
		this(Array!T hostData)
		{
			_hostData = hostData;
		}

		Array!T hostData()
		{
			return _hostData;
		}

		bool initialize(Context context = Context.GetDefault())
		{
			setPtr(&_hostData[0]);
			setSize(_hostData.size());
			return super.initialize(context);
		}

		bool finalize(Queue queue = Queue.GetDefault()) 
		{
			return super.finalize(queue);
		}
	}

	private {
		Array!T _hostData;
	}
}


