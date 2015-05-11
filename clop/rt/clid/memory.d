module clop.rt.clid.memory;

import std.container.array;
import std.stdio;

import derelict.opencl.cl;
import clop.rt.clid.imemory;
import clop.rt.clid.arraymemory;
import clop.rt.clid.context;
import clop.rt.clid.queue;

class Memory(T) : IMemory {
	public {
		override {
			size_t size()  { return 1; }
			ref cl_mem implementation() { return *(cast(cl_mem *) &_hostValue); }


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


		this(T hostValue)
		{
			_hostValue = hostValue;
		}

	}

	private {
		T _hostValue;
	}
}


class Memory(T : T[]) : ArrayMemory!T {
	public {
		this()
		{}

		this(cl_ulong size)
		{
			_hostData = new T[size];
		}
		this(T[] data)
		{
			_hostData = data;
		}


		T[] hostData()
		{
			return _hostData;
		}

		override {
			bool initialize(Context context = Context.GetDefault())
			{
				setPtr(_hostData);
				setSize(_hostData.length);
				return super.initialize(context);
			}

			bool finalize(Queue queue = Queue.GetDefault())
			{
				return super.finalize(queue);
			}
		}
	}

	private {
		T[] _hostData;
	}
}
