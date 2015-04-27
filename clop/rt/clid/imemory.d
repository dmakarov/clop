module clop.rt.clid.imemory;
import derelict.opencl.cl;
import clop.rt.clid.queue;
import clop.rt.clid.context;


class IMemory {
public
	~this() {}
	size_t size() const;
	cl_mem implementation();

	cl_mem implementation() const 
	{
		IMemory that = const_cast!IMemory(this);
		return that.implementation();
	}

	bool initialize(const Context context = Context.GetDefault());
	bool updateDevice(const Queue queue = Queue.GetDefault());
	bool updateHost(const Queue queue = Queue.GetDefault());
	bool finalize(const Queue queue = Queue.GetDefault());
	bool commit(const Context context = Context.GetDefault(), const Queue queue = Queue.GetDefault());

	size_t sizeOfMemory() const 
	{
		return sizeof(implementation());
	}

	void * pointer()
	{
		return cast(void *) implementation();
	}

}

