module clop.rt.clid.imemory;
import derelict.opencl.cl;
import clop.rt.clid.queue;
import clop.rt.clid.context;


abstract class IMemory {
	public {
		~this() {}
	
		cl_mem implementation()  
		{
			IMemory that = cast(IMemory)(this);
			return that.implementation();
		}

		abstract {
			size_t size();
			bool initialize( Context context = Context.GetDefault());
			bool updateDevice( Queue queue = Queue.GetDefault());
			bool updateHost( Queue queue = Queue.GetDefault());
			bool finalize( Queue queue = Queue.GetDefault());
			bool commit( Context context = Context.GetDefault(),  Queue queue = Queue.GetDefault());
		}

		size_t sizeOfMemory()  
		{
			return implementation().sizeof;
		}

		void * pointer()
		{
			return cast(void *) implementation();
		}
	}
}

