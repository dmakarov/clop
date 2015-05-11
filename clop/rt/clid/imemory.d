module clop.rt.clid.imemory;
import derelict.opencl.cl;
import clop.rt.clid.queue;
import clop.rt.clid.context;


abstract class IMemory {
	public {
		~this() {}



		abstract {
			size_t size();
			bool initialize( Context context = Context.GetDefault());
			bool updateDevice( Queue queue = Queue.GetDefault());
			bool updateHost( Queue queue = Queue.GetDefault());
			bool finalize( Queue queue = Queue.GetDefault());
			bool commit( Context context = Context.GetDefault(),  Queue queue = Queue.GetDefault());
			ref cl_mem implementation();
		}

		size_t sizeOfMemory()
		{
			return implementation().sizeof;
		}

		void * pointer()
		{
			return cast(void *) &implementation();
		}
	}
}

