module clop.rt.clid.arglist;

import std.container.array;
import std.stdio;

import clop.rt.clid.imemory;
import clop.rt.clid.context;
import clop.rt.clid.queue;
import clop.rt.clid.kernel;
import clop.rt.clid.clerror;
import clop.rt.clid.platform;




class ArgList {
	public
	void setNArgs(size_t n)
	{
		_args.length(n);
	}

	void pushBack(IMemory arg)
	{
		_args.insertBack(arg);
	}

	void arg(size_t argNum, IMemory arg)
	{
		uint nArgs =  cast(uint)_args.length();
		if(argNum >= nArgs) {
			_args.length(argNum+1);
		}
		_args[argNum] = arg;
	}

	void clear()
	{
		_args.clear();
	}

	size_t nArgs() 	{
		return _args.length();
	}

	IMemory arg(size_t index) {
		return _args[index];
	}

	bool setTo(Kernel kernel)
	{
		bool ok = true;
		for(uint argNum = 0; argNum < nArgs(); ++argNum) {
			ok = setTo(kernel, argNum);
		}

		return ok;
	}

	bool finalize(Queue queue = Queue.GetDefault())
	{
		bool ok = true;
		for(size_t argNum = 0; argNum < nArgs(); ++argNum) {
			ok = _args[argNum].finalize(queue);
		}

		return ok;
	}

	bool commit(Context context = Context.GetDefault(), Queue queue = Queue.GetDefault())
	{
		bool ok = true;
		for(size_t argNum = 0; argNum < nArgs(); ++argNum) {
			ok = _args[argNum].commit(context, queue);
		}

		return ok;
	}

	bool updateHost(Queue queue = Queue.GetDefault())
	{
		bool ok = true;
		for(size_t argNum = 0; argNum < nArgs(); ++argNum) {
			ok = _args[argNum].updateHost(queue);
		}

		return ok;
	}

	bool setTo(Kernel kernel, uint argNum)
	{
		assert(!(kernel.implementation() is null));



		IMemory mem = _args[argNum];
		
		assert(!(mem.pointer() is null));

		CLError err = new CLError(clSetKernelArg(kernel.implementation(), argNum, mem.sizeOfMemory(), mem.pointer()));
		if(!err.success()) {
			writeln("[Error] CLKernel setting arg %d", argNum);
		}

		return err.success();
	}

private:
	Array!IMemory _args;



}

