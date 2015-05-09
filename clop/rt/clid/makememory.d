module clop.rt.clid.makememory;


import derelict.opencl.cl;
import clop.rt.clid.imemory;
import clop.rt.clid.memory;

struct MemoryMaker(Arg) 
{
	Memory!Arg make(Arg arg) 
	{
		return new Memory!Arg(arg);	
	}
}

struct MemoryMaker(T : Memory!Arg, Arg)
{
	Memory!Arg make(T arg) 
	{
		return arg;
	}
}

struct MemoryMaker(T : IMemory)
{
	IMemory make(T arg) 
	{
		return arg;
	}
}

auto MakeMemory(T)(T arg)
{
	MemoryMaker!T mm;
	return mm.make(arg);
}

Memory!T MakeNumber(T)(T arg)
{
	return new Memory!T(arg);
}
