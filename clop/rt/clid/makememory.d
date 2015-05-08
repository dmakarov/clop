module clop.rt.clid.makememory;


import derelict.opencl.cl;
import clop.rt.clid.imemory;

struct MemoryMaker(Arg) 
{
	Memory make(Arg arg) 
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

IMemory MakeMemory(T)(T arg)
{
	MemoryMaker!T mm;
	return mm.make(arg);
}

IMemory MakeNumber(T)(T arg)
{
	return new Memory!T(arg);
}
