module clop.examples.wrapper_example;

import std.stdio;
import clop.rt.clid.context;
import clop.rt.clid.clerror;
import clop.rt.clid.settings;
import derelict.opencl.cl;

int main(string[] args)
{
	//Settings.Instance().setUseGPU();
	Settings.Instance().setUseCPU();
	Context c = Context.GetDefault();


	return 0;
}