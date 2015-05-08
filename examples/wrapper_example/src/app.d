module clop.examples.wrapper_example;

import std.stdio;
import clop.rt.clid.context;
import clop.rt.clid.clerror;
import clop.rt.clid.settings;
import derelict.opencl.cl;

int main(string[] args)
{
	Settings.Instance().setUseCPU();

	Context c1 = Context.GetDefault();
	Context c2 = Context.GetDefault();
	Context c3 = Context.GetDefault();

	return 0;
}