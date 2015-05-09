module clop.examples.wrapper_example;

import std.stdio;
import std.container.array;
import std.algorithm.mutation;

import derelict.opencl.cl;


import clop.rt.clid.context;
import clop.rt.clid.clerror;
import clop.rt.clid.settings;
import clop.rt.clid.imemory;
import clop.rt.clid.memory;
import clop.rt.clid.program;
import clop.rt.clid.kernel;
import clop.rt.clid.arglist;
import clop.rt.clid.makememory;


alias Memory!(Array!(double)) VecMemRefDouble;


class Matrix(T) {
	public {
		this(int rows, int cols)
		{
			this.rows = rows;
			this.cols = cols;
			this.data = new T[rows*cols];
		}

		int size() { return rows*cols; }
		int rows, cols;
		T[] data;
	}
}

void RunMatrixExample()
{
	string path = "/Users/patrick/Desktop/Code/moonolith/clipp/data/matrix_program.cl";
	Program program = new Program();
	bool ok = program.load(path);
	assert(ok);
	if(!ok) return;

	Matrix!double mat = new Matrix!double(2, 2);


	Kernel pfill = program.createKernel("Fill");
	pfill.setGlobalWorkSize(mat.size());

	Kernel scale = program.createKernel("Scale");
	scale.setGlobalWorkSize(mat.size());

	Kernel subtract = program.createKernel("Subtract");
	subtract.setGlobalWorkSize(mat.size());

	ArgList args = new ArgList();
	args.arg(0, MakeMemory(mat.data));
	args.arg(1, MakeNumber!int(mat.size()));
	args.arg(2, MakeNumber!double(2));

	pfill.call(args);
	scale.call(args);	

	args.arg(2, MakeNumber!double(-4));
	subtract.call(args);	

	args.updateHost();
	writeln(mat.data);
}



int main(string[] args)
{
	//Settings.Instance().setUseGPU();
	Settings.Instance().setUseCPU();
	Context c = Context.GetDefault();
	RunMatrixExample(); 
	return 0;
}
