
import std.container.array;

import derelict.opencl.cl;
import clop.rt.clid.kernel;
import clop.rt.clid.context;
import clop.rt.clid.clerror;



class Program {
	public

	bool compile(string program, string flags = "")
	{
		if(program is null || program.length == 0)
			return false;

		cl_int err;

		cl_ulong[] empty;
		_program = clCreateProgramWithSource(_context.implementation(), cast(uint)1, &cast(const char *)program.ptr, cast(const cl_ulong *)empty.ptr, &err);
		CLError ret = new CLError(err);
		if(!ret.success()) {
			return false;
		}

		string args = flags ~ " -Werror";
		size_t nDevices = _context.devices().size();
		for(size_t i = 0; i < nDevices; ++i) {

			cl_device_id dev = _context.device(i).getId();
			Error ret = clBuildProgram(_program, 1, &dev, args.ptr, null, null );
			if(!ret.success()) {
				size_t diagnosticSize;


				clGetProgramBuildInfo(_program, dev, CL_PROGRAM_BUILD_LOG, 0, null, &diagnosticSize);

				string diagnostic;
				diagnostic.resize(diagnosticSize);


				clGetProgramBuildInfo(_program, dev, CL_PROGRAM_BUILD_LOG, diagnosticSize, diagnostic, null);

				writeln(diagnostic);
				return false;
			}
		}

		return true;
	}

	~this()
	{
		clReleaseProgram(_program);
	}

	this(Context context = Context.GetDefault())
	{
		_context =  context;
	}

	Kernel createKernel(string name)
	{
		cl_int err;
		Kernel kernel = new Kernel( clCreateKernel(_program, name.c_str(), &err) );
		if(!CLError.Check(err)) {
			writeln("[Error] unable to create kernel\n");
			return null;
		}

		kernel.setContext(_context);
		return kernel;
	}

	bool load(string path, string flags = "") {
		string program;
		if(!readText(path)) {
			return false;
		}

		return compile(path, flags);
	}

	private
	cl_program _program;
	Context _context;
}


