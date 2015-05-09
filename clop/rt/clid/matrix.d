module clop.rt.clid.matrix;

import std.stdio;
import std.container.array;
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


class Matrix(T) {
	public {
		this(int rows, int cols)
		{
			if(!programLoaded) {
				writeln("[Warning] loading default Matrix program");
				bool ok = LoadMatrixProgram();
				assert(ok); 
				if(!ok) {
					writeln("[Error] unable to load program");
				}
			}

			this.rows = rows;
			this.cols = cols;
			this.data = new T[rows*cols];
			this.mem = MakeMemory(data);
		}



		static bool LoadMatrixProgram(string path = "/Users/patrick/Desktop/Code/moonolith/clipp/data/matrix_program.cl")
		{
			Program program = new Program();
			bool ok = program.load(path);
			assert(ok);
			if(!ok) { 
				writeln("[Error] error in loading program");
				return false;
			}

			kfill = program.createKernel("Fill");
			kscale = program.createKernel("Scale");
			ksubtract = program.createKernel("Subtract");
			kmmmultiply = program.createKernel("MMMultiply");
			programLoaded = true;
			return true;
		}

		int size() { return rows*cols; }

		void fill(T value)
		{
			kfill.setGlobalWorkSize(size());
			kfill.call(simpleArgList(value));
		}

		void scale(T value)
		{
			kscale.setGlobalWorkSize(size());
			kscale.call(simpleArgList(value));
		}

		void subtract(T value)
		{
			ksubtract.setGlobalWorkSize(size());
			ksubtract.call(simpleArgList(value));
		}

		void describe()
		{
			mem.updateHost();
			writeln(data);
		}

		void updateDevice()
		{
			mem.updateDevice();
		}

		void updateHost()
		{
			mem.updateHost();
		}

		Matrix opMul(Matrix other)
		{
			Matrix result = new Matrix(rows, other.cols);
			result.fill(0);

			ArgList args = new ArgList();
			args.arg(0, mem);
			args.arg(1, other.mem);
			args.arg(2, result.mem);
			args.arg(3, MakeNumber!int(rows));
			args.arg(4, MakeNumber!int(other.cols));
			
			kmmmultiply.call(args);
			return result;
		}
	}

	private {
		int rows, cols;
		T[] data;
		Memory!(T[]) mem;

		ArgList simpleArgList(T value)
		{
			ArgList args = new ArgList();
			args.arg(0, mem);
			args.arg(1, MakeNumber!int(size()));
			args.arg(2, MakeNumber!T(value));
			return args;
		}

		static bool programLoaded = false;

			//simple args fun
			static Kernel ksubtract;
			static Kernel kscale;
			static Kernel kfill;

			//permutations
			static Kernel kmmmultiply;
		}
	}