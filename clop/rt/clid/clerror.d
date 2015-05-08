module clop.rt.clid.clerror;
public import derelict.opencl.cl;
import std.string;
import std.stdio;

class CLError {
	public {
		this(const cl_int code = CL_SUCCESS)
		{
			check(code);
		}
		
		bool success() const
		{
			return _code == CL_SUCCESS;
		}


		bool check(const cl_int code)
		{
			_code = code;

			switch(code) {
				case CL_SUCCESS:  {
					_message = "";
					return true;				  
				} default: {
					writeln("error %d", code);
					return false;
				}
			}
		}

		static bool Check(const cl_int code)
		{
			CLError err = new CLError(code);
			return err.success();
		}
	}

	private {
		cl_int _code;
		string _message;
	}
}

