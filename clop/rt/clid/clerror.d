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
				case CL_SUCCESS:  
				{
					_message = "CL_SUCCESS";
					return true;				  
				} 
				case CL_INVALID_PLATFORM:
				{
					writeln("CL_INVALID_PLATFORM");
					break;
				}
				case CL_INVALID_DEVICE_TYPE:
				{
					writeln("CL_INVALID_DEVICE_TYPE");
					break;
				}
				case CL_INVALID_VALUE:
				{
					writeln("CL_INVALID_VALUE");
					break;
				}
				case CL_DEVICE_NOT_FOUND:
				{
					writeln("CL_DEVICE_NOT_FOUND");
					break;
				}
				case CL_INVALID_COMMAND_QUEUE:
				{
					writeln("CL_INVALID_COMMAND_QUEUE");
					break;
				}
				case CL_INVALID_OPERATION:
				{
					writeln("CL_INVALID_OPERATION");
					break;
				}
				case CL_COMPILER_NOT_AVAILABLE:
				{
					writeln("CL_COMPILER_NOT_AVAILABLE");
					break;
				}
				case CL_OUT_OF_HOST_MEMORY:
				{
					writeln("CL_OUT_OF_HOST_MEMORY");
					break;
				}
				case CL_BUILD_PROGRAM_FAILURE:
				{
					writeln("CL_BUILD_PROGRAM_FAILURE");
					break;
				}
				case CL_INVALID_DEVICE:
				{
					writeln("CL_INVALID_DEVICE");
					break;
				}
				case CL_INVALID_HOST_PTR:
				{
					writeln("CL_INVALID_HOST_PTR");
					break;
				}

				//clSetKernelArg
				case CL_INVALID_KERNEL:
				{
					writeln("CL_INVALID_KERNEL");
					break;
				}
				case CL_INVALID_ARG_INDEX:
				{
					writeln("CL_INVALID_ARG_INDEX");
					break;
				}
				case CL_INVALID_ARG_VALUE:
				{
					writeln("CL_INVALID_ARG_VALUE");
					break;
				}
				case CL_INVALID_MEM_OBJECT:
				{
					writeln("CL_INVALID_MEM_OBJECT");
					break;
				}
				case CL_INVALID_ARG_SIZE:
				{
					writeln("CL_INVALID_ARG_SIZE");
					break;
				}
				case CL_INVALID_SAMPLER:
				{
					writeln("CL_INVALID_SAMPLER");
					break;
				}

				case CL_MAP_FAILURE:
				{
					writeln("CL_MAP_FAILURE");
					break;
				}
				case CL_INVALID_GLOBAL_WORK_SIZE:
				{
					writeln("CL_INVALID_GLOBAL_WORK_SIZE");
					break;
				}

				case CL_INVALID_GL_OBJECT:
				{
					writeln("CL_INVALID_GL_OBJECT");
					break;
				}
				case CL_INVALID_CONTEXT:
				{
					writeln("CL_INVALID_CONTEXT");
					break;
				}
				case CL_INVALID_QUEUE_PROPERTIES:
				{
					writeln("CL_INVALID_QUEUE_PROPERTIES");
					break;
				}
				case CL_INVALID_KERNEL_NAME:
				{
					writeln("CL_INVALID_KERNEL_NAME");
					break;
				}
				case CL_INVALID_KERNEL_DEFINITION:
				{
					writeln("CL_INVALID_KERNEL_DEFINITION");
					break;
				}
				case CL_INVALID_EVENT_WAIT_LIST:
				{
					writeln("CL_INVALID_EVENT_WAIT_LIST");
					break;
				}
				case CL_INVALID_WORK_DIMENSION:
				{
					writeln("CL_INVALID_WORK_DIMENSION");
					break;
				}
				case CL_INVALID_WORK_GROUP_SIZE:
				{
					writeln("CL_INVALID_WORK_GROUP_SIZE");
					break;
				}
				case CL_INVALID_WORK_ITEM_SIZE:
				{
					writeln("CL_INVALID_WORK_ITEM_SIZE");
					break;
				}
				case CL_MEM_OBJECT_ALLOCATION_FAILURE:
				{
					writeln("CL_MEM_OBJECT_ALLOCATION_FAILURE");
					break;
				}
				case CL_INVALID_KERNEL_ARGS:
				{
					writeln("CL_INVALID_KERNEL_ARGS");
					break;
				}
				default: 
				{
					writeln("error ", code);
					return false;
				}
			}

			return false;
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

