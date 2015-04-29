module clop.examples.wrapper_example;
//public import clop.rt.clid.context;
//public import clop.rt.clid.clerror;
import derelict.opencl.cl;


bool GetIDs(cl_platform_id[] platforms, cl_uint maxPlatforms = 1)
{
	DerelictCL.load();
	cl_uint numPlatforms;
	platforms.length = maxPlatforms;
	cl_int status = clGetPlatformIDs(maxPlatforms, platforms.ptr, &numPlatforms);
	platforms.length = numPlatforms;
	return status == CL_SUCCESS;
}

int main(string[] args)
{
	//Context c1 = Context.GetDefault();
	//Context c2 = Context.GetDefault();
	//Context c3 = Context.GetDefault();

	//CLError err = new CLError(CL_SUCCESS);

	return 0;
}