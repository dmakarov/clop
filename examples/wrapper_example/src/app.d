module clop.examples.wrapper_example;
public import clop.rt.clid.context;
public import clop.rt.clid.clerror;

int main(string[] args)
{
	Context c1 = Context.GetDefault();
	Context c2 = Context.GetDefault();
	Context c3 = Context.GetDefault();

	CLError err = new CLError(CL_SUCCESS);

	return 0;
}