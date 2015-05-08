module clop.rt.clid.type;
import derelict.opencl.cl;

class CLType(T) {}

class CLType(T : bool) {
public
	alias Type = cl_bool;
}

class CLType(T : double) {
public
	alias Type = cl_double;
}

class CLType(T : float) {
public
	alias Type = cl_float;
}

class CLType(T : long) {
public
	alias Type = cl_long;
}

class CLType(T : int) {
public
	alias Type = cl_int;
}

