
__kernel void Scale(__global double *values, int n, const double scaleFactor)
{
	for(int i = get_global_id(0); i < n; i+= get_global_size(0)) {
		values[i] *= scaleFactor;
		// printf("%g\n", values[i]);
	}
}

__kernel void Subtract(__global double *values, int n, const double scaleFactor)
{
	for(int i = get_global_id(0); i < n; i+= get_global_size(0)) {
		values[i] -= scaleFactor;
		// printf("%g\n", values[i]);
	}
}


__kernel void Fill(__global double *values, int n, const double value)
{

	for(int i = get_global_id(0); i < n; i+= get_global_size(0)) {
		values[i] = value;
		// printf("%g\n", values[i]);

	}
}


__kernel void MMMultiply(__global double *left, __global double *right, __global double *result, int rows, int cols)
{
	for(int i = get_global_id(0); i < rows; i+= get_global_size(0)) {
		for(int j = get_global_id(1); j < cols; j+= get_global_size(1)) {
			int offsetI = i*cols;
			double tmp = 0;
			for (int k=0; k < cols; k++) { 
				tmp += left[offsetI+k] * right[k*cols+j]; 
			}
			printf("%g\n", result[i]);
			result[i*cols+j] = tmp;
		}
	}
}