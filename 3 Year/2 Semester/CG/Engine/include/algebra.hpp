#include <cmath>

void multMatrixVector(double m[4][4], double *v, double *res);


void buildRotMatrix(double *x, double *y, double *z, double *m);


void cross(double *a, double *b, double *res);


void normalize(double *a);


void multiply(double * mat1,
              double * mat2,
              double * res);