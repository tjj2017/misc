#include <assert.h>
void unconstrained_param (int first, int last, int length, int a [], int *f, int *l, int *len)
{
  *f = first;
  *l = last;
  *len = length;
};


void main ()
{
int xf, xl, xlen;
int u_arr [2];
int xfirst;
int xlast;
int xlength;

xfirst = 3;
xlast = 4;
xlength = xlast - xfirst + 1;

unconstrained_param (xfirst, xlast, xlength, u_arr, &xf, &xl, &xlen);
assert (xlen == xlength);
}
  
