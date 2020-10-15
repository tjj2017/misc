#include <assert.h>
void P (int length, int val, int *ret)
{
  int a[length];
  a[0] = val;
  *ret = a[0];
};


void main ()
{
int i, x, r;
i = 9;
x = 23;
P(i, x, &r);
assert (r == x);
}
  
