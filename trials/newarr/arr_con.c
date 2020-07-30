#include <assert.h>
void constrained (int a [10], int *f)
{
  *f = a [5];
};

void main ()
{
int i, x;
int arr [10 - 1 + 1];

i = 5;
arr[i] = 23;
constrained (arr, &x);
assert (x == 23);
}
  
