#include <assert.h>
void main ()
{
int i, x;
int arr[10 - 1 + 1];

i = 5;
arr[i] = 23;
x = arr[i];
assert (x == 19);
}
  
