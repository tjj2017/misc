#include <assert.h>
void PC1 (int a [10], int *f)
{
  *f = a [5];
};

void PC5 (int a [], int *f)
{
  *f = a [5];
};

void PC7 (int aa[][5], int *f)
{
  *f = aa [1] [2];
}

void main ()
{
int i, x;
int arr [10 - 1 + 1];
int arr_arr [5] [5];

i = 5;
arr[i] = 23;
assert (arr[i] == x);
PC1 (arr, &x);
assert (x == 23);
PC5 (arr, &x);
assert (x == 23);

arr_arr [1][2] = 7;
PC7 (arr_arr, &x);
assert (x == 7);
}
  
