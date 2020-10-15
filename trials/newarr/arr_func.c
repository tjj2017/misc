#include <assert.h>
int * f1 (int a [10])
{
  int i;
  for (i = 0; i < 10; i = i + 1){
    a [i] = i;
   };
  for (i = 0; i < 10; i = i + 1){
  assert (a [i] == i);
};
  return a;
};

void main ()
{
int i;
int arr [10];
int *arr2;
arr2 = f1 (arr);
for (i = 0; i < 10; i = i + 1){
  assert (arr [i] == i);
};
for (i = 0; i < 10; i = i + 1){
  assert (arr2 [i] == i);
};
}
  
