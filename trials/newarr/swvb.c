#include <assert.h>


void main ()
{
 int i = 2;
 int x = 5;
 const unsigned long c = i * x;
 int a[i * x];

typedef struct MyStruct {
    int b [c];
    int i;
 } XX;
XX y, z;
int idx;

for (idx = 0; idx < c; idx = idx + 1) {
  z.b [idx] = idx + 1;
}; 
z.i = z.b[1];
assert (z.i == 2);

y = z;
for (idx = 0; idx < c; idx = idx + 1) {
  assert (y.b [idx] == idx + 1);
}; 
assert (y.i = 2);
}
  
