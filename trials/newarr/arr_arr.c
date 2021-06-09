#include <assert.h>
void main ()
{
int i; char x;
char arr_arr[9] [10];


i = 5;
arr_arr[3] [i] = 'a';
x = arr_arr[3] [i];
assert (x == 'a');
}
 
