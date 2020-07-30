#include <assert.h>
   struct unconstrained_record_array
   {
      int low;
      int high;
      int data [1];
    };

   struct constrained_record_array
   {
      int low;
      int high;
      int data [10];
   };

void f (struct unconstrained_record_array r)
  {
     r.data[r.high - 1] = 10;
   };

void main ()
{
   struct constrained_record_array ra = {1, 10};

   struct constrained_record_array rar [7];

   int x [0][0][0][0];
   
//   f(ra);
   assert (ra.low == 1);
   assert (ra.high == 10);
   assert (ra.data[ra.high - 1] = 10);
   ra.low = 1;
   ra.high = 1;
   ra.data[1] = 3;
 
   rar [0].data[0] = 3;
}
  
