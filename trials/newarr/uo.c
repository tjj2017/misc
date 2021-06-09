#include <assert.h>
void main ()
{
   int constrained [4];
   constrained [0] = 1;
   constrained [1] = 2;
   constrained [0] = 3;
   constrained [1] = 4;

   int unconstrained [3];
   int unconstrained_first = 1;
   int unconstrained_last  = 3; 

   unconstrained [0] = constrained [0];
   unconstrained [1] = constrained [1];
   unconstrained [0] = constrained [2];
   
   {
      int i;
      for (i = unconstrained_first; i <= unconstrained_last; i = i + 1)
      {
        assert (unconstrained [i - unconstrained_first] = i);
      };
    };
};
  
