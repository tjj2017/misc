#include <assert.h>
struct Header {
    int i;
    unsigned count;
};
struct Buffer {
    struct Header head;
    char data [10];
};


extern int nondet ();
extern void nondet_head_plus(struct Buffer *b, int count);

int main () {
    struct My_Buffer {
        struct Header head;
        char a1 [5];
        char a2 [5];
    };
    struct My_Buffer b;
    int j;
  
    b.head.i = 5;
    b.head.count = 7;
    
    for (j = 0; j < 5; j++) {
        b.a1 [j] = 3;
        b.a2 [j] = 1;
    };
    

    assert (b.head.i == 5);
    assert (b.head.count == 7);
    
    for (j = 0; j < 5; j++) {
        assert (b.a1 [j] == 3);
        assert (b.a2 [j] == 1);
    };
    
    b.head.i = nondet();
    assert (b.head.i == 5);

    b.head.i = 5;

    nondet_head_plus((struct Buffer *)&b, 5);
    
    assert (b.head.i == 5);
    assert (b.head.count == 7);
    
    for (j = 0; j < 5; j++) {
        assert (b.a1 [j] == 3);
        assert (b.a2 [j] == 1);
    };

    return 0;
};

int nondet () {
    int nondet_var;
    return nondet_var;
};

void nondet_head_plus (struct Buffer *b, int count) {
    int i;
    b->head.i = nondet();
    b->head.count = nondet();
    for ( i = 0; i < count; i++) {
        b->data[i] = nondet();
    };
}

  
  
     
