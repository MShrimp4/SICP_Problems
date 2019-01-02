#include <stddef.h>
#include "struct_def.h"
#include "types.h"
t_obj the_stack;

void save(t_obj o){
  the_stack = t_pair(o,the_stack);
}
void save_cont(int label){
  t_obj o = _t_label(label);
  save(o);
}
void restore(t_obj * o){
  *o = Car(the_stack);
  the_stack = Cdr(the_stack);
}
void restore_cont(int* label){
  t_obj o = Car(the_stack);
  *label = o.int_;
  the_stack = Cdr(the_stack);
}
void init_stack(){
   the_stack = t_nil();
}
