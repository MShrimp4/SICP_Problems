#include <stdio.h>
#include <string.h>
#include <stdlib.h> //remove
#include <stddef.h>
#include "struct_def.h"
#include "heap.h"
#include "types.h" //those functions use each other...

t_obj _make_typed_obj(int t, void* mallocd_data){
    t_obj o;
    o.t = t;
    o.data = mallocd_data;
    return o;
}

t_obj t_dupe(t_obj o){
  t_obj dupe_pair;
  switch(o.t){
  case Symbol:
    return t_symbol((char*)o.data);
  case Vector:
    return o; //TODO
  case Pair:
    return t_pair(t_dupe(Car(o)), t_dupe(Cdr(o)));
  case Quoted:
  case Environment:
    dupe_pair = t_pair(t_dupe(Car(o)), t_dupe(Cdr(o)));
    dupe_pair.t = o.t;
    return dupe_pair;
  default:
    return o; //Unimplemented
  }
}

t_obj t_shallow_dupe(t_obj o){
  switch(o.t){
  case Symbol:
    return t_symbol((char*)o.data);
  case Vector:
    return o; //TODO
  default:
    return o; //Unimplemented
  }
}

t_obj t_float(float f){
  t_obj o;
  o.t = Float;
  o.float_ = f;
  return o;
}
t_obj t_int(int i){
  t_obj o;
  o.t = Int;
  o.int_ = i;
  return o;
}

t_obj t_symbol(char * s){
    size_t size = sizeof(char) * (strlen(s) + 1);
    char* c_p = (char *)h_alloc(size);
    memcpy(c_p, s, size); //strdup?
    return _make_typed_obj(Symbol,c_p);
}

t_obj t_vector_vacant(size_t len){
    t_vec * vector = h_alloc(sizeof(t_vec) + sizeof(t_obj) * len);
    vector->len = len;
    return _make_typed_obj(Vector, (void *)vector);
}

t_obj t_pair(t_obj car, t_obj cdr){
    _t_pair* p = make_pair();
    p->car = car;
    p->cdr = cdr;
    return _make_typed_obj(Pair, (void *)p);
}

t_obj Car(t_obj p){
  return ((_t_pair*)(p.data))->car;
}

void set_car(t_obj p, t_obj val){
  ((_t_pair*)(p.data))->car = val;
}

t_obj Cdr(t_obj p){
  return ((_t_pair*)(p.data))->cdr;
}

void set_cdr(t_obj p, t_obj val){
  ((_t_pair*)(p.data))->cdr = val;
}

t_obj _t_label(int l){
  t_obj o;
  o.t = _label;
  o.int_ = l;
  return o;
}

t_obj t_quote(t_obj o){
  t_obj quoted = t_pair(t_symbol(QUOTE_SYMBOL) , o);
  quoted.t = Quoted;
  return quoted;
}

t_obj text_of_quotation(t_obj exp){
  return Cdr(exp);
}

t_obj t_nil(){
    return _make_typed_obj(Nil, NULL);
}

t_obj t_bool(bool b){
  t_obj o;
  o.t = Bool;
  o.bool_ = b;
  return o;
}

bool isFalse(t_obj val){
  return val.t == Bool && val.bool_ == false;
}

bool isTrue(t_obj val){
  return !isFalse(val);
}

t_obj t_env(t_obj name, t_obj frame, t_obj base_env){
  t_obj frames = t_pair(frame,base_env);
  t_obj env = t_pair(name, frames);
  env.t = Environment;
  return env;
}

t_obj environment_name(t_obj env){
  return Car(env);
}

bool isPrimitive_Procedure(t_obj proc){
  return proc.t == Primitive_Procedure;
}
bool isCompound_Procedure(t_obj proc){
  return proc.t == Procedure;
}
bool isProcedure(t_obj proc){
  return proc.t == Procedure || proc.t == Primitive_Procedure;
}

t_obj procedure_parameters(t_obj proc){
  return Car(Cdr(proc));
}
  
t_obj procedure_environment(t_obj proc){
  return Cdr(Cdr(Cdr(proc)));
}
t_obj make_frame(t_obj var, t_obj val){
  t_obj new_val = t_dupe(val);
  return t_pair(var,new_val);
}

void add_binding_to_frame(t_obj var, t_obj val, t_obj frame){
  set_car(frame, t_pair(var, Car(frame)));
  set_cdr(frame, t_pair(val, Cdr(frame)));
}

t_obj extend_environment(t_obj proc, t_obj var, t_obj val, t_obj env){// values must be duped
  t_obj frame = make_frame(var, val);
  return t_env(Car(proc), frame, env);
}

t_obj procedure_body(t_obj proc) {
  return Car(Cdr(Cdr(proc)));
}


t_obj make_to_list(t_obj* objs, int count){
  t_obj list ;
  if(count > 2 && objs[count-2].t == Symbol && strcmp(objs[count - 2].data,".") == 0){//Dotted trail
    list = t_pair(objs[count-3],objs[count-1]);
    for(int i = count - 4;i>=0;i--){
      list = t_pair(objs[i], list);
    }
  }
  else{
    list = t_nil();
    for(int i = count - 1;i>=0;i--){
      list = t_pair(objs[i], list);
    }
  }
  return list;
}

t_obj unsafe_reverse_list(t_obj list){
  t_obj new_list = t_nil();
  t_obj next;
  while(list.t == Pair){
    next = Cdr(list);
    set_cdr(list, new_list);
    new_list = list;
    list = next;
  }
  if(list.t != Nil) new_list = t_pair(list, new_list);
  return new_list;
}

t_obj t_prim_pointer(void * p){
  t_obj o;
  o.t = Primitive_Pointer;
  o.data = p;
  return o;
}

bool isAnyPair(t_obj o){
  switch(o.t){
  case Procedure:
  case Primitive_Procedure:
  case Macro:
  case Pair:
  case Quoted:
  case Environment:
    return true;
  default:
    return false;
  }
}

t_obj make_primitive_procedure(t_obj (*func)(t_obj)){
  t_obj prim_proc = t_pair(t_symbol(UNNAMED_PROC), t_prim_pointer(func));
  prim_proc.t = Primitive_Procedure;
  return prim_proc;
}

t_obj make_procedure(t_obj parameters, t_obj body, t_obj env){
  t_obj body_env = t_pair(body,env);
  t_obj par_body_env = t_pair(parameters,body_env);
  t_obj name = t_symbol(UNNAMED_PROC);
  t_obj procedure = t_pair(name,par_body_env);
  procedure.t = Procedure;
  return procedure;
}

void set_procedure_name(t_obj proc, t_obj name){
  set_car(proc, name);
}

t_obj procedure_name(t_obj proc){
  return Car(proc);
}

void user_print(t_obj val){
  switch(val.t){
  case Int:
    printf("%d",val.int_);
    break;
  case Float:
    printf("%f",val.float_);
    break;
  case _label:
    printf("Label");
    break;
  case Symbol:
    printf("%s",(char*)val.data);
    break;
  case Bool:
    if(isFalse(val)) printf("false");
    else printf("true");
    break;
  case Vector:
    printf("Vector()");
    break;
  case Quoted:
    putchar('\'');
    user_print(Cdr(val));
    break;
  case Pair:
    putchar('(');
    user_print(Car(val));
    t_obj rest = Cdr(val);
    while(rest.t == Pair){
      putchar(' ');
      user_print(Car(rest));
      rest = Cdr(rest);
    }
    if(rest.t != Nil){
      printf(" . ");
      user_print(rest);
    }
    putchar(')');
    break;
  case Procedure:
    printf("<procedure ");
    user_print(procedure_name(val));
    printf(">");
    break;
  case Primitive_Procedure:
    printf("<*procedure ");
    user_print(procedure_name(val));
    printf(">");
    break;
  case Primitive_Pointer:
    printf("*primitive-pointer*");
    break;
  case Macro:
    printf("*macro*");
    break;
  case Environment:
    printf("<environment ");
    user_print(environment_name(val));
    printf(">");
    break;
  case Nil:
    printf("Nil");
    break;
  default:
    printf("*Unknown_Object* : enum type %d",val.t);
  }
}
