#include <stdio.h>
#include <stdbool.h>
#include "struct_def.h"
#include "types.h"
#include "util.h"
#include "heap.h"

t_obj f_add (t_obj list){
  int i = 0;
  float f;
  bool use_float = false;
  for(t_obj j = list;j.t == Pair;j = Cdr(j)){
    t_obj num = Car(j);
    if(num.t == Int){
      if(use_float) f += (float)num.int_;
      else i += num.int_;
    }
    else if(num.t == Float){
      if(use_float) f += num.float_;
      else{
	use_float = true;
	f = (float) i;
	f += num.float_;
      }
    }
    else{
      return constant("Arithmetic Error");
    }
  }
  return use_float ? t_float(f) : t_int(i);
}
t_obj f_sub (t_obj list){
  int i;
  float f;
  bool use_float = false;
  if(list.t != Pair){
    if(list.t == Int){
      i = - list.int_;
      return t_int(i);
    }
    else if(list.t == Float){
      f = - list.float_;
      return t_float(f);
    }
    else return constant("Arithmetic Error");
  }
  else{
    t_obj num = Car(list);
    list = Cdr(list);
    if(num.t == Int) i = num.int_;
    else if(num.t == Float){
      f = num.float_;
      use_float = true;
    }
    else return constant("Arithmetic Error");
  }
  
  for(t_obj j = list;j.t == Pair;j = Cdr(j)){
    t_obj num = Car(j);
    if(num.t == Int){
      if(use_float) f -= (float)num.int_;
      else i -= num.int_;
    }
    else if(num.t == Float){
      if(use_float) f -= num.float_;
      else{
	use_float = true;
	f = (float) i;
	f -= num.float_;
      }
    }
    else{
      return constant("Arithmetic Error");
    }
  }
  return use_float ? t_float(f) : t_int(i);
}
t_obj f_mul (t_obj list){
  int i = 1;
  float f;
  bool use_float = false;
  for(t_obj j = list;j.t == Pair;j = Cdr(j)){
    t_obj num = Car(j);
    if(num.t == Int){
      if(use_float) f *= (float)num.int_;
      else i *= num.int_;
    }
    else if(num.t == Float){
      if(use_float) f *= num.float_;
      else{
	use_float = true;
	f = (float) i;
	f *= num.float_;
      }
    }
    else{
      return constant("Arithmetic Error");
    }
  }
  return use_float ? t_float(f) : t_int(i);
}
t_obj f_div (t_obj list){
  float f;
  if(list.t == Nil) return constant("Arithmetic Error");
  t_obj dividend = Car(list);
  f = dividend.float_;
  for(t_obj j = Cdr(list);j.t == Pair;j = Cdr(j)){
    t_obj num = Car(j);
    if(num.t == Int){
      f /= (float)num.int_;
    }
    else if(num.t == Float){
      f /= num.float_;
    }
    else{
      return constant("Arithmetic Error");
    }
  }
  return t_float(f);
}
t_obj f_equal(t_obj list){
  if(list.t != Pair) return t_bool(false);
  t_obj o1 = Car(list);
  list = Cdr(list);
  if(list.t != Pair) return t_bool(false);
  t_obj o2 = Car(list);
  if(o1.t == o2.t){
    if(o1.t == Int) return t_bool(o1.int_ == o2.int_);
    else if(o1.t == Float) return t_bool(o1.float_ == o2.float_);
    else return constant("= Comparison Error");
  }
  return t_bool(false);
}

t_obj f_car(t_obj list){
  if(list.t != Pair) return t_nil();
  t_obj pair = Car(list);
  return Car(pair);
}

t_obj f_cdr(t_obj list){
  if(list.t != Pair) return t_nil();
  t_obj pair = Car(list);
  return Cdr(pair);
}
t_obj f_cons(t_obj list){
  if(list.t != Pair) return t_nil();
  t_obj car = Car(list);
  list = Cdr(list);
  if(list.t != Pair) return t_nil();
  t_obj cdr = Car(list);
  return t_pair(car,cdr);
}
t_obj f_u_set_car(t_obj list){
  if(list.t != Pair) return t_nil();
  t_obj p = Car(list);
  list = Cdr(list);
  if(list.t != Pair) return t_nil();
  t_obj obj = Car(list);
  set_car(p, obj);
  return p;
}
t_obj f_u_set_cdr(t_obj list){
  if(list.t != Pair) return t_nil();
  t_obj p = Car(list);
  list = Cdr(list);
  if(list.t != Pair) return t_nil();
  t_obj obj = Car(list);
  set_cdr(p, obj);
  return p;
}

t_obj f_list(t_obj list){
  return list;
}
t_obj f_isNull(t_obj list){
  if(list.t != Pair) return t_nil();
  t_obj o = Car(list);
  return t_bool(o.t == Nil);
}

t_obj f_show_heap(t_obj list){
  for(_t_pair * i = _heap();i< _heap_current();i++){
    printf("%p : car->",i);
    user_print(i->car);
    printf(", cdr->");
    user_print(i->cdr);
    printf("\n");
  }
  return t_nil();
}

#define Global_n 15
t_obj init_global_env(){
  t_obj vars[Global_n] =
    {
     t_symbol("+"),
     t_symbol("-"),
     t_symbol("*"),
     t_symbol("/"),
     t_symbol("="),
     t_symbol("true"),
     t_symbol("false"),
     t_symbol("car"),
     t_symbol("cdr"),
     t_symbol("cons"),
     t_symbol("set-car!"),
     t_symbol("set-cdr!"),
     t_symbol("list"),
     t_symbol("null?"),
     t_symbol("show-heap")
    };
  t_obj vals[Global_n] =
    {
     make_primitive_procedure(f_add),
     make_primitive_procedure(f_sub),
     make_primitive_procedure(f_mul),
     make_primitive_procedure(f_div),
     make_primitive_procedure(f_equal),
     t_bool(true),
     t_bool(false),
     make_primitive_procedure(f_car),
     make_primitive_procedure(f_cdr),
     make_primitive_procedure(f_cons),
     make_primitive_procedure(f_u_set_car),
     make_primitive_procedure(f_u_set_cdr),
     make_primitive_procedure(f_list),
     make_primitive_procedure(f_isNull),
     make_primitive_procedure(f_show_heap)
    };
  t_obj var_l = make_to_list(vars,Global_n);
  t_obj val_l = make_to_list(vals,Global_n);
  t_obj frame = make_frame(var_l, val_l);
  t_obj globalenv = t_env(t_symbol("*Global*"), frame, t_nil());
  return globalenv;
}
