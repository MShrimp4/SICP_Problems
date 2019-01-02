#pragma once
#include <stdbool.h>
#include <stddef.h>
#include "struct_def.h"

#define QUOTE_SYMBOL "quote"
#define UNNAMED_PROC "Lambda"

t_obj _make_typed_obj(int t, void* mallocd_data);

void _free_typed_obj(t_obj o);

t_obj t_dupe(t_obj o);

t_obj t_float(float f);

t_obj t_int(int i);

t_obj t_symbol(char * s);

t_obj t_vector_vacant(size_t len);

t_obj t_pair(t_obj car, t_obj cdr);

t_obj Car(t_obj p);

void set_car(t_obj p, t_obj val);

t_obj Cdr(t_obj p);

void set_cdr(t_obj p, t_obj val);

t_obj _t_label(int l);

t_obj t_quote(t_obj o);

t_obj text_of_quotation(t_obj exp);

t_obj t_nil();

t_obj t_bool(bool b);

bool isFalse(t_obj val);

bool isTrue(t_obj val);

t_obj t_env(t_obj name, t_obj frame, t_obj base_env);

t_obj environment_name(t_obj env);

bool isPrimitive_Procedure(t_obj proc);

bool isCompound_Procedure(t_obj proc);

bool isProcedure(t_obj proc);

t_obj procedure_parameters(t_obj proc);
  
t_obj procedure_environment(t_obj proc);

t_obj make_frame(t_obj var, t_obj val);

void add_binding_to_frame(t_obj var, t_obj val, t_obj frame);

t_obj extend_environment(t_obj proc, t_obj var, t_obj val, t_obj env);

t_obj procedure_body(t_obj proc);

t_obj make_to_list(t_obj* objs, int count);

t_obj t_prim_pointer(void * p);

bool isAnyPair(t_obj o);

t_obj make_primitive_procedure(t_obj (*func)(t_obj));

t_obj make_procedure(t_obj parameters, t_obj body, t_obj env);

void set_procedure_name(t_obj proc, t_obj name);

t_obj procedure_name(t_obj proc);

void user_print(t_obj val);
