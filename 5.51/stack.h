#pragma once
#include "struct_def.h"

void save(t_obj o);
void save_cont(int label);
void restore(t_obj * o);
void restore_cont(int* label);
void init_stack(t_obj * s);
