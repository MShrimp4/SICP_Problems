#pragma once
#include "struct_def.h"

_t_pair * init_heap(size_t size);

void add_track(t_obj * , size_t n);

_t_pair * make_pair();

_t_pair * heart_breaker(t_obj * _global_env);

_t_pair * _heap();

_t_pair * _heap_current();

_t_pair * _heap_end();
