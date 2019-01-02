#pragma once
#include <stdbool.h>
#include "struct_def.h"

_t_pair * init_heap(size_t p_size, size_t a_size);

void set_gc_root(t_obj * roots[], size_t count);

_t_pair * make_pair();

void * h_alloc();

bool isHeapFull();

_t_pair * heart_breaker();

_t_pair * _heap();

_t_pair * _heap_current();

_t_pair * _heap_end();
