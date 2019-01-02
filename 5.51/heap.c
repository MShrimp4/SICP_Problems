#include <stdio.h>
#include <stdlib.h>
#include "struct_def.h"
#include "types.h"
#include "heap.h"

typedef char byte;

#define PAIR_FULL 0.97
#define ALLOC_FULL 0.80

#define MAX_ROOT 12
t_obj *gc_root[MAX_ROOT];
size_t root_count;

size_t pair_size;
size_t alloc_size;
_t_pair * pair_start;
_t_pair * pair_current;
_t_pair * pair_end;
byte * alloc_start;
byte * alloc_current;
byte * alloc_end;

_t_pair * init_heap(size_t p_size, size_t a_size){
  pair_size = p_size - (p_size % sizeof(_t_pair));
  pair_start = (_t_pair *) malloc(pair_size); // 
  pair_current = pair_start;
  pair_end = pair_start + (p_size / sizeof(_t_pair));

  alloc_size = a_size;
  alloc_start = (byte *) malloc(alloc_size);
  alloc_current = alloc_start;
  alloc_end = alloc_start + alloc_size;
  return pair_start;
}

void set_gc_root(t_obj * roots[], size_t count){
  if(count > MAX_ROOT) {
    perror("Too much root nodes");
    exit(1);
  }
  else{
    root_count = count;
    for(size_t i = 0; i < count; i++){
      gc_root[i] = roots[i];
    }
  }
}

_t_pair * make_pair(){
  /*
    if(pair_current >= pair_end){
      perror("Pair Full");
      exit(1);
      }*/
    _t_pair* new_pair = pair_current;
    pair_current++;
    return new_pair;
}

void * h_alloc(size_t size){
  /*if((alloc_current + size) > alloc_end){
    perror("Alloc Full");
    exit(1);
    }*/
  if((alloc_current + size) > alloc_end){
    perror("Allocating size too big");
    exit(1);
  }
  void * allocated = (void *) alloc_current;
  alloc_current += size;
  return allocated;
}

t_obj t_broken_heart(_t_pair* new_location){
  return _make_typed_obj(Broken_Heart, new_location);
}

bool isBrokenHeart(t_obj o){
  return o.t == Broken_Heart;
}

_t_pair p_broken_heart(_t_pair* new_location){
  _t_pair p;
  p.car = t_broken_heart(new_location);
  p.cdr = t_nil();
  return p;
}

bool isLinkBroken(t_obj pair){
  _t_pair * pair_on_heap = pair.data;
  return isBrokenHeart(pair_on_heap->car);
}

void relocate(t_obj* p){
  _t_pair * broken_heart = (_t_pair*) p->data;
  _t_pair * new_location = (_t_pair*) broken_heart->car.data;
  p->data = new_location;
}

void dupe_or_relocate(t_obj * p){
  if(isAnyPair(*p)){
    relocate(p);
  }
  else{
    *p = t_shallow_dupe(*p);
  }
}

void move_pair(t_obj* pair){
  _t_pair * new_location = make_pair();
  //*new_location = *(_t_pair*)(pair->data);
  _t_pair * old_location = (_t_pair*)(pair->data);
  new_location->car = t_shallow_dupe(old_location->car);
  new_location->cdr = t_shallow_dupe(old_location->cdr);

  *old_location = p_broken_heart(new_location);
  if(isAnyPair(new_location->car)){
    if(isLinkBroken(new_location->car)){
      relocate(&new_location->car);
    }
    else{
      move_pair(&new_location->car);
      relocate(&new_location->car);
    }
  }
  if(isAnyPair(new_location->cdr)){
    if(isLinkBroken(new_location->cdr)){
      relocate(&new_location->cdr);
    }
    else{
      move_pair(&new_location->cdr);
      relocate(&new_location->cdr);
    }
  }
}

bool isHeapFull(){
  return ((float)(pair_current - pair_start)/ (float) (pair_end - pair_start)) > PAIR_FULL || ((float)(alloc_current - alloc_start)/ (float) alloc_size) > ALLOC_FULL;
}

_t_pair * heart_breaker(){
  _t_pair * prev_heap = pair_start;
  byte * prev_alloc = alloc_start;
  if(init_heap(pair_size,alloc_size) == NULL){
    perror("Allocation failed, heart_breaker()");
    exit(1);
  }
  for(size_t i = 0; i < root_count; i++){
    if (isAnyPair(*gc_root[i]) && !isLinkBroken(*gc_root[i])) move_pair(gc_root[i]);
    dupe_or_relocate(gc_root[i]);
  }
  free(prev_heap);
  free(prev_alloc);
  return pair_start;
}

_t_pair * _heap(){
  return pair_start;
}

_t_pair * _heap_current(){
  return pair_current;
}

_t_pair * _heap_end(){
  return pair_end;
}
