#include <stdio.h>
#include <stdlib.h>
#include "struct_def.h"
#include "types.h"

size_t pair_count;
_t_pair * heap;
_t_pair * current;
_t_pair * end;

_t_pair * init_heap(size_t size){
    pair_count = size;
    heap = (_t_pair *) malloc(size * sizeof(_t_pair));
    current = heap;
    end = heap + size;
    return heap;
}

_t_pair* make_pair(){
    if (current > end){
      perror("Pair Heap Full , make_pair()");
      exit(1);
    }
    _t_pair* new_pair = current;
    current++;
    return new_pair;
}

t_obj t_broken_heart(_t_pair* new_location){
  return _make_typed_obj(Broken_Heart, new_location);
}

_t_pair p_broken_heart(_t_pair* new_location){
  _t_pair p;
  p.car = t_broken_heart(new_location);
  p.cdr = t_nil();
  return p;
}

bool isBrokenHeart(t_obj p){
  _t_pair * pair = (_t_pair*)p.data;
  return pair->car.t == Broken_Heart;
}

void relocate(t_obj* p){
  _t_pair * broken_heart = (_t_pair*) p->data;
  if(broken_heart->car.t != Broken_Heart){
    perror("Broken Heart err");
    exit(1);
  }
  _t_pair * new_location = (_t_pair*) broken_heart->car.data;
  p->data = new_location;
}

void free_pairs_left(_t_pair * prev_heap,_t_pair * prev_end){
  for(_t_pair * i = prev_heap;i <= prev_end;i++){
    _free_typed_obj(i->car);
    _free_typed_obj(i->cdr);
      }
}

void move_pair(t_obj* pair){
  if(pair->t == Broken_Heart) return;
  _t_pair * new_location = make_pair();
  *new_location = *(_t_pair*)(pair->data);

  *(_t_pair*)(pair->data) = p_broken_heart(new_location);
  if(isAnyPair(new_location->car)){
    if(isBrokenHeart(new_location->car)){
      relocate(&new_location->car);
    }
    else{
      move_pair(&new_location->car);
      relocate(&new_location->car);
    }
  }
  if(isAnyPair(new_location->cdr)){
    if(isBrokenHeart(new_location->cdr)){
      relocate(&new_location->cdr);
    }
    else{
      move_pair(&new_location->cdr);
      relocate(&new_location->cdr);
    }
  }
}

_t_pair * heart_breaker(t_obj * _global_env){
  _t_pair * prev_heap = heap;
  _t_pair * prev_current = current;
  if(init_heap(pair_count) == NULL){
    perror("Pair Heap Full, heart_breaker()");
    exit(1);
  }
  move_pair(_global_env);
  relocate(_global_env);
  free_pairs_left(prev_heap,prev_current);
  free(prev_heap);
  return heap;
}

_t_pair * _heap(){
  return heap;
}

_t_pair * _heap_current(){
  return current;
}

_t_pair * _heap_end(){
  return end;
}
