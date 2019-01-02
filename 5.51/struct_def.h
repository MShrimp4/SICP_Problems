#pragma once
#include <stdbool.h>

typedef struct {
  int t;
  union{
    int int_;
    float float_;
    bool bool_;
    void * data;
  };
} t_obj;

typedef struct {
    t_obj car;
    t_obj cdr;
} _t_pair;

typedef struct {
    size_t len;
    t_obj vec[];
} t_vec;


//TYPES
enum types{
    //Normal types - Does not need allocation
    Int,
    Float,
    Bool,
    _label,
    //
    Symbol,
    Vector,
    //Procedures
    Procedure,
    Primitive_Procedure,
    Primitive_Pointer,
    Macro,
    //Normal pairs and Nametagging pairs
    Pair,
    Quoted,
    Environment,
    //data = NULL s
    Nil,
    Broken_Heart
};
