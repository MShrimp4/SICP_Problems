#include <stdio.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include "struct_def.h"
#include "types.h"
#include "util.h"

bool isCarSymbol(t_obj exp, char* str){
  if (exp.t == Pair){
    t_obj car = Car(exp);
    if(car.t == Symbol){
      if(strcmp(car.data,str) == 0)
	return true;
    }
  }
  return false;
}

bool isAssignment(t_obj exp){
  return isCarSymbol(exp, ASSIGN_SYMBOL);
}

bool isDefinition(t_obj exp){
  return isCarSymbol(exp, DEFINITION_SYMBOL);
}

bool isIf(t_obj exp){
  return isCarSymbol(exp, IF_SYMBOL);
}

bool isLambda(t_obj exp){
  return isCarSymbol(exp, LAMBDA_SYMBOL);
}

bool isBegin(t_obj exp){
  return isCarSymbol(exp, BEGIN_SYMBOL);
}

t_obj enclosing_environment(t_obj env) {
  return Cdr(Cdr(env));
}

t_obj env_frame(t_obj env){
  return Car(Cdr(env));
}

t_obj constant(char * exp){
  return t_quote(t_symbol(exp));
}

bool isSameSymbol(t_obj o1, t_obj o2){
  char * c1 = (char*)o1.data;
  char * c2 = (char*)o2.data;
  return strcmp(c1,c2) == 0;
}

t_obj lookup_variable_value(t_obj var, t_obj env){
  for(t_obj e = env; e.t != Nil; e = enclosing_environment(e)){
    t_obj frame = env_frame(e);
    t_obj vars = Car(frame);
    t_obj vals = Cdr(frame);
    for(; vars.t == Pair && vals.t == Pair; vars = Cdr(vars), vals = Cdr(vals)){
      if(isSameSymbol(Car(vars),var)) {
	return Car(vals);
      }
    }
    if(vars.t == Symbol){
      if(isSameSymbol(vars,var)){
	return vals; //Dotted trails capture all list
      }
    }
  }
  return constant("Variable Not Found");
}

t_obj lambda_parameters(t_obj exp){
  return Car(Cdr(exp));
}

t_obj lambda_body(t_obj exp){
  return Cdr(Cdr(exp));
}

t_obj operands(t_obj exp){
  return Cdr(exp);
}

t_obj operator(t_obj exp){
  return Car(exp);
}

t_obj empty_arglist() {
  return t_nil();
}

bool isNo_Operands(t_obj unev){
  return unev.t == Nil;
}

t_obj first_operand(t_obj unev){
  return Car(unev);
}

t_obj rest_operands(t_obj unev){
  return Cdr(unev);
}

bool isLast_Operand(t_obj unev){
  return isNo_Operands(rest_operands(unev));
}

t_obj adjoin_arg(t_obj val, t_obj argl){
  return t_pair(val, argl);
}

t_obj begin_actions(t_obj exp){
  return Cdr(exp);
}

t_obj first_exp(t_obj exp){
  return Car(exp);
}

bool isLast_Exp(t_obj unev){
  t_obj next_exp = Cdr(unev);
  return next_exp.t == Nil;
}

t_obj rest_exps(t_obj unev){
  return Cdr(unev);
}

t_obj if_predicate(t_obj exp){
  return Car(Cdr(exp));
}

t_obj if_alternative(t_obj exp){
  t_obj cdddr = Cdr(Cdr(Cdr(exp)));
  if(Nil != cdddr.t){
    return Car(cdddr);
  }
  else return t_bool(false);
}

t_obj if_consequent(t_obj exp){
  return Car(Cdr(Cdr(exp)));
}

t_obj assignment_variable(t_obj exp){
  return Car(Cdr(exp));
}

t_obj assignment_value(t_obj exp) {
  return Car(Cdr(Cdr(exp)));
}

void set_variable_value(t_obj _var, t_obj _val, t_obj env){
  t_obj var = t_dupe(_var);
  t_obj val = t_dupe(_val);
  if(isProcedure(val)) set_procedure_name(val, t_dupe(var));
  for(t_obj e = env; e.t != Nil; e = enclosing_environment(e)){
    t_obj frame = env_frame(e);
    t_obj vars = Car(frame);
    t_obj vals = Cdr(frame);
    for(; vars.t == Pair && vals.t == Pair; vars = Cdr(vars), vals = Cdr(vals)){
      if(isSameSymbol(Car(vars),var)) {
	set_car(vals, val);
	return;
      }
    }
    if(vars.t == Symbol){//Weird implementation...
      if(isSameSymbol(vars, var)){
	add_binding_to_frame(var,val,frame);
      }
    }
  }
}

t_obj definition_variable(t_obj exp){
  t_obj cadr = Car(Cdr(exp));
  if(cadr.t == Symbol) return cadr;
  else return Car(cadr);
}

t_obj make_lambda(t_obj param, t_obj body){
  return t_pair(t_symbol(LAMBDA_SYMBOL) , t_pair(param, body));
}

t_obj definition_value(t_obj exp){
  t_obj cadr = Car(Cdr(exp));
  if(cadr.t == Symbol) return Car(Cdr(Cdr(exp)));
  else return make_lambda(Cdr(cadr), Cdr(Cdr(exp)));
}

void define_variable(t_obj var, t_obj val, t_obj e){
  if(isProcedure(val)) set_procedure_name(val, var);
  t_obj frame = env_frame(e);
  t_obj vars = Car(frame);
  t_obj vals = Cdr(frame);
  for(; vars.t == Pair && vals.t == Pair; vars = Cdr(vars), vals = Cdr(vals)){
    if(isSameSymbol(Car(vars),var)) {
      set_car(vals, val);
      return;
    }
  }
  add_binding_to_frame(var, val, frame);
}

t_obj apply_primitive_procedure(t_obj proc, t_obj argl){
  t_obj proc_ptr = Cdr(proc);
  t_obj (* func)(t_obj) = (t_obj (*)(t_obj)) (proc_ptr.data);
  return func(argl);
}


char * stt_of_list(char * str){
  if (str == NULL) return NULL;
  while(*str != '\0'){
    if(*str == '\'' && *(str+1) == '(') return str;
    if(*str == '(') return str;
    str++;
  }
  return NULL;
}

char * end_of_list(char * str){
  if (str == NULL) return NULL;
  int level = 1;
  if(*str == '\'')str++;
  str++;
  for(;*str != '\0';str++){
    switch(*str){
    case '(':
      level++;
      break;
    case ')':
      level--;
      break;
    default:
      break;
    }
    if(level == 0) return str;
  }
  return NULL;
}

t_obj parse(char * stt, char * end){
  char * l_stt = stt_of_list(stt);
  char * l_end = end_of_list(l_stt);
  if(l_stt == NULL || end <= l_stt){ // symbol
    bool quote = false;
    l_stt = stt;
    while(isspace(*l_stt)) l_stt++;
    if(*l_stt == '\''){
      quote = true;
      l_stt++;
    }
    l_end = l_stt;
    while(!isspace(*l_end) && *l_end != '(' && *l_end != ')' && *l_end != '\'' && *l_end != '\0') l_end++;
    size_t size = l_end - l_stt;
    char sym[size + 1];
    memcpy(sym, l_stt, size);
    *(sym + size) = '\0';
    t_obj symbol;
    int i;
    float f;
    
    if(sscanf(sym, "%f", &f) != 0){
      if(strchr(sym,'.') != NULL) symbol = t_float(f);
      else if(sscanf(sym, "%d", &i) != 0 ) symbol = t_int(i);
      else symbol = t_float(f);
    }
    else symbol = t_symbol(sym);
    return quote? t_quote(symbol) : symbol;
  }
  else{ //list
    bool quote = false;
    t_obj arr[100];
    int index = 0;
    if(*l_stt == '\''){
      l_stt++;
      quote = true;
    }
    l_stt++;
    char * next_list = stt_of_list(l_stt);
    for(;l_stt<l_end;l_stt++){
      if(l_stt == next_list){
	char * subl_end = end_of_list(l_stt);
	arr[index++] = parse(l_stt,subl_end);
	l_stt = subl_end;
	next_list = stt_of_list(l_stt);
      }
      else if(isspace(*l_stt));
      else{
	char * subs_end = l_stt + 1;
	while(!isspace(*subs_end) && *subs_end != '(' && *subs_end != ')' && *subs_end != '\'' && *subs_end != '\0') subs_end++;
	arr[index++] = parse(l_stt,subs_end);
	l_stt = subs_end;
      }
    }
    t_obj list = make_to_list(arr, index);
    return quote? t_quote(list) : list;
  }
}

bool isValidString(char * str){
  char *stt = stt_of_list(str);
  if(stt == NULL) return true;
  char *end = end_of_list(str);
  if(end == NULL) return false;
  return true;
}

t_obj read(){
    static char read[1000000];
    fgets(read,1000000,stdin);
    if (!isValidString(read)) return t_nil();
    return parse(read,read+strlen(read));
}
