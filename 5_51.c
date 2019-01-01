#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <ctype.h>

#define PAIR_COUNT 1024*1024*80
//64M


//Heap & Pair

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
_t_pair * heart_breaker();

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

typedef struct {
    size_t len;
    t_obj vec[];
} t_vec;

t_obj _make_typed_obj(int t, void* mallocd_data){
    t_obj o;
    o.t = t;
    o.data = mallocd_data;
    return o;
}

void _free_typed_obj(t_obj o){
  switch(o.t){
  case Symbol:
  case Vector:
    free(o.data);
  default:
    break;
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
    char* c_p = (char *)malloc(size);
    memcpy(c_p, s, size); //strdup?
    return _make_typed_obj(Symbol,c_p);
}

t_obj t_vector_vacant(size_t len){
    t_vec * vector = malloc(sizeof(t_vec) + sizeof(t_obj) * len);
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
  _free_typed_obj(Car(p));
  ((_t_pair*)(p.data))->car = val;
}

t_obj Cdr(t_obj p){
  return ((_t_pair*)(p.data))->cdr;
}

void set_cdr(t_obj p, t_obj val){
  _free_typed_obj(Cdr(p));
  ((_t_pair*)(p.data))->cdr = val;
}

t_obj _t_label(int l){
  t_obj o;
  o.t = _label;
  o.int_ = l;
  return o;
}

#define QUOTE_SYMBOL "quote"
t_obj t_quote(t_obj o){
  t_obj quoted = t_pair(t_symbol(QUOTE_SYMBOL) , o);
  quoted.t = Quoted;
  return quoted;
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

t_obj t_env(t_obj name, t_obj frame, t_obj base_env){
  t_obj frames = t_pair(frame,base_env);
  t_obj env = t_pair(name, frames);
  env.t = Environment;
  return env;
}

t_obj environment_name(t_obj env){
  return Car(env);
}

void user_print(t_obj val);
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

//Memory Swipe
t_obj* _global_env;

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

_t_pair * heart_breaker(){
  _t_pair * prev_heap = heap;
  _t_pair * prev_current = current;
  //_t_pair * prev_end = end; //unused
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

//environment
t_obj* _exp;
t_obj* _env;
t_obj* _val;
int* _cont;
t_obj* _proc;
t_obj* _argl;
t_obj* _unev;
t_obj* _the_stack;

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


#define ASSIGN_SYMBOL "set!"
bool isAssignment(t_obj exp){
  return isCarSymbol(exp, ASSIGN_SYMBOL);
}
#define DEFINITION_SYMBOL "define"
bool isDefinition(t_obj exp){
  return isCarSymbol(exp, DEFINITION_SYMBOL);
}
#define IF_SYMBOL "if"
bool isIf(t_obj exp){
  return isCarSymbol(exp, IF_SYMBOL);
}
#define LAMBDA_SYMBOL "lambda"
bool isLambda(t_obj exp){
  return isCarSymbol(exp, LAMBDA_SYMBOL);
}
#define BEGIN_SYMBOL "begin"
bool isBegin(t_obj exp){
  return isCarSymbol(exp, BEGIN_SYMBOL);
}

#define enclosing_environment(env) Cdr(Cdr(env))
#define env_frame(env) Car(Cdr(env))
#define constant(exp) t_quote(t_symbol(exp))
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

t_obj text_of_quotation(t_obj exp){
  return Cdr(exp);
}
t_obj lambda_parameters(t_obj exp){
  return Car(Cdr(exp));
}
t_obj lambda_body(t_obj exp){
  return Cdr(Cdr(exp));
}
#define UNNAMED_PROC "Lambda"
t_obj make_procedure(t_obj parameters, t_obj body, t_obj env){
  t_obj body_env = t_pair(body,env);
  t_obj par_body_env = t_pair(parameters,body_env);
  t_obj name = t_symbol(UNNAMED_PROC);
  t_obj procedure = t_pair(name,par_body_env);
  procedure.t = Procedure;
  return procedure;
}

t_obj make_primitive_procedure(t_obj (*func)(t_obj)){
  t_obj prim_proc = t_pair(t_symbol(UNNAMED_PROC), t_prim_pointer(func));
  prim_proc.t = Primitive_Procedure;
  return prim_proc;
}

void set_procedure_name(t_obj proc, t_obj name){
  set_car(proc, name);
}

t_obj procedure_name(t_obj proc){
  return Car(proc);
}

void save(t_obj o){
  *_the_stack = t_pair(o,*_the_stack);
}
void save_cont(int label){
  t_obj o = _t_label(label);
  save(o);
}
void restore(t_obj * o){
  *o = Car(*_the_stack);
  *_the_stack = Cdr(*_the_stack);
}
void restore_cont(int* label){
  t_obj o = Car(*_the_stack);
  *label = o.int_;
  *_the_stack = Cdr(*_the_stack);
}
#define operands(exp) Cdr(exp)
#define operator(exp) Car(exp)
#define empty_arglist() t_nil()
bool isNo_Operands(t_obj unev){
  return unev.t == Nil;
}
#define first_operand(unev) Car(unev)
#define rest_operands(unev) Cdr(unev)
bool isLast_Operand(t_obj unev){
  return isNo_Operands(rest_operands(unev));
}
#define adjoin_arg(val, argl) t_pair(val, argl)
bool isPrimitive_Procedure(t_obj proc){
  return proc.t == Primitive_Procedure;
}
bool isCompound_Procedure(t_obj proc){
  return proc.t == Procedure;
}
bool isProcedure(t_obj proc){
  return proc.t == Procedure || proc.t == Primitive_Procedure;
}
t_obj apply_primitive_procedure(t_obj proc, t_obj argl){
  t_obj proc_ptr = Cdr(proc);
  t_obj (* func)(t_obj) = (t_obj (*)(t_obj)) (proc_ptr.data);
  return func(argl);
}
//TODO Possible Memory Leak: Put it in a dummy pair
#define procedure_parameters(proc) Car(Cdr(proc))
#define procedure_environment(proc) Cdr(Cdr(Cdr(proc)))
t_obj make_frame(t_obj var, t_obj val){
  t_obj new_val = t_dupe(val);
  return t_pair(var,new_val);
}
t_obj extend_environment(t_obj proc, t_obj var, t_obj val, t_obj env){// values must be duped
  t_obj frame = make_frame(var, val);
  return t_env(Car(proc), frame, env);
}
#define procedure_body(proc) Car(Cdr(Cdr(proc)))
#define begin_actions(exp) Cdr(exp)
#define first_exp(exp) Car(exp)
bool isLast_Exp(t_obj unev){
  t_obj next_exp = Cdr(unev);
  return next_exp.t == Nil;
}
#define rest_exps(unev) Cdr(unev)
#define if_predicate(exp) Car(Cdr(exp))
bool isFalse(t_obj val){
  return val.t == Bool && val.bool_ == false;
}
bool isTrue(t_obj val){
  return !isFalse(val);
}
t_obj if_alternative(t_obj exp){
  t_obj cdddr = Cdr(Cdr(Cdr(exp)));
  if(Nil != cdddr.t){
    return Car(cdddr);
  }
  else return t_bool(false);
}
#define if_consequent(exp) Car(Cdr(Cdr(exp)))
#define assignment_variable(exp) Car(Cdr(exp))
#define assignment_value(exp) Car(Cdr(Cdr(exp)))

void add_binding_to_frame(t_obj var, t_obj val, t_obj frame);

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

void add_binding_to_frame(t_obj var, t_obj val, t_obj frame){
  set_car(frame, t_pair(var, Car(frame)));
  set_cdr(frame, t_pair(val, Cdr(frame)));
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
    printf("_%s_",(char*)val.data);
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

//ECE operations
void init_stack(){
   *_the_stack = t_nil();
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

t_obj get_global_environment(){ //_pair is the only pointer we care about
    return *_global_env;
}


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
  int i = 0;
  float f;
  bool use_float = false;
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
t_obj f_isNull(t_obj list){
  if(list.t != Pair) return t_nil();
  t_obj o = Car(list);
  return t_bool(o.t == Nil);
}

t_obj f_show_heap(t_obj list){
  for(_t_pair * i = heap;i<current;i++){
    printf("%p : car->",i);
    user_print(i->car);
    printf(", cdr->");
    user_print(i->cdr);
    printf("\n");
  }
  return t_nil();
}

#define Global_n 12
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
     make_primitive_procedure(f_isNull),
     make_primitive_procedure(f_show_heap)
    };
  t_obj var_l = make_to_list(vars,Global_n);
  t_obj val_l = make_to_list(vals,Global_n);
  t_obj frame = make_frame(var_l, val_l);
  t_obj globalenv = t_env(t_symbol("*Global*"), frame, t_nil());
  return globalenv;
}

//Merely a translation of ECE

enum labels{
    read_eval_print_loop,
    print_result,
    eval_dispatch,
    ev_self_eval,
    ev_variable,
    ev_quoted,
    ev_lambda,
    ev_application,
    ev_appl_did_operator,
    ev_appl_operand_loop,
    ev_appl_accumulate_arg,
    ev_appl_last_arg,
    ev_appl_accum_last_arg,
    apply_dispatch,
    primitive_apply,
    compound_apply,
    ev_begin,
    ev_sequence,
    ev_sequence_continue,
    ev_sequence_last_exp,
    ev_if,
    ev_if_decide,
    ev_assignment,
    ev_assignment_1,
    ev_definition,
    ev_definition_1,
    unknown_expression_type,
    unknown_procedure_type,
    signal_error,
    exit_all
};
enum labels current_label;

void explicit_control_evaluator(){
    int current_label = read_eval_print_loop;
    t_obj exp = t_nil();
    _exp = &exp;
    t_obj env = t_nil();
    _env = &env;
    t_obj val = t_nil();
    _val = &val;
    int cont = exit_all;
    _cont = &cont;
    t_obj proc = t_nil();
    _proc = &proc;
    t_obj argl = t_nil();
    _argl = &argl;
    t_obj unev = t_nil();
    _unev = &unev;
    t_obj global_env = init_global_env();
    _global_env = &global_env;
    t_obj the_stack = t_nil();
    _the_stack = &the_stack;
    while(current_label != exit_all){
        switch(current_label){
        case read_eval_print_loop:
	  //user_print(Cdr(global_env));
	  //printf("\nREPL - Line %d\n",__LINE__);
            init_stack();
            printf("\n;;;EC-Eval input:");
            exp = read();
            env = get_global_environment();
            cont = print_result;
            current_label = eval_dispatch;
            break;
	case print_result:
	  //printf("\nprint_result - Line %d\n",__LINE__);
	  //printf("\n;;;EC-Eval value:");
	  user_print(val);
	  //printf("\n");
	  /*
	  if(heart_breaker() == NULL){
	    perror("Pair Heap Full,REPL()");
	    exit(1);
	    }*/
	  current_label = read_eval_print_loop;
	  break;
        case eval_dispatch:
	  //printf("\neval_dispatch - Line %d\n",__LINE__);
            switch(exp.t){
	    case Int:
            case Float:
            case Vector:
            case Procedure:
	    case Primitive_Procedure:
	    case Primitive_Pointer:
            case Macro:
	    case Nil:
                current_label = ev_self_eval; //self-evaluating?
                break;
            case Symbol:
                current_label = ev_variable; //variable?
                break;
            case Quoted:
                current_label = ev_quoted; //quoted?
                break;
            case Pair:
                if(isIf(exp)) current_label = ev_if;
                else if(isLambda(exp)) current_label = ev_lambda;
                else if(isDefinition(exp)) current_label = ev_definition;
                else if(isBegin(exp)) current_label = ev_begin;
                else if(isAssignment(exp)) current_label = ev_assignment;
                else current_label = ev_application; //application?
                break;
            default:
                current_label = unknown_expression_type;
            }
            break;
        case ev_self_eval:
	  //printf("\nev_self_eval - Line %d\n",__LINE__);
            val = exp;
            current_label = cont;
            break;
        case ev_variable:
	  //printf("\nev_variable(faulty?) - Line %d\n",__LINE__);
	  val = lookup_variable_value(exp,env);
	  current_label = cont;
	  break;
        case ev_quoted:
	  //printf("\nev_quoted - Line %d\n",__LINE__);
	  val = text_of_quotation(exp);
	  current_label = cont;
	  break;
        case ev_lambda:
	  //printf("\nev_lambda - Line %d\n",__LINE__);
	  unev = lambda_parameters(exp);
	  exp = lambda_body(exp);
	  val = make_procedure(unev,exp,env);
	  current_label = cont;
	  break;
        case ev_application:
	  //printf("\nev_application - Line %d\n",__LINE__);
            save_cont(cont);
            save(env);
            unev = operands(exp);
            save(unev);
            exp = operator(exp);
            cont = ev_appl_did_operator;
            current_label = eval_dispatch;
            break;
        case ev_appl_did_operator:
	  //printf("\nev_appl_did_operator - Line %d\n",__LINE__);
            restore(&unev);
            restore(&env);
            argl = empty_arglist();
            proc = val;
            if(isNo_Operands(unev)){
                current_label = apply_dispatch;
                break;
                }
            save(proc);
            //No break; on purpose.
        case ev_appl_operand_loop:
	  //printf("\nev_appl_operand_loop - Line %d\n",__LINE__);
            save(argl);
            exp = first_operand(unev);
            if(isLast_Operand(unev)){
                current_label = ev_appl_last_arg;
                break;
            }
            save(env);
            save(unev);
            cont = ev_appl_accumulate_arg;
            current_label = eval_dispatch;
            break;
        case ev_appl_accumulate_arg:
	  //printf("\nev_appl_accumulate - Line %d\n",__LINE__);
            restore(&unev);
            restore(&env);
            restore(&argl);
            argl = adjoin_arg(val,argl);
            unev = rest_operands(unev);
            current_label = ev_appl_operand_loop;
            break;
        case ev_appl_last_arg:
	  //printf("\nev_appl_last_arg - Line %d\n",__LINE__);
            cont = ev_appl_accum_last_arg;
            current_label = eval_dispatch;
            break;
        case ev_appl_accum_last_arg:
	  //printf("\nev_appl_accum_last_arg - Line %d\n",__LINE__);
            restore(&argl);
            argl = adjoin_arg(val,argl);
            restore(&proc);
            current_label = apply_dispatch;
            break;
        case apply_dispatch:
	  //printf("\napply_dispatch Line %d\n",__LINE__);
	  if(isPrimitive_Procedure(proc)){
	    current_label = primitive_apply;
	  }
	  else if(isCompound_Procedure(proc)){
	    current_label = compound_apply;
	  }
	  else current_label = unknown_procedure_type;
	  break;
        case primitive_apply:
	  //printf("\nprimitive_apply - Line %d\n",__LINE__);
            val = apply_primitive_procedure(proc,argl);
            restore_cont(&cont);
            current_label = cont;
            break;
        case compound_apply:
	  printf("\ncompound_apply - Line %d\n",__LINE__);
            unev = procedure_parameters(proc);
            env = procedure_environment(proc);
            env = extend_environment(proc,unev,argl,env);
            unev = procedure_body(proc);
            current_label = ev_sequence;
            break;
        case ev_begin:
	  //printf("\nev_begin - Line %d\n",__LINE__);
            unev = begin_actions(exp);
            save_cont(cont);
            current_label = ev_sequence;
            break;
        case ev_sequence:
	  //printf("\nev_sequence - Line %d\n",__LINE__);
            exp = first_exp(unev);
            if(isLast_Exp(unev)){
                current_label = ev_sequence_last_exp;
                break;
            }
            save(unev);
            save(env);
            cont = ev_sequence_continue;
            current_label = eval_dispatch;
            break;
        case ev_sequence_continue:
	  //printf("\nev_sequence_continue - Line %d\n",__LINE__);
            restore(&env);
            restore(&unev);
            unev = rest_exps(unev);
            current_label = ev_sequence;
            break;
        case ev_sequence_last_exp:
	  //printf("\nev_sequence_last_exp - Line %d\n",__LINE__);
            restore_cont(&cont);
            current_label =eval_dispatch;
            break;
        case ev_if:
	  //printf("\nev_if - Line %d\n",__LINE__);
            save(exp);
            save(env);
            save_cont(cont);
            cont = ev_if_decide;
            exp = if_predicate(exp);
            current_label = eval_dispatch;
            break;
        case ev_if_decide:
	  //printf("\nev_if_decide - Line %d\n",__LINE__);
            restore_cont(&cont);
            restore(&env);
            restore(&exp);
            if(isTrue(val)) exp = if_consequent(exp);
            else exp = if_alternative(exp);
            current_label = eval_dispatch;
            break;
        case ev_assignment:
	  //printf("\nev_assignment - Line %d\n",__LINE__);
            unev = assignment_variable(exp);
            save(unev);
            exp = assignment_value(exp);
            save(env);
            save_cont(cont);
            cont = ev_assignment_1;
            current_label = eval_dispatch;
            break;
        case ev_assignment_1:
	  //printf("\nev_assignment_1 - Line %d\n",__LINE__);
            restore_cont(&cont);
            restore(&env);
            restore(&unev);
            set_variable_value(unev,val,env);
            //val = constant("ok");
            current_label = cont;
            break;
        case ev_definition:
	  //printf("\nev_definition - Line %d\n",__LINE__);
            unev = definition_variable(exp);
            save(unev);
            exp = definition_value(exp);
            save(env);
            save_cont(cont);
            cont = ev_definition_1;
            current_label = eval_dispatch;
            break;
        case ev_definition_1:
	  //printf("\nev_definition_1 - Line %d\n",__LINE__);
            restore_cont(&cont);
            restore(&env);
            restore(&unev);
            define_variable(unev,val,env);
            //val = constant("ok");
            current_label = cont;
            break;
        case unknown_expression_type:
	  //printf("\nunknown_expression_type - Line %d\n",__LINE__);
            val = constant("unknown-expression-type-error");
            current_label = signal_error;
            break;
        case unknown_procedure_type:
	  //printf("\nunknown_procedure_type - Line %d\n",__LINE__);
            val = constant("unknown-procedure-type-error");
            current_label = signal_error;
            break;
        case signal_error:
	  //printf("\nsignal_error - Line %d\n",__LINE__);
            user_print(val);
            current_label = read_eval_print_loop;
        case exit_all:
        default:
            break;
        }
    }
}




//End of ECE

int main(){
    if(init_heap(PAIR_COUNT) == NULL){
        perror("Heap Allocation Failed.");
        return 1;
    }
    explicit_control_evaluator();
}
