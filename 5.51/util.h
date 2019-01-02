#pragma once
#include "struct_def.h"
#include "types.h"

#define ASSIGN_SYMBOL "set!"
#define DEFINITION_SYMBOL "define"
#define IF_SYMBOL "if"
#define LAMBDA_SYMBOL "lambda"
#define BEGIN_SYMBOL "begin"

bool isCarSymbol(t_obj exp, char* str);

bool isAssignment(t_obj exp);

bool isDefinition(t_obj exp);

bool isIf(t_obj exp);

bool isLambda(t_obj exp);

bool isBegin(t_obj exp);

t_obj enclosing_environment(t_obj env);

t_obj env_frame(t_obj env);

t_obj constant(char * exp);

bool isSameSymbol(t_obj o1, t_obj o2);

t_obj lookup_variable_value(t_obj var, t_obj env);

t_obj lambda_parameters(t_obj exp);

t_obj lambda_body(t_obj exp);

t_obj operands(t_obj exp);

t_obj operator(t_obj exp);

t_obj empty_arglist();

bool isNo_Operands(t_obj unev);

t_obj first_operand(t_obj unev);

t_obj rest_operands(t_obj unev);

bool isLast_Operand(t_obj unev);

t_obj adjoin_arg(t_obj val, t_obj argl);

t_obj begin_actions(t_obj exp);

t_obj first_exp(t_obj exp);

bool isLast_Exp(t_obj unev);

t_obj rest_exps(t_obj unev);

t_obj if_predicate(t_obj exp);

t_obj if_alternative(t_obj exp);

t_obj if_consequent(t_obj exp);

t_obj assignment_variable(t_obj exp);

t_obj assignment_value(t_obj exp);

void set_variable_value(t_obj _var, t_obj _val, t_obj env);

t_obj definition_variable(t_obj exp);

t_obj make_lambda(t_obj param, t_obj body);

t_obj definition_value(t_obj exp);

void define_variable(t_obj var, t_obj val, t_obj e);

t_obj apply_primitive_procedure(t_obj proc, t_obj argl);

t_obj read();
