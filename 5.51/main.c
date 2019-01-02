#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "struct_def.h"
#include "heap.h"
#include "stack.h"
#include "types.h"
#include "util.h"
#include "global_env.h"

#define PAIR_COUNT 1024*1024*80

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
    t_obj env = t_nil();
    t_obj val = t_nil();
    int cont = exit_all;
    t_obj proc = t_nil();
    t_obj argl = t_nil();
    t_obj unev = t_nil();
    t_obj global_env = init_global_env();
    t_obj the_stack = t_nil();
    while(current_label != exit_all){
        switch(current_label){
        case read_eval_print_loop:
            init_stack();
            printf("\n;;;EC-Eval input: ");
            exp = read();
            env = global_env;
            cont = print_result;
            current_label = eval_dispatch;
            break;
	case print_result:
	  printf("\n;;; EC-Eval value: ");
	  user_print(val);
	  printf("\n");
	  /*if(heart_breaker(&global_env) == NULL){
	    perror("Pair Heap Full,REPL()");
	    exit(1);
	    }*/
	  current_label = read_eval_print_loop;
	  break;
        case eval_dispatch:
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
            val = exp;
            current_label = cont;
            break;
        case ev_variable:
	  val = lookup_variable_value(exp,env);
	  current_label = cont;
	  break;
        case ev_quoted:
	  val = text_of_quotation(exp);
	  current_label = cont;
	  break;
        case ev_lambda:
	  unev = lambda_parameters(exp);
	  exp = lambda_body(exp);
	  val = make_procedure(unev,exp,env);
	  current_label = cont;
	  break;
        case ev_application:
            save_cont(cont);
            save(env);
            unev = operands(exp);
            save(unev);
            exp = operator(exp);
            cont = ev_appl_did_operator;
            current_label = eval_dispatch;
            break;
        case ev_appl_did_operator:
            restore(&unev);
            restore(&env);
            argl = empty_arglist();
            proc = val;
            if(isNo_Operands(unev)){
                current_label = apply_dispatch;
                break;
                }
            save(proc);
        case ev_appl_operand_loop:
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
            restore(&unev);
            restore(&env);
            restore(&argl);
            argl = adjoin_arg(val,argl);
            unev = rest_operands(unev);
            current_label = ev_appl_operand_loop;
            break;
        case ev_appl_last_arg:
            cont = ev_appl_accum_last_arg;
            current_label = eval_dispatch;
            break;
        case ev_appl_accum_last_arg:
            restore(&argl);
            argl = adjoin_arg(val,argl);
            restore(&proc);
            current_label = apply_dispatch;
            break;
        case apply_dispatch:
	  if(isPrimitive_Procedure(proc)){
	    current_label = primitive_apply;
	  }
	  else if(isCompound_Procedure(proc)){
	    current_label = compound_apply;
	  }
	  else current_label = unknown_procedure_type;
	  break;
        case primitive_apply:
            val = apply_primitive_procedure(proc,argl);
            restore_cont(&cont);
            current_label = cont;
            break;
        case compound_apply:
            unev = procedure_parameters(proc);
            env = procedure_environment(proc);
            env = extend_environment(proc,unev,argl,env);
            unev = procedure_body(proc);
            current_label = ev_sequence;
            break;
        case ev_begin:
            unev = begin_actions(exp);
            save_cont(cont);
            current_label = ev_sequence;
            break;
        case ev_sequence:
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
            restore(&env);
            restore(&unev);
            unev = rest_exps(unev);
            current_label = ev_sequence;
            break;
        case ev_sequence_last_exp:
            restore_cont(&cont);
            current_label =eval_dispatch;
            break;
        case ev_if:
            save(exp);
            save(env);
            save_cont(cont);
            cont = ev_if_decide;
            exp = if_predicate(exp);
            current_label = eval_dispatch;
            break;
        case ev_if_decide:
            restore_cont(&cont);
            restore(&env);
            restore(&exp);
            if(isTrue(val)) exp = if_consequent(exp);
            else exp = if_alternative(exp);
            current_label = eval_dispatch;
            break;
        case ev_assignment:
            unev = assignment_variable(exp);
            save(unev);
            exp = assignment_value(exp);
            save(env);
            save_cont(cont);
            cont = ev_assignment_1;
            current_label = eval_dispatch;
            break;
        case ev_assignment_1:
            restore_cont(&cont);
            restore(&env);
            restore(&unev);
            set_variable_value(unev,val,env);
            current_label = cont;
            break;
        case ev_definition:
            unev = definition_variable(exp);
            save(unev);
            exp = definition_value(exp);
            save(env);
            save_cont(cont);
            cont = ev_definition_1;
            current_label = eval_dispatch;
            break;
        case ev_definition_1:
            restore_cont(&cont);
            restore(&env);
            restore(&unev);
            define_variable(unev,val,env);
            current_label = cont;
            break;
        case unknown_expression_type:
            val = constant("unknown-expression-type-error");
            current_label = signal_error;
            break;
        case unknown_procedure_type:
            val = constant("unknown-procedure-type-error");
            current_label = signal_error;
            break;
        case signal_error:
            user_print(val);
            current_label = read_eval_print_loop;
        case exit_all:
        default:
            break;
        }
    }
}





int main(){
    if(init_heap(PAIR_COUNT) == NULL){
        perror("Heap Allocation Failed.");
        return 1;
    }
    explicit_control_evaluator();
}
