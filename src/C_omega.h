
#ifndef C_OMEGA_H
#define C_OMEGA_H

#ifdef __cplusplus
extern "C" {
#endif

/* This is a copy of 'coef_t'.  Can't use the original because it's in
 * a C++ header file. */
typedef long long coefficient_t;

/* This is a copy of struct Variable_Info.  Can't use the original because
 * it's in a C++ header file. */
typedef struct Variable_Info_struct {
  struct Var_Decl *var;
  coefficient_t    coef;
} Variable_Info_struct;

struct Relation *new_relation(int n_input, int n_output);
struct Relation *new_set(int n);
void free_relation(struct Relation *rel);
char *relation_show(struct Relation *rel);
struct Var_Decl *input_var(struct Relation *rel, int n);
struct Var_Decl *output_var(struct Relation *rel, int n);
struct Var_Decl *set_var(struct Relation *rel, int n);
int is_lower_bound_satisfiable(struct Relation *rel);
int is_upper_bound_satisfiable(struct Relation *rel);
int is_obvious_tautology(struct Relation *rel);
int is_definite_tautology(struct Relation *rel);
int is_exact(struct Relation *rel);
int is_inexact(struct Relation *rel);
int is_unknown(struct Relation *rel);

struct F_And *relation_add_and(struct Relation *rel);
struct Formula *relation_add_or(struct Relation *rel);
struct Formula *relation_add_not(struct Relation *rel);
struct F_Declaration *relation_add_forall(struct Relation *rel);
struct F_Declaration *relation_add_exists(struct Relation *rel);
void relation_finalize(struct Relation *rel);

struct F_And *formula_add_and(struct Formula *rel);
struct Formula *formula_add_or(struct Formula *rel);
struct Formula *formula_add_not(struct Formula *rel);
struct F_Declaration *formula_add_forall(struct Formula *rel);
struct F_Declaration *formula_add_exists(struct Formula *rel);
void formula_finalize(struct Formula *rel);

struct Var_Decl *declaration_declare(struct F_Declaration *rel);

struct F_And *formula_to_and(struct Formula *rel);

void add_constraint(struct F_And *formula,
		    int is_eq,
		    int num_vars,
		    int *coefficients,
		    struct Var_Decl **vars,
		    int constant);

struct DNF_Iterator *query_dnf(struct Relation *rel);
struct Conjunct *dnf_iterator_next(struct DNF_Iterator *iter);
void dnf_iterator_free(struct DNF_Iterator *iter);

struct Tuple_Iter *get_conjunct_variables(struct Conjunct *conj);
void *tuple_iterator_next(struct Tuple_Iter *iter);
void tuple_iterator_free(struct Tuple_Iter *iter);

struct EQ_Iterator *get_eqs(struct Conjunct *conj);
struct EQ_Handle *eqs_next(struct EQ_Iterator *g);
void eqs_free(struct EQ_Iterator *g);
void eq_handle_free(struct EQ_Handle *hdl);
struct Tuple_Iter *eq_get_coefficients(struct EQ_Handle *hdl);

struct GEQ_Iterator *get_geqs(struct Conjunct *conj);
struct GEQ_Handle *geqs_next(struct GEQ_Iterator *g);
void geqs_free(struct GEQ_Iterator *g);
void geq_handle_free(struct GEQ_Handle *hdl);

struct Constraint_Handle_;	/* Use a different name to get rid of C++ warning */
coefficient_t constraint_get_const(struct Constraint_Handle_ *hdl);
struct Constr_Vars_Iter *constraint_get_coefficients(struct Constraint_Handle_ *hdl);
int constr_vars_next(Variable_Info_struct *out, struct Constr_Vars_Iter *iter);
void constr_vars_free(struct Constr_Vars_Iter *iter);



void debug_print_eq(struct EQ_Handle *hdl);
void debug_print_geq(struct GEQ_Handle *hdl);

#ifdef __cplusplus
}
#endif

#endif
