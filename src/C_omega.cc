
#include <omega.h>
#include <string.h>

#include "C_omega.h"

extern "C"
Relation *new_relation(int n_input, int n_output)
{
  return new Relation(n_input, n_output);
}

extern "C"
Relation *new_set(int n)
{
  return new Relation(n);
}

extern "C"
void free_relation(Relation *rel)
{
  delete rel;
}

extern "C"
char *relation_show(Relation *rel)
{
  return strdup((const char *)rel->print_with_subs_to_string());
}

extern "C"
int num_input_vars(Relation *rel)
{
  return rel->n_inp();
}

extern "C"
int num_output_vars(Relation *rel)
{
  return rel->n_out();
}

extern "C"
int num_set_vars(Relation *rel)
{
  return rel->n_set();
}

extern "C"
Var_Decl *input_var(Relation *rel, int n)
{
  return rel->input_var(n);
}

extern "C"
Var_Decl *output_var(Relation *rel, int n)
{
  return rel->output_var(n);
}
extern "C"
Var_Decl *set_var(Relation *rel, int n)
{
  return rel->set_var(n);
}

extern "C"
int is_lower_bound_satisfiable(Relation *rel)
{
  return rel->is_lower_bound_satisfiable();
}

extern "C"
int is_upper_bound_satisfiable(Relation *rel)
{
  return rel->is_lower_bound_satisfiable();
}

extern "C"
int is_obvious_tautology(Relation *rel)
{
  return rel->is_obvious_tautology();
}
extern "C"
int is_definite_tautology(Relation *rel)
{
  return rel->is_tautology();
}

extern "C"
int is_exact(Relation *rel)
{
  return rel->is_exact();
}

extern "C"
int is_inexact(Relation *rel)
{
  return rel->is_inexact();
}

extern "C"
int is_unknown(Relation *rel)
{
  return rel->is_unknown();
}

extern "C"
Relation *relation_union(Relation *r, Relation *s)
{
  return new Relation(Union(copy(*r), copy(*s)));
}

extern "C"
Relation *relation_intersection(Relation *r, Relation *s)
{
  return new Relation(Intersection(copy(*r), copy(*s)));
}

extern "C"
Relation *relation_composition(Relation *r, Relation *s)
{
  return new Relation(Composition(copy(*r), copy(*s)));
}

extern "C"
Relation *relation_restrict_domain(Relation *r, Relation *s)
{
  return new Relation(Restrict_Domain(copy(*r), copy(*s)));
}

extern "C"
Relation *relation_restrict_range(Relation *r, Relation *s)
{
  return new Relation(Restrict_Range(copy(*r), copy(*s)));
}

extern "C"
Relation *relation_domain(Relation *rel)
{
  return new Relation(Domain(copy(*rel)));
}

extern "C"
Relation *relation_range(Relation *rel)
{
  return new Relation(Range(copy(*rel)));
}

extern "C"
F_And *relation_add_and(Relation *rel)
{
  return rel->add_and();
}

extern "C"
Formula *relation_add_or(Relation *rel)
{
  return rel->add_or();
}

extern "C"
Formula *relation_add_not(Relation *rel)
{
  return rel->add_not();
}

extern "C"
F_Declaration *relation_add_forall(Relation *rel)
{
  return rel->add_forall();
}

extern "C"
F_Declaration *relation_add_exists(Relation *rel)
{
  return rel->add_exists();
}

extern "C"
void relation_finalize(Relation *rel)
{
  rel->finalize();
}

extern "C"
Var_Decl *declaration_declare(F_Declaration *rel)
{
  return rel->declare();
}

extern "C"
F_And *formula_to_and(Formula *rel)
{
  F_And *and_formula = dynamic_cast<F_And *>(rel);

  /* If the parameter is already an 'and', return it */
  if (and_formula) return and_formula;

  /* Otherwise add an 'and' */
  return rel->add_and();
}

extern "C"
F_And *formula_add_and(Formula *rel)
{
  return rel->add_and();
}

extern "C"
Formula *formula_add_or(Formula *rel)
{
  return rel->add_or();
}

extern "C"
Formula *formula_add_not(Formula *rel)
{
  return rel->add_not();
}

extern "C"
F_Declaration *formula_add_forall(Formula *rel)
{
  return rel->add_forall();
}

extern "C"
F_Declaration *formula_add_exists(Formula *rel)
{
  return rel->add_exists();
}

extern "C"
void formula_finalize(Formula *rel)
{
  rel->finalize();
}

/* add_constraint creates an equality or inequality constraint,
 * fills in the coefficients for each variable, and fills in the
 * constant term. */
extern "C"
void add_constraint(F_And *formula,
		    int is_eq,
		    int num_vars,
		    int *coefficients,
		    Var_Decl **vars,
		    int constant)
{
  Constraint_Handle *hdl = is_eq
    ? (Constraint_Handle *)new EQ_Handle(formula->add_EQ())
    : (Constraint_Handle *)new GEQ_Handle(formula->add_GEQ());

  /* Update each coefficient in the array */
  for (; num_vars; num_vars--)
    {
      int index = num_vars - 1;
      hdl->update_coef(vars[index], coefficients[index]);
    }

  /* Update the constant part of the constraint */
  hdl->update_const(constant);

  hdl->finalize();
  free(hdl);
}

/* Helper for the following routine */
static void
write_dimension_to_output(Relation **out, Relation &rel)
{
    /* Approximate so that existential vars are eliminated */
    Approximate(rel);

    /* Save into the output */
    *out = new Relation(copy(rel));
    (*out)->finalize();
}

/* For each output variable in rel, produce a relation whose constraints
 * mention only that output variable and input variables.
 */
extern "C"
void
separate_relation_dimensions(Relation **rel_out, Relation *rel)
{
  int n_outputs = rel->n_out();

  /* We keep a copy of rel in which the first few output variables are
   * projected out.  In iteration i, variables 0 .. i-1 are projected out
   * and variables i .. n_outputs-1 are still in effect.
   */
  Relation projected = copy(*rel);

  /* For all but the last output variable */
  for (int i = 0; i < n_outputs-1; i++) {
    /* Make a copy of 'projected' which wil be the output relation */
    Relation rel_i = copy(projected);

    /* Project out all variables i+1 .. n_outputs-1 */
    for (int j = i + 1; j < n_outputs - 1; j++)
      Project (rel_i, j+1, Output_Var);

    write_dimension_to_output(&rel_out[i], rel_i);

    /* Project another variable */
    Project(projected, i+1, Output_Var);
  }

  /* Last output */
  write_dimension_to_output(&rel_out[n_outputs-1], projected);
} 

/* These are all for inspecting a DNF formula */

extern "C"
DNF_Iterator *query_dnf(Relation *rel)
{
  return new DNF_Iterator(rel->query_DNF());
}

extern "C"
Conjunct *dnf_iterator_next(DNF_Iterator *iter)
{
  if (!iter->live()) return NULL;

  Conjunct *c = **iter;
  ++*iter;
  return c;
}

extern "C"
void dnf_iterator_free(DNF_Iterator *iter)
{
  delete iter;
}

/* Use to iterate over the tuple of the variables that are used in the
 * conjunct.  The variables obtained should not be freed. */
extern "C"
struct Tuple_Iter *get_conjunct_variables(Conjunct *conj)
{
  Tuple_Iterator<void *> *ti =
    reinterpret_cast<Tuple_Iterator<void *> *>
    (new Tuple_Iterator<Variable_ID>(*conj->variables()));
  return (struct Tuple_Iter *)ti;
}

extern "C"
void *
tuple_iterator_next(struct Tuple_Iter *iter)
{
  Tuple_Iterator<void *> *ti = (Tuple_Iterator<void *> *)iter;

  if (!ti->live()) return NULL;	// Exhausted?

  void *ret = (void *)**ti;
  ++*ti;
  return ret;
}

extern "C"
void
tuple_iterator_free(struct Tuple_Iter *iter)
{
  delete (Tuple_Iterator<void *> *)iter;
}

/* Use to iterate over the EQ constraints in a conjunct.  The constraints
 * obtained should be freed once you're done with them. */
extern "C"
struct EQ_Iterator *
get_eqs(Conjunct *conj)
{
  return new EQ_Iterator(conj->EQs());
}

extern "C"
struct EQ_Handle *
eqs_next(struct EQ_Iterator *g)
{
  if (!g->live()) return NULL;	// Exhausted?

  EQ_Handle *hdl = new EQ_Handle(**g);
  ++*g;
  return hdl;
}

extern "C"
void
eqs_free(struct EQ_Iterator *g)
{
  delete g;
}

extern "C"
void
eq_handle_free(struct EQ_Handle *hdl)
{
  delete hdl;
}

/* Use to iterate over the GEQ constraints in a conjunct.  Works like
 * get_eqs. */
extern "C"
struct GEQ_Iterator *get_geqs(Conjunct *conj)
{
  return new GEQ_Iterator(conj->GEQs());
}

extern "C"
struct GEQ_Handle *
geqs_next(struct GEQ_Iterator *g)
{
  if (!g->live()) return NULL;	// Exhausted?

  GEQ_Handle *hdl = new GEQ_Handle(**g);
  ++*g;
  return hdl;
}

extern "C"
void
geqs_free(struct GEQ_Iterator *g)
{
  delete g;
}

extern "C"
void
geq_handle_free(struct GEQ_Handle *hdl)
{
  delete hdl;
}

extern "C"
coefficient_t
constraint_get_const(struct Constraint_Handle_ *hdl)
{
  return ((struct Constraint_Handle *)hdl)->get_const();
}

extern "C"
Constr_Vars_Iter *
constraint_get_coefficients(struct Constraint_Handle_ *hdl)
{
  return new Constr_Vars_Iter(*(Constraint_Handle *)hdl);  
}

extern "C"
int
constr_vars_next(Variable_Info_struct *out, Constr_Vars_Iter *iter)
{
  if (!iter->live()) return 0;

  Variable_Info info(**iter);
  ++*iter;

  out->var = info.var;
  out->coef = info.coef;

  return 1;
}

extern "C"
void
constr_vars_free(Constr_Vars_Iter *iter)
{
  delete iter;
}

/* For debugging */

extern "C"
void
debug_print_eq(struct EQ_Handle *hdl)
{
  String s(hdl->print_to_string());
  puts(s);
}

extern "C"
void
debug_print_geq(struct GEQ_Handle *hdl)
{
  String s(hdl->print_to_string());
  puts(s);
}

/* Find an array element equal to v.  Return the element index,
 * or -1 if no element matches. */
static int
find_variable_index(Var_Decl *v, int num_vars, Var_Decl **vars)
{
  int n;
  for (n = 0; n < num_vars; n++) {
    if (v == vars[n]) return n;
  }
  return -1;
}
