#ifndef SEMANT_H_
#define SEMANT_H_

#include <assert.h>
#include <iostream>  
#include "cool-tree.h"
#include "stringtab.h"
#include "symtab.h"
#include "list.h"

#define TRUE 1
#define FALSE 0

class ClassTable;
typedef ClassTable *ClassTableP;

// This is a structure that may be used to contain the semantic
// information such as the inheritance graph.  You may use it or not as
// you like: it is only here to provide a container for the supplied
// methods.

class ClassTable {
private:
  int semant_errors;
  void install_basic_classes();
  ostream& error_stream;
  Classes classes;

public:
  ClassTable(Classes);
  int errors() { return semant_errors; }
  ostream& semant_error();
  ostream& semant_error(Class_ c);
  ostream& semant_error(Symbol filename, tree_node *t);
  Classes get_classes() { return classes; }

  class__class *sym2class(Symbol s);
  M_info *M(Symbol T, Symbol f);
  Symbol A(Symbol T, Symbol a);
  int check_inheritance(Symbol a, Symbol b, tree_node *t);
  Symbol get_lub(Symbol a, Symbol b, tree_node *t);

  Symbol check_Class_(int i, Class__class *c);
  Symbol check_Class_(Class__class *c);
  Symbol check_Feature(SymbolTable<Symbol, M_info> *mtl, SymbolTable<Symbol, Entry> *atl, Feature_class *f);
  Symbol check_Feature(Feature_class *f);
  Symbol check_Formal(Formal_class *f);
  Symbol check_Case(Case_class *c);
  Symbol check_Expression(Expression_class *e);

};



#endif
