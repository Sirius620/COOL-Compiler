

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "semant.h"
#include "utilities.h"
#include <typeinfo>


extern int semant_debug;
extern char *curr_filename;

static Class_ Object_class, IO_class, Int_class, Bool_class, Str_class;

static class__class *curr_class;

static SymbolTable<Symbol, Entry> * Obj_env = new SymbolTable<Symbol, Entry>;


//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
    arg,
    arg2,
    Bool,
    concat,
    cool_abort,
    copy,
    Int,
    in_int,
    in_string,
    IO,
    length,
    Main,
    main_meth,
    No_class,
    No_type,
    Object,
    out_int,
    out_string,
    prim_slot,
    self,
    SELF_TYPE,
    Str,
    str_field,
    substr,
    type_name,
    val,
    meth_name;

//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
    arg         = idtable.add_string("arg");
    arg2        = idtable.add_string("arg2");
    Bool        = idtable.add_string("Bool");
    concat      = idtable.add_string("concat");
    cool_abort  = idtable.add_string("abort");
    copy        = idtable.add_string("copy");
    Int         = idtable.add_string("Int");
    in_int      = idtable.add_string("in_int");
    in_string   = idtable.add_string("in_string");
    IO          = idtable.add_string("IO");
    length      = idtable.add_string("length");
    Main        = idtable.add_string("Main");
    main_meth   = idtable.add_string("main");
    //   _no_class is a symbol that can't be the name of any 
    //   user-defined class.
    No_class    = idtable.add_string("_no_class");
    No_type     = idtable.add_string("_no_type");
    Object      = idtable.add_string("Object");
    out_int     = idtable.add_string("out_int");
    out_string  = idtable.add_string("out_string");
    prim_slot   = idtable.add_string("_prim_slot");
    self        = idtable.add_string("self");
    SELF_TYPE   = idtable.add_string("SELF_TYPE");
    Str         = idtable.add_string("String");
    str_field   = idtable.add_string("_str_field");
    substr      = idtable.add_string("substr");
    type_name   = idtable.add_string("type_name");
    val         = idtable.add_string("_val");
}

/*
    Constructor of classTable.
    Add basic classes to classes list.
    Build the inheritance tree(including basic classes) for classes.
    * Check:
    (1) basic classes cannot be redefined.
    (2) Int, Str and Bool cannot be inherited.
    (3) inheritance loop should not exist.
    (4) the Main class should exist.
 */
ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {

    install_basic_classes();
    classes = append_Classes(single_Classes(IO_class), classes);
    classes = append_Classes(single_Classes(Int_class), classes);
    classes = append_Classes(single_Classes(Bool_class), classes);
    classes = append_Classes(single_Classes(Str_class), classes);
    classes = append_Classes(single_Classes(Object_class), classes);
    this->classes = classes;
    int class_num = classes->len();

    for(int i = class_num-1; i >= 0; i--) {
        class__class *curr = (class__class *)(classes->nth(i));

        if (i < 5)  goto A;
        if (curr->get_name() == Object || curr->get_name() == IO || curr->get_name() == Int \
            || curr->get_name() == Bool || curr->get_name() == Str || curr->get_name() == SELF_TYPE \
            || curr->get_name() == No_class || curr->get_name() == No_type) {
            semant_error(curr) << "Redefinition of basic class " << curr->get_name()->get_string()\
                << "." << endl;
            continue;
        }

        if (curr->get_parent() == Int || curr->get_parent() == Str || curr->get_parent() == Bool\
            || curr->get_parent() == SELF_TYPE) {
            semant_error(curr) << "Class " << curr->get_name()->get_string()\
            << " cannot inherit class " << curr->get_parent()->get_string() << "." << endl;
            continue;
        }
A:       
        // if (curr->marked != -1) {  
        //     continue; 
        // }

        curr->marked = i;
        while (curr->get_parent() != No_class) {
            int j;
            for (j = 0; j < class_num; j++) {
                if(((class__class *)classes->nth(j))->get_name() == curr->get_parent()) {
                    curr->set_parent_class((class__class *)(classes->nth(j)));
                    curr = (class__class *)classes->nth(j);
                    if(curr->marked == i) {
                        curr = (class__class *)(classes->nth(i));
                        semant_error(curr) << "Class " << curr->get_name()->get_string() \
                            << ", or an ancestor of " << curr->get_name()->get_string() \
                            << ", is involved in an inheritance cycle." << endl;
                        goto outerLoop;
                    }
                    // else if(curr->marked == -1) {
                    //     ((class__class *)classes->nth(j))->marked = i;
                    // }
                    // else {
                    //     goto outerLoop;
                    // }
                    else 
                        ((class__class *)classes->nth(j))->marked = i;

                break;
                }
            }
            if (j == class_num) {
                semant_error(curr) << "Class " << curr->get_name()->get_string() \
                    << " inherits from an undefined class " << curr->get_parent()->get_string() << "." << endl;
                goto outerLoop;
            }
        }
outerLoop: continue;
    }


}



void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
    // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");
    
    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.
    
    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Object_class =
	class_(Object, 
	       No_class,
	       append_Features(
			       append_Features(
					       single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
					       single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
			       single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	       filename);

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    IO_class = 
	class_(IO, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       single_Features(method(out_string, single_Formals(formal(arg, Str)),
										      SELF_TYPE, no_expr())),
							       single_Features(method(out_int, single_Formals(formal(arg, Int)),
										      SELF_TYPE, no_expr()))),
					       single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
			       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
	       filename);  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Int_class =
	class_(Int, 
	       Object,
	       single_Features(attr(val, prim_slot, no_expr())),
	       filename);

    //
    // Bool also has only the "val" slot.
    //
    Bool_class =
	class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Str_class =
	class_(Str, 
	       Object,
	       append_Features(
			       append_Features(
					       append_Features(
							       append_Features(
									       single_Features(attr(val, Int, no_expr())),
									       single_Features(attr(str_field, prim_slot, no_expr()))),
							       single_Features(method(length, nil_Formals(), Int, no_expr()))),
					       single_Features(method(concat, 
								      single_Formals(formal(arg, Str)),
								      Str, 
								      no_expr()))),
			       single_Features(method(substr, 
						      append_Formals(single_Formals(formal(arg, Int)), 
								     single_Formals(formal(arg2, Int))),
						      Str, 
						      no_expr()))),
	       filename);

}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                             
    return semant_error(c->get_filename(),c);
}    

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
} 





/*   This is the entry point to the semantic checker.

     Your checker should do the following two things:

     1) Check that the program is semantically correct
     2) Decorate the abstract syntax tree with type information
        by setting the `type' field in each Expression node.
        (see `tree.h')

     You are free to first do 1), make sure you catch all semantic
     errors. Part 2) can be done in a second stage, when you want
     to build mycoolc.
 */
void program_class::semant()
{
    initialize_constants();
    /* ClassTable constructor may do some semantic analysis */
    ClassTable *classtable = new ClassTable(classes);

    // Stop parsing if cycle occurs
    if (classtable->errors()) {
	    cerr << "Compilation halted due to static semantic errors." << endl;
	    exit(1);
    }

    /* some semantic analysis code may go here */
    Obj_env->enterscope();

    Classes classes_ = classtable->get_classes();
    // First pass: build (1)symbol-class table and (2)method environment
    for(int i = classes_->first(); classes_->more(i); i = classes_->next(i)) {
        classtable->check_Class_(1, (class__class *)classes_->nth(i));
    }  

    if (classtable->sym2class(Main) == NULL) {
        classtable->semant_error() << "Class Main is not defined." << endl; 
    }

    // Second pass: type checking
    for(int i = 5; i < classes_->len(); i++) {
        classtable->check_Class_((class__class *)classes_->nth(i));
    }

    if (classtable->errors()) {
	    cerr << "Compilation halted due to static semantic errors." << endl;
	    exit(1);
    }

    Obj_env->exitscope();

}

/* 
    The first pass through a class.
    Construct method environment M_env.
    * Check:
    (1) class cannot be redefined.
    (2) main method should be defined in the Main class.
    (3) the method in IO cannot be redefined when a class inherits from IO.
 */
Symbol ClassTable::check_Class_(int i, Class__class *c) {

        int have_main = 0;
        Symbol name = ((class__class *)c)->get_name();
        Features features = ((class__class *)c)->get_features();

        curr_class = (class__class *)c;

        if(Obj_env->lookup(name) != NULL) {
            semant_error(c) << "Class " << name->get_string() \
            << " was previously defined." << endl;
        }
        else 
            Obj_env->addid(name, type_name);

        curr_class->mtl = new SymbolTable<Symbol, M_info>;
        curr_class->atl = new SymbolTable<Symbol, Entry>;
 
        curr_class->mtl->enterscope();
        curr_class->atl->enterscope();

        for(int i = features->first(); features->more(i); i = features->next(i)) {
            Feature f = features->nth(i);
            
            check_Feature(curr_class->mtl, curr_class->atl, f);  
            
            if(typeid(*f) == typeid(method_class) && ((method_class *)f)->get_name() == main_meth)
                have_main++;

            if(((class__class *)c)->get_parent() == IO && typeid(*f) == typeid(method_class)) 
                if(((method_class *)f)->get_name() == out_string \
                    || ((method_class *)f)->get_name() == out_int \
                    || ((method_class *)f)->get_name() == in_string \
                    || ((method_class *)f)->get_name() == in_int )
                    semant_error(c) << "the method in IO cannot be redefined: " << ((method_class *)f)->get_name()->get_string() << endl; 
        }

        if(((class__class *)c)->get_name() == Main && have_main == 0)
            semant_error(c) << "no main method in the Main class." << endl;

        return name;
   }

/*
    The second pass through a class.
 */
Symbol ClassTable::check_Class_(Class__class *c) {

      Symbol name = ((class__class *)c)->get_name();
      Features features = ((class__class *)c)->get_features();

      curr_class = (class__class *)c;

      Obj_env->enterscope();
      Obj_env->addid(self, SELF_TYPE);
      for(int i = features->first(); features->more(i); i = features->next(i)) {
         check_Feature(features->nth(i));
      }
      Obj_env->exitscope();

      return name;
   }


/*
    The first pass through a feature.
    Construct method environment M_env, attribute environment A_env.
    * Check:
    (1) feature cannot be redefined in a class.
    (2) main method should take no formal parameters.
    (3) "self" cannot be an attribute name.
 */
Symbol ClassTable::check_Feature(SymbolTable<Symbol, M_info> *mtl, SymbolTable<Symbol, Entry> *atl, Feature_class *f) {

    if(typeid(*f) == typeid(method_class)) 
    {   
        Symbol name = ((method_class *)f)->get_name();
        Formals formals = ((method_class *)f)->get_formals();
        Symbol return_type = ((method_class *)f)->get_return_type();
        Expression expr = ((method_class *)f)->get_expr();
        
        if (name == main_meth && formals->len() > 0)
            semant_error(curr_class->get_filename(), f) << "method main cannot take formal parameters." <<endl;
        M_info *m = new M_info; 

        m->len = formals->len()+1;
        m->name_list = new Symbol[m->len];
        m->type_list = new Symbol[m->len];
        
        for(int i = formals->first(); formals->more(i); i = formals->next(i)) {
            m->name_list[i] = ((formal_class *)formals->nth(i))->get_name();
            m->type_list[i] = ((formal_class *)formals->nth(i))->get_type_decl();
            if(m->name_list[i] == self) {
                semant_error(curr_class->get_filename(), f) << "'self' cannot be the name of a formal parameter." << endl;
                return Object;
            }
            if(m->type_list[i] == SELF_TYPE) {
                semant_error(curr_class->get_filename(), f) << "Formal parameter "\
                    << m->name_list[i] << " cannot have type SELF_TYPE." << endl;
                return Object;
            }
        }
        m->type_list[m->len-1] = return_type;

        if(mtl->lookup(name) != NULL)
            semant_error(curr_class->get_filename(), f) << "Method " << name->get_string() \
                << " is multiply defined." << endl;
        else
            mtl->addid(name, m);
        return return_type;
    }

    if(typeid(*f) == typeid(attr_class))
    {
        Symbol name = ((attr_class *)f)->get_name(); 
        Symbol type_decl = ((attr_class *)f)->get_type_decl();

        if(name == self)
            semant_error(curr_class->get_filename(), f) << "'self' cannot be the name of an attribute." << endl; 

        if(atl->lookup(name)!= NULL)
            semant_error(curr_class->get_filename(), f) << "Attribute " << name->get_string() \
                    << " is multiply defined in class." << endl;
        else
            atl->addid(name, type_decl);

        return type_decl;
   
    } 

    return NULL;    
}

/* 
    The second pass through a feature.
    * Check:
    (1) inherited attribute cannot be redefined.
    (2) inherited method must be redefined with the same types.
 */
Symbol ClassTable::check_Feature(Feature_class *f) {

    if(typeid(*f) == typeid(method_class)) {
        Symbol name = ((method_class *)f)->get_name();
        Formals formals = ((method_class *)f)->get_formals();
        Symbol return_type = ((method_class *)f)->get_return_type();
        Expression expr = ((method_class *)f)->get_expr();

        if(sym2class(return_type) == NULL) {
            semant_error(curr_class->get_filename(), f) << "Undefined return type " \
                << return_type->get_string() << " in method " << name->get_string() << "." << endl;
        }

        M_info *m1 = M(curr_class->get_name(), name);
        if(m1 != NULL) {
            M_info *m2 = curr_class->mtl->lookup(name);
            if(m1->len != m2->len) {
                semant_error(curr_class->get_filename(), f) \
                << "Incompatible number of formal parameters in redefined method "\
                << name->get_string() << "." << endl;
                return Object;
            }
            else {
                for(int i = 0; i < m1->len-1; i++) {
                    if(m1->type_list[i] != m2->type_list[i]) {
                        semant_error(curr_class->get_filename(), f) << "In redefined method " << name->get_string() \
                            << ", parameter type " << m2->type_list[i]->get_string() << " is different from original type "\
                            << m1->type_list[i]->get_string() << endl;
                    }
                }
                if(m1->type_list[m1->len-1] != m2->type_list[m2->len-1]) {
                    semant_error(curr_class->get_filename(), f) << "In redefined method " << name->get_string() \
                        << ", return type " << m2->type_list[m1->len-1]->get_string() << " is different from original return type "\
                        << m1->type_list[m2->len-1]->get_string() << "." << endl;
                }
            } 
        }

            
        Obj_env->enterscope();
        for(int i = formals->first(); formals->more(i); i = formals->next(i)) {
            check_Formal(formals->nth(i));
        }
        Symbol T = check_Expression(expr);
            
        if (!check_inheritance(T, return_type, f)) {
            semant_error(curr_class->get_filename(), f) << "Inferred return type " << T->get_string() \
                << " of method " << name->get_string() << " does not conform to declared return type " \
                << return_type->get_string() << "." << endl;
        }

        Obj_env->exitscope();
        return return_type;
    }

    if(typeid(*f) == typeid(attr_class))
    {
        Symbol name = ((attr_class *)f)->get_name(); 
        Symbol type_decl = ((attr_class *)f)->get_type_decl();
        Expression init = ((attr_class *)f)->get_init();

        if(sym2class(type_decl) == NULL) {
            semant_error(curr_class->get_filename(), f) << "Undefined declared type " \
                << type_decl->get_string() << " of attribute " << name->get_string() << "." << endl;
        }

        if(A(curr_class->get_parent(), name) != NULL)
            semant_error(curr_class->get_filename(), f) << "Attribute "<< name->get_string() \
                <<" is an attribute of an inherited class." << endl;
        else 
            if (Obj_env->probe(name) == NULL)
                Obj_env->addid(name, type_decl);

        Symbol T = check_Expression(init);

        if(!check_inheritance(T, type_decl, f)) {
            semant_error(curr_class->get_filename(), f) << "Inferred type " << T->get_string() \
                << " of initialization of " << name->get_string() << " does not conform to identifier's declared return type " \
                << type_decl->get_string() << "." << endl;
            return Object;
        }

        return T;
    }

    return NULL;
}

/*
    The second pass through a formal.
    Check if redefine a formal.
    Return the formal's type_decl.
 */
Symbol ClassTable::check_Formal(Formal_class *f) {
    Symbol name = ((formal_class *)f)->get_name();
    Symbol type_decl = ((formal_class *)f)->get_type_decl();

    if(Obj_env->probe(name) != NULL) 
        semant_error(curr_class->get_filename(), f) << "Formal parameter " \
            << name->get_string() << " is multiply defined." << endl;
    else
        Obj_env->addid(name, type_decl);
    return type_decl;
}

/*
    The second pass through a case.
 */
Symbol ClassTable::check_Case(Case_class *c) {
    Symbol name = ((branch_class *)c)->get_name();
    Symbol type_decl = ((branch_class *)c)->get_type_decl();
    Expression expr = ((branch_class *)c)->get_expr();

    if(name == self) {
        semant_error(curr_class->get_filename(), c) \
            << "'self' cannot be bound in a 'case' branch." << endl;
    }

    Obj_env->enterscope();
    Obj_env->addid(name, type_decl);
    Symbol return_type = check_Expression(expr);
    Obj_env->exitscope();

    return return_type;
} 

/* 
    The second pass through an expression.
 */
Symbol ClassTable::check_Expression(Expression_class *e) {

    if(typeid(*e) == typeid(assign_class)) 
    {
        Symbol name = ((assign_class *)e)->get_name();
        Expression expr = ((assign_class *)e)->get_expr();

        if(name == self) {
            semant_error(curr_class->get_filename(), e) << "Cannot assign to 'self'." << endl;
        }

        Symbol type = Obj_env->lookup(name);
        if(type == NULL) {
            type = A(curr_class->get_name(), name);
        }
        if(type == NULL) {
            semant_error(curr_class->get_filename(), e) << "Undeclared identifier "\
                << name->get_string() << "." << endl;
            e->set_type(Object); return Object;
        }

        Symbol T = check_Expression(expr);
        if(!check_inheritance(T, type, e)) {
            semant_error(curr_class->get_filename(), e) << "Type " << T->get_string() \
                << " of assigned expression does not conform to declared type " \
                << type->get_string() << " of identifier " << name->get_string() << "." << endl;
            e->set_type(Object); return Object;
        }

        e->set_type(T);
        return T;
    }

    if(typeid(*e) == typeid(static_dispatch_class))
    {
        Expression expr = ((static_dispatch_class *)e)->get_expr();
        Symbol type_name = ((static_dispatch_class *)e)->get_type_name();
        Symbol name = ((static_dispatch_class *)e)->get_name();
        Expressions actual = ((static_dispatch_class *)e)->get_actual();
        
        Symbol return_type;
        int err = 0;
        
        M_info *m = M(type_name, name);
        if(sym2class(type_name) == NULL) {
            semant_error(curr_class->get_filename(), e) << "Dispatch on undefined class "\
                << type_name->get_string() << "." << endl;
        }
        else if(m == NULL) {
            semant_error(curr_class->get_filename(), e) << "Dispatch to undefined method "\
                << name->get_string() << "." << endl; 
        }

        Symbol T0 = check_Expression(expr);
        if(!check_inheritance(T0, type_name, e))  {
            semant_error(curr_class->get_filename(), e) << "Expression type "\
                << T0->get_string() << " does not conform to declared static dispatch type " \
                << type_name->get_string() << "." << endl;
            err++;
        }

        Symbol *actual_type_list = new Symbol[actual->len()];
        for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
            actual_type_list[i] = check_Expression(actual->nth(i));    
        }

        if(m == NULL) { return_type = Object; goto done1; }

        if(m->len != actual->len()+1) {
            semant_error(curr_class->get_filename(), e) << "In call of method " << name->get_string() \
                << ", number of actual parameters does not conform to declared formal parameters." << endl;
            return_type = Object; goto done1;
        }

        for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
            if(!check_inheritance(actual_type_list[i], m->type_list[i], e)) {
                semant_error(curr_class->get_filename(), e) << "In call of method " << name->get_string() << ", type "\
                    << actual_type_list[i]->get_string() << " of parameter " << m->name_list[i] << " does not conform to declared type " \
                    << m->type_list[i]->get_string() << "." << endl;
            }
        }
        if(err) {return_type = Object; goto done1;}

        return_type = m->type_list[m->len-1] == SELF_TYPE? T0: m->type_list[m->len-1];

    done1:
        e->set_type(return_type);
        return return_type;   
   
    }

    if(typeid(*e) == typeid(dispatch_class))
    {
        Expression expr = ((dispatch_class *)e)->get_expr();
        Symbol name = ((dispatch_class *)e)->get_name();
        Expressions actual = ((dispatch_class *)e)->get_actual();

        Symbol return_type;
        int err = 0;

        Symbol T0 = check_Expression(expr);
        M_info *m = M(T0, name);
        if(sym2class(T0) == NULL) {
            semant_error(curr_class->get_filename(), e) << "Dispatch on undefined class "\
                << T0->get_string() << "." << endl;
        }
        else if(m == NULL) {
            semant_error(curr_class->get_filename(), e) << "Dispatch to undefined method "\
                << name->get_string() << "." << endl; 
        }

        Symbol *actual_type_list = new Symbol[actual->len()];
        for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
            actual_type_list[i] = check_Expression(actual->nth(i));    
        }

        if(m == NULL)  { return_type = Object; goto done2; }

        if(m->len != actual->len()+1) {
            semant_error(curr_class->get_filename(), e) << "In call of method " << name->get_string() \
                << ", number of actual parameters does not conform to declared formal parameters." << endl;
            return_type = Object; goto done2;
        }

        for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
            if(!check_inheritance(actual_type_list[i], m->type_list[i], e)) {
                semant_error(curr_class->get_filename(), e) << "In call of method " << name->get_string() << ", type "\
                    << actual_type_list[i]->get_string() << " of parameter " << m->name_list[i] << " does not conform to declared type " \
                    << m->type_list[i]->get_string() << "." << endl;
                    err++;
            }
        }
        // if(err) {return_type = Object; goto done2;}

        return_type = m->type_list[m->len-1] == SELF_TYPE? T0: m->type_list[m->len-1];

    done2:
        e->set_type(return_type);
        return return_type;   
    }

    if(typeid(*e) == typeid(cond_class)) 
    {
        Expression pred = ((cond_class *)e)->get_pred();
        Expression then_exp = ((cond_class *)e)->get_then_exp();
        Expression else_exp = ((cond_class *)e)->get_else_exp();

        Symbol return_type;
        Symbol T0 = check_Expression(pred);

        if(T0 != Bool) {
            semant_error(curr_class->get_filename(), e) << "Predicate of 'if' does not have type Bool." << endl;
        }

        Symbol T1 = check_Expression(then_exp);
        Symbol T2 = check_Expression(else_exp);
        return_type = get_lub(T1, T2, e);

        e->set_type(return_type);
        return return_type;
    
    }

    if(typeid(*e) == typeid(loop_class)) 
    {
        Expression pred = ((loop_class *)e)->get_pred();
        Expression body = ((loop_class *)e)->get_body();

        Symbol return_type = Object;
        Symbol T0 = check_Expression(pred);

        if(T0 != Bool) {
            semant_error(curr_class->get_filename(), e) << "Loop condition does not have type Bool." << endl;
        }

        Symbol T1 = check_Expression(body);

        e->set_type(return_type);
        return return_type;
   
    }

    if(typeid(*e) == typeid(typcase_class)) 
    {
        Expression expr = ((typcase_class *)e)->get_expr();
        Cases cases = ((typcase_class *)e)->get_cases();

        Symbol return_type;
        Symbol T0 = check_Expression(expr);

        SymbolTable<Symbol, Entry> *duplicate_type_check = new SymbolTable<Symbol, Entry>;
        Symbol *type_list = new Symbol[cases->len()];
        Symbol *return_type_list = new Symbol[cases->len()];

        duplicate_type_check->enterscope();
        for(int i = cases->first(); cases->more(i); i = cases->next(i)) {
            type_list[i] = ((branch_class *)cases->nth(i))->get_type_decl();
            if(duplicate_type_check->probe(type_list[i]) != NULL) {
                semant_error(curr_class->get_filename(), e) << "Duplicate branch "\
                    << type_list[i]->get_string() << " in case statement." << endl;
            }
            else {
                duplicate_type_check->addid(type_list[i], type_list[i]);
            }
            return_type_list[i] = check_Case(cases->nth(i));
            return_type = (i == 0)? return_type_list[i]: get_lub(return_type, return_type_list[i], e);
        }
        duplicate_type_check->exitscope();
        e->set_type(return_type);
        return return_type;
    }

    if(typeid(*e) == typeid(block_class)) 
    {
        Expressions body = ((block_class *)e)->get_body();
        Symbol return_type;
        for(int i = body->first(); body->more(i); i = body->next(i)) {
            return_type = check_Expression(body->nth(i));
        }
        e->set_type(return_type);
        return return_type;
   
    }

    if(typeid(*e) == typeid(let_class)) 
    {
        Symbol identifier = ((let_class *)e)->get_identifier();
        Symbol type_decl = ((let_class *)e)->get_type_decl();
        Expression init = ((let_class *)e)->get_init();
        Expression body = ((let_class *)e)->get_body();

        if(identifier == self) {
            semant_error(curr_class->get_filename(), e) << "'self' cannot be bound in a 'let' expression." << endl;
        }

        if(sym2class(type_decl) == NULL) {
            semant_error(curr_class->get_filename(), e) << "Class " << type_decl->get_string() \
                << " of let-bound identifier " << identifier->get_string() << " is undefined." << endl;
        }

        Symbol return_type;
        Symbol T1 = check_Expression(init);

        if(!check_inheritance(T1, type_decl, e)) {
            semant_error(curr_class->get_filename(), e) << "Inferred type " << T1->get_string() \
                << " of initialization of " << identifier->get_string() \
                << " does not conform to identifier's declared type " << type_decl->get_string() << "." << endl;
        }

        Obj_env->enterscope();
        Obj_env->addid(identifier, type_decl);
        Symbol T2 = check_Expression(body);
        Obj_env->exitscope();

        return_type = T2;
   
    done5:
        e->set_type(return_type);
        return return_type;
    }

    if(typeid(*e) == typeid(plus_class))
    {
        Expression e1 = ((plus_class *)e)->get_e1();
        Expression e2 = ((plus_class *)e)->get_e2();

        Symbol return_type;
        Symbol T1 = check_Expression(e1);
        Symbol T2 = check_Expression(e2);
        if(T1 != Int || T2 != Int) {
            semant_error(curr_class->get_filename(), e) << "non-Int arguments: " \
                << T1->get_string() << " + " << T2->get_string() << endl;
        }
        return_type = Int;
    done60:
        e->set_type(return_type);
        return return_type;
   
    }

    if(typeid(*e) == typeid(sub_class))
    {
        Expression e1 = ((sub_class *)e)->get_e1();
        Expression e2 = ((sub_class *)e)->get_e2();

        Symbol return_type;
        Symbol T1 = check_Expression(e1);
        Symbol T2 = check_Expression(e2);
        if(T1 != Int || T2 != Int) {
            semant_error(curr_class->get_filename(), e) << "non-Int arguments: " \
                << T1->get_string() << " - " << T2->get_string() << endl;
        }
        return_type = Int;
    done61:
        e->set_type(return_type);
        return return_type;
   
    }

    if(typeid(*e) == typeid(mul_class))
    {
        Expression e1 = ((mul_class *)e)->get_e1();
        Expression e2 = ((mul_class *)e)->get_e2();

        Symbol return_type;
        Symbol T1 = check_Expression(e1);
        Symbol T2 = check_Expression(e2);
        if(T1 != Int || T2 != Int) {
            semant_error(curr_class->get_filename(), e) << "non-Int arguments: " \
                << T1->get_string() << " * " << T2->get_string() << endl;
        }
        return_type = Int;
    done62:
        e->set_type(return_type);
        return return_type;
   
    }

    if(typeid(*e) == typeid(divide_class))
    {
        Expression e1 = ((divide_class *)e)->get_e1();
        Expression e2 = ((divide_class *)e)->get_e2();

        Symbol return_type;
        Symbol T1 = check_Expression(e1);
        Symbol T2 = check_Expression(e2);
        if(T1 != Int || T2 != Int) {
            semant_error(curr_class->get_filename(), e) << "non-Int arguments: " \
                << T1->get_string() << " / " << T2->get_string() << endl;
        }
        return_type = Int;
    done63:
        e->set_type(return_type);
        return return_type;
   
    }

    if(typeid(*e) == typeid(neg_class))
    {
        Expression e1 = ((neg_class *)e)->get_e1();

        Symbol return_type;
        Symbol T1 = check_Expression(e1);
        if(T1 != Int) {
            semant_error(curr_class->get_filename(), e) << "Argument of '~' has type "\
                << T1->get_string() << " instead of Int." << endl;
        }
        return_type = Int;
    done7:
        e->set_type(return_type);
        return return_type;
    }

    if(typeid(*e) == typeid(lt_class))
    {
        Expression e1 = ((lt_class *)e)->get_e1();
        Expression e2 = ((lt_class *)e)->get_e2();

        Symbol return_type;
        Symbol T1 = check_Expression(e1);
        Symbol T2 = check_Expression(e2);
        if(T1 != Int || T2 != Int) {
            semant_error(curr_class->get_filename(), e) << "non-Int arguments: "\
                << T1->get_string() << " < " << T2->get_string() << endl;
        }
        return_type = Bool;
    done80:
        e->set_type(return_type);
        return return_type;
    }

    if(typeid(*e) == typeid(leq_class))
    {
        Expression e1 = ((leq_class *)e)->get_e1();
        Expression e2 = ((leq_class *)e)->get_e2();

        Symbol return_type;
        Symbol T1 = check_Expression(e1);
        Symbol T2 = check_Expression(e2);
        if(T1 != Int || T2 != Int) {
            semant_error(curr_class->get_filename(), e) << "non-Int arguments: "\
                << T1->get_string() << " <= " << T2->get_string() << endl;
        }
        return_type = Bool;
    done81:
        e->set_type(return_type);
        return return_type;
    }

    if(typeid(*e) == typeid(eq_class))
    {
        Expression e1 = ((eq_class *)e)->get_e1();
        Expression e2 = ((eq_class *)e)->get_e2();

        Symbol return_type;
        Symbol T1 = check_Expression(e1);
        Symbol T2 = check_Expression(e2);
        if( ((T1 == Int || T1 == Bool || T1 == Str) || (T2 == Int || T2 == Bool || T2 == Str)) \
            && T1 != T2) {
            semant_error(curr_class->get_filename(), e) << "Illegal comparison with a basic type." << endl;
        }
        return_type = Bool;
    done9:
        e->set_type(return_type);
        return return_type;
    }

    if(typeid(*e) == typeid(comp_class)){
        Expression e1 = ((comp_class *)e)->get_e1();

        Symbol return_type;
        Symbol T1 = check_Expression(e1);
        if(T1 != Bool) {
            semant_error(curr_class->get_filename(), e) << "Argument of 'not' has type "\
                << T1->get_string() << " instead of Bool." << endl;
        }
        return_type = Bool;
    done10:
        e->set_type(return_type);
        return return_type;
    }

    if(typeid(*e) == typeid(int_const_class)){
        e->set_type(Int);
        return Int;
    }

    if(typeid(*e) == typeid(bool_const_class)){
        e->set_type(Bool);
        return Bool;
    }

    if(typeid(*e) == typeid(string_const_class)){
        e->set_type(Str);
        return Str;
    }

    if(typeid(*e) == typeid(new__class)){
        Symbol type_name = ((new__class *)e)->get_type_name();

        if(type_name == SELF_TYPE) {
            e->set_type(SELF_TYPE);
            return SELF_TYPE;
        }

        if(sym2class(type_name) == NULL) {
            semant_error(curr_class->get_filename(), e) << "'new' used with undefined class "\
                << type_name->get_string() << "." << endl;
            e->set_type(Object);
            return Object;
        }

        e->set_type(type_name);
        return type_name;
    }

    if(typeid(*e) == typeid(isvoid_class)){
        Expression e1 = ((isvoid_class *)e)->get_e1();
        Symbol T = check_Expression(e1);
        e->set_type(Bool);
        return Bool;
    }

    if(typeid(*e) == typeid(no_expr_class)){
        e->set_type(No_type);
        return No_type;
    }

    if(typeid(*e) == typeid(object_class)){
        Symbol name = ((object_class *)e)->get_name();
        if(name == self) {
            e->set_type(SELF_TYPE);
            return SELF_TYPE;
        }
        Symbol return_type = Obj_env->lookup(name);
        if(return_type == NULL) {
            return_type = A(curr_class->get_name(), name);
        }
        if(return_type == NULL) {
            semant_error(curr_class->get_filename(), e) << "Undeclared identifier "<< name->get_string() << "." << endl;
            return_type = Object;
        }

        e->set_type(return_type);
        return return_type;
   }

   return NULL;
}

/* 
    Convert a symbol to a class.
 */
class__class *ClassTable::sym2class(Symbol s) {

    if(s == SELF_TYPE) return (class__class *)curr_class;

    for(int i = classes->first(); classes->more(i); i = classes->next(i)) {
        if(((class__class *)classes->nth(i))->get_name() == s)
            return (class__class *)classes->nth(i);
    }
    return NULL;
    
}

/*
    Get the parameter number and typelist of method f in class T.
 */
M_info *ClassTable::M(Symbol T, Symbol f) {
    class__class *c = sym2class(T);
    if(c == NULL) return NULL;

    M_info *curr_m = NULL;
    M_info *parent_m = NULL;
    while(c != NULL) {
        if(c->mtl == NULL)   break; 
        parent_m = c->mtl->lookup(f);
        c = c->get_parent_class();
        if(parent_m != NULL)   
            curr_m = parent_m;
    }

    return curr_m;
   
} 

/*
    Get the type_decl of attribute a in class T.
 */
Symbol ClassTable::A(Symbol T, Symbol a) {
    class__class *c = sym2class(T);
    if(c == NULL) return NULL;

    Symbol curr_type = NULL;
    Symbol parent_type = NULL;
    while(c != NULL) {
        if(c->atl == NULL)    break; 
        parent_type = c->atl->lookup(a);
        c = c->get_parent_class();
        if(parent_type != NULL)
            curr_type = parent_type;    
    }
    return curr_type;
} 


/*
    Check if class A inherits class B.
 */
int ClassTable::check_inheritance(Symbol a, Symbol b, tree_node *t) 
{
    if(a == No_type) return 1;

    if(a == b) return 1;

    if(b == SELF_TYPE) {
        if(a == SELF_TYPE) return 1;
        else return 0;
    }

    if(sym2class(b) == NULL) return 1;

    a = (a == SELF_TYPE)? curr_class->get_name() : a;

    class__class *curr = sym2class(a);
    while(curr) {
        if(curr->get_name() == b)
        return 1;
        curr = curr->get_parent_class();
    }

    return 0;

}

/*
    Get the lub class of class A and class B.
 */
Symbol ClassTable::get_lub(Symbol a, Symbol b, tree_node *t) 
{
    if(a==b)    return a;

    a = (a == SELF_TYPE)? curr_class->get_name() : a;
    b = (b == SELF_TYPE)? curr_class->get_name() : b;

    class__class *A = sym2class(a);
    class__class *curr = A;
    while(curr) {
        curr->set_child_class(A);
        curr = curr->get_parent_class(); 
      }
    curr = sym2class(b);
    while(curr) {
        if(curr->get_child_class() == A) 
            return curr->get_name();
        curr = curr->get_parent_class();
    }
        
    semant_error(curr_class->get_filename(), t) << "error in get lub" << endl;
    return Object;
   
}






