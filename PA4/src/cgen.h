#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0
#define VOID 0

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;


template <class DAT>
class layer_offset
{
public:
   int layer;
   DAT* info;
   layer_offset(int l, DAT* i) { layer = l; info = i; }
};


template <class SYM, class DAT>
class mySymbolTable
{
   typedef SymtabEntry<SYM,DAT> ScopeEntry;
   typedef List<ScopeEntry> Scope;
   typedef List<Scope> ScopeList;
private:
   ScopeList  *tbl;
public:
   mySymbolTable(): tbl(NULL) { }     // create a new symbol table

   // Create pointer to current symbol table.
   mySymbolTable &operator =(const mySymbolTable &s) { tbl = s.tbl; return *this; }

   mySymbolTable Copy(mySymbolTable *s) { tbl = s->tbl; return *this; }

   void fatal_error(char * msg)
   {
      cerr << msg << "\n";
      exit(1);
   } 

   // Enter a new scope.  A symbol table is organized as a list of
   // lists.  The head of the list is the innermost scope, the tail
   // holds the outer scopes.  A scope must be entered before anything
   // can be added to the table.

   void enterscope()
   {
       // The cast of NULL is required for template instantiation to work
       // correctly.
      tbl = new ScopeList((Scope *) NULL, tbl);
   }

   // Pop the first scope off of the symbol table.
   void exitscope()
   {
      // It is an error to exit a scope that doesn't exist.
      if (tbl == NULL) {
         fatal_error("exitscope: Can't remove scope from an empty symbol table.");
      }
      tbl = tbl->tl();
   }

   // Add an item to the symbol table.
   ScopeEntry *addid(SYM s, DAT *i)
   {
      // There must be at least one scope to add a symbol.
      if (tbl == NULL) fatal_error("addid: Can't add a symbol without a scope.");
      ScopeEntry * se = new ScopeEntry(s,i);
      tbl = new ScopeList(new Scope(se, tbl->hd()), tbl->tl());
      return(se);
   }
   
   // Lookup an item through all scopes of the symbol table.  If found
   // it returns the associated information field, if not it returns
   // NULL.

   layer_offset<DAT> * lookup(SYM s)
   {
      layer_offset<DAT> *lo;
      int layer = 0;
      for(ScopeList *i = tbl; i != NULL; i=i->tl()) {
         for( Scope *j = i->hd(); j != NULL; j = j->tl()) {
            if (s == j->hd()->get_id()) {
               lo = new layer_offset<DAT>(layer, j->hd()->get_info());
               return lo;
            }
         }
         layer++;
      }
      return NULL;
   }

   // probe the symbol table.  Check the top scope (only) for the item
   // 's'.  If found, return the information field.  If not return NULL.
   DAT *probe(SYM s)
   {
      if (tbl == NULL) {
         fatal_error("probe: No scope in symbol table.");
      }
      for(Scope *i = tbl->hd(); i != NULL; i = i->tl()) {
         if (s == i->hd()->get_id()) {
            return(i->hd()->get_info());
         }
      }
      return(NULL);
   }

   // Prints out the contents of the symbol table  
   void dump()
   {
      for(ScopeList *i = tbl; i != NULL; i = i->tl()) {
         cerr << "\nScope: \n";
         for(Scope *j = i->hd(); j != NULL; j = j->tl()) {
            cerr << "  " << j->hd()->get_id() << endl;
         }
      }
   }
 
};



class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;


// The following methods emit code for
// constants and global declarations.

   void code_global_data();
   void code_global_text();
   void code_bools(int);
   void code_select_gc();
   void code_constants();

   void code_protobjs();
   void code_disptables();
   void code_class_nameTab();
   void code_class_objTab();
   void code_inits();
   void code_methods();

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
   void set_classtags();
   void build_offset_table();
public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
};


class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise
   int classtag, max_child_tag;
   int class_slots = 0;
   int method_num = 0;

public:
   StringEntryP class_name;
   mySymbolTable<Symbol, int> *attr_ofs;
   mySymbolTable<Symbol, int> *method_ofs;
   
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }

   
   int get_classtag() { return classtag; }
   int get_class_slots() { return class_slots; }
   int get_method_num() { return method_num; }

   void set_tag();
   void set_ofs();
   int get_attr_ofs(Symbol s) { return *(attr_ofs->lookup(s)->info); }
   int get_method_ofs(Symbol s) { return *(method_ofs->lookup(s)->info); }
   
   void code_protobj_def(ostream&);
   void code_attr_proto(ostream&);

   void code_disptable_def(ostream&);
   void code_method_disp(ostream&, SymbolTable<Symbol,Entry> *, SymbolTable<Symbol, Entry> *);
   
   void code_init_def(ostream&);
   void code_method_def(ostream&);

   int get_max_child_tag() { return max_child_tag; }

   

};

class BoolConst 
{
private: 
  int val;
public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};


