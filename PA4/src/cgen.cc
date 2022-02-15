
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include <typeinfo>

#include "cgen.h"
#include "cgen_gc.h"

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

static CgenNodeP curr_class;
static int next_classtag;
static int next_label;
static SymbolTable<int,CgenNode> *class_nameTab;
static SymbolTable<Symbol,CgenNode> *CgenNode_Tab;
static mySymbolTable<Symbol,int> *attr_offset;
static mySymbolTable<Symbol,int> *method_offset;
static mySymbolTable<Symbol,int> *local_offset;

void init_global_data() 
{
  next_label = 0;
  class_nameTab = new SymbolTable<int,CgenNode>;
  CgenNode_Tab = new SymbolTable<Symbol,CgenNode>;
  attr_offset = new mySymbolTable<Symbol,int>;
  method_offset = new mySymbolTable<Symbol,int>;
  local_offset = new mySymbolTable<Symbol,int>;
  class_nameTab->enterscope();
  CgenNode_Tab->enterscope();

}

void release_global_data()
{
  class_nameTab->exitscope();
  CgenNode_Tab->exitscope();
}

int new_classtag() 
{
  int new_classtag = next_classtag;
  next_classtag++;
  return new_classtag;
}

int new_label()
{
  int new_label = next_label;
  next_label++;
  return new_label;  
}


//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
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
       val;
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

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  init_global_data();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);
  release_global_data();
  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_slt(char *dest, char *src1, char *src2, ostream& s)
{ s << SLT << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}


//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg, 0, SP, str);
  emit_addiu(SP, SP, -4, str);
}

//
// Pop the top of stack to a register. The stack shrinks towards greater addresses.
//
static void emit_pop(char *reg, ostream& str)
{
  emit_load(reg, 1, SP, str);
  emit_addiu(SP, SP, 4, str);
}



//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}

/*
  addiu	sp, sp -12
  sw    fp, 12(sp)
  sw 	  s0, 8(sp)
  sw	  ra, 4(sp)
  addiu	fp, sp 4
  mv    s0, a0
*/
static void emit_record_begin(ostream &s)
{
  emit_addiu(SP, SP, -12, s);
  emit_store(FP, 3, SP, s);
  emit_store(SELF, 2, SP, s);
  emit_store(RA, 1, SP, s);
  emit_addiu(FP, SP, 4, s);
  emit_move(SELF, ACC, s);
}

/*
  lw	  fp, 12(sp)
  lw	  s0, 8(sp)
  lw	  ra, 4(sp)
  addiu	sp, sp 12
  jr	  ra
*/		
static void emit_record_end(ostream &s)
{
  emit_load(FP, 3, SP, s);
  emit_load(SELF, 2, SP, s);
  emit_load(RA, 1, SP, s);
  emit_addiu(SP, SP, 12, s);
  emit_return(s);
}



///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD;
      
 /***** Add dispatch information for class String ******/
  emit_disptable_ref(Str, s);

      s << endl;                                              // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; 

 /***** Add dispatch information for class Int ******/
  emit_disptable_ref(Int, s);

      s << endl;                                          // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;

 /***** Add dispatch information for class Bool ******/
  emit_disptable_ref(Bool, s);

      s << endl;                                            // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}






//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}

void CgenClassTable::code_protobjs()
{
  for (auto l = nds; l!= NULL; l = l->tl()) {
    auto nd = l->hd();
    nd->code_protobj_def(str);
  }
}

void CgenClassTable::code_class_nameTab()
{
  str << CLASSNAMETAB << LABEL; 
  for (int i=0; i < next_classtag; i++) {
    auto nd = class_nameTab->probe(i);
    str << WORD;
    nd->class_name->code_ref(str); 
    str << endl;
  }
  
}

void CgenClassTable::code_disptables()
{
  for (auto l = nds; l!= NULL; l = l->tl()) {
    auto nd = l->hd();
    nd->code_disptable_def(str);
  } 
}

void CgenClassTable::code_class_objTab()
{
  str << CLASSOBJTAB << LABEL; 
  for (int i=0; i < next_classtag; i++) {
    auto nd = class_nameTab->probe(i);
    str << WORD; emit_protobj_ref(nd->get_name(), str); str << endl;
    str << WORD; emit_init_ref(nd->get_name(), str); str << endl;
  }
}

void CgenClassTable::code_inits()
{
  for (auto l = nds; l!= NULL; l = l->tl()) {
    auto nd = l->hd();
    curr_class = nd;
    nd->code_init_def(str);
  } 
}

void CgenClassTable::code_methods()
{
  for (auto l = nds; l!= NULL; l = l->tl()) {
    auto nd = l->hd();
    curr_class = nd;
    if (!nd->basic())
      nd->code_method_def(str);
  } 
}


CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
  intclasstag = 1;
  boolclasstag = 2;
  stringclasstag = 3;
  next_classtag = 0;

  enterscope();
  if (cgen_debug) cout << "# Building CgenClassTable" << endl;
  install_basic_classes();
  install_classes(classes);
  build_inheritance_tree();
  set_classtags();
  build_offset_table();

  code();
  exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
	new CgenNode(class_(No_class,No_class,nil_Features(),filename),
			    Basic,this));
  addid(SELF_TYPE,
	new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
			    Basic,this));
  addid(prim_slot,
	new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
			    Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
	   No_class,
	   append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
	   filename),
    Basic,this));


//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
	    Object,
            single_Features(attr(val, prim_slot, no_expr())),
	    filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
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
	     filename),
        Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
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
	   filename),	    
    Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  nd->class_name = stringtable.add_string(name->get_string());

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenClassTable::set_classtags()
{
  CgenNodeP nd = root();
  nd->set_tag();  
}

void CgenNode::set_tag()
{
  classtag = new_classtag();
  class_nameTab->addid(classtag, this);
  CgenNode_Tab->addid(name, this);
  for(auto l = children; l; l = l->tl()) {
    l->hd()->set_tag();
  }
  max_child_tag = next_classtag-1;
}

void CgenClassTable::build_offset_table()
{
  CgenNodeP nd = root();
  nd->set_ofs();
}

void CgenNode::set_ofs()
{
  attr_offset->enterscope();
  method_offset->enterscope();

  int ofs1 = parentnd->get_class_slots();
  int ofs2 = parentnd->get_method_num();

  for(int i = features->first(); features->more(i); i = features->next(i)) {
      auto feature = features->nth(i);
      if(typeid(*feature) == typeid(attr_class)) {
          auto attr = (attr_class *)feature;
          attr_offset->addid(attr->name, new int(ofs1+DEFAULT_OBJFIELDS));
          ofs1++; 
      }
      else {
          auto method = (method_class *)feature;
          if(method_offset->lookup(method->name) == NULL) {
              method_offset->addid(method->name, new int(ofs2));
              ofs2++;
          }
      }
  }
  class_slots = ofs1;
  method_num = ofs2;

  for(auto l = children; l; l = l->tl()) {
      l->hd()->set_ofs();
  }

  attr_ofs = new mySymbolTable<Symbol, int>;
  method_ofs = new mySymbolTable<Symbol, int>;
  attr_ofs->Copy(attr_offset);
  method_ofs->Copy(method_offset);

  attr_offset->exitscope();
  method_offset->exitscope();

}



void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}


void CgenClassTable::code()
{
  if (cgen_debug) cout << "# coding global data" << endl;
  code_global_data();

  if (cgen_debug) cout << "# choosing gc" << endl;
  code_select_gc();

  if (cgen_debug) cout << "# coding constants" << endl;
  code_constants();

  if (cgen_debug) cout << "# coding class name table" << endl;
  code_class_nameTab();                                         // - class_nameTab

  if (cgen_debug) cout << "# coding class object table" << endl;
  code_class_objTab();                                          // - class_objTab

  if (cgen_debug) cout << "# coding dispatch tables" << endl;
  code_disptables();                                            // - dispatch tables

  if (cgen_debug) cout << "# coding prototype objects" << endl; 
  code_protobjs();                                              // - prototype objects

  if (cgen_debug) cout << "# coding global text" << endl;
  code_global_text();

  if (cgen_debug) cout << "# coding object initializer" << endl; 
  code_inits();                                                // - object initializer

  if (cgen_debug) cout << "# coding class methods" << endl; 
  code_methods();                                              // - object initializer


}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
}

void CgenNode::code_protobj_def(ostream& s)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  emit_protobj_ref(name, s); s << LABEL                           // label
      << WORD << classtag << endl                                 // class tag
      << WORD << (DEFAULT_OBJFIELDS + class_slots) << endl  // object size
      << WORD;    
      emit_disptable_ref(name, s);                                // dispatch table
      s << endl;

  code_attr_proto(s);

}

void CgenNode::code_attr_proto(ostream& s)
{
  if (name == Object) {
    return;
  }
  parentnd->code_attr_proto(s);
  for (int i = features->first(); features->more(i); i = features->next(i)) {
    auto feature = features->nth(i);
    if (typeid(*feature) == typeid(attr_class)) {
      auto attr = (attr_class *)feature;
      auto type = attr->type_decl;

      auto int_default = inttable.add_int(0);
      auto str_default = stringtable.add_string("");
      s << WORD; 
        if (type == Int) { int_default->code_ref(s); }
        else if (type == Str) { str_default->code_ref(s); }
        else if (type == Bool) { falsebool.code_ref(s); }
        else { s << VOID; }
      s << endl;
    }    
  }
}

void CgenNode::code_disptable_def(ostream& s)
{
  emit_disptable_ref(name, s); s << LABEL;
  SymbolTable<Symbol,Entry> *override_table, *emitted;

  override_table = new SymbolTable<Symbol, Entry>;
  emitted = new SymbolTable<Symbol, Entry>;
  emitted->enterscope();
  override_table->enterscope();
  
  code_method_disp(s, override_table, emitted);

  override_table->exitscope();
  emitted->exitscope();
  free(override_table);
  free(emitted);
}

void CgenNode::code_method_disp(ostream& s, SymbolTable<Symbol,Entry> *ot, SymbolTable<Symbol,Entry> *em)
{
  if(name == No_class)  return;

  for (int i = features->first(); features->more(i); i = features->next(i)) {
    auto feature = features->nth(i);
    if (typeid(*feature) == typeid(method_class)) {
      auto meth = (method_class *)feature; 
      if(ot->probe(meth->name) == NULL) {
        ot->addid(meth->name, name);
      }   
    }    
  }

  parentnd->code_method_disp(s, ot, em);
  for (int i = features->first(); features->more(i); i = features->next(i)) {
    auto feature = features->nth(i);
    if (typeid(*feature) == typeid(method_class)) {
      auto meth = (method_class *)feature; 
      if(em->probe(meth->name) == NULL) {
        s << WORD;
        emit_method_ref(ot->probe(meth->name), meth->name, s);
        // s << " offset " << get_method_ofs(meth->name);
        s << endl;
        em->addid(meth->name, name);
      }
    }    
  }

}


/*
C_init:
  addiu	sp, sp -12
  sw    fp, 12(sp)
  sw 	  s0, 8(sp)
  sw	  ra, 4(sp)
  addiu	fp, sp 4
  mv    s0, a0

  mv    a0, T1_protObj
  cgen(init_1)
  sw    a0, 12(s0)

  mv    a0, T2_protObj
  cgen(init_2)
  sw    a0, 16(s0)
  ...
  cgen(init_n)
  ...
  mv    a0, s0

  jal   <parent classname>_init
  mv    a0, s0

  lw	  fp, 12(sp)
  lw	  s0, 8(sp)
  lw	  ra, 4(sp)
  addiu	sp, sp 12
  jr	  ra
*/	
void CgenNode::code_init_def(ostream& s)
{
  emit_init_ref(name,s); s << LABEL;
  emit_record_begin(s);

  if (name != Object) {
    s << JAL;
    emit_init_ref(parentnd->name, s);
    s << endl;
  }

  if (!basic()) {
    for (int i = features->first(); features->more(i); i = features->next(i)) {
      auto feature = features->nth(i);
      if (typeid(*feature) == typeid(attr_class)) {
        auto attr = (attr_class *)feature; 
        int ofs = get_attr_ofs(attr->name);
        Symbol T = attr->type_decl;
        if(T == SELF_TYPE) { emit_move(ACC, SELF, s); }
        else { emit_partial_load_address(ACC, s); emit_protobj_ref(T, s); s << endl; }
        attr->init->code(s);
        emit_store(ACC, ofs, SELF, s);
      }
    }
    emit_move(ACC, SELF, s);
  }

  emit_move(ACC, SELF, s);
  emit_record_end(s);
}

void CgenNode::code_method_def(ostream& s)
{
  local_offset->enterscope();
  for (int i = features->first(); features->more(i); i = features->next(i)) {
    auto feature = features->nth(i);
    if (typeid(*feature) == typeid(method_class)) {
      auto meth = (method_class *)feature; 
      emit_method_ref(name, meth->name, s);
      s << LABEL;
      meth->code(s);
    }
  }
  local_offset->exitscope();
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************

/*
C.m:
  addiu	sp, sp -12
  sw    fp, 12(sp)
  sw 	  s0, 8(sp)
  sw	  ra, 4(sp)
  addiu	fp, sp 4
  mv    s0, a0
  
  cgen(e)

  lw	  fp, 12(sp)
  lw	  s0, 8(sp)
  lw	  ra, 4(sp)
  addiu	sp, sp 12
  addiu sp, sp, 4*n
  jr	  ra
*/
void method_class::code(ostream &s) {
  local_offset->enterscope(); int ofs = formals->len()+2;

  for(int i = formals->first(); formals->more(i); i = formals->next(i)) {
    auto formal = (formal_class *)formals->nth(i);
    local_offset->addid(formal->name, new int(ofs));
    ofs--;
  }
  emit_record_begin(s);
  expr->code(s);

  emit_load(FP, 3, SP, s);
  emit_load(SELF, 2, SP, s);
  emit_load(RA, 1, SP, s);
  emit_addiu(SP, SP, 12, s);
  emit_addiu(SP, SP, 4*formals->len(), s);
  emit_return(s);
  local_offset->exitscope();
}

/*
attribute:
  cgen(e)
  sw    a0, ofs(s0)
local:
  cgen(e)
  mv    t1, fp
  lw    t1, 2(fp)    # repeat (layer) times
  sw    a0, ofs(t1) 
*/
void assign_class::code(ostream &s) {
  if (cgen_debug) cout << "# coding assign" << endl; 
  int layer, ofs;
  layer_offset<int> *lo = local_offset->lookup(name);

  expr->code(s);
  if (lo != NULL) {
    layer = lo->layer;
    ofs = *(lo->info);
    emit_move(T1, FP, s);
    for(int i = 0; i < layer; i++) {
      emit_load(T1, 2, T1, s);
    }
    emit_store(ACC, ofs, T1, s);
  }
  else {
    ofs = curr_class->get_attr_ofs(name);
    emit_store(ACC, ofs, SELF, s);
  } 
  if (cgen_debug) cout << "# coding assign end" << endl; 
}

/*
  cgen(e1)
  push(a0)
  ...
  cgen(en)
  push(a0)
  cgen(e)

  bne   $a0, $zero, start         # if void, _dispatch_abort
  la    $a0, str_const0
  li    $t1, line_number
  jal   _dispatch_abort
  b     end
start:
  la    t1, T_protObj
  lw    t1, 2(t1)                 # t1 gets address of dispatch table
  lw    t1, ofs(t1)               # t1 gets method entry
  jalr  t1                        # a0 contains pointer to obj(e)
end:
*/
void static_dispatch_class::code(ostream &s) {
  if (cgen_debug) cout << "# coding static dispatch" << endl; 
  int start = new_label();
  int end = new_label();
  Symbol T = type_name == SELF_TYPE? curr_class->get_name() : type_name;
  int ofs = CgenNode_Tab->probe(T)->get_method_ofs(name);

  for (int i=actual->first(); actual ->more(i); i=actual->next(i)){
    actual->nth(i)->code(s);
    emit_push(ACC,s);
  }
  expr->code(s);
  emit_bne(ACC, ZERO, start, s);
  emit_load_address(ACC, "str_const0", s);
  emit_load_imm(T1, line_number, s);
  emit_jal("_dispatch_abort", s);
  emit_branch(end, s); 

emit_label_def(start, s);
  emit_partial_load_address(T1, s); emit_protobj_ref(T, s); s << endl;
  emit_load(T1, DISPTABLE_OFFSET, T1, s);
  emit_load(T1, ofs, T1, s);
  emit_jalr(T1,s);
emit_label_def(end, s);
  if (cgen_debug) cout << "# coding static dispatch end" << endl; 
}

/*
  cgen(e1)
  push(a0)
  ...
  cgen(en)
  push(a0)
  cgen(e)               # a0 contains pointer to obj(e)

  bne   $a0, $zero, start         # if void, _dispatch_abort
  la    $a0, str_const0
  li    $t1, line_number
  jal   _dispatch_abort
  b     end
start:
  lw    t1, 2(a0)       # t1 gets address of dispatch table
  lw    t1, ofs(t1)     # t1 gets method entry
  jalr  t1
end:
*/
void dispatch_class::code(ostream &s) {
  if (cgen_debug) cout << "# coding dispatch" << endl; 
  int start = new_label();
  int end = new_label();
  Symbol T = expr->get_type(); T = T == SELF_TYPE? curr_class->get_name() : T;
  int ofs = CgenNode_Tab->probe(T)->get_method_ofs(name);

  for (int i=actual->first(); actual ->more(i); i=actual->next(i)){
    actual->nth(i)->code(s);
    emit_push(ACC,s);
  }
  expr->code(s);
  emit_bne(ACC, ZERO, start, s);
  emit_load_address(ACC, "str_const0", s);
  emit_load_imm(T1, line_number, s);
  emit_jal("_dispatch_abort", s);
  emit_branch(end, s); 

emit_label_def(start, s);
  emit_load(T1, DISPTABLE_OFFSET, ACC, s);
  emit_load(T1, ofs, T1, s);
  emit_jalr(T1,s);
emit_label_def(end, s);
  if (cgen_debug) cout << "# coding dispatch end" << endl; 
}

/*
  cgen(e1)
  lw    a0, 3(a0)
  beqz  a0, else
  cgen(e2)
  b     end
else:
  cgen(e3)
end:
*/
void cond_class::code(ostream &s) {
  if (cgen_debug) cout << "# coding cond" << endl; 
  int else_branch = new_label();
  int end = new_label();

  pred->code(s);
  emit_load(ACC, 3, ACC, s);
  emit_beqz(ACC, else_branch, s);
  then_exp->code(s);
  emit_branch(end, s);
emit_label_def(else_branch, s);  
  else_exp->code(s);
emit_label_def(end, s);
 if (cgen_debug) cout << "# coding cond end" << endl; 
}

/*
predcate:
  cgen(e1)
  lw    a0, 3(a0)
  beqz  a0, end
  cgen(e2)
  b     predicate
end:
*/
void loop_class::code(ostream &s) {
  if (cgen_debug) cout << "# coding loop" << endl; 
  int predicate = new_label();
  int end = new_label();
emit_label_def(predicate, s);
  pred->code(s);
  emit_load(ACC, 3, ACC, s);
  emit_beqz(ACC, end, s);
  body->code(s);
  emit_branch(predicate, s);
emit_label_def(end,s);
 if (cgen_debug) cout << "# coding loop end" << endl; 
}

/*
  cgen(e)
  bne   $a0, $zero, case_start        # if void, _case_abort2
  la    $a0, str_const0
  li    $t1, line_number
  jal   _case_abort2
  b     case_end

case_start:
  lw    $t2, 0($a0)                   # t2 contains expr's classtag                  
  # in the first loop:
  li    $t3, -1                       # t3 contains max possible tag
  blti  $t2, c_tag, case1        
  bgti  $t2, max_tag, case1           # if t2 < c_tag or t2 > max_tag, goto next case    
  bgti  $t3, c_tag, skip_change1      # if t3(max) > c_tag, skip update 
  li    $t3, c_tag                    # update t3(max)   
skip_change1:

case1:
  blti  $t2, c_tag, case2   
  bgti  $t2, max_tag, case2
  bgti  $t3, c_tag, skip_change2
  li    $t3, c_tag
skip_change2:
  
case2:
  ...
skip_change(n-1):                     # t3 contains max possible tag

  li   $t2, -1
  bne  $t3, $t2, expr_start          # if t3 = -1, _case_abort  
  jal  _case_abort

case_expr:  
  push($a0)
  record begin   

  li   $t2, c_tag
  bne  $t2, $t3, expr1                # if tag not match, goto next expr
  cgen(e1)
  b    expr_end


expr1:
  li   $t2, c_tag
  bne  $t2, $t3, expr2
  cgen(e1)
  b    case_end

...

expr(n-1):
  li   $t2, c_tag
  bne  $t2, $t3, expr(n)
  cgen(e1)
  b    case_end

expr(n):
expr_end:
  record end
  pop
case_end:

*/
void typcase_class::code(ostream &s) {
  if (cgen_debug) cout << "# coding typcase" << endl; 
  expr->code(s);
  
  int case_start = new_label();
  int case_end = new_label();
  int expr_start = new_label();
  int expr_end = new_label();
  
  emit_bne(ACC, ZERO, case_start, s);
  emit_load_address(ACC, "str_const0", s);
  emit_load_imm(T1, line_number, s);
  emit_jal("_case_abort2", s);
  emit_branch(case_end, s);

emit_label_def(case_start, s);
  emit_load(T2, 0, ACC, s);

  for (int i = cases->first(); cases->more(i); i = cases->next(i)){
    branch_class* c = (branch_class*)cases->nth(i);
    auto c_nd = CgenNode_Tab->probe(c->type_decl);
    auto c_tag = c_nd->get_classtag();
    auto max_child_tag = c_nd->get_max_child_tag();
    int next_case = new_label();
    int skip_change = new_label();
    if (i == 0){
      emit_load_imm(T3, -1, s);
    }

    emit_blti(T2, c_tag, next_case, s);
    emit_bgti(T2, max_child_tag, next_case, s);
    emit_bgti(T3, c_tag, skip_change ,s);
    emit_load_imm(T3, c_tag, s);
  emit_label_def(skip_change, s);
    
  emit_label_def(next_case,s);
  }

  emit_load_imm(T2, -1, s);
  emit_bne(T3, T2, expr_start, s);
  emit_jal("_case_abort", s);

emit_label_def(expr_start, s);

  emit_push(ACC, s);

  emit_addiu(SP, SP, -12, s);
  emit_store(FP, 3, SP, s);
  emit_store(SELF, 2, SP, s);
  emit_store(RA, 1, SP, s);
  emit_addiu(FP, SP, 4, s);
  local_offset->enterscope(); int ofs = 3;
  for (int i = cases->first(); cases->more(i); i=cases->next(i)){
    branch_class* c = (branch_class*)cases->nth(i);
    auto c_nd = CgenNode_Tab->probe(c->type_decl);
    auto c_tag = c_nd->get_classtag();
    int next_expr = new_label();
    local_offset->addid(c->name, new int(ofs));
    emit_load_imm(T2, c_tag, s);
    emit_bne(T2, T3, next_expr, s);
    c->expr->code(s);
    emit_branch(expr_end, s);
  emit_label_def(next_expr,s);
  }
emit_label_def(expr_end ,s);
  emit_load(FP, 3, SP, s);
  emit_load(SELF, 2, SP, s);
  emit_load(RA, 1, SP, s);
  emit_addiu(SP, SP, 12, s);
  emit_addiu(SP, SP, 4, s);
emit_label_def(case_end ,s);
 
  local_offset->exitscope();
  if (cgen_debug) cout << "# coding typcase end" << endl; 
}

/*
  cgen(e1)
  cgen(e2)
  ...
  cgen(en)
*/
void block_class::code(ostream &s) {
  if (cgen_debug) cout << "# coding block" << endl; 
  for (auto i=body->first();body ->more(i);i=body->next(i)){
    body->nth(i)->code(s);
  }
  if (cgen_debug) cout << "# coding block end" << endl; 
}

/*
SELF_TYPE:
  mv   a0, s0
OTHERL
  mv   a0, T_protObj    

  cgen(e1)
  push(a0)
  begin record
  cgen(e2)
  end record
  pop
*/
void let_class::code(ostream &s) {
  if (cgen_debug) cout << "# coding let" << endl; 
  // put classtag in a0
  if (type_decl == SELF_TYPE) {
    emit_move(ACC, SELF, s);
  }
  else {
    emit_partial_load_address(ACC, s);
    emit_protobj_ref(type_decl, s);
    s << endl;
  }
  init->code(s);
  emit_push(ACC, s);

  local_offset->enterscope(); int ofs = 3;
  local_offset->addid(identifier, new int(ofs));
  
  emit_addiu(SP, SP, -12, s);
  emit_store(FP, 3, SP, s);
  emit_store(SELF, 2, SP, s);
  emit_store(RA, 1, SP, s);
  emit_addiu(FP, SP, 4, s);
  body->code(s);
  emit_load(FP, 3, SP, s);
  emit_load(SELF, 2, SP, s);
  emit_load(RA, 1, SP, s);
  emit_addiu(SP, SP, 12, s);
  emit_addiu(SP, SP, 4, s);
  
  local_offset->exitscope(); 
  if (cgen_debug) cout << "# coding let end" << endl; 
}

/*    
  cgen(e1)
  lw    a0, 3(a0)
  push(a0)
  cgen(e2)
  lw    a0, 3(a0)
  pop(t1)
  add   a0, t1, a0
  push(a0)
  la     a0, Int_protObj        
  jal    Object.copy            # new obj pointer is put in a0
  jal    Int_init
  pop(t1)
  sw     t1, 3(a0)
*/
void plus_class::code(ostream &s) {
  if (cgen_debug) cout << "# coding plus" << endl; 
  e1->code(s);
  emit_load(ACC, 3, ACC, s);
  emit_push(ACC,s);
  e2->code(s);
  emit_load(ACC, 3, ACC, s);
  emit_pop(T1,s);
  emit_add(ACC, T1, ACC, s);
  emit_push(ACC, s);
  emit_load_address(ACC, "Int_protObj", s);
  emit_jal("Object.copy", s);
  emit_jal("Int_init", s);
  emit_pop(T1, s);
  emit_store(T1, 3, ACC, s);
  if (cgen_debug) cout << "# coding plus end" << endl;  
}

/*
  cgen(e1)
  lw    a0, 3(a0)
  push(a0)
  cgen(e2)
  lw    a0, 3(a0)
  pop(t1)
  sub   a0, t1, a0
  push(a0)
  la     a0, Int_protObj        
  jal    Object.copy            # new obj pointer is put in a0
  jal    Int_init
  pop(t1)
  sw     t1, 3(a0)
*/
void sub_class::code(ostream &s) {
  if (cgen_debug) cout << "# coding sub" << endl; 
  e1->code(s);
  emit_load(ACC, 3, ACC, s);
  emit_push(ACC,s);
  e2->code(s);
  emit_load(ACC, 3, ACC, s);
  emit_pop(T1,s);
  emit_sub(ACC, T1, ACC, s);
  emit_push(ACC, s);
  emit_load_address(ACC, "Int_protObj", s);
  emit_jal("Object.copy", s);
  emit_jal("Int_init", s);
  emit_pop(T1, s);
  emit_store(T1, 3, ACC, s); 
  if (cgen_debug) cout << "# coding sub end" << endl; 
}

/*
  cgen(e1)
  lw    a0, 3(a0)
  push(a0)
  cgen(e2)
  lw    a0, 3(a0)
  pop(t1)
  mul   a0, t1, a0
  push(a0)
  la     a0, Int_protObj        
  jal    Object.copy            # new obj pointer is put in a0
  jal    Int_init
  pop(t1)
  sw     t1, 3(a0)
*/
void mul_class::code(ostream &s) {
  if (cgen_debug) cout << "# coding mul" << endl; 
  e1->code(s);
  emit_load(ACC, 3, ACC, s);
  emit_push(ACC,s);
  e2->code(s);
  emit_load(ACC, 3, ACC, s);
  emit_pop(T1,s);
  emit_mul(ACC, T1, ACC, s);
  emit_push(ACC, s);
  emit_load_address(ACC, "Int_protObj", s);
  emit_jal("Object.copy", s);
  emit_jal("Int_init", s);
  emit_pop(T1, s);
  emit_store(T1, 3, ACC, s); 
  if (cgen_debug) cout << "# coding mul end" << endl; 
}

/*
  cgen(e1)
  lw    a0, 3(a0)
  push(a0)
  cgen(e2)
  lw    a0, 3(a0)
  pop(t1)
  div   a0, t1, a0
  push(a0)
  la     a0, Int_protObj        
  jal    Object.copy            # new obj pointer is put in a0
  jal    Int_init
  pop(t1)
  sw     t1, 3(a0)
*/
void divide_class::code(ostream &s) {
  if (cgen_debug) cout << "# coding divide" << endl; 
  e1->code(s);
  emit_load(ACC, 3, ACC, s);
  emit_push(ACC,s);
  e2->code(s);
  emit_load(ACC, 3, ACC, s);
  emit_pop(T1,s);
  emit_div(ACC, T1, ACC, s);
  emit_push(ACC, s);
  emit_load_address(ACC, "Int_protObj", s);
  emit_jal("Object.copy", s);
  emit_jal("Int_init", s);
  emit_pop(T1, s);
  emit_store(T1, 3, ACC, s); 
  if (cgen_debug) cout << "# coding divide end" << endl; 
}

/*
  cgen(e)
  lw     a0, 3(a0)
  neg    a0, a0
  push(a0)
  la     a0, Int_protObj        
  jal    Object.copy            # new obj pointer is put in a0
  jal    Int_init
  pop(t1)
  sw     t1, 3(a0)
*/
void neg_class::code(ostream &s) {
  if (cgen_debug) cout << "# coding neg" << endl; 
  e1->code(s);
  emit_load(ACC, 3, ACC, s);
  emit_neg(ACC, ACC, s);
  emit_push(ACC, s);
  emit_load_address(ACC, "Int_protObj", s);
  emit_jal("Object.copy", s);
  emit_jal("Int_init", s);
  emit_pop(T1, s);
  emit_store(T1, 3, ACC, s);
  if (cgen_debug) cout << "# coding neg end" << endl; 
}

/*
  cgen(e1)
  lw    a0, 3(a0)
  push(a0)
  cgen(e2)
  lw    a0, 3(a0)
  pop(t3)
  la    t1, bool_const0
  la    t2, bool_const1
  blt   t3, a0, return_true       # if val = 0, return bool_const1
  mv    a0, t1             
  j     end
return_true:
  mv    a0, t2
end:
*/
void lt_class::code(ostream &s) {
  if (cgen_debug) cout << "# coding lt" << endl; 
  int return_true = new_label();
  int end = new_label();
  e1->code(s);
  emit_load(ACC, 3, ACC, s);
  emit_push(ACC,s);
  e2->code(s);
  emit_load(ACC, 3, ACC, s);
  emit_pop(T3,s);
  emit_load_address(T1, "bool_const0", s);
  emit_load_address(T2, "bool_const1", s);
  emit_blt(T3, ACC, return_true, s);
  emit_move(ACC, T1, s);
  emit_branch(end, s);
emit_label_def(return_true, s);
  emit_move(ACC, T2, s);
emit_label_def(end, s);
  if (cgen_debug) cout << "# coding lt end" << endl; 
}

/*
  cgen(e1)
  push(a0)
  cgen(e2)
  mv    t2, a0
  pop(t1)
  la    a0, bool_const1
  la    a1, bool_const0
  beq   t1, t2, return_true
  jal   equality_test
return_true:
*/
void eq_class::code(ostream &s) {
  int return_true = new_label();
  if (cgen_debug) cout << "# coding eq" << endl; 
  e1->code(s);
  emit_push(ACC, s);
  e2->code(s);
  emit_move(T2, ACC, s);
  emit_pop(T1, s);
  emit_load_address(ACC, "bool_const1", s);
  emit_load_address(A1, "bool_const0", s);
  emit_beq(T1, T2, return_true, s);
  emit_jal("equality_test", s);
emit_label_def(return_true, s);
  if (cgen_debug) cout << "# coding eq end" << endl; 
}

/*
  cgen(e1)
  lw    a0, 3(a0)
  push(a0)
  cgen(e2)
  lw    a0, 3(a0)
  pop(t3)
  la    t1, bool_const0
  la    t2, bool_const1
  bleq  t3, a0, return_true       # if val = 0, return bool_const1
  mv    a0, t1             
  j     end
return_true:
  mv    a0, t2
end:
*/
void leq_class::code(ostream &s) {
  if (cgen_debug) cout << "# coding leq" << endl; 
  int return_true = new_label();
  int end = new_label();
  e1->code(s);
  emit_load(ACC, 3, ACC, s);
  emit_push(ACC,s);
  e2->code(s);
  emit_load(ACC, 3, ACC, s);
  emit_pop(T3,s);
  emit_load_address(T1, "bool_const0", s);
  emit_load_address(T2, "bool_const1", s);
  emit_bleq(T3, ACC, return_true, s);
  emit_move(ACC, T1, s);
  emit_branch(end, s);
emit_label_def(return_true, s);
  emit_move(ACC, T2, s);
emit_label_def(end, s);
  if (cgen_debug) cout << "# coding leq end" << endl; 
}

/*
  cgen(e)
  la    t1, bool_const0
  la    t2, bool_const1
  lw    a0, 3(a0)             # get the val
  beqz  a0, return_true       # if val = 0, return bool_const1
  mv    a0, t1             
  j     end
return_true:
  mv    a0, t2
end:
*/
void comp_class::code(ostream &s) {
  if (cgen_debug) cout << "# coding comp" << endl; 
  int return_true = new_label();
  int end = new_label();
  e1->code(s);
  emit_load_address(T1, "bool_const0", s);
  emit_load_address(T2, "bool_const1", s);
  emit_load(ACC, 3, ACC, s);
  emit_beqz(ACC, return_true, s);
  emit_move(ACC, T1, s);
  emit_branch(end, s);
  emit_label_def(return_true, s);
  emit_move(ACC, T2, s);
  emit_label_def(end, s);
  if (cgen_debug) cout << "# coding comp end" << endl; 
}

void int_const_class::code(ostream& s)  
{
  //
  // Need to be sure we have an IntEntry *, not an arbitrary Symbol
  //
  emit_load_int(ACC,inttable.lookup_string(token->get_string()),s);
}

void string_const_class::code(ostream& s)
{
  emit_load_string(ACC,stringtable.lookup_string(token->get_string()),s);
}

void bool_const_class::code(ostream& s)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

/*
SELF_TYPE
  la     t1, "class_objTab"
  lw     t2, 0(s0)               # get classtag
  sll    t2, t2, 3               # get protobj ofs
  add    t2, t2, t1              # get protobj address
  push(t2)
  lw     t2, 0(t2)               # get protobj
  mv     a0, t2                  # put protobj on a0      
  jal    Object.copy             # new obj pointer is put in a0
  pop(t2)
  addiu  t2, t2, 4               # get init address
  lw     t2, 0(t2)               # get init entry
  jalr   t2
OTHER
  la     a0, C_protObj
  jal    Object.copy
  jal    C_init
*/
void new__class::code(ostream &s) {
  if (cgen_debug) cout << "# coding new" << endl; 
  if(type_name == SELF_TYPE) {
    emit_load_address(T1, CLASSOBJTAB, s);
    emit_load(T2, TAG_OFFSET, SELF, s);
    emit_sll(T2, T2, 3, s);
    emit_add(T2, T2, T1, s);
    emit_push(T2, s);
    emit_load(T2, 0, T2, s);
    emit_move(ACC, T2, s);
    emit_jal("Object.copy", s);
    emit_pop(T2, s);
    emit_addiu(T2, T2, 4, s);
    emit_load(T2, 0, T2, s);
    emit_jalr(T2, s);
    if (cgen_debug) cout << "# coding new end" << endl; 
  }
  else{
    emit_partial_load_address(ACC, s);
    emit_protobj_ref(type_name, s);
    s << endl;
    emit_jal("Object.copy", s);
    s << JAL;
    emit_init_ref(type_name, s);
    s << endl;
  }
}

/*
  cgen(e)
  la    t1, bool_const0
  la    t2, bool_const1
  beqz  a0, return_true       # if pointer = VOID, return bool_const1
  mv    a0, t1             
  j     end
return_true:
  mv    a0, t2
end:
*/
void isvoid_class::code(ostream &s) {
  if (cgen_debug) cout << "# coding isvoid" << endl; 
  int return_true = new_label();
  int end = new_label();
  e1->code(s);
  emit_load_address(T1, "bool_const0", s);
  emit_load_address(T2, "bool_const1", s);
  emit_beqz(ACC, return_true, s);
  emit_move(ACC, T1, s);
  emit_branch(end, s);
emit_label_def(return_true, s);
  emit_move(ACC, T2, s);
emit_label_def(end, s);
  if (cgen_debug) cout << "# coding isvoid end" << endl; 
}

/*
  lw    a0, 0(a0)             # a0 gets classtag
  la    t1, int_tag
  la    t2, str_tag
  la    t3, bool_tag
  lw    t1, 0(t1)
  lw    t2, 0(t2)
  lw    t3, 0(t3)
  beq   a0, t1, int_default
  beq   a0, t2, str_default
  beq   a0, t1, bool_default
  li    a0, VOID
  b     end
int_default:
  la    a0, int_const0
  b     end
str_default:
  la    a0, str_const0
  b     end
bool_default:
  la    a0, bool_const0
end:
*/
void no_expr_class::code(ostream &s) {
  if (cgen_debug) cout << "# coding no_expr" << endl; 
  int int_default = new_label();
  int str_default = new_label();
  int bool_default = new_label();
  auto int_0 = inttable.add_int(0);
  auto str_0 = stringtable.add_string("");
  int end = new_label();
  emit_load(ACC, TAG_OFFSET, ACC, s);
  emit_load_address(T1, INTTAG, s);
  emit_load_address(T2, STRINGTAG, s);
  emit_load_address(T3, BOOLTAG, s);
  emit_load(T1, 0, T1, s);
  emit_load(T2, 0, T2, s);
  emit_load(T3, 0, T3, s);
  emit_beq(ACC, T1, int_default, s);
  emit_beq(ACC, T2, str_default, s);
  emit_beq(ACC, T3, bool_default, s);
  emit_load_imm(ACC, VOID, s);
  emit_branch(end, s);
emit_label_def(int_default, s);
  emit_partial_load_address(ACC, s); int_0->code_ref(s); s << endl;
  emit_branch(end, s);
emit_label_def(str_default, s);
  emit_partial_load_address(ACC, s); str_0->code_ref(s); s << endl;
  emit_branch(end, s);
emit_label_def(bool_default, s);
  emit_partial_load_address(ACC, s); falsebool.code_ref(s); s << endl;
emit_label_def(end, s);
  if (cgen_debug) cout << "# coding no_expr end" << endl; 
}

/*
self:
  mv    a0, s0
attribute:
  lw    a0, ofs(s0)
local:
  mv    t1, fp
  lw    t1, 2(fp)    # repeat (layer) times
  lw    a0, ofs(t1) 
*/
void object_class::code(ostream &s) {
  if (cgen_debug) cout << "# coding object" << endl; 
  int layer, ofs;
  if (name == self) {
    emit_move(ACC, SELF, s);
    if (cgen_debug) cout << "# coding object end" << endl; 
    return;
  }
  layer_offset<int> *lo = local_offset->lookup(name);
  if (lo != NULL) {
    layer = lo->layer;
    ofs = *(lo->info);
    emit_move(T1, FP, s);
    for(int i = 0; i < layer; i++) {
      emit_load(T1, 2, T1, s);
    }
    emit_load(ACC, ofs, T1, s);
  }
  else {
    ofs = curr_class->get_attr_ofs(name);
    emit_load(ACC, ofs, SELF, s);
  } 
  if (cgen_debug) cout << "# coding object end" << endl; 
}





