/* A Bison parser, made by GNU Bison 3.7.5.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30705

/* Bison version string.  */
#define YYBISON_VERSION "3.7.5"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1


/* Substitute the variable and function names.  */
#define yyparse         cool_yyparse
#define yylex           cool_yylex
#define yyerror         cool_yyerror
#define yydebug         cool_yydebug
#define yynerrs         cool_yynerrs
#define yylval          cool_yylval
#define yychar          cool_yychar

/* First part of user prologue.  */
#line 6 "cool.y"

#include <iostream>
#include "cool-tree.h"
#include "stringtab.h"
#include "utilities.h"

/* Add your own C declarations here */


/************************************************************************/
/*                DONT CHANGE ANYTHING IN THIS SECTION                  */

extern int yylex();           /* the entry point to the lexer  */
extern int curr_lineno;
extern char *curr_filename;
Program ast_root;            /* the result of the parse  */
Classes parse_results;       /* for use in semantic analysis */
int omerrs = 0;              /* number of errors in lexing and parsing */

/*
   The parser will always call the yyerror function when it encounters a parse
   error. The given yyerror implementation (see below) justs prints out the
   location in the file where the error was found. You should not change the
   error message of yyerror, since it will be used for grading puproses.
*/
void yyerror(const char *s);

/*
   The VERBOSE_ERRORS flag can be used in order to provide more detailed error
   messages. You can use the flag like this:

     if (VERBOSE_ERRORS)
       fprintf(stderr, "semicolon missing from end of declaration of class\n");

   By default the flag is set to 0. If you want to set it to 1 and see your
   verbose error messages, invoke your parser with the -v flag.

   You should try to provide accurate and detailed error messages. A small part
   of your grade will be for good quality error messages.
*/
extern int VERBOSE_ERRORS;


#line 122 "cool.tab.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

#include "cool.tab.h"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_CLASS = 3,                      /* CLASS  */
  YYSYMBOL_ELSE = 4,                       /* ELSE  */
  YYSYMBOL_FI = 5,                         /* FI  */
  YYSYMBOL_IF = 6,                         /* IF  */
  YYSYMBOL_IN = 7,                         /* IN  */
  YYSYMBOL_INHERITS = 8,                   /* INHERITS  */
  YYSYMBOL_LET = 9,                        /* LET  */
  YYSYMBOL_LOOP = 10,                      /* LOOP  */
  YYSYMBOL_POOL = 11,                      /* POOL  */
  YYSYMBOL_THEN = 12,                      /* THEN  */
  YYSYMBOL_WHILE = 13,                     /* WHILE  */
  YYSYMBOL_CASE = 14,                      /* CASE  */
  YYSYMBOL_ESAC = 15,                      /* ESAC  */
  YYSYMBOL_OF = 16,                        /* OF  */
  YYSYMBOL_DARROW = 17,                    /* DARROW  */
  YYSYMBOL_NEW = 18,                       /* NEW  */
  YYSYMBOL_ISVOID = 19,                    /* ISVOID  */
  YYSYMBOL_STR_CONST = 20,                 /* STR_CONST  */
  YYSYMBOL_INT_CONST = 21,                 /* INT_CONST  */
  YYSYMBOL_BOOL_CONST = 22,                /* BOOL_CONST  */
  YYSYMBOL_TYPEID = 23,                    /* TYPEID  */
  YYSYMBOL_OBJECTID = 24,                  /* OBJECTID  */
  YYSYMBOL_ASSIGN = 25,                    /* ASSIGN  */
  YYSYMBOL_NOT = 26,                       /* NOT  */
  YYSYMBOL_LE = 27,                        /* LE  */
  YYSYMBOL_ERROR = 28,                     /* ERROR  */
  YYSYMBOL_29_ = 29,                       /* '<'  */
  YYSYMBOL_30_ = 30,                       /* '='  */
  YYSYMBOL_31_ = 31,                       /* '+'  */
  YYSYMBOL_32_ = 32,                       /* '-'  */
  YYSYMBOL_33_ = 33,                       /* '*'  */
  YYSYMBOL_34_ = 34,                       /* '/'  */
  YYSYMBOL_35_ = 35,                       /* '~'  */
  YYSYMBOL_36_ = 36,                       /* '@'  */
  YYSYMBOL_37_ = 37,                       /* '.'  */
  YYSYMBOL_38_ = 38,                       /* '{'  */
  YYSYMBOL_39_ = 39,                       /* '}'  */
  YYSYMBOL_40_ = 40,                       /* ';'  */
  YYSYMBOL_41_ = 41,                       /* ':'  */
  YYSYMBOL_42_ = 42,                       /* ','  */
  YYSYMBOL_43_ = 43,                       /* '('  */
  YYSYMBOL_44_ = 44,                       /* ')'  */
  YYSYMBOL_YYACCEPT = 45,                  /* $accept  */
  YYSYMBOL_program = 46,                   /* program  */
  YYSYMBOL_class_list = 47,                /* class_list  */
  YYSYMBOL_class = 48,                     /* class  */
  YYSYMBOL_formal = 49,                    /* formal  */
  YYSYMBOL_formal_list = 50,               /* formal_list  */
  YYSYMBOL_dummy_formal_list = 51,         /* dummy_formal_list  */
  YYSYMBOL_normal_formal_list = 52,        /* normal_formal_list  */
  YYSYMBOL_feature_list = 53,              /* feature_list  */
  YYSYMBOL_dummy_feature_list = 54,        /* dummy_feature_list  */
  YYSYMBOL_normal_feature_list = 55,       /* normal_feature_list  */
  YYSYMBOL_feature = 56,                   /* feature  */
  YYSYMBOL_expression_list = 57,           /* expression_list  */
  YYSYMBOL_dummy_expression_list = 58,     /* dummy_expression_list  */
  YYSYMBOL_normal_experssion_list = 59,    /* normal_experssion_list  */
  YYSYMBOL_expression_list_ = 60,          /* expression_list_  */
  YYSYMBOL_dummy_expression_list_ = 61,    /* dummy_expression_list_  */
  YYSYMBOL_normal_experssion_list_ = 62,   /* normal_experssion_list_  */
  YYSYMBOL_inlet = 63,                     /* inlet  */
  YYSYMBOL_let_ = 64,                      /* let_  */
  YYSYMBOL_case = 65,                      /* case  */
  YYSYMBOL_case_list = 66,                 /* case_list  */
  YYSYMBOL_expression = 67                 /* expression  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_uint8 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                            \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if !defined yyoverflow

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* !defined yyoverflow */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  9
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   344

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  45
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  23
/* YYNRULES -- Number of rules.  */
#define YYNRULES  67
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  154

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   284


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      43,    44,    33,    31,    42,    32,    37,    34,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    41,    40,
      29,    30,     2,     2,    36,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,    38,     2,    39,    35,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,     2
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   125,   125,   129,   131,   137,   140,   142,   145,   155,
     161,   163,   169,   173,   175,   181,   183,   189,   193,   195,
     200,   202,   204,   206,   213,   215,   221,   225,   227,   233,
     235,   241,   245,   247,   249,   257,   258,   263,   265,   267,
     274,   278,   280,   286,   288,   290,   292,   294,   296,   298,
     300,   302,   304,   306,   308,   310,   312,   314,   316,   318,
     320,   322,   324,   326,   328,   330,   332,   334
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if YYDEBUG || 0
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "CLASS", "ELSE", "FI",
  "IF", "IN", "INHERITS", "LET", "LOOP", "POOL", "THEN", "WHILE", "CASE",
  "ESAC", "OF", "DARROW", "NEW", "ISVOID", "STR_CONST", "INT_CONST",
  "BOOL_CONST", "TYPEID", "OBJECTID", "ASSIGN", "NOT", "LE", "ERROR",
  "'<'", "'='", "'+'", "'-'", "'*'", "'/'", "'~'", "'@'", "'.'", "'{'",
  "'}'", "';'", "':'", "','", "'('", "')'", "$accept", "program",
  "class_list", "class", "formal", "formal_list", "dummy_formal_list",
  "normal_formal_list", "feature_list", "dummy_feature_list",
  "normal_feature_list", "feature", "expression_list",
  "dummy_expression_list", "normal_experssion_list", "expression_list_",
  "dummy_expression_list_", "normal_experssion_list_", "inlet", "let_",
  "case", "case_list", "expression", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_int16 yytoknum[] =
{
       0,   256,   284,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,    60,
      61,    43,    45,    42,    47,   126,    64,    46,   123,   125,
      59,    58,    44,    40,    41
};
#endif

#define YYPACT_NINF (-112)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-34)

#define yytable_value_is_error(Yyn) \
  ((Yyn) == YYTABLE_NINF)

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      26,   -29,     0,  -112,    34,    12,  -112,     9,    -1,  -112,
    -112,  -112,    28,     1,    29,    19,   -19,    25,  -112,  -112,
       2,     1,  -112,    45,    46,    31,  -112,    33,    -4,    36,
      41,    43,  -112,  -112,  -112,    35,   141,  -112,    62,    46,
      48,  -112,   141,     4,   141,   141,    67,   141,  -112,  -112,
    -112,    -5,   141,   141,    60,   141,   261,  -112,  -112,    71,
     208,    55,    61,  -112,   185,   220,  -112,    -6,   141,   141,
     296,    -6,    70,    73,  -112,  -112,   273,   231,   141,   141,
     141,   141,   141,   141,   141,    93,    94,  -112,    79,   141,
       4,    96,   141,    97,   296,    80,  -112,  -112,   247,   114,
    -112,    87,  -112,   307,   307,   307,    21,    21,    -6,    -6,
      92,    88,   141,   160,  -112,   112,   196,    98,   101,   127,
    -112,   141,  -112,  -112,   119,   141,   285,   141,   141,    -3,
    -112,   121,    97,  -112,  -112,   102,   104,   106,   172,   296,
     141,     4,   134,  -112,   141,  -112,  -112,  -112,   296,  -112,
     141,   109,   296,  -112
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       0,     0,     0,     8,     0,     0,     3,     0,     0,     1,
       4,     7,     0,     0,     0,     0,     0,     0,    15,    16,
       0,     0,    23,     0,    12,     0,    18,     0,     0,     0,
      14,     0,    10,    11,     5,     0,     0,    22,     0,     0,
       0,     6,     0,     0,     0,     0,     0,     0,    66,    65,
      67,    64,     0,     0,     0,     0,     0,     9,    13,     0,
       0,     0,     0,    50,     0,     0,    52,    53,     0,    26,
      62,    58,     0,     0,    29,    30,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    21,     0,     0,
       0,     0,     0,     0,    43,     0,    24,    25,    28,     0,
      49,     0,    63,    60,    59,    61,    54,    55,    56,    57,
       0,     0,     0,     0,    39,    35,     0,     0,     0,     0,
      46,     0,    34,    32,     0,    26,     0,     0,     0,     0,
      48,     0,    41,    51,    27,     0,     0,     0,     0,    36,
       0,     0,     0,    42,    26,    45,    20,    47,    37,    38,
       0,     0,    40,    44
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -112,  -112,  -112,   151,  -112,  -112,  -112,   129,   137,  -112,
     146,  -112,  -111,  -112,    49,  -112,  -112,   -39,  -112,   -89,
    -112,    37,   -36
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_uint8 yydefgoto[] =
{
       0,     4,     5,     6,    30,    31,    32,    33,    17,    18,
      19,    20,    95,    96,    97,    73,    74,    75,   129,    63,
     118,   119,    98
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      56,   114,    15,    15,   140,    61,    60,    12,    64,    65,
       7,    67,    -2,     1,   136,     2,    70,    71,    76,    77,
      68,    36,    23,     8,    24,    16,    16,     1,    62,     2,
      85,    86,    94,   151,     9,     3,    37,    13,    69,   141,
     -17,   -19,   103,   104,   105,   106,   107,   108,   109,     3,
      11,    14,   149,   113,    83,    84,   116,    85,    86,    22,
     122,    72,   123,    76,    25,    76,    42,    21,    28,    43,
      29,    34,    35,    44,    45,    41,   126,    38,    46,    47,
      48,    49,    50,    39,    51,    57,    52,    40,    72,    59,
      66,   138,   139,    42,    88,    53,    43,    90,    54,   -31,
      44,    45,    91,    55,   148,    46,    47,    48,    49,    50,
      99,    51,   100,    52,   152,    72,   110,   112,   111,   115,
      42,   117,    53,    43,   120,    54,   -33,    44,    45,   124,
      55,   125,    46,    47,    48,    49,    50,   128,    51,   131,
      52,   132,   133,   135,   142,   144,   146,    42,   145,    53,
      43,   150,    54,   153,    44,    45,    10,    55,    27,    46,
      47,    48,    49,    50,   127,    51,    26,    52,    58,   143,
     134,     0,     0,     0,     0,     0,    53,   147,     0,    54,
       0,     0,     0,     0,    55,     0,     0,    78,     0,    79,
      80,    81,    82,    83,    84,    92,    85,    86,     0,    78,
       0,    79,    80,    81,    82,    83,    84,   130,    85,    86,
       0,     0,    78,     0,    79,    80,    81,    82,    83,    84,
      89,    85,    86,    78,     0,    79,    80,    81,    82,    83,
      84,     0,    85,    86,     0,    78,    93,    79,    80,    81,
      82,    83,    84,     0,    85,    86,     0,    78,     0,    79,
      80,    81,    82,    83,    84,     0,    85,    86,    78,     0,
      79,    80,    81,    82,    83,    84,     0,    85,    86,     0,
       0,     0,     0,     0,    78,   102,    79,    80,    81,    82,
      83,    84,     0,    85,    86,     0,     0,     0,    78,   121,
      79,    80,    81,    82,    83,    84,     0,    85,    86,     0,
      78,    87,    79,    80,    81,    82,    83,    84,     0,    85,
      86,     0,    78,   101,    79,    80,    81,    82,    83,    84,
       0,    85,    86,    78,   137,    79,    80,    81,    82,    83,
      84,     0,    85,    86,   -34,     0,   -34,   -34,    81,    82,
      83,    84,     0,    85,    86
};

static const yytype_int16 yycheck[] =
{
      36,    90,     1,     1,     7,     1,    42,     8,    44,    45,
      39,    47,     0,     1,   125,     3,    52,    53,    54,    55,
      25,    25,    41,    23,    43,    24,    24,     1,    24,     3,
      36,    37,    68,   144,     0,    23,    40,    38,    43,    42,
      39,    39,    78,    79,    80,    81,    82,    83,    84,    23,
      41,    23,   141,    89,    33,    34,    92,    36,    37,    40,
      99,     1,   101,    99,    39,   101,     6,    38,    23,     9,
      24,    40,    39,    13,    14,    40,   112,    41,    18,    19,
      20,    21,    22,    42,    24,    23,    26,    44,     1,    41,
      23,   127,   128,     6,    23,    35,     9,    42,    38,    39,
      13,    14,    41,    43,   140,    18,    19,    20,    21,    22,
      40,    24,    39,    26,   150,     1,    23,    38,    24,    23,
       6,    24,    35,     9,    44,    38,    39,    13,    14,    37,
      43,    43,    18,    19,    20,    21,    22,    25,    24,    41,
      26,    40,    15,    24,    23,    43,    40,     6,    44,    35,
       9,    17,    38,    44,    13,    14,     5,    43,    21,    18,
      19,    20,    21,    22,     4,    24,    20,    26,    39,   132,
     121,    -1,    -1,    -1,    -1,    -1,    35,     5,    -1,    38,
      -1,    -1,    -1,    -1,    43,    -1,    -1,    27,    -1,    29,
      30,    31,    32,    33,    34,    10,    36,    37,    -1,    27,
      -1,    29,    30,    31,    32,    33,    34,    11,    36,    37,
      -1,    -1,    27,    -1,    29,    30,    31,    32,    33,    34,
      12,    36,    37,    27,    -1,    29,    30,    31,    32,    33,
      34,    -1,    36,    37,    -1,    27,    16,    29,    30,    31,
      32,    33,    34,    -1,    36,    37,    -1,    27,    -1,    29,
      30,    31,    32,    33,    34,    -1,    36,    37,    27,    -1,
      29,    30,    31,    32,    33,    34,    -1,    36,    37,    -1,
      -1,    -1,    -1,    -1,    27,    44,    29,    30,    31,    32,
      33,    34,    -1,    36,    37,    -1,    -1,    -1,    27,    42,
      29,    30,    31,    32,    33,    34,    -1,    36,    37,    -1,
      27,    40,    29,    30,    31,    32,    33,    34,    -1,    36,
      37,    -1,    27,    40,    29,    30,    31,    32,    33,    34,
      -1,    36,    37,    27,    39,    29,    30,    31,    32,    33,
      34,    -1,    36,    37,    27,    -1,    29,    30,    31,    32,
      33,    34,    -1,    36,    37
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,     1,     3,    23,    46,    47,    48,    39,    23,     0,
      48,    41,     8,    38,    23,     1,    24,    53,    54,    55,
      56,    38,    40,    41,    43,    39,    55,    53,    23,    24,
      49,    50,    51,    52,    40,    39,    25,    40,    41,    42,
      44,    40,     6,     9,    13,    14,    18,    19,    20,    21,
      22,    24,    26,    35,    38,    43,    67,    23,    52,    41,
      67,     1,    24,    64,    67,    67,    23,    67,    25,    43,
      67,    67,     1,    60,    61,    62,    67,    67,    27,    29,
      30,    31,    32,    33,    34,    36,    37,    40,    23,    12,
      42,    41,    10,    16,    67,    57,    58,    59,    67,    40,
      39,    40,    44,    67,    67,    67,    67,    67,    67,    67,
      23,    24,    38,    67,    64,    23,    67,    24,    65,    66,
      44,    42,    62,    62,    37,    43,    67,     4,    25,    63,
      11,    41,    40,    15,    59,    24,    57,    39,    67,    67,
       7,    42,    23,    66,    43,    44,    40,     5,    67,    64,
      17,    57,    67,    44
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int8 yyr1[] =
{
       0,    45,    46,    47,    47,    48,    48,    48,    48,    49,
      50,    50,    51,    52,    52,    53,    53,    54,    55,    55,
      56,    56,    56,    56,    57,    57,    58,    59,    59,    60,
      60,    61,    62,    62,    62,    63,    63,    64,    64,    64,
      65,    66,    66,    67,    67,    67,    67,    67,    67,    67,
      67,    67,    67,    67,    67,    67,    67,    67,    67,    67,
      67,    67,    67,    67,    67,    67,    67,    67
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     1,     2,     6,     8,     3,     1,     3,
       1,     1,     0,     3,     1,     1,     1,     0,     2,     1,
      10,     6,     4,     2,     1,     1,     0,     3,     1,     1,
       1,     0,     3,     2,     3,     0,     2,     6,     6,     3,
       5,     2,     3,     3,     8,     6,     4,     7,     5,     3,
       2,     5,     2,     2,     3,     3,     3,     3,     2,     3,
       3,     3,     2,     3,     1,     1,     1,     1
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
# ifndef YY_LOCATION_PRINT
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif


# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yykind < YYNTOKENS)
    YYPRINT (yyo, yytoknum[yykind], *yyvaluep);
# endif
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
                 int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)]);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif






/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep)
{
  YY_USE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/* Lookahead token kind.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;




/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;



#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    goto yyexhaustedlab;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* program: class_list  */
#line 125 "cool.y"
                     { ast_root = program((yyvsp[0].classes)); }
#line 1321 "cool.tab.c"
    break;

  case 3: /* class_list: class  */
#line 130 "cool.y"
                { (yyval.classes) = single_Classes((yyvsp[0].class_)); }
#line 1327 "cool.tab.c"
    break;

  case 4: /* class_list: class_list class  */
#line 132 "cool.y"
                { (yyval.classes) = append_Classes((yyvsp[-1].classes),single_Classes((yyvsp[0].class_))); }
#line 1333 "cool.tab.c"
    break;

  case 5: /* class: CLASS TYPEID '{' feature_list '}' ';'  */
#line 138 "cool.y"
                { (yyval.class_) = class_((yyvsp[-4].symbol),idtable.add_string("Object"),(yyvsp[-2].features),
                              stringtable.add_string(curr_filename)); }
#line 1340 "cool.tab.c"
    break;

  case 6: /* class: CLASS TYPEID INHERITS TYPEID '{' feature_list '}' ';'  */
#line 141 "cool.y"
                { (yyval.class_) = class_((yyvsp[-6].symbol),(yyvsp[-4].symbol),(yyvsp[-2].features),stringtable.add_string(curr_filename)); }
#line 1346 "cool.tab.c"
    break;

  case 7: /* class: error '}' ':'  */
#line 143 "cool.y"
                { if (VERBOSE_ERRORS)
                        fprintf(stderr, " error happened in class. \n");  }
#line 1353 "cool.tab.c"
    break;

  case 8: /* class: TYPEID  */
#line 146 "cool.y"
                { yychar = TYPEID;
                  yyerror("syntax error");
                  if (VERBOSE_ERRORS)
                        fprintf(stderr, " error happened in class. \n");
                  yyclearin; }
#line 1363 "cool.tab.c"
    break;

  case 9: /* formal: OBJECTID ':' TYPEID  */
#line 156 "cool.y"
                { (yyval.formal) = formal((yyvsp[-2].symbol), (yyvsp[0].symbol)); }
#line 1369 "cool.tab.c"
    break;

  case 10: /* formal_list: dummy_formal_list  */
#line 162 "cool.y"
                { (yyval.formals) = (yyvsp[0].formals); }
#line 1375 "cool.tab.c"
    break;

  case 11: /* formal_list: normal_formal_list  */
#line 164 "cool.y"
                { (yyval.formals) = (yyvsp[0].formals); }
#line 1381 "cool.tab.c"
    break;

  case 12: /* dummy_formal_list: %empty  */
#line 169 "cool.y"
                { (yyval.formals) = nil_Formals(); }
#line 1387 "cool.tab.c"
    break;

  case 13: /* normal_formal_list: formal ',' normal_formal_list  */
#line 174 "cool.y"
                { (yyval.formals) = append_Formals(single_Formals((yyvsp[-2].formal)),(yyvsp[0].formals)); }
#line 1393 "cool.tab.c"
    break;

  case 14: /* normal_formal_list: formal  */
#line 176 "cool.y"
                { (yyval.formals) = single_Formals((yyvsp[0].formal)); }
#line 1399 "cool.tab.c"
    break;

  case 15: /* feature_list: dummy_feature_list  */
#line 182 "cool.y"
                { (yyval.features) = (yyvsp[0].features); }
#line 1405 "cool.tab.c"
    break;

  case 16: /* feature_list: normal_feature_list  */
#line 184 "cool.y"
                { (yyval.features) = (yyvsp[0].features); }
#line 1411 "cool.tab.c"
    break;

  case 17: /* dummy_feature_list: %empty  */
#line 189 "cool.y"
                {  (yyval.features) = nil_Features(); }
#line 1417 "cool.tab.c"
    break;

  case 18: /* normal_feature_list: feature normal_feature_list  */
#line 194 "cool.y"
                { (yyval.features) = append_Features(single_Features((yyvsp[-1].feature)), (yyvsp[0].features)); }
#line 1423 "cool.tab.c"
    break;

  case 19: /* normal_feature_list: feature  */
#line 196 "cool.y"
                { (yyval.features) = single_Features((yyvsp[0].feature)); }
#line 1429 "cool.tab.c"
    break;

  case 20: /* feature: OBJECTID '(' formal_list ')' ':' TYPEID '{' expression '}' ';'  */
#line 201 "cool.y"
                { (yyval.feature) = method((yyvsp[-9].symbol), (yyvsp[-7].formals), (yyvsp[-4].symbol), (yyvsp[-2].expression)); }
#line 1435 "cool.tab.c"
    break;

  case 21: /* feature: OBJECTID ':' TYPEID ASSIGN expression ';'  */
#line 203 "cool.y"
                { (yyval.feature) = attr((yyvsp[-5].symbol), (yyvsp[-3].symbol), (yyvsp[-1].expression)); }
#line 1441 "cool.tab.c"
    break;

  case 22: /* feature: OBJECTID ':' TYPEID ';'  */
#line 205 "cool.y"
                { (yyval.feature) = attr((yyvsp[-3].symbol), (yyvsp[-1].symbol), no_expr()); }
#line 1447 "cool.tab.c"
    break;

  case 23: /* feature: error ';'  */
#line 207 "cool.y"
                { if (VERBOSE_ERRORS)
                        fprintf(stderr, " error happened in features. \n"); }
#line 1454 "cool.tab.c"
    break;

  case 24: /* expression_list: dummy_expression_list  */
#line 214 "cool.y"
                { (yyval.expressions) = (yyvsp[0].expressions); }
#line 1460 "cool.tab.c"
    break;

  case 25: /* expression_list: normal_experssion_list  */
#line 216 "cool.y"
                { (yyval.expressions) = (yyvsp[0].expressions); }
#line 1466 "cool.tab.c"
    break;

  case 26: /* dummy_expression_list: %empty  */
#line 221 "cool.y"
                { (yyval.expressions) = nil_Expressions(); }
#line 1472 "cool.tab.c"
    break;

  case 27: /* normal_experssion_list: expression ',' normal_experssion_list  */
#line 226 "cool.y"
                { (yyval.expressions) = append_Expressions(single_Expressions((yyvsp[-2].expression)),(yyvsp[0].expressions)); }
#line 1478 "cool.tab.c"
    break;

  case 28: /* normal_experssion_list: expression  */
#line 228 "cool.y"
                { (yyval.expressions) = single_Expressions((yyvsp[0].expression)); }
#line 1484 "cool.tab.c"
    break;

  case 29: /* expression_list_: dummy_expression_list_  */
#line 234 "cool.y"
                { (yyval.expressions) = (yyvsp[0].expressions); }
#line 1490 "cool.tab.c"
    break;

  case 30: /* expression_list_: normal_experssion_list_  */
#line 236 "cool.y"
                { (yyval.expressions) = (yyvsp[0].expressions); }
#line 1496 "cool.tab.c"
    break;

  case 31: /* dummy_expression_list_: %empty  */
#line 241 "cool.y"
                { (yyval.expressions) = nil_Expressions(); }
#line 1502 "cool.tab.c"
    break;

  case 32: /* normal_experssion_list_: expression ';' normal_experssion_list_  */
#line 246 "cool.y"
                { (yyval.expressions) = append_Expressions(single_Expressions((yyvsp[-2].expression)),(yyvsp[0].expressions)); }
#line 1508 "cool.tab.c"
    break;

  case 33: /* normal_experssion_list_: expression ';'  */
#line 248 "cool.y"
                { (yyval.expressions) = single_Expressions((yyvsp[-1].expression)); }
#line 1514 "cool.tab.c"
    break;

  case 34: /* normal_experssion_list_: error ';' normal_experssion_list_  */
#line 250 "cool.y"
                { if (VERBOSE_ERRORS)
                        fprintf(stderr, " error happened in block. \n"); }
#line 1521 "cool.tab.c"
    break;

  case 35: /* inlet: %empty  */
#line 257 "cool.y"
                { (yyval.expression) = no_expr(); }
#line 1527 "cool.tab.c"
    break;

  case 36: /* inlet: ASSIGN expression  */
#line 259 "cool.y"
                { (yyval.expression) = (yyvsp[0].expression); }
#line 1533 "cool.tab.c"
    break;

  case 37: /* let_: OBJECTID ':' TYPEID inlet IN expression  */
#line 264 "cool.y"
                { (yyval.expression) = let((yyvsp[-5].symbol), (yyvsp[-3].symbol), (yyvsp[-2].expression), (yyvsp[0].expression)); }
#line 1539 "cool.tab.c"
    break;

  case 38: /* let_: OBJECTID ':' TYPEID inlet ',' let_  */
#line 266 "cool.y"
                { (yyval.expression) = let((yyvsp[-5].symbol), (yyvsp[-3].symbol), (yyvsp[-2].expression), (yyvsp[0].expression)); }
#line 1545 "cool.tab.c"
    break;

  case 39: /* let_: error ',' let_  */
#line 268 "cool.y"
                { if (VERBOSE_ERRORS)
                        fprintf(stderr, " error happened in let bind. \n"); }
#line 1552 "cool.tab.c"
    break;

  case 40: /* case: OBJECTID ':' TYPEID DARROW expression  */
#line 275 "cool.y"
                { (yyval.case_) = branch((yyvsp[-4].symbol), (yyvsp[-2].symbol), (yyvsp[0].expression)); }
#line 1558 "cool.tab.c"
    break;

  case 41: /* case_list: case ';'  */
#line 279 "cool.y"
                { (yyval.cases) = single_Cases((yyvsp[-1].case_)); }
#line 1564 "cool.tab.c"
    break;

  case 42: /* case_list: case ';' case_list  */
#line 281 "cool.y"
                { (yyval.cases) = append_Cases(single_Cases((yyvsp[-2].case_)), (yyvsp[0].cases)); }
#line 1570 "cool.tab.c"
    break;

  case 43: /* expression: OBJECTID ASSIGN expression  */
#line 287 "cool.y"
                { (yyval.expression) = assign((yyvsp[-2].symbol),(yyvsp[0].expression)); }
#line 1576 "cool.tab.c"
    break;

  case 44: /* expression: expression '@' TYPEID '.' OBJECTID '(' expression_list ')'  */
#line 289 "cool.y"
                { (yyval.expression) = static_dispatch((yyvsp[-7].expression), (yyvsp[-5].symbol), (yyvsp[-3].symbol), (yyvsp[-1].expressions)); }
#line 1582 "cool.tab.c"
    break;

  case 45: /* expression: expression '.' OBJECTID '(' expression_list ')'  */
#line 291 "cool.y"
                { (yyval.expression) = dispatch((yyvsp[-5].expression), (yyvsp[-3].symbol), (yyvsp[-1].expressions)); }
#line 1588 "cool.tab.c"
    break;

  case 46: /* expression: OBJECTID '(' expression_list ')'  */
#line 293 "cool.y"
                { (yyval.expression) = dispatch(object(idtable.add_string("self")), (yyvsp[-3].symbol), (yyvsp[-1].expressions)); }
#line 1594 "cool.tab.c"
    break;

  case 47: /* expression: IF expression THEN expression ELSE expression FI  */
#line 295 "cool.y"
                { (yyval.expression) = cond((yyvsp[-5].expression), (yyvsp[-3].expression), (yyvsp[-1].expression)); }
#line 1600 "cool.tab.c"
    break;

  case 48: /* expression: WHILE expression LOOP expression POOL  */
#line 297 "cool.y"
                { (yyval.expression) = loop((yyvsp[-3].expression), (yyvsp[-1].expression)); }
#line 1606 "cool.tab.c"
    break;

  case 49: /* expression: '{' expression_list_ '}'  */
#line 299 "cool.y"
                { (yyval.expression) = block((yyvsp[-1].expressions)); }
#line 1612 "cool.tab.c"
    break;

  case 50: /* expression: LET let_  */
#line 301 "cool.y"
                { (yyval.expression) = (yyvsp[0].expression); }
#line 1618 "cool.tab.c"
    break;

  case 51: /* expression: CASE expression OF case_list ESAC  */
#line 303 "cool.y"
                { (yyval.expression) = typcase((yyvsp[-3].expression), (yyvsp[-1].cases)); }
#line 1624 "cool.tab.c"
    break;

  case 52: /* expression: NEW TYPEID  */
#line 305 "cool.y"
                { (yyval.expression) = new_((yyvsp[0].symbol)); }
#line 1630 "cool.tab.c"
    break;

  case 53: /* expression: ISVOID expression  */
#line 307 "cool.y"
                { (yyval.expression) = isvoid((yyvsp[0].expression)); }
#line 1636 "cool.tab.c"
    break;

  case 54: /* expression: expression '+' expression  */
#line 309 "cool.y"
                { (yyval.expression) = plus((yyvsp[-2].expression),(yyvsp[0].expression)); }
#line 1642 "cool.tab.c"
    break;

  case 55: /* expression: expression '-' expression  */
#line 311 "cool.y"
                { (yyval.expression) = sub((yyvsp[-2].expression),(yyvsp[0].expression)); }
#line 1648 "cool.tab.c"
    break;

  case 56: /* expression: expression '*' expression  */
#line 313 "cool.y"
                { (yyval.expression) = mul((yyvsp[-2].expression),(yyvsp[0].expression)); }
#line 1654 "cool.tab.c"
    break;

  case 57: /* expression: expression '/' expression  */
#line 315 "cool.y"
                { (yyval.expression) = divide((yyvsp[-2].expression),(yyvsp[0].expression)); }
#line 1660 "cool.tab.c"
    break;

  case 58: /* expression: '~' expression  */
#line 317 "cool.y"
                { (yyval.expression) = neg((yyvsp[0].expression)); }
#line 1666 "cool.tab.c"
    break;

  case 59: /* expression: expression '<' expression  */
#line 319 "cool.y"
                { (yyval.expression) = lt((yyvsp[-2].expression),(yyvsp[0].expression)); }
#line 1672 "cool.tab.c"
    break;

  case 60: /* expression: expression LE expression  */
#line 321 "cool.y"
                { (yyval.expression) = leq((yyvsp[-2].expression), (yyvsp[0].expression)); }
#line 1678 "cool.tab.c"
    break;

  case 61: /* expression: expression '=' expression  */
#line 323 "cool.y"
                { (yyval.expression) = eq((yyvsp[-2].expression), (yyvsp[0].expression)); }
#line 1684 "cool.tab.c"
    break;

  case 62: /* expression: NOT expression  */
#line 325 "cool.y"
                { (yyval.expression) = comp((yyvsp[0].expression)); }
#line 1690 "cool.tab.c"
    break;

  case 63: /* expression: '(' expression ')'  */
#line 327 "cool.y"
                { (yyval.expression) = (yyvsp[-1].expression); }
#line 1696 "cool.tab.c"
    break;

  case 64: /* expression: OBJECTID  */
#line 329 "cool.y"
                { (yyval.expression) = object((yyvsp[0].symbol)); }
#line 1702 "cool.tab.c"
    break;

  case 65: /* expression: INT_CONST  */
#line 331 "cool.y"
                { (yyval.expression) = int_const((yyvsp[0].symbol)); }
#line 1708 "cool.tab.c"
    break;

  case 66: /* expression: STR_CONST  */
#line 333 "cool.y"
                { (yyval.expression) = string_const((yyvsp[0].symbol)); }
#line 1714 "cool.tab.c"
    break;

  case 67: /* expression: BOOL_CONST  */
#line 335 "cool.y"
                { (yyval.expression) = bool_const((yyvsp[0].boolean)); }
#line 1720 "cool.tab.c"
    break;


#line 1724 "cool.tab.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      yyerror (YY_("syntax error"));
    }

  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;


#if !defined yyoverflow
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturn;
#endif


/*-------------------------------------------------------.
| yyreturn -- parsing is finished, clean up and return.  |
`-------------------------------------------------------*/
yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif

  return yyresult;
}

#line 339 "cool.y"


/* This function is called automatically when Bison detects a parse error. */
void yyerror(const char *s)
{
  cerr << "\"" << curr_filename << "\", line " << curr_lineno << ": " \
    << s << " at or near ";
  print_cool_token(yychar);
  cerr << endl;
  omerrs++;

  if(omerrs>20) {
      if (VERBOSE_ERRORS)
         fprintf(stderr, "More than 20 errors\n");
      exit(1);
  }
}
