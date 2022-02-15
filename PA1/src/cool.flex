/*
 *  The scanner definition for COOL.
 */

/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
  if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
    YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */

 int comment_layer;
%}

%option noyywrap


/* state */
%x single_line_comment nest_comment string null_string long_string

/*
 * Define names for regular expressions here.
 */

CLASS       ?i:class 
ELSE        ?i:else 
FALSE       f(?i:alse)
FI          ?i:fi
IF          ?i:if
IN          ?i:in
INHERITS    ?i:inherits
ISVOID      ?i:isvoid
LET         ?i:let 
LOOP        ?i:loop
POOL        ?i:pool
THEN        ?i:then
WHILE       ?i:while
CASE        ?i:case
ESAC        ?i:esac 
NEW         ?i:new
OF          ?i:of 
NOT         ?i:not
TRUE        t(?i:rue) 

DARROW      =>
ASSIGN      <-
LE          <=

DIGIT       [0-9]
INTEGER     {DIGIT}+

TYPEID      [A-Z][A-Za-z0-9_]*
OBJID       [a-z][A-Za-z0-9_]*
SELFID      self
SELFTYPEID  SELF_TYPE

WHITESPACE  [ \t\r\v\f]


%%

 /*
  * Define regular expressions for the tokens of COOL here. Make sure, you
  * handle correctly special cases, like:
  *   - Nested comments
  *   - String constants: They use C like systax and can contain escape
  *     sequences. Escape sequence \c is accepted for all characters c. Except
  *     for \n \t \b \f, the result is c.
  *   - Keywords: They are case-insensitive except for the values true and
  *     false, which must begin with a lower-case letter.
  *   - Multiple-character operators (like <-): The scanner should produce a
  *     single token for every such operator.
  *   - Line counting: You should keep the global variable curr_lineno updated
  *     with the correct line number
  */

 /*  Single line comments */

"--"  { BEGIN(single_line_comment); }
<single_line_comment><<EOF>> { yyterminate(); }
<single_line_comment>[^\n]* {}
<single_line_comment>"\n" {
  BEGIN(INITIAL); 
  curr_lineno++;
}

 /* nest comments */

"(*" {
  comment_layer=1;
  BEGIN(nest_comment);
}
<nest_comment>"(*" {
  comment_layer++;
}

"*)"  { 
  cool_yylval.error_msg = "Unmatched *)"; 
  return ERROR;
}

<nest_comment>"*)"  {
  comment_layer--; 
  if(comment_layer==0){
    BEGIN(INITIAL);
  }
}

<nest_comment><<EOF>>  {
  BEGIN(INITIAL);
  cool_yylval.error_msg = "EOF in comment";
  return ERROR;
}
<nest_comment>"\n" { curr_lineno++; }
<nest_comment>. { }

 /* String */

\"  {
  string_buf_ptr = string_buf;
  BEGIN(string);
}

<string><<EOF>> {
  cool_yylval.error_msg = "EOF in string constant";
  BEGIN(INITIAL);
  *string_buf_ptr = '\0';
  return ERROR;
}

<string>\\\0 {
  BEGIN(null_string);
}

<string>\0 {
  BEGIN(null_string);
}

<string>\\\n {
  curr_lineno++;
  if(string_buf_ptr == string_buf + MAX_STR_CONST-1){
    cool_yylval.error_msg = "String constant too long";
    BEGIN(long_string);
    return ERROR;
  }
  *string_buf_ptr++ = '\n';
}

<string>\\. {
  if(string_buf_ptr == string_buf + MAX_STR_CONST-1){
    cool_yylval.error_msg = "String constant too long";
    BEGIN(long_string);
    return ERROR;
  }
  switch(yytext[1]){
    case'b': *string_buf_ptr++ = '\b';break;
    case'n': *string_buf_ptr++ = '\n';break;
    case'f': *string_buf_ptr++ = '\f';break;
    case't': *string_buf_ptr++ = '\t';break;
    default: *string_buf_ptr++ = yytext[1];
  }
}

 /* if string is too long */

<long_string>\n {
  curr_lineno++;
  BEGIN(INITIAL);
}
<long_string,null_string>\\\n {
  curr_lineno++;
}
<long_string,null_string>\\\" { }
<long_string>\" { 
  BEGIN(INITIAL);
}
<long_string><<EOF>> { BEGIN(INITIAL); }

 /* if the string contains null */
<null_string>\n {
  curr_lineno++;
  BEGIN(INITIAL);
  cool_yylval.error_msg = "Unterminated string constant";
  return ERROR;
}
<null_string>\" { 
  cool_yylval.error_msg = "String contains null character.";
  BEGIN(INITIAL);
  return ERROR;
}
<null_string><<EOF>> {
  cool_yylval.error_msg = "String contains null character.";
  BEGIN(INITIAL);
  return ERROR;
}
<long_string,null_string>. {}


 /* if string contain \n */
<string>\n {
  curr_lineno++;
  cool_yylval.error_msg = "Unterminated string constant";
  BEGIN(INITIAL);
  return ERROR;
}

 /* the end of thr string */
<string>\"  {
  BEGIN(INITIAL);
  *string_buf_ptr = '\0';
  cool_yylval.symbol = stringtable.add_string(string_buf);
  return STR_CONST;
}

 /* read the legal part of string */
<string>[^\\\n\"\0]+  {
  char *yptr = yytext;
  while ( *yptr ){
    if(string_buf_ptr == string_buf + MAX_STR_CONST-1){
      cool_yylval.error_msg = "String constant too long";
      BEGIN(long_string);
      return ERROR;
    }
    *string_buf_ptr++ = *yptr++;
  }
}

 /* token */
{CLASS}        { return CLASS; }
{ELSE}         { return ELSE; }
{FI}           { return FI; }
{IF}           { return IF; }
{IN}           { return IN; }
{INHERITS}     { return INHERITS; }
{ISVOID}       { return ISVOID; }
{LET}          { return LET; }
{LOOP}         { return LOOP; }
{POOL}         { return POOL; }
{THEN}         { return THEN; }
{CASE}         { return CASE; }
{ESAC}         { return ESAC; }
{NEW}          { return NEW; }
{OF}           { return OF; }
{NOT}          { return NOT; }
{WHILE}        { return WHILE;}

{TRUE} { 
  cool_yylval.boolean = true;
  return BOOL_CONST;
}
{FALSE} { 
  cool_yylval.boolean = false;
  return BOOL_CONST;
}
{DARROW} {
  return DARROW;
}
{LE} {return LE;}

[()\{\}] { return yytext[0]; }

[+\-*/=<~:;,\.@] { return yytext[0]; }

{WHITESPACE} {}
\n { curr_lineno++; }

{INTEGER} {
  cool_yylval.symbol = inttable.add_string(yytext);
  return INT_CONST;
}
{ASSIGN} {return ASSIGN;}



{TYPEID} {
  cool_yylval.symbol = idtable.add_string(yytext);
  return TYPEID;
}

{OBJID} {
  cool_yylval.symbol = idtable.add_string(yytext);
  return OBJECTID;
}

<<EOF>> { yyterminate(); }

 /* the not accepted part */
. { 
  cool_yylval.error_msg = yytext;
  return ERROR;
}
%%
