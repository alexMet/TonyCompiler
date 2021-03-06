%{
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

#include "error.h"
#include "general.h"
#include "parser.h"

#define YY_NO_INPUT
#define T_eof        0

int com = 0;

/*
 * D    -> digit
 * L    -> letter
 * W    -> whitespace
 * pri  -> printable characters (x20 - xFF)
 * esc  -> escape sequence
 * ops  -> operators
 * sep  -> separators
 */
 
%}

D       [0-9]
L       [a-zA-Z]
W       [ \r\t]
pri     [^\\\'\"\x00-\x1F]
esc     \\([\\\'\"ntr0]|x[0-9a-fA-F]{2})
ops     [\+\=\-\*\/\#\<\>]
sep     [\(\)\[\]\,\;\:]

%x      COMMENT
%option nounput

%%

\n          linecount++;
{W}+        /* clear whitespace */

"and"       return T_and;
"bool"      return T_bool;
"char"      return T_char;
"decl"      return T_decl;
"def"       return T_def;
"else"      return T_else;
"elsif"     return T_elsif;
"end"       return T_end;
"exit"      return T_exit;
"false"     return T_false;
"for"       return T_for;
"head"      return T_head;
"if"        return T_if;
"int"       return T_int;
"list"      return T_list;
"mod"       return T_mod;
"new"       return T_new;
"nil"       return T_nil;
"nil?"      return T_isnil;
"not"       return T_not;
"or"        return T_or;
"ref"       return T_ref;
"return"    return T_return;
"skip"      return T_skip;
"tail"      return T_tail;
"true"      return T_true;

"<>"        return T_ne;
">="        return T_ge;
"<="        return T_le;
":="        return T_assign;

{ops}       return yytext[0];
{sep}       return yytext[0];

\%[^\n]*\n  linecount++;

"<*"                { ++com; BEGIN(COMMENT); } 
<COMMENT>"<*"       { ++com; } 
<COMMENT>"*>"       { --com; if (com == 0) BEGIN(INITIAL); } 
<COMMENT>"*"        { /* nothing */ } 
<COMMENT>\n         { linecount++; }
<COMMENT>[^*\n]+    { /* nothing */ }

\"({pri}|{esc})*\" { 
    yylval.name = strdup(yytext); 
    return T_const_string; 
}

\"[^\"\n]*\" {
    error("Invalid characters in string constant.");
    yylval.name = "\0"; 
    return T_const_string;
}

\'({pri}|{esc})\' {
    yylval.name = strdup(yytext); 
    return T_const_char;
}

\'({pri}|{esc}){2,}\' {
    error("Too many characters in char constant.");
    yylval.name = "\0"; 
    return T_const_char;
}

\'[^\'\n]*\' {
    error("Invalid characters in char constant.");
    yylval.name = "\0"; 
    return T_const_char;
}

{L}+({L}|{D}|_|\?)* {
    yylval.name = strdup(yytext);
    return T_id;
}

{D}+ {
    // yylval.number = atoi(yytext);
    yylval.name = strdup(yytext);
    return T_const_int;
}

{D}+({L}|{D}|_|\?)* {
    // TODO: edw mporei na einai eite metavlhth lathos eite arithmitikh stathera lathos
    yylval.name = "\0";
    return T_id;
}

<<EOF>> {
    if (YY_START == COMMENT)
        error("End of file found while in comment block.");
        
    return T_eof; 
}

. {
    error("Illegal character found");
}
