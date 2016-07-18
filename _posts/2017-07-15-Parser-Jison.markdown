---
layout: post
title:  "Parser Jison"
date:   2016-07-15
categories: js
---

In the formal grammatical rules for a language, each kind of syntactic unit or grouping is named by a `symbol`. 

* Those which are built by grouping smaller constructs according to grammatical rules are called `nonterminal symbols`;We call a piece corresponding to a single nonterminal symbol a `grouping`.

* those which can't be subdivided are called `terminal symbols` or `token types`. We call a piece of input corresponding to a single terminal symbol a `token`

We can use the C language as an example of what symbols, terminal and nonterminal, mean.

#### In C

The `tokens` of C are identifiers, constants (numeric and string), and the various keywords, arithmetic operators and punctuation marks.

So the `terminal symbols` of a grammar for C include identifier, number, string, plus one symbol for each keyword, operator or punctuation mark: if, return, const, static, int, char, plus-sign, open-brace, close-brace, comma and many more. (These tokens can be subdivided into characters, but that is a matter of lexicography, not grammar.)

```c
int             /* keyword `int' */
square (x)      /* identifier, open-paren, */
                /* identifier, close-paren */
     int x;     /* keyword `int', identifier, semicolon */
{               /* open-brace */
  return x * x; /* keyword `return', identifier, */
                /* asterisk, identifier, semicolon */
}               /* close-brace */
```

The example above is a function definition; it contains one `declaration`, and one `statement`. In the statement, each `x` is an expression and so is `x * x`.

The syntactic groupings of C include the expression, the statement, the declaration, and the function definition. These are represented in the grammar of C by nonterminal symbols `expression`, `statement`, `declaration` and `function definition`.

#### Bison
[http://dinosaur.compilertools.net/bison/bison_5.html](http://dinosaur.compilertools.net/bison/bison_5.html)

***rpcalc***

```c
/* Reverse polish notation calculator. */

%{
#define YYSTYPE double
#include <math.h>
%}

%token NUM

%% /* Grammar rules and actions follow */
input:    /* empty */
        | input line
;

line:     '\n'
        | exp '\n'  { printf ("\t%.10g\n", $1); }
;

exp:      NUM             { $$ = $1;         }
        | exp exp '+'     { $$ = $1 + $2;    }
        | exp exp '-'     { $$ = $1 - $2;    }
        | exp exp '*'     { $$ = $1 * $2;    }
        | exp exp '/'     { $$ = $1 / $2;    }
      /* Exponentiation */
        | exp exp '^'     { $$ = pow ($1, $2); }
      /* Unary minus    */
        | exp 'n'         { $$ = -$1;        }
;
%%

```

***calc***

```c
/* Infix notation calculator--calc */

%{
#define YYSTYPE double
#include <math.h>
%}

/* BISON Declarations */
%token NUM
%left '-' '+'
%left '*' '/'
%left NEG     /* negation--unary minus */
%right '^'    /* exponentiation        */

/* Grammar follows */
%%
input:    /* empty string */
        | input line
;

line:     '\n'
        | exp '\n'  { printf ("\t%.10g\n", $1); }
;

exp:      NUM                { $$ = $1;         }
        | exp '+' exp        { $$ = $1 + $3;    }
        | exp '-' exp        { $$ = $1 - $3;    }
        | exp '*' exp        { $$ = $1 * $3;    }
        | exp '/' exp        { $$ = $1 / $3;    }
        | '-' exp  %prec NEG { $$ = -$2;        }
        | exp '^' exp        { $$ = pow ($1, $3); }
        | '(' exp ')'        { $$ = $2;         }
;
%%
```






***mfcalc***

```c
%{
#include <math.h>  /* For math functions, cos(), sin(), etc. */
#include "calc.h"  /* Contains definition of `symrec'        */
%}
%union {
double     val;  /* For returning numbers.                   */
symrec  *tptr;   /* For returning symbol-table pointers      */
}

%token <val>  NUM        /* Simple double precision number   */
%token <tptr> VAR FNCT   /* Variable and Function            */
%type  <val>  exp

%right '='
%left '-' '+'
%left '*' '/'
%left NEG     /* Negation--unary minus */
%right '^'    /* Exponentiation        */

/* Grammar follows */

%%
```