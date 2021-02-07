/* A Bison parser, made by GNU Bison 3.7.1.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
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

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

#ifndef YY_YY_ADA_TAB_H_INCLUDED
# define YY_YY_ADA_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif
/* "%code requires" blocks.  */
#line 157 "ada.y"
 //see https://stackoverflow.com/a/4941440/671627
    struct id_kind_t {
      char *id;
      int kind;     //to indicate a function, a procedure or something else
    };
    struct id_ref_t {
      char *id;
      char line[10];
      char column[10];
    };
    struct id_ref_t error_recovery_identifier;  //used during error recovery
    struct id_decision {
      char *id;
      int is_a_decision;
    };
    struct id_subprogram_t {
      char *id;
      char *xref_subprogram_name;
    };
    struct true_line_column_t {
        int line;
        int column;
    };

#line 74 "ada.tab.h"

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    PAC_START_ELABORATION = 258,   /* PAC_START_ELABORATION  */
    pac_package_name = 259,        /* pac_package_name  */
    PAC_SPEC = 260,                /* PAC_SPEC  */
    PAC_BODY = 261,                /* PAC_BODY  */
    PAC_DOT = 262,                 /* PAC_DOT  */
    REF_COLON = 263,               /* REF_COLON  */
    REF_DECL = 264,                /* REF_DECL  */
    REF_BODY = 265,                /* REF_BODY  */
    REF_REF = 266,                 /* REF_REF  */
    REF_MODI = 267,                /* REF_MODI  */
    ref_string = 268,              /* ref_string  */
    ref_filename = 269,            /* ref_filename  */
    ref_identifier = 270,          /* ref_identifier  */
    ref_character_literal = 271,   /* ref_character_literal  */
    ref_integer = 272,             /* ref_integer  */
    TIC = 273,                     /* TIC  */
    DOT_DOT = 274,                 /* DOT_DOT  */
    LT_LT = 275,                   /* LT_LT  */
    BOX = 276,                     /* BOX  */
    GT_GT = 277,                   /* GT_GT  */
    IS_ASSIGNED = 278,             /* IS_ASSIGNED  */
    RIGHT_SHAFT = 279,             /* RIGHT_SHAFT  */
    ABORT = 280,                   /* ABORT  */
    ABS = 281,                     /* ABS  */
    ABSTRACT = 282,                /* ABSTRACT  */
    ACCEPT = 283,                  /* ACCEPT  */
    ACCESS = 284,                  /* ACCESS  */
    ALIASED = 285,                 /* ALIASED  */
    ALL = 286,                     /* ALL  */
    AND = 287,                     /* AND  */
    ARRAY = 288,                   /* ARRAY  */
    AT = 289,                      /* AT  */
    BEGiN = 290,                   /* BEGiN  */
    BODY = 291,                    /* BODY  */
    CASE = 292,                    /* CASE  */
    CONSTANT = 293,                /* CONSTANT  */
    DECLARE = 294,                 /* DECLARE  */
    DELAY = 295,                   /* DELAY  */
    DELTA = 296,                   /* DELTA  */
    DIGITS = 297,                  /* DIGITS  */
    DO = 298,                      /* DO  */
    ELSE = 299,                    /* ELSE  */
    ELSIF = 300,                   /* ELSIF  */
    END = 301,                     /* END  */
    ENTRY = 302,                   /* ENTRY  */
    EXCEPTION = 303,               /* EXCEPTION  */
    EXIT = 304,                    /* EXIT  */
    FOR = 305,                     /* FOR  */
    FUNCTION = 306,                /* FUNCTION  */
    GENERIC = 307,                 /* GENERIC  */
    GOTO = 308,                    /* GOTO  */
    IF = 309,                      /* IF  */
    IN = 310,                      /* IN  */
    INTERFACE = 311,               /* INTERFACE  */
    IS = 312,                      /* IS  */
    LIMITED = 313,                 /* LIMITED  */
    LOOP = 314,                    /* LOOP  */
    MOD = 315,                     /* MOD  */
    NEW = 316,                     /* NEW  */
    NOT = 317,                     /* NOT  */
    NuLL = 318,                    /* NuLL  */
    OF = 319,                      /* OF  */
    OR = 320,                      /* OR  */
    OTHERS = 321,                  /* OTHERS  */
    OUT = 322,                     /* OUT  */
    OVERRIDING = 323,              /* OVERRIDING  */
    PACKAGE = 324,                 /* PACKAGE  */
    PRAGMA = 325,                  /* PRAGMA  */
    PRIVATE = 326,                 /* PRIVATE  */
    PROCEDURE = 327,               /* PROCEDURE  */
    PROTECTED = 328,               /* PROTECTED  */
    RAISE = 329,                   /* RAISE  */
    RANGE = 330,                   /* RANGE  */
    RECORD = 331,                  /* RECORD  */
    REM = 332,                     /* REM  */
    RENAMES = 333,                 /* RENAMES  */
    REQUEUE = 334,                 /* REQUEUE  */
    RETURN = 335,                  /* RETURN  */
    REVERSE = 336,                 /* REVERSE  */
    SELECT = 337,                  /* SELECT  */
    SEPARATE = 338,                /* SEPARATE  */
    SUBTYPE = 339,                 /* SUBTYPE  */
    SYNCHRONIZED = 340,            /* SYNCHRONIZED  */
    TAGGED = 341,                  /* TAGGED  */
    TASK = 342,                    /* TASK  */
    TERMINATE = 343,               /* TERMINATE  */
    THEN = 344,                    /* THEN  */
    TYPE = 345,                    /* TYPE  */
    UNTIL = 346,                   /* UNTIL  */
    USE = 347,                     /* USE  */
    WHEN = 348,                    /* WHEN  */
    WHILE = 349,                   /* WHILE  */
    WITH = 350,                    /* WITH  */
    XOR = 351,                     /* XOR  */
    PLUS = 352,                    /* PLUS  */
    MINUS = 353,                   /* MINUS  */
    MULT = 354,                    /* MULT  */
    DIV = 355,                     /* DIV  */
    EXPON = 356,                   /* EXPON  */
    CONC = 357,                    /* CONC  */
    EQUAL = 358,                   /* EQUAL  */
    NE = 359,                      /* NE  */
    LT = 360,                      /* LT  */
    LT_EQ = 361,                   /* LT_EQ  */
    GT = 362,                      /* GT  */
    GE = 363,                      /* GE  */
    character_literal = 364,       /* character_literal  */
    identifier = 365,              /* identifier  */
    string_literal = 366,          /* string_literal  */
    numeric_literal = 367          /* numeric_literal  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 182 "ada.y"
char *id;                           // for REF, PAC and ADA most stuff
        struct id_kind_t id_kind;           // for identifiers and strings in REF
        struct id_ref_t  id_ref;            // only for ADA identifiers, character literals, string literals, operators
        struct id_decision id_deci;         // for relation and expression_2 to indicate if we are within a decision
        struct id_subprogram_t id_subprogram;       //for subprograms only
        struct true_line_column_t true_line_column; //used for recording line and columns of detected bran,deci,cond, rune and gates
       

#line 212 "ada.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_ADA_TAB_H_INCLUDED  */
