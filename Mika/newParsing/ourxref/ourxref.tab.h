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

#ifndef YY_YY_OURXREF_TAB_H_INCLUDED
# define YY_YY_OURXREF_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif
/* "%code requires" blocks.  */
#line 62 "ourxref.y"

    struct line_col_t {
      char *line;
      char letter;
      char *column;
    };

#line 57 "ourxref.tab.h"

/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    RUBBISH = 258,                 /* RUBBISH  */
    DLETTER = 259,                 /* DLETTER  */
    XLETTER = 260,                 /* XLETTER  */
    ASTERISK = 261,                /* ASTERISK  */
    RENAME_REF = 262,              /* RENAME_REF  */
    SQUARE_BRACKETED = 263,        /* SQUARE_BRACKETED  */
    BRACKETED = 264,               /* BRACKETED  */
    CURLY_BRACKETED = 265,         /* CURLY_BRACKETED  */
    ANGLE_BRACKETED = 266,         /* ANGLE_BRACKETED  */
    SOURCE_NAME = 267,             /* SOURCE_NAME  */
    FILENO = 268,                  /* FILENO  */
    START_LINE_CHAR_COL = 269,     /* START_LINE_CHAR_COL  */
    LINE_CHAR_COL = 270,           /* LINE_CHAR_COL  */
    SUBUNIT = 271,                 /* SUBUNIT  */
    ENTITY = 272,                  /* ENTITY  */
    INTEGER = 273                  /* INTEGER  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 70 "ourxref.y"
char *id;
        struct line_col_t line_col;
       

#line 97 "ourxref.tab.h"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_OURXREF_TAB_H_INCLUDED  */
