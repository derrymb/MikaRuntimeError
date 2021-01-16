/******************************************************************************************************************************************/
/*                                 Copyright 2020 Dr Christophe Meudec                                                                    */
/*                                     <http://www.echancrure.eu/>                                                                        */
/* This file is part of Mika.                                                                                                             */
/* Mika is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by      */  
/*   the Free Software Foundation, either version 3 of the License, or (at your option) any later version.                                */
/* Mika is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of                 */
/*   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.                           */
/* You should have received a copy of the GNU General Public License along with Mika.  If not, see <https://www.gnu.org/licenses/>.       */
/******************************************************************************************************************************************/
/*                                                                          */
/* win_flex --wincompat ourxref.l                                           */
/* generates lex.yy.c                                                       */
/************************************************************************/

%option yylineno
%{
#pragma warning( disable : 4996 )  //ignore warnings such as : warning C4996: 'strcpy': This function or variable may be unsafe. Consider using strcpy_s instead. To disable deprecation, use _CRT_SECURE_NO_WARNINGS. See online help for details.

#include "string.h"
#include "stdlib.h"
#include "ctype.h"
#include "math.h"

#include "ourxref.tab.h"

void get_line_col(char *);
void read_up_to_DLETTER(FILE *);

%}

/*do not comment the macros : lex does not like it.*/
DIGIT                   [0-9]
INTEGER                 ({DIGIT}{DIGIT}*)            
%%
([a-zA-Z0-9\!\�\$\\%\\^\&\(\)\-\+\_\=\[\]\{\}\;\'\@\#\~\,\.])*\.(ads|adb)   { yylval.id = malloc((strlen(yytext)+1)*sizeof(char));
                                                                             strcpy(yylval.id, yytext);
                                                                             //all the filenames mentionned in foo.xref are added to a binary tree of filenames (see binary_filename.c)
                                                                             // this structure is then checked during parsing of foo.bind in ada.y to see if a file mantionned in foo.bind should really be added to the queue of file code to analyse
                                                                             //if (!filenames_in_xref) filenames_in_xref = add_filename(filenames_in_xref, filenames_in_xref, create_filename_node(yylval.id));
                                                                             //else add_filename(filenames_in_xref, filenames_in_xref, create_filename_node(yylval.id));
                                                                             return(SOURCE_NAME);
                                                                           }
\"(\"\"|[^\n\"])*\"     {
                          yylval.id = malloc((strlen(yytext)+1)*sizeof(char));
                          strcpy(yylval.id, yytext);
                          return(ENTITY);       //a string to be more precise
                        }
"'"."'"                 {
                          yylval.id = malloc((strlen(yytext)+1)*sizeof(char));
                          strcpy(yylval.id, yytext);
                          return(ENTITY);       //a character literal to be more precise
                        }
\([^\)]*\)              {
                          yylval.id = malloc((strlen(yytext)+1)*sizeof(char));
                          strcpy(yylval.id, yytext);
                          return(BRACKETED);
                        }
\{[^\}]*\}              {
                          yylval.id = malloc((strlen(yytext)+1)*sizeof(char));
                          strcpy(yylval.id, yytext);
                          return(CURLY_BRACKETED);
                        }
\<[^\>]*\>              {
                          yylval.id = malloc((strlen(yytext)+1)*sizeof(char));
                          strcpy(yylval.id, yytext);
                          return(ANGLE_BRACKETED);
                        }
\[[^\]]*\]              {
                          yylval.id = malloc((strlen(yytext)+1)*sizeof(char));
                          strcpy(yylval.id, yytext);
                          return(SQUARE_BRACKETED);
                        }

{INTEGER}\              {
                          yylval.id = malloc((strlen(yytext)+1)*sizeof(char));
                          strcpy(yylval.id, yytext);
                          return(INTEGER);
                        }

{INTEGER}\|             {
                          yylval.id = malloc((strlen(yytext)+1)*sizeof(char));
                          strncpy(yylval.id, yytext, strlen(yytext)-1);
                          yylval.id[strlen(yytext)-1] = '\0';   //necessary because not automatic
                          return(FILENO);   //the index mentioned
                        }
\={INTEGER}.{INTEGER}     {
                          yylval.id = malloc((strlen(yytext)+1)*sizeof(char));
                          strcpy(yylval.id, yytext);
                          return(RENAME_REF);
                        }
{INTEGER}[a-z]([0-9]*[a-z]+)+([a-z0-9])*    { 
                                             return RUBBISH;
                                            }
{INTEGER}[a-zA-Z\>\<\=\^](\<.*\>)?{INTEGER}     {
                          get_line_col(yytext); // sets yylval.line, .letter and .column appropriately
                          return(LINE_CHAR_COL);
                        }

\n{INTEGER}.{INTEGER}     {
                          get_line_col(&yytext[1]); // sets yylval.line, .letter and .column appropriately
                          return(START_LINE_CHAR_COL);
                        }

[a-zA-Z](_?[a-zA-Z0-9])*    {
                              yylval.id = malloc((strlen(yytext)+1)*sizeof(char));
                              strcpy(yylval.id, yytext);
                              return(ENTITY);
                            }

([a-zA-Z0-9\!\�\$\\%\\^\&\(\)\-\+\_\=\[\]\{\}\;\'\@\#\~\,])*\.([a-zA-Z0-9\_])*  { yylval.id = malloc((strlen(yytext)+1)*sizeof(char));
                                                                  strcpy(yylval.id, yytext);
                                                                  return(SUBUNIT);
                                                                }
\n\X\                   {return(XLETTER);}
\n\D\                   {return(DLETTER);}
"*"                     {return(ASTERISK);}
\n\.                    {;}
[ \t\f]                 {;}
.                       {;}
%%
// standard lex function called automatically at the end of file being parsed
// re-written (normally only returns 1) to allow parsing of various input files
// parsing order:   1st standard.xref by default (see ada.y)
//                  2nd standard.ads
//                  3rd foo.xref is parsed next
//                  4th foo.bind is parsed next as created by gnatmake
//                              then code files according to elaboration order (including foo.ads)
int yywrap()
{
  extern char *get_next_ali();
  extern int debugMode;
  char *ali_file;
  fclose(yyin);
  yylineno = 0;  //re-initialisation for new file
  ali_file = get_next_ali();
  if (ali_file) {
    yyin = fopen(ali_file, "r");
    if (!yyin) {
      fprintf(stderr, "OURXREF ERROR: %s:  Can't open internal file", ali_file);
      fflush(stderr);
      exit(212);
    }
    if (debugMode) fprintf(stderr, "Referencing : %s\n", ali_file);
    else fprintf(stderr, ".");
    read_up_to_DLETTER(yyin);
    return (0); //keep on trucking
  }
  else
    return (1); //no more files to parse
}//yywrap function

void get_line_col(char *text) // e.g. text 72e11 or 202b<c,__gnat_Unwind_ForcedUnwind>52
{
  int i = 0;
  while (isdigit(text[i])) {
    i++;
  }
  yylval.line_col.line = malloc((i)*sizeof(char));
  strncpy(yylval.line_col.line, text, i);
  yylval.line_col.line[i] = '\0';
  yylval.line_col.letter = text[i];
  if (text[i+1] == '<') {
    i++;
    while (text[i] != '>') {
      i++;
    }
  }
  yylval.line_col.column = malloc((strlen(&text[i+1])+1)*sizeof(char));
  strcpy(yylval.line_col.column, &text[i+1]);
}
//read all the rubbish up to the first DLetter
void read_up_to_DLETTER(FILE *f)
{ int ch;
  do {
    do {
      ;
    } while (fgetc(f) != '\n');
    yylineno++;
    ch = fgetc(f);
    if (ch == 'D') break;
    ungetc(ch, f);
  } while (1);
  ungetc('D', f);
  ungetc('\n', f);
}

/**************************************END OF ourxref.l FILE *********************************************/