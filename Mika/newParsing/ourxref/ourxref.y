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
/*******************************************************************/
/* Performs the role of gnatxref: read the .ali files to build     */
/*   references data structures                                    */
/* to generate parser call: win_bison -d ourxref.y                 */
/* to test: ourxref -d -l"C:\GNAT\2010\bin\gnatls" ch7_1           */
/*******************************************************************/

%{
#pragma warning( disable : 4996 )  //ignore warnings such as : warning C4996: 'strcpy': This function or variable may be unsafe. Consider using strcpy_s instead. To disable deprecation, use _CRT_SECURE_NO_WARNINGS. See online help for details.

#include <errno.h>  //system error constants
#include <stdio.h>
#include <ctype.h>
#include <io.h>
#include <stdlib.h>
#include <process.h>            //for system function
#include <direct.h>             //for directory creation
#include <time.h>               //for time and date retrieval

extern int yylex();
extern int yyparse();
extern FILE* yyin;
extern int yylineno;
extern char *yytext;
void yyerror(const char*);


void my_exit(int);             //attempts to close handles and delete generated files prior to caling exit(int);

#include "find_path.c"          //used to find file names using gnat ls
#include "list_of_dependencies.c"
#include "list_of_ali_files.c"

char original_file[_MAX_PATH*10];           // original file name
char current_referenced_file[_MAX_PATH];    // the file that we use for the current reference
char original_referenced_file[_MAX_PATH];
char ali_path_found[_MAX_PATH*10];
char user_gnatls_call_str[_MAX_PATH*10];
char user_gnatproject_call_str[_MAX_PATH*10];
char gnatlsExe[_MAX_PATH];                  // string denoting the name of the gnat ls executable
int debugMode = 0;
int error_count = 0;    //number of parsing errors
int first_ref;
int new_file;
char *tmp_str;
extern void read_up_to_DLETTER(FILE *);
extern FILE *yyin;
char *get_ali(char *);

%}
//see https://stackoverflow.com/a/4941440/671627
%code requires {
    struct line_col_t {
      char *line;
      char letter;
      char *column;
    };
}
//values returned by lex.l via yylval
%union {char *id;
        struct line_col_t line_col;
       }

%token RUBBISH
%token DLETTER
%token XLETTER
%token ASTERISK
%token RENAME_REF
%token SQUARE_BRACKETED
%token <id> BRACKETED
%token <id> CURLY_BRACKETED
%token <id> ANGLE_BRACKETED
%token <id> SOURCE_NAME
%token <id> FILENO
%token <line_col> START_LINE_CHAR_COL
%token <line_col> LINE_CHAR_COL
%token <id> SUBUNIT
%token <id> ENTITY
%token <id> INTEGER

%type <id> subunit_opt

%%
goal_symbol : ali_file_list
            ;

ali_file_list : ali_file
              | ali_file_list
                {init_list_dependencies();  //data structure to keep track of declared dependencies in foo.ali
                }
                ali_file
              ;

ali_file : dependency_list
           { if (debugMode) {
               print_queue();
               //fgetc(stdin);
             }
           }
           cross_reference_list
           { if (debugMode) {
               print_ali_queue();
             }
           }
         ;

dependency_list : dependency
                | dependency_list dependency
                ;

dependency : DLETTER SOURCE_NAME INTEGER hexa subunit_opt
             {if ($5 == NULL) {
                 if (debugMode) fprintf(stderr, "\nRead Dependency to file: %s", $2);
                 add_dependency($2, 0);
               }
               else {
                 if (debugMode) fprintf(stderr, "\n%s is sub-unit\n", $2);
                 add_dependency($2, 1);
               }
             }
           ;

/* this extremely messy and dirty but best we could do */
hexa : INTEGER
     | LINE_CHAR_COL
     | ENTITY
     | RUBBISH
     ;

subunit_opt : /* empty */
              {$$ = NULL;}
            | SUBUNIT
              {$$ = $1;
              }
            ;

cross_reference_list : cross_reference
                     | cross_reference_list cross_reference
                     ;

cross_reference : XLETTER INTEGER SOURCE_NAME
                  {dependency_node_ptr x;
                   if (debugMode) fprintf(stderr, "\nAnalysis number : %s for %s\n", $2, $3);
                   strcpy(current_referenced_file, $3);
                   strcpy(original_referenced_file, $3);
                   x = retrieve_dependency_file_name(atoi($2));
                   if (debugMode) fprintf(stderr, "Retrieved reference to file %s subunit: %i\n", x->file_name, x->is_a_subunit);
                   if (!x->is_a_subunit) add_ali(get_ali(x->file_name));
                  }
                  entity_reference_list
                ;

entity_reference_list : entity_reference
                      | entity_reference_list {strcpy(current_referenced_file, original_referenced_file);} entity_reference
                      ;

entity_reference : START_LINE_CHAR_COL level_opt ENTITY
                   {fprintf(stdout, "\n%s ", $3);
                    if ($1.letter == 'U') fprintf(stdout, "procedure\n");
                    else if ($1.letter == 'V') fprintf(stdout, "function\n");
                    else fprintf(stdout, " \n");
                    fprintf(stdout, "  Decl:  %s    %s:%s", current_referenced_file, $1.line, $1.column);
                    fflush(stdout);
                    first_ref = 1;
                    new_file = 1;
                   }
                   rename_opt instance_opt brackets_list_opt reference_list
                   {;}
                 ;

reference_list : /* empty */
               | reference_list reference
               ;

reference : FILENO LINE_CHAR_COL instance_opt
            {if (first_ref)
               fprintf(stdout, "\n  Ref: ");
             else
               fprintf(stdout, "\n        ");
             fflush(stdout);
             first_ref = 0;
             new_file = 0;
             strcpy(current_referenced_file, retrieve_dependency_file_name(atoi($1))->file_name);
             fprintf(stdout, "  %s    %s:%s", current_referenced_file, $2.line, $2.column);
             fflush(stdout);
            }
          | LINE_CHAR_COL instance_opt
            {if (first_ref)
               fprintf(stdout, "\n  Ref:  %s", current_referenced_file);
             else
               fprintf(stdout, "  ");
             fflush(stdout);
             first_ref = 0;
             fprintf(stdout, "  %s:%s", $1.line, $1.column);
             fflush(stdout);
            }
          ;

level_opt : /* empty */
          | ASTERISK
          ;

rename_opt : /* empty */
           | RENAME_REF
           ;

instance_opt : /* empty */
             | SQUARE_BRACKETED
             ;
brackets_list_opt : /* empty */
                  | brackets_list_opt bracket
                  ;

bracket : BRACKETED         {free($1);} //{if (debugMode) fprintf(stderr, "\nbracket %s", $1);}
        | CURLY_BRACKETED   {free($1);} //{if (debugMode) fprintf(stderr, "\ncurly %s", $1);}
        | ANGLE_BRACKETED   {free($1);} //{if (debugMode) fprintf(stderr, "\nangle %s", $1);}
        ;
%%
//#include "lex.yy.c"        //the lexical analyser created by FLex

int main(int argc, char *argv[])
{
    //extern int yyparse();
    //extern int yylineno;
    int i;
    char path_found[_MAX_PATH];             //the path that was searched
    strcpy(user_gnatls_call_str, "");
    strcpy(user_gnatproject_call_str, "");

    for(i=1;i<argc-1;i++)
        {//processing switches
         if(argv[i][0] == '-') {
           switch(argv[i][1]) {
                    case 'd' :
                        debugMode = 1;  //we are in debug mode : will affect output of warnings amongst other things
                        break;
                    case 'c' :
                        strcpy(user_gnatls_call_str, &argv[i][2]);
                        break;
                    case 'e' :
                        strcpy(user_gnatproject_call_str, &argv[i][2]);
                        break;
                    case 'l' :
                        strcpy(gnatlsExe, &argv[i][2]);     //the caller supplied string executable for gnat ls
                        break;
                    default :
                        {fprintf(stderr, "Ourxref Warning: unknown option is ignored : %s\n", argv[i]);
                         fflush(stderr);
                        }
           }
         }
         else {fprintf(stderr, "Ourxref Warning: unknown parameter is ignored : %s\n", argv[i]); fflush(stderr);}

        }//end processing switches
    strcpy(original_file, argv[argc-1]);    //the last argument

    if (!(tmp_str = find_path(original_file, ".ali", " -a -o ", gnatlsExe, user_gnatproject_call_str, user_gnatls_call_str, debugMode))) {
      fprintf(stderr, "OURXREF error: %s.ali file could not be found\n", original_file);
      exit(12);
    }
    strcpy(path_found, tmp_str);
    strcat(path_found, "\\");
    strcat(path_found, original_file);
    strcat(path_found, ".ali");             //e.g. F:\bck_Mika\working directory\bck_Mika\RRnew\turbineoverheat.ali
    if (debugMode) fprintf(stderr, "OURXREF DEBUG: path found: %s\n", path_found);

    //if we reach this far the arguments passed are fine

    init_list_alies();  //queue of ali files to process

    add_ali(path_found);    //just for the first one
    get_next_ali();         //dummy call to ensure that the data structure of ali files is up to date (i.e. next ali file is pointing to NULL)

    yyin = fopen(path_found, "r");
    if (!yyin) {
      fprintf(stderr, "OURXREF ERROR: %s:  Can't open internal file", path_found);
      fflush(stderr);
      exit(21);
    }
    read_up_to_DLETTER(yyin);
    if (debugMode) fprintf(stderr, "Referencing : %s\n", path_found);
    else fprintf(stderr, ".");
    init_list_dependencies(); //data structure to keep track of declared dependencies in foo.ali
    yyparse();
    if (!debugMode) fprintf(stderr, "\n");
    exit(EXIT_SUCCESS);

} //end main

//     get_ali("s-auxdec.ads") == path of "s-auxdec.ali"
char *get_ali(char *source_name)
{  char ali_filename[_MAX_PATH];
   int i = 0;

   while (source_name[i] != '.') {
     ali_filename[i] = source_name[i];
     i++;
   }
  ali_filename[i] = '\0';
  if (!(tmp_str = find_path(ali_filename, ".ali", " -a -o ", gnatlsExe, user_gnatproject_call_str, user_gnatls_call_str, debugMode))) {
    fprintf(stderr, "\nOURXREF error in get_ali(): %s file could not be found\n", ali_filename);
    exit(128);
  }
  strcpy(ali_path_found, tmp_str);
  strcat(ali_path_found, "\\");
  strcat(ali_path_found, ali_filename);
  strcat(ali_path_found, ".ali");
  return ali_path_found;
}

//handles parsing errors
void yyerror(const char* s)
{
        extern int yychar;
        error_count++;

        //fprintf(stderr,"  %s", s);
        if (yylineno) {
          fprintf(stderr,"OURXREF Error: on line %d", yylineno);
          }
        fprintf(stderr," on input: ");
        if (yychar >= 0400) {
                switch (yychar) {
                        case DLETTER : fprintf(stderr, "%s a DLETTER\n", yytext);
                                break;
                        case XLETTER : fprintf(stderr, "%s a XLETTER\n", yytext);
                                break;
                        case SOURCE_NAME : fprintf(stderr, "%s a SOURCE_NAME\n", yytext);
                                break;
                        case INTEGER : fprintf(stderr, "%s a INTEGER\n", yytext);
                                break;
                        case ENTITY : fprintf(stderr, "%s a ENTITY\n", yytext);
                                break;
                        case SUBUNIT : fprintf(stderr, "%s a SUBUNIT\n", yytext);
                                break;
                        case LINE_CHAR_COL : fprintf(stderr, "%s a LINE_CHAR_COL\n", yytext);
                                break; 
                        default :
                                fprintf(stderr, "(token) %s\n", yytext);
                }
        } else {
                switch (yychar) {
                        case '\t': fprintf(stderr,"horizontal-tab\n"); break;
                        case '\n': fprintf(stderr,"newline\n"); break;
                        case '\0': fprintf(stderr,"end\n"); break;
                        case ' ': fprintf(stderr, "(blank)\n"); break;
                        default : fprintf(stderr,"(char) %c\n", yychar);
                }
        }
        exit(EXIT_FAILURE);
}// yyerror function

void my_exit(int exit_code) {
  exit(exit_code);
}

/**************************************END OF FILE *********************************************/