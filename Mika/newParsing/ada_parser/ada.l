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
/* A FLEX grammar for Ada                                               */
/* win_flex --wincompat ada.l                                           */
/************************************************************************/
/* there are three lexical analysers :                                  */
/*      REF to read foo.xref about the cross referencing                */
/*      PAC to read foo.bind about the elaboration order                */
/*      ADA to read the Ada source files                                */
/************************************************************************/

%s REF
%s PAC
%s ADA
 // 23/09/07 AA only used for dealing with ambiguity in the grammar with tic eg. Ok := Character'('X') < 'L'; 
%s AA                       
%option yylineno
%{
#pragma warning( disable : 4996 )  //ignore warnings such as : warning C4996: 'strcpy': This function or variable may be unsafe. Consider using strcpy_s instead. To disable deprecation, use _CRT_SECURE_NO_WARNINGS. See online help for details.
#include "string.h"
#include "stdlib.h"
#include "ctype.h"
#include "math.h"

#include "queue.h"              //used to keep the list of package names to process (according to elaboration order as per foo.bind)
#include "ada.tab.h"

extern struct binary_filename_node *filenames_in_xref;
extern char install_dir[];
extern char xref_file[];
extern char bind_file[];
extern int debugMode;
extern int old_lineno;
extern int is_standard;
extern char ada_version[];
extern struct unit_type *current_unit;
extern FILE *subprograms;
extern struct binary_filename_node *create_filename_node(char *name);
extern struct binary_filename_node *add_filename(struct binary_filename_node *root, struct binary_filename_node *r, struct binary_filename_node *c);

extern void my_exit(int);
extern int filename_has_been_parsed(struct binary_filename_node *root, char *key);

void count(void);           //count the column numbers in column var
void fill_id_ref(int);      //fill in the line and column fields of the id_ref structure to be returned via yylval
int lk_keyword(char *);     // returns the identifier(into the yylval.id_ref structure) or the actual Ada keyword token
void f_todecimal(char *);   //transform literal as in page 68 of Barnes into decimals
int get_next_source_file(void); // returns the next source file in the queue according to elaboration order
void to_lower(char *);
void check_subprograms(void);   //check if a idendtifier in XREF is a subprogram

char out[3000];         //used to output numbers via f_todecimal function
int error_count;        //number of parsing errors
int column = 0;
int next_yywrap = 0;    //to control yywrap's behaviour

%}
 //do not comment the macros : flex does not like it.
DIGIT                   [0-9]
EXTENDED_DIGIT          [0-9a-zA-Z]
INTEGER                 ({DIGIT}(_?{DIGIT})*)
EXPONENT                ([eE](\+?|-){INTEGER})
DECIMAL_LITERAL         {INTEGER}(\.?{INTEGER})?{EXPONENT}?
BASE                    {INTEGER}
BASED_INTEGER           {EXTENDED_DIGIT}(_?{EXTENDED_DIGIT})*
BASED_LITERAL           {BASE}#{BASED_INTEGER}(\.{BASED_INTEGER})?#{EXPONENT}?
%%
<REF>([a-zA-Z0-9\!\�\$\\%\\^\&\(\)\-\+\_\=\[\]\{\}\;\'\@\#\~\,\.])*\.(ads|adb)  { count();
                                                                                  to_lower(yytext);
                                                                                  yylval.id = malloc((strlen(yytext)+1)*sizeof(char));
                                                                                  strcpy(yylval.id, yytext);
                                                                                  //all the filenames mentionned in foo.xref are added to a binary tree of filenames (see binary_filename.c)
                                                                                  // this structure is then checked during parsing of foo.bind in ada.y to see if a file mantionned in foo.bind should really be added to the queue of file code to analyse
                                                                                  if (!filenames_in_xref) filenames_in_xref = add_filename(filenames_in_xref, filenames_in_xref, create_filename_node(yylval.id));
                                                                                  else add_filename(filenames_in_xref, filenames_in_xref, create_filename_node(yylval.id));
                                                                                  return(ref_filename);
                                                                                }
<REF>Decl:                   { count(); return(REF_DECL); }
<REF>Body:                   { count(); return(REF_BODY); }
<REF>Ref:                    { count(); return(REF_REF); }
<REF>Modi:                   { count(); return(REF_MODI); }
<REF>\"(\"\"|[^\n\"])*\"" "(.)*\n     { /* for ref_string, since gnat distributed with gcc 3.4.1, the type of the identifier is not preceded by the keyword "Type:"
                                           therefore, typically, we have to analyse things like '"+"      Something Object\n', where Something Object can be many things
                                           thus our strategy is to read the string as before, a space, rubbish and a newline into yytext
                                           and then process yytext to retain only the string up to the last " and pass it on to the parser via yylval.id as before
                                           ###
                                           It was decided (17/09/2004) to transform strings into 'string_' folowed by the ASCII codes of forming the string (due to parsing problem with special characters in Prolog
                                           and also because Return_"+" is not a valid variable name in Prolog (from a string operator in Ada))
                                           so internally all the parser knows is that strings are of the form string__...
                                        */            
                                        int n;
                                        char *ptr_last_quotes;
                                        int i = 1;
                                        char tmp[10];   //to hold character ascii int code

                                        count();
                                        ptr_last_quotes = strrchr(yytext, '"');     //pointer to last quote
                                        n = strlen(yytext)-strlen(ptr_last_quotes); //length of string up to last quote
                                        yylval.id_kind.id= malloc((n*4+8)*sizeof(char));    // *3 because ascii codes takes up to 3 characters + 1 for _
                                        strcpy(yylval.id_kind.id, "string");
                                        while (&yytext[i] != ptr_last_quotes) {
                                          itoa(yytext[i], tmp, 10);     //convert ascii number to string
                                          strcat(yylval.id_kind.id, "_");
                                          strcat(yylval.id_kind.id, tmp);
                                          i++;
                                        }
                                        check_subprograms();   //fills yylval.id_kind.kind
                                        return(ref_string);
                                      }
<REF>[a-zA-Z](_?[a-zA-Z0-9])*" "(.)*\n {//for ref_identifier, since gnat distributed with gcc 3.4.1, the type of the identifier is not preceded by the keyword "Type:"
                                        //therefore, typically, we have to analyse things like 'speed      Integer Object\n', where Integer Object can be many things
                                        //thus our strategy is to read the identifier as before, a space, rubbish and a newline into yytext
                                        //and then process yytext to retain only the string up to (but excluded) the first space and pass it on to the parser via yylval.id as before
                                           
                                         int n;
                                         count();
                                         n = strcspn(yytext, " ");      //get length of substring up to first occurence of space
                                         yylval.id_kind.id = malloc((n+1)*sizeof(char));
                                         yylval.id_kind.id[0] = '\0';
                                         strncat(yylval.id_kind.id, yytext, n); //copy substring up to n character
                                         to_lower(yylval.id_kind.id);
                                         if (strlen(yylval.id_kind.id)>48) {
                                             fprintf(stdout, "\nMika ERROR: the %s identifier is too long, length must be less than 49 characters\n", yylval.id_kind.id);
                                             fflush(stdout);
                                             my_exit(31);
                                         }
                                         check_subprograms();   //fills yylval.id_kind.kind
                                         return(ref_identifier);
                                       }
<REF>"'"."'"(" "|"{".*"}")(.)*\n   { /*for ref_character_literal, since gnat distributed with gcc 3.4.1, the type of the identifier is not preceded by the keyword "Type:"
                                        therefore, typically, we have to analyse things like 'a'      Something Object\n', where Something Object can be many things
                                        thus our strategy is to read the string as before, a space, rubbish and a newline into yytext
                                        ***
                                        It was decided (17/09/2004) to transform characters into 'char__' folowed by the ASCII codes of forming the character (due to parsing problem with special characters in Prolog )
                                        so internally all the parser knows is that characters are of the form char__...
                                        ***
                                        as noted 19/09/2004, the character ''' is outputted as '''{3E6} by gnatxref: we simply ignore this extra stuff in between { and }
                                      */
                                      char tmp[10];                             //to hold ascii int character code
                                      int read;
                                      count();
                                      yylval.id_kind.id = malloc(10*sizeof(char));
                                      strcpy(yylval.id_kind.id, "char__");              //also below in Ada
                                      if (yytext[1]<0) read = 256 + yytext[1];  //to deal with ascii > 128
                                      else read = yytext[1];
                                      itoa(read, tmp, 10);                      //convert ascii number to string
                                      strcat(yylval.id_kind.id, tmp);
                                      check_subprograms();   //fills yylval.id_kind.kind
                                      return(ref_character_literal);
                                    }
<REF>{INTEGER}               { count();
                               yylval.id = malloc((strlen(yytext)+1)*sizeof(char));
                               strcpy(yylval.id, yytext);
                               return(ref_integer);
                             }
<REF>":"                     { count(); return(REF_COLON); }
<REF>[ \t\n\f]               { count(); }
<REF>--.*                    { count(); } //added 08/05/09 to allow comments in our foo.xref file
<REF>.                       { count();
                               fprintf(stdout, "REF: Illegal character:%c: on line %d, column %d\n",*yytext, yylineno, column);
                               fflush(stdout);
                               error_count++;
                             }

<PAC>"ELABORATION ORDER"        { count(); return(PAC_START_ELABORATION); }
<PAC>"(spec)"                   { count(); return(PAC_SPEC); }
<PAC>"(body)"                   { count(); return(PAC_BODY); }
<PAC>"."                        { count(); return(PAC_DOT); }
<PAC>([a-zA-Z0-9\!\�\$\\%\\^\&\(\)\-\+\_\=\[\]\{\}\;\@\#\~\,])* { count();
                                                                  to_lower(yytext);
                                                                  yylval.id = malloc((strlen(yytext)+1)*sizeof(char));
                                                                  strcpy(yylval.id, yytext);
                                                                  return(pac_package_name);
                                                                }
<PAC>[ \t\n\f]               { count(); }
<PAC>.                       { count();
                               fprintf(stdout, "PAC: Illegal character:%c: on line %d, column %d\n",*yytext, yylineno, column);
                               fflush(stdout);
                               error_count++;
                             }

<ADA,AA>"."                     { count(); BEGIN(ADA); return('.'); }
<ADA,AA>"<"                     { count();
                                  BEGIN(ADA);
                                  yylval.id_ref.id = malloc(10);
                                  strcpy(yylval.id_ref.id, "string_60");
                                  fill_id_ref(1);
                                  return(LT);
                                }
<ADA,AA>"("                     { count(); BEGIN(ADA); return('('); }
<ADA,AA>"+"                     { count();
                                  BEGIN(ADA);
                                  yylval.id_ref.id = malloc(10);
                                  strcpy(yylval.id_ref.id, "string_43");
                                  fill_id_ref(1);
                                  return(PLUS);
                                }
<ADA,AA>"|"                     { count(); BEGIN(ADA); return('|'); }
<ADA,AA>"!"                     { count(); BEGIN(ADA); return('|'); }  //see compliance test A2A031A.ADA
<ADA,AA>"&"                     { count();
                                  BEGIN(ADA);
                                  yylval.id_ref.id = malloc(10);
                                  strcpy(yylval.id_ref.id, "string_38");
                                  fill_id_ref(1);
                                  return(CONC);
                                }
<ADA,AA>"*"                     { count();
                                  BEGIN(ADA);
                                  yylval.id_ref.id = malloc(10);
                                  strcpy(yylval.id_ref.id, "string_42");
                                  fill_id_ref(1);
                                  return(MULT);
                                }
<ADA,AA>")"                     { count(); BEGIN(ADA); return(')'); }
<ADA,AA>";"                     { count(); BEGIN(ADA); return(';'); }
<ADA,AA>"-"                     { count();
                                  BEGIN(ADA);
                                  yylval.id_ref.id = malloc(10);
                                  strcpy(yylval.id_ref.id, "string_45");
                                  fill_id_ref(1);
                                  return(MINUS);
                                }
<ADA,AA>"/"                     { count();
                                  BEGIN(ADA);
                                  yylval.id_ref.id = malloc(10);
                                  strcpy(yylval.id_ref.id, "string_47");
                                  fill_id_ref(1);
                                  return(DIV);
                                }
<ADA,AA>","                     { count(); BEGIN(ADA); return(','); }
<ADA,AA>">"                     { count();
                                  BEGIN(ADA);
                                  yylval.id_ref.id = malloc(10);
                                  strcpy(yylval.id_ref.id, "string_62");
                                  fill_id_ref(1);
                                  return(GT);
                                }
<ADA,AA>":"                     { count(); BEGIN(ADA); return(':'); }
<ADA,AA>"="                     { count();
                                  BEGIN(ADA);
                                  yylval.id_ref.id = malloc(10);
                                  strcpy(yylval.id_ref.id, "string_61");
                                  fill_id_ref(1);
                                  return(EQUAL);
                                }
<ADA,AA>"'"                     { count(); BEGIN(ADA); return(TIC); }
<ADA,AA>".."                    { count(); BEGIN(ADA); return(DOT_DOT); }
<ADA,AA>"<<"                    { count(); BEGIN(ADA); return(LT_LT); }
<ADA,AA>"<>"                    { count(); BEGIN(ADA); return(BOX); }
<ADA,AA>"<="                    { count();
                                  BEGIN(ADA);
                                  yylval.id_ref.id = malloc(13);
                                  strcpy(yylval.id_ref.id, "string_60_61");
                                  fill_id_ref(2);
                                  return(LT_EQ);
                                }
<ADA,AA>"**"                    { count();
                                  BEGIN(ADA);
                                  yylval.id_ref.id = malloc(13);
                                  strcpy(yylval.id_ref.id, "string_42_42");
                                  fill_id_ref(2);
                                  return(EXPON);
                                }
<ADA,AA>"/="                    { count();
                                  BEGIN(ADA);
                                  yylval.id_ref.id = malloc(13);
                                  strcpy(yylval.id_ref.id, "string_60_62");
                                  fill_id_ref(2);
                                  return(NE);
                                }
<ADA,AA>">>"                    { count(); BEGIN(ADA); return(GT_GT); }
<ADA,AA>">="                    { count();
                                  BEGIN(ADA);
                                  yylval.id_ref.id = malloc(13);
                                  strcpy(yylval.id_ref.id, "string_62_61");
                                  fill_id_ref(2);
                                  return(GE);
                                }
<ADA,AA>":="                    { count(); BEGIN(ADA); return(IS_ASSIGNED); }
<ADA,AA>"=>"                    { count(); BEGIN(ADA); return(RIGHT_SHAFT); }
<ADA,AA>[a-zA-Z](_?[a-zA-Z0-9])* { count(); BEGIN(ADA); return(lk_keyword(yytext)); }
<ADA>"'"."'"                 { //for Ada rule 2.5
                               char tmp[10];                            //to hold ascii int character code
                               int read;
                               count();
                               yylval.id_ref.id = malloc(10);
                               strcpy(yylval.id_ref.id, "char__");      //also above in REF
                               if (yytext[1]<0) read = 256 + yytext[1]; //to deal with ascii > 128
                                      else read = yytext[1];
                               itoa(read, tmp, 10);                     //convert ascii number to string
                               strcat(yylval.id_ref.id, tmp);
                               fill_id_ref(strlen(yytext));             //fill the line and column fields
                               BEGIN(ADA);
                               return(character_literal);
                             }
<ADA,AA>\"(\"\"|[^\n\"])*\"  { //same remark as for strings in REF parsing above
                               int n;
                               int i = 1;
                               char tmp[10];    //to hold ascii int character code

                               count();
                               n = strlen(yytext)-2;    //the length of the actual string without the quotes
                               yylval.id_ref.id = malloc((n*4+8)*sizeof(char));
                               strcpy(yylval.id_ref.id, "string");
                               while (i <= n) {
                                     itoa(yytext[i], tmp, 10);      //convert ascii number to string
                                     strcat(yylval.id_ref.id, "_");
                                     strcat(yylval.id_ref.id, tmp);
                                     i++;
                               }
                               fill_id_ref(strlen(yytext));    //fill the line and column fields
                               BEGIN(ADA);
                               return(string_literal);
                             }
<ADA,AA>{DECIMAL_LITERAL}    { char tmp_s[300];
                               int i = 0;
                               int j = 0;
                               count();
                               //f_todecimal only handle based literals for simplicity
                               //hence DECIMAL_LITERALs need to be transformed into a BASED_LITERAL prior sending to f_todecimal
                               //e.g. 10.2 needs to be transformed into 10#10.2#
                               //e.g. 34e+2 into 10#34#e+2
                               strcpy(tmp_s, "10#");
                               j = strlen(tmp_s);
                               while (yytext[i] != 'e' && yytext[i] != 'E' && yytext[i] != '\0') {
                                 tmp_s[j] = yytext[i];
                                 i++;
                                 j++;
                               }
                               tmp_s[j] = '#';
                               j++;
                               while (yytext[i] != '\0') {
                                 tmp_s[j] = yytext[i];
                                 i++;
                                 j++;
                               }
                               tmp_s[j] = '\0';
                               f_todecimal(tmp_s);
                               yylval.id = malloc((strlen(out)+1)*sizeof(char));
                               //printf("hi1 from ada.l\n");
                               strcpy(yylval.id, out);
                               //printf("hi2 from ada.l\n");
                               BEGIN(ADA);
                               return(numeric_literal);
                             }
<ADA,AA>{BASED_LITERAL}      { count();
                               f_todecimal(yytext);
                               yylval.id = malloc((strlen(out)+1)*sizeof(char));
                               strcpy(yylval.id, out);
                               BEGIN(ADA);
                               return(numeric_literal);
                             }
<ADA,AA>--.*                  { // 13/06/07 was <ADA>--.*\n but then a file could not end with a comment...
                                count(); 
                                BEGIN(ADA);
                              }
<ADA,AA>[ \t\n\f]             { count(); BEGIN(ADA);}
<ADA,AA>.                     { count();
                                fprintf(stdout, "Illegal in lex character:%c: on line %d, column %d", *yytext, yylineno, column);
                                fflush(stdout);
                                error_count++;
                              }
%%
// counts the number of column as, unlike the line number, it is not directly available in lex
void count() {
        int i;

        for (i = 0; yytext[i] != '\0'; i++)
                if (yytext[i] == '\n')
                        column = 0;
                else if (yytext[i] == '\t')
                        column += 8 - (column % 8);
                else
                        column++;
}

// standard flex function called automatically at the end of file being parsed
// re-written (normally only returns 1) to allow parsing of various input files
// parsing order:   1st standard.xref by default (see ada.y)
//                  2nd standard.ads
//                  3rd foo.xref is parsed next
//                  4th foo.bind is parsed next as created by gnatmake
//                              then code files according to elaboration order (including foo.ads)
int yywrap()
{ char tmp_s[_MAX_PATH];

  //i.e. standard.xref has already been analysed (see ada.y for initial call)
  if (next_yywrap == 0) {
    fclose(yyin);
    //print cross reference binary_tst
    //decl_print_start();
    yylineno = 1;  //re-initialisation for new file
    column = 0;    //re-initialisation for new file
    next_yywrap++; //next time
    BEGIN(ADA);       //now lexically analysing the Ada code standard.ads
    strcpy(tmp_s, install_dir);
    strcat(tmp_s, "\\..\\resources\\");
    if (!strcmp(ada_version, "-gnat83"))
      strcat(tmp_s, "standard83/standard.ads");  //because they are different (e.g. characters are only from 0 to 127)
    else if (!strcmp(ada_version, "-gnat95"))
      strcat(tmp_s, "standard95/standard.ads");
    else if (!strcmp(ada_version, "-gnat05"))
      strcat(tmp_s, "standard05/standard.ads");
    else {
      fprintf(stdout, "Mika ERROR: Ada version is not valid somehow: %s\n", ada_version);
      fflush(stdout);
      my_exit(7);
    }
    current_unit = malloc (sizeof *(current_unit));
    current_unit->name = "standard";
    current_unit->filename = "standard";
    current_unit->suffix = ".ads";
    current_unit->path = malloc(_MAX_PATH);
    strcpy(current_unit->path, install_dir);
    strcat(current_unit->path, "\\..\\resources");
    yyin = fopen(tmp_s, "r");
    if (!yyin) {
      fprintf(stdout, "Mika ERROR: Can't open %s\n", tmp_s);
      my_exit(32);
    }
    if (debugMode) {
        fprintf(stdout, "\nMika START code file: %s\n", tmp_s);
        fflush(stdout);
    }
    return(0);    //parsing carries on yyin
  }
  if (next_yywrap == 1) {
    fclose(yyin);
    is_standard = 0;    //we are now longer parsing a standard file
    yylineno = 1;       //re-initialisation for new file
    column = 0;         //re-initialisation for new file
    next_yywrap++;      //next time
    BEGIN(REF);       //now lexically analysing the Ref file foo.xref
    yyin = fopen(xref_file, "r");
    if (!yyin) {
      fprintf(stdout, "Mika ERROR: Can't open %s", xref_file);
      my_exit(33);
    }
    if (debugMode) {
        fprintf(stdout, "\nMika START xref file: %s\n", xref_file);
        fflush(stdout);
    }
    return(0);    //parsing carries on yyin
  }
  if (next_yywrap == 2) { //next is initialised at 2
    fclose(yyin);   //closing the xref file
    if (!debugMode) {
        remove(xref_file);  //part of stdio.h : delete the xref file
    }
    fclose(subprograms);    //nothing more will be added to it now that foo.xref has been fully processed
    yylineno = 1;  //re-initialisation for new file
    column = 0;    //re-initialisation for new file
    next_yywrap++; //next time
    BEGIN(PAC);       //now lexically analysing a file of packages (foo.bind)
    yyin = fopen(bind_file, "r");
    if (!yyin) {
      fprintf(stdout, "Mika ERROR: Can't open %s", bind_file);
      my_exit(34);
    }
    if (debugMode) {
        fprintf(stdout, "\nMika START bind file: %s\n", bind_file);
        fflush(stdout);
    }
    return(0);    //parsing carries on yyin
  }
  if (next_yywrap == 3) {
    fclose(yyin);   //closing the bind file
    if (!debugMode) {
        remove(bind_file);  //part of stdio.h : delete the bind file
    }
    else {
      fprintf(stdout, "Mika DEBUG The queue of files present in foo.bind is:");
      print_queue();
      //print cross reference binary_tst
      //ref = stdout;
      //decl_print_start();
    }
    BEGIN(ADA);  //now lexically analysing Ada code files
    next_yywrap++; //next time
    return get_next_source_file();     //first source file
  }
  if (next_yywrap == 4 ) { //only Ada code files (as per elaboration order defined in foo.bind) from now on
    fclose(yyin);
    old_lineno = yylineno-1;
    //printf("yywrap4 called\n");
    return get_next_source_file();   //next source file (may end yywrap)
  }
  fprintf(stdout, "Mika ERROR: parsing problem");
  my_exit(35);
  return 1;  //just to return something to keep the compiler happy
}//yywrap function

//set the lexer to recognise REF tokens
void callBEGIN_REF() {
    BEGIN(REF);
}

// returns the next source file in the queue according to elaboration order
// uses queue.c
int get_next_source_file()
{
  char tmp_full_path[_MAX_PATH];
  char tmp_full_filename[_MAX_PATH];

  yylineno = 1;  //re-initialisation for new file
  column = 0;    //re-initialisation for new file
  if (!queue_isEmpty()) { //more files to analyse
    current_unit = get_queue();
    strcpy(tmp_full_filename, current_unit->filename);
    strcat(tmp_full_filename, current_unit->suffix);
    strcpy(tmp_full_path, current_unit->path);
    strcat(tmp_full_path, "\\");
    strcat(tmp_full_path, tmp_full_filename);
    yyin = fopen(tmp_full_path, "r");
    if (!yyin) {
      fprintf(stdout, "Mika ERROR: Can't open %s\n", tmp_full_path);
      my_exit(38);
    }
    if (!filename_has_been_parsed(filenames_in_xref, tmp_full_filename)) {
      fprintf(stdout, "Mika ERROR: parsing extra file %s\n", tmp_full_filename);
      my_exit(39);
    }
    if (debugMode) {
        fprintf(stdout, "MIka DEBUG Intermediate Prolog generation for: %s\n", tmp_full_filename);
        fflush(stdout);
    }
    return(0);  //parsing carries on yyin
  }
  else return(1);   //we have processed the entire queue: no more source file yywrap will stop
}// get_next_source_file function


//result is in the out variable
//transform literal as in page 68 of Barnes into decimals
//see the literal folder for test driver
void f_todecimal(char * s_literal)
{
#define MAX_INT 16777215
char s1[100], s2[100], s3[100];
int radix, ibase, sign,  exp, d;
int is_an_integer = 1;
_int64 initial;
double after_decimal_point;

double n, ipart, fpart, exp2;
int i = 0;
int j = 0;
int k = 0;

 while (s_literal[i] != '#')
 {
         if (s_literal[i] != '_') {
                 s1[j] = s_literal[i];          //collect the base or the entire number if no base
                 j++;
     }
         i++;
 }
 s1[j] = '\0';
 ibase = atoi(s1);
 j = 0;
 i++; //to skip the '#'
 while (s_literal[i] != '#')
 {
         if (s_literal[i] != '_') {
                 s2[j] = s_literal[i];          //collect the number if it is based
                 j++;
         }
         i++;
 }
 s2[j] = '\0';
 radix = strcspn(s2, ".");      //radix is the index of the '.'
 k = radix-1;
 j = 0;
 n = 0;
 initial = 0;
 while (k != -1) {                      //the integer part
         if (isdigit(s2[k])) d = s2[k] - 48;
                 else d = toupper(s2[k]) - 65 + 10;
         initial = initial + d*(_int64)pow(ibase, j);
         j++;
         k--;
 }
 n = initial;
 if ((unsigned int) radix != strlen(s2)) { //there is a radix point
         is_an_integer = 0;
         after_decimal_point = 0;
         k = radix+1;
         j = -1;
         while (s2[k] != '\0') {
                 if (isdigit(s2[k])) d = s2[k] - 48;
                 else d = toupper(s2[k]) - 65 + 10;
         n = n + d*pow(ibase, j);
             j--;
             k++;
         }
 }
 j = 0;
 i++; //to skip the '#'
 while (s_literal[i] != '\0')
 {
         if (s_literal[i] != '_') {
                 s3[j] = s_literal[i];          //collect the exponent
                 j++;
         }
         i++;
 }
 s3[j] = '\0';

 if (s3[0] == '\0') { // no exponent
         if (is_an_integer) sprintf(out, "%I64i", initial);
         else sprintf(out, "%.13f", n); //13 digits after the decimal point
         return;
 }
 if (s3[1] == '-') {
         sign = -1;
         i = 2;
 }
 else if (s3[1] == '+') {
         sign = 1;
         i = 2;
 }
 else {
         sign = 1;
         i = 1;
 }
 exp = atoi(&s3[i]);

 if (pow(ibase, exp) < 1.0e20) { //this check is needed otherwise the expression for n below may be too large
    n = (n*pow(ibase, exp*sign));
    fpart = modf(n, &ipart);
    if ((unsigned int)radix == strlen(s2)) sprintf(out, "%.0f", ipart);      //an integer
    else sprintf(out, "%.20f", n); //20 digits after the decimal point
 }
 else { //too large : uses exponent notation
    exp2 = exp * sign * log10(ibase);
    fpart = modf(exp2, &ipart);        //Splits a floating-point value into fractional and integer parts.
    //printf("\nipart is %f, fpart is %f,", ipart, fpart);
    n = (n*pow(10, fpart));
    if ((unsigned int) radix == strlen(s2)) sprintf(out, "%.0f", ipart);  // an integer
    else
        if (ipart > 308) { //should check that number is not > than 1.8e308 (otherwise SICSTUS float range is exceeded)
            if (!is_standard || debugMode) {
              fprintf(stdout, "Mika Warning: Found a float too large %fe%.0f reduced to %fe308\n", n, ipart, 1.79*sign);
              fflush(stdout);
            }
            sprintf(out, "%fe308", 1.79*sign);
        }
        else sprintf(out, "%.20fe%.0f", n, ipart);
 }
 return;
} //f_todecimal

typedef struct {
        char    * kw;
        int     kwv;
        } KEY_TABLE;

#define NUM_KEYWORDS  72   //number of Ada keywords in key_tab table

// keywords sorted in alphabetical order used in lk_keyword function
KEY_TABLE key_tab[NUM_KEYWORDS] = {
                {"abort",       ABORT},
                {"abs",         ABS},
                {"abstract",    ABSTRACT},
                {"accept",      ACCEPT},
                {"access",      ACCESS},
                {"aliased",     ALIASED},
                {"all",         ALL},
                {"and",         AND},
                {"array",       ARRAY},
                {"at",          AT},
                {"begin",       BEGiN},
                {"body",        BODY},
                {"case",        CASE},
                {"constant",    CONSTANT},
                {"declare",     DECLARE},
                {"delay",       DELAY},
                {"delta",       DELTA},
                {"digits",      DIGITS},
                {"do",          DO},
                {"else",        ELSE},
                {"elsif",       ELSIF},
                {"end",         END},
                {"entry",       ENTRY},
                {"exception",   EXCEPTION},
                {"exit",        EXIT},
                {"for",         FOR},
                {"function",    FUNCTION},
                {"generic",     GENERIC},
                {"goto",        GOTO},
                {"if",          IF},
                {"in",          IN},
                {"interface",   INTERFACE},
                {"is",          IS},
                {"limited",     LIMITED},
                {"loop",        LOOP},
                {"mod",         MOD},
                {"new",         NEW},
                {"not",         NOT},
                {"null",        NuLL},
                {"of",          OF},
                {"or",          OR},
                {"others",      OTHERS},
                {"out",         OUT},
                {"overriding",  OVERRIDING},
                {"package",     PACKAGE},
                {"pragma",      PRAGMA},
                {"private",     PRIVATE},
                {"procedure",   PROCEDURE},
                {"protected",   PROTECTED},
                {"raise",       RAISE},
                {"range",       RANGE},
                {"record",      RECORD},
                {"rem",         REM},
                {"renames",     RENAMES},
                {"requeue",     REQUEUE},
                {"return",      RETURN},
                {"reverse",     REVERSE},
                {"select",      SELECT},
                {"separate",    SEPARATE},
                {"subtype",     SUBTYPE},
                {"synchronized",SYNCHRONIZED},
                {"tagged",      TAGGED},
                {"task",        TASK},
                {"terminate",   TERMINATE},
                {"then",        THEN},
                {"type",        TYPE},
                {"until",       UNTIL},
                {"use",         USE},
                {"when",        WHEN},
                {"while",       WHILE},
                {"with",        WITH},
                {"xor",         XOR}
                };

// put a string into lower case
void to_lower(char *str) {
        char * cp;
        for (cp=str; *cp; cp++) {
                if (isupper(*cp)) *cp += ('a' - 'A') ;
        }
}//to_lower function

// returns the identifier(into the yylval.id_ref structure) or the actual Ada keyword token
// part of the original .y file
int lk_keyword(char *str)
 {
        int min;
        int max;
        int guess, compare;

        min = 0;
        max = NUM_KEYWORDS-1;
        guess = (min + max) / 2;
        to_lower(str);

        for (guess=(min+max)/2; min<=max; guess=(min+max)/2) {
                if ((compare = strcmp(key_tab[guess].kw, str)) < 0) {
                        min = guess + 1;
                } else if (compare > 0) {
                        max = guess - 1;
                } else if (!strcmp(key_tab[guess].kw, "and")) { //not a very effcient way : should be caught before hand : but this is the least disruptive way
                        BEGIN(ADA);
                        yylval.id_ref.id = malloc(18);
                        strcpy(yylval.id_ref.id, "string_97_110_100");
                        fill_id_ref(3);
                        return(AND);
                } else if (!strcmp(key_tab[guess].kw, "return")) {
                        BEGIN(ADA);
                        yylval.id_ref.id = malloc(7);
                        strcpy(yylval.id_ref.id, "return");
                        fill_id_ref(6);
                        return(RETURN);
                } else if (!strcmp(key_tab[guess].kw, "in")) {
                        BEGIN(ADA);
                        yylval.id_ref.id = malloc(3);
                        strcpy(yylval.id_ref.id, "in");
                        fill_id_ref(2);
                        return(IN);
                } else if (!strcmp(key_tab[guess].kw, "mod")) {
                        BEGIN(ADA);
                        yylval.id_ref.id = malloc(19);
                        strcpy(yylval.id_ref.id, "string_109_111_100");
                        fill_id_ref(3);
                        return(MOD);
                } else if (!strcmp(key_tab[guess].kw, "or")) {
                        BEGIN(ADA);
                        yylval.id_ref.id = malloc(15);
                        strcpy(yylval.id_ref.id, "string_111_114");
                        fill_id_ref(2);
                        return(OR);
                } else if (!strcmp(key_tab[guess].kw, "rem")) {
                        BEGIN(ADA);
                        yylval.id_ref.id = malloc(19);
                        strcpy(yylval.id_ref.id, "string_114_101_109");
                        fill_id_ref(3);
                        return(REM);
                } else if (!strcmp(key_tab[guess].kw, "xor")) {
                        BEGIN(ADA);
                        yylval.id_ref.id = malloc(19);
                        strcpy(yylval.id_ref.id, "string_120_111_114");
                        fill_id_ref(3);
                        return(XOR);
                } else if (!strcmp(key_tab[guess].kw, "abs")) {
                        BEGIN(ADA);
                        yylval.id_ref.id = malloc(17);
                        strcpy(yylval.id_ref.id, "string_97_98_115");
                        fill_id_ref(3);
                        return(ABS);
                } else if (!strcmp(key_tab[guess].kw, "not")) {
                        BEGIN(ADA);
                        yylval.id_ref.id = malloc(19);
                        strcpy(yylval.id_ref.id, "string_110_111_116");
                        fill_id_ref(3);
                        return(NOT);
                }
                else
                { BEGIN(ADA);
                  return key_tab[guess].kwv;
                }
        }
        yylval.id_ref.id = malloc((strlen(str)+1)*sizeof(char));
        strcpy(yylval.id_ref.id, str);
        fill_id_ref(strlen(str));   //fill the line and column fields
        BEGIN(AA);
        return identifier;
 }//lk_keyword function

//fill in the line and column fields of the id_ref structure to be returned via yylval
void fill_id_ref(int len)
{       char lineno[10];
        char columnno[10];
        int correct_column;
        itoa(yylineno, lineno, 10);
        strcpy(yylval.id_ref.line, lineno);
        correct_column = column - len + 1;
        itoa(correct_column, columnno, 10);
        strcpy(yylval.id_ref.column, columnno);
}//fill_id_ref function

void check_subprograms(void)
{
    if (!strncmp("function", &yytext[strlen(yytext)-9], 8)) {
      // it is a function
      yylval.id_kind.kind = 1;
    }
    else
      if (!strncmp("procedure", &yytext[strlen(yytext)-10], 9)) {
        // it is a procedure
        yylval.id_kind.kind = 2;
      }
      else {
        yylval.id_kind.kind = 0;
      }
}  //check_subprograms function
/**************************************END OF ADA.L FILE *********************************************/