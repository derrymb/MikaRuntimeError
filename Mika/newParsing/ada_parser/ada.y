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
/******* A bison grammar for Ada ************************************/
/* Midoan Software Engineering Solutions Ltd.                       */
/* win_bison -d ada.y                                               */
/********************************************************************/

%{
#pragma warning( disable : 4996 )  //ignore warnings such as : warning C4996: 'strcpy': This function or variable may be unsafe. Consider using strcpy_s instead. To disable deprecation, use _CRT_SECURE_NO_WARNINGS. See online help for details.

#include <errno.h>		//system error constants
#include <stdio.h>
#include <ctype.h>
#include <io.h>
#include <stdlib.h>
#include <string.h>
#include <process.h>            //for system function
#include <direct.h>             //for directory creation
#include <time.h>               //for time and date retrieval

extern int yylex();
extern void callBEGIN_REF();

extern int yylineno;
extern FILE* yyin;
extern char *yytext;
extern int error_count;
extern struct {
        char    * kw;
        int     kwv;
        } key_tab[];

void yyerror(const char*);
void my_exit(int);             //attempts to close handles and delete generated files prior to caling exit(int);

FILE *parsed;                   //main output file foo.pl
FILE *Fcond_ids;                //will contain conditions, decisions and branches ids and source code links
FILE *subprograms;              //contains the list of subprograms contained in the original file

#include "find_path.c"          //used to find file names using gnat ls
#include "queue.h"              //used to keep the list of package names to process (according to elaboration order as per foo.bind)
#include "binary_filename.c"    //used to store filenames appearing in foo.xref and searched during processing of foo.bind
#include "double_tst.c"         //used to store gnatxref output
#include "getdatetime.c"        //generate a pointer to a string formating the current date and time in format yy_mm_dd_hh_mm_ss
#include "transform_path_to_prolog.c" //transform a windows path with '\' and no final '\' to a prolog path containing only '/' and terminating with a '/'
#include "list_of_not_cross_referenced_operator.c" //09/03/2010 track not actualy user defined operators   

#define YYSTACK_SIZE 1000               //Parser generator constant
#define SAFETY 5                        //number of characters added to malloc
#define COND 0
#define GATE 1
#define DECI 2
#define BRAN 3
#define RUNE 4

struct unit_type *current_unit;
char install_dir[_MAX_PATH*10];      // install directory of ada_par.exe set in main
char gnat_install_dir[_MAX_PATH*10];      // install directory of MinGW set in main
char ada_version[10] = "";               // must be set to "-gnat95", "-gnat83" or "-gnat05"
char gnatmakeExe[_MAX_PATH];        // string denoting the name of the gnatmake executable (could be "dotnet-gnatmake.exe")
char gnatkrExe[_MAX_PATH];          // string denoting the name of the gnatkr executable
char gnatlsExe[_MAX_PATH];          // string denoting the name of the gnat ls executable
char gnatbindExe[_MAX_PATH];          // string denoting the name of the gnat bind executable
char user_gnatproject_call_str[_MAX_PATH*10];
char user_gnatls_call_str[_MAX_PATH*10];

void find_filename_path(char *, char *, char **, char **);
char *handle_operator_calls(struct id_ref_t);
char *handle_identifiers(struct id_ref_t, int);
int is_predefined_id(char *);
void printout_shortened(char *);
int is_new_intrinsic(char *, char *);
void initialise_parsed();
void to_lower(char *);
void print_coverage_details(int, char *, struct unit_type *, int, int);

char key[1000];         /* used for searching double_tst structure */
struct binary_filename_node *filenames_in_xref = NULL;  //see binary_filename.c (used to keep track of the files mentionned in foo.xref; build in ada.l)
struct binary_filename_node *predefined_ids_root = NULL;    //root of predefined ids binary tree see binary_filename.c (used to keep track of the ids mentionned in standard.xref)

int tmp_token;          /* temporary token holder */
char *tmp_chr_ptr;
char tmp_s[_MAX_PATH*10];        /* temporary string holder */
char tmp_s2[_MAX_PATH*10];        /* temporary string holder */
char tmp_s3[_MAX_PATH*10];        /* temporary string holder */
char *tmp_filename_s;   //temporary filename holder during non-terminal ref_reference and ref_body parsing

char type[20];  //to record the type of packages put onto the stack (subunit or otherwise)
char not_cross_referenced_operator[256];

int condition_nb = 1;   //counter for the number of conditions
int gate_nb = 1;        //counter for individual gate 
                        //(i.e. boolean operators : and, or, xor, not and_then, or_else, not)
int decision_nb = 1;    //counter for the number of decisions
int branch_nb = 1;      //counter for the number of branches
int runtime_nb = 1;     //counter for the number of runtime error checks

int debugMode = 0;      //flag to indicate if we are in debug mode set by by -d command line switch
int is_standard = 1;    //flag to indicate that we are parsing ada predefined standard package
int is_ascii = 0;       //flag to indicate that we are within a 'selected(Ascii, ...)' construct (Total hack see 28/09/04 notes)
int in_a_pragma = 0;    //flag to indicate that we are within a pragma : changes the behaviour of handle_identifier (if unxrefed return id 'as is', e.g. 'C' calling convention will be returned as 'C' within a pragma)

int old_lineno = 0;
int total_line_no = 0;
int correct_column = 0;
extern int column;      //from ada.l

struct decl_item *found_decl;           // the unique declaration point returned when processing identifiers

struct decl_item *current_decl;         // the current declaration during foo.xref parsing: a structure such as ("Set_9", "a.adb:10:12:set")
struct decl_item *current_unxrefed;
char *dcurrent_id;                      // the current identifier during foo.xref parsing
int dcurrent_kind;                      // the current kind of the identifier during foo.xref parsing
char xref_id[500];
char *dxref_id;                         // e.g. "a.ads:10:12:speed" during REF parsing
char *key2;

char current_subprogram_name[_MAX_PATH];
char current_package_name[_MAX_PATH];
char original_package_name[_MAX_PATH];  // stores argv[argc-1]
char xref_file[_MAX_PATH];  //foo.xref file with full path
char bind_file[_MAX_PATH];  //foo.bind file with full path
char pl_file[_MAX_PATH];    //foo.pl file with full path
char po_file[_MAX_PATH];    //foo.po file with full path
char subprograms_file[_MAX_PATH];  //foo.subprograms file with full path

char output_file_name[_MAX_PATH];
char cwd[_MAX_PATH];             //the current working directory
char new_dir[_MAX_PATH];                                // newly created, time dependent, directory

typedef struct {
  char    *filename;
  char    *id;
  int     printed;
} INTRINSIC_TABLE;

# define NUM_INTRINSIC  4   //number of intrinsic subprograms

INTRINSIC_TABLE intrinsic_tab[NUM_INTRINSIC] = {
        {"unchconv.ads",    "unchecked_conversion", 0},
        {"a-unccon.ads",    "unchecked_conversion", 0},
        {"unchdeal.ads",    "unchecked_deallocation", 0},
        {"a-uncdea.ads",    "unchecked_deallocation", 0}
};

void recover_keyword(char *);
void build_expression(struct id_decision, char **, struct unit_type *, int, int);
void build_condition(struct id_decision, char **, struct unit_type *, int, int);
%}
%code requires { //see https://stackoverflow.com/a/4941440/671627
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
}
//values returned by lexer or the parser
%union {char *id;                           // for REF, PAC and ADA most stuff
        struct id_kind_t id_kind;           // for identifiers and strings in REF
        struct id_ref_t  id_ref;            // only for ADA identifiers, character literals, string literals, operators
        struct id_decision id_deci;         // for relation and expression_2 to indicate if we are within a decision
        struct id_subprogram_t id_subprogram;       //for subprograms only
        struct true_line_column_t true_line_column; //used for recording line and columns of detected bran,deci,cond, rune and gates
       }

%token PAC_START_ELABORATION
%token <id> pac_package_name
%token PAC_SPEC
%token PAC_BODY
%token PAC_DOT

%type <id> pac_name
%type <id> pac_dot_opt
%type <id> pac_spec_or_body

%token REF_COLON
%token REF_DECL
%token REF_BODY
%token REF_REF
%token REF_MODI

%token <id_kind> ref_string
%token <id> ref_filename
%token <id_kind> ref_identifier
%token <id_kind> ref_character_literal
%token <id> ref_integer

%token TIC //'
%token DOT_DOT //..
%token LT_LT //<<
%token BOX
%token GT_GT //>>
%token IS_ASSIGNED
%token RIGHT_SHAFT
//start of the keywords : keep together for yyerror
%token ABORT
%token <id_ref> ABS 
%token ABSTRACT
%token ACCEPT
%token ACCESS
%token ALIASED
%token ALL
%token <id_ref> AND
%token ARRAY
%token AT
%token BEGiN
%token BODY
%token CASE
%token CONSTANT
%token DECLARE
%token DELAY
%token DELTA
%token DIGITS
%token DO
%token ELSE
%token ELSIF
%token END
%token ENTRY
%token EXCEPTION
%token EXIT
%token FOR
%token FUNCTION
%token GENERIC
%token GOTO
%token IF
%token <id_ref> IN
%token INTERFACE
%token IS
%token LIMITED
%token LOOP
%token <id_ref> MOD
%token NEW
%token <id_ref> NOT
%token NuLL
%token OF
%token <id_ref> OR
%token OTHERS
%token OUT
%token OVERRIDING
%token PACKAGE
%token PRAGMA
%token PRIVATE
%token PROCEDURE
%token PROTECTED
%token RAISE
%token RANGE
%token RECORD
%token <id_ref> REM
%token RENAMES
%token REQUEUE
%token <id_ref> RETURN
%token REVERSE
%token SELECT
%token SEPARATE
%token SUBTYPE
%token SYNCHRONIZED
%token TAGGED
%token TASK
%token TERMINATE
%token THEN
%token TYPE
%token UNTIL
%token USE
%token WHEN
%token WHILE
%token WITH
%token <id_ref> XOR
//end of keywords

%token <id_ref> PLUS
%token <id_ref> MINUS
%token <id_ref> MULT
%token <id_ref> DIV
%token <id_ref> EXPON
%token <id_ref> CONC //concatenate?
%token <id_ref> EQUAL
%token <id_ref> NE
%token <id_ref> LT
%token <id_ref> LT_EQ
%token <id_ref> GT
%token <id_ref> GE
%token <id_ref> character_literal
%token <id_ref> identifier
%token <id_ref> string_literal
%token <id> numeric_literal

/* %type <id> goal_symbol */
%type <id> new_ada_2005_reserved_words
%type <id> new_ada_95_reserved_words
%type <id> pragma
%type <id> pragma_argument_association_list
%type <id> pragma_argument_association
%type <id> pragma_s
%type <id> object_declaration
%type <id> number_declaration
%type <id> identifier_list
%type <id> identifier_rule
%type <id> object_qualifier_opt
%type <id> object_subtype_definition
%type <id> init_opt
%type <id> type_declaration
%type <id> type_definition
%type <id> type_completion
%type <id> discriminant_part_opt
%type <id> interface_type_definition
%type <id> kind_opt
%type <id> interface_list_item_s
%type <id> interface_list_item_sl
%type <id> interface_list_opt
%type <id> access_type_definition
%type <id> access_type_definition_part
%type <id> subtype_declaration
%type <id> subtype_indication
%type <id> constraint
%type <id> digits_constraint
%type <id> delta_constraint
%type <id> derived_type_definition
%type <id> private_type_kind
%type <id> range_constraint
%type <id> range
%type <id> enumeration_type_definition
%type <id> enum_identifier_list
%type <id> enum_identifier
%type <id> integer_type_definition
%type <id> real_type_definition
%type <id> floating_point_definition
%type <id> range_specification_opt
%type <id> range_specification
%type <id> range_constraint_opt
%type <id> fixed_point_definition
%type <id> record_type_definition
%type <id> record_definition
%type <id> component_list
%type <id> component_declaration_list
%type <id> variant_part_opt
%type <id> component_declaration
%type <id> discriminant_part
%type <id> discriminant_spec_s
%type <id> discriminant_spec
%type <id> access_or_subtype_disc
%type <id> variant_part
/* %type <id> variant_s */
/* %type <id> variant */
%type <id> array_type_definition
%type <id> unconstrained_array_definition
%type <id> component_definition
%type <id> index_subtype_definition_list
%type <id> index_subtype_definition
%type <id> constrained_array_definition
%type <id> discrete_range_list
%type <id> name_list
%type <id> aliased_opt
%type <id> aggregate
%type <id> value_s_2
%type <id> comp_assoc
%type <id> choice_s
%type <id> choice
%type <id> discrete_range
%type <id> discrete_with_range
%type <id> protected_opt
%type <id> null_exclusion_opt
%type <id> access_definition
%type <id> name
%type <id> compound_name
%type <id> direct_name
%type <id> indexed_component
%type <id> value_list
%type <id> value
%type <id> selected_component
%type <id> attribute_reference
%type <id> attribute_id
%type <id> expression
%type <id_deci> expression_2
%type <id_ref> boolean_operator
%type <id_deci> relation
%type <id> range_or_name
%type <id_ref> membership
%type <id_deci> simple_expression
%type <id> unary_adding_operator
%type <id> binary_adding_operator
%type <id_deci> term
%type <id> multiplying_operator
%type <id_deci> factor
%type <id_deci> primary
%type <id> literal
%type <id_ref> relational_operator
%type <id> qualified_expression
%type <id_deci> parenthesized_primary
%type <id> cond_expression_list
%type <id> cond_expression
%type <id> else_expression_opt
%type <id> allocator
%type <id> sequence_of_statements
%type <id> unlabeled_statement
%type <id> statement
%type <id> simple_statement
%type <id> compound_statement
%type <id> label
%type <id> null_statement
%type <id> assignement_statement
%type <id> if_statement
%type <id> cond_clause_list
%type <id> cond_clause
%type <id> cond_part
%type <id> decision
%type <id> else_opt
%type <id> case_statement
%type <id> case_hdr
%type <id> alternative_list
%type <id> alternative
%type <id> choice_s2
%type <id> choice2
%type <id> loop_statement
%type <id> label_opt
%type <id> statement_identifier
%type <id> iteration_opt
%type <id> iter_part
%type <id> reverse_opt
%type <id> basic_loop
/* %type <id> id_opt */
%type <id> block_body
%type <id> block
%type <id> block_declaration
%type <id> handled_statement_s
%type <id> exception_handler_part_opt
%type <id> exit_statement
%type <id> name_opt
%type <id> when_opt
%type <id> simple_return_statement
%type <id> extended_return_statement
%type <id> constant_opt
%type <id> opt_handled_statement_s
//%type <id> null_procedure_declaration
%type <id> goto_statement
%type <id> procedure_call_statement
%type <id> subprogram_declaration
%type <id_subprogram> subprogram_specification
%type <id> designator
%type <id> formal_part_opt
%type <id> formal_part
%type <id> parameter_specification_list
%type <id> parameter_specification
%type <id> mode
%type <id_subprogram> subprogram_specification_is_push
%type <id> subprogram_body
%type <id> declarative_part
%type <id> decl_item_s
%type <id> decl_item_s1
%type <id> decl_item
%type <id> decl_item_or_body_sl
%type <id> decl_item_or_body
%type <id> decl
%type <id> body
//%type <id> proper_body2
%type <id> package_declaration
%type <id> package_specification
%type <id> private_part_opt
//%type <id> c_id_opt
%type <id> package_body
%type <id> body_opt
%type <id> limited_opt
/* %type <id> use_type_clause   */

%type <id> rename_declaration
%type <id> rename_unit
%type <id> renames
%type <id> name_or_character_literal

%type <id> operator_symbol_or_string


%type <id> delay_statement
%type <id> abort_statement
%type <id> raise_statement
%type <id> requeue_statement
%type <id> accept_statement
%type <id> select_statement
%type <id> task_declaration
%type <id> protected_declaration
/* %type <id> compilation       */
/* %type <id> compilation_unit  */
// %type <id> private_opt
// %type <id> context_specification
/* %type <id> with_clause       */
/* %type <id> with_list         */
// %type <id> file_name
// %type <id> use_clause_opt
//%type <id> unit

%type <id> overriding_indicator_opt
%type <id> overriding_indicator
%type <id> private_type_definition

%type <id> body_stub
/* %type <id> subunit           */
%type <id> exception_declaration
%type <id> exception_handler_part
%type <id> exception_handler
%type <id> exception_choice_s
%type <id> exception_choice
%type <id> generic_declaration
%type <id> generic_formal_part
%type <id> generic_formal
%type <id> generic_discriminant_part_opt
%type <id> subp_default
%type <id> generic_type_definition
%type <id> generic_subp_inst
%type <id> generic_pkg_inst
%type <id> generic_inst
%type <id> aspect_clause
%type <id> enumeration_or_attribute_definition_clause
%type <id> record_representation_clause
%type <id> mod_clause_opt
%type <id> component_clause
%type <id> component_clause_sl
%type <id> component_clause_item
%type <id> at_clause
%%
//Top level rule
//1st foo.xref is parsed (initial call) : yywrap is then called: yyin becomes foo.bind
//2nd foo.bind is parsed : yywrap is then called: yyin becomes the first ada code file as according to foo.bind
//3rd 1st code file is parsed : yywrap is called: yyin becomes the second ada code file as according to foo.bind
// etc
goal_symbol : ref_xref_list compilation {fprintf(parsed, ",");} ref_xref_list  pac_elaboration_order compilation
            ;

/********************* REF ***************************/
ref_xref_list : ref_xref
              | ref_xref_list ref_xref
              ;

ref_xref : ref_identifier_or_string_or_character_literal ref_decl ref_body_opt ref_modi_opt ref_ref_opt
            {
                 free(dcurrent_id);
            }
         ;

ref_identifier_or_string_or_character_literal : ref_identifier
                                                {dcurrent_id = $1.id;
                                                 dcurrent_kind = $1.kind;
                                                }
                                              | ref_string              //can only be a string operator
                                                {dcurrent_id = $1.id;
                                                 dcurrent_kind = $1.kind;
                                                }
                                              | ref_character_literal
                                                {dcurrent_id = $1.id;
                                                 dcurrent_kind = $1.kind;
                                                }
                                              ;

//add every declaration to the double TST for declarations
//Careful: in the new gnat from gcc 3.4.1 the same declaration can appear twice in foo.xref
ref_decl : REF_DECL ref_filename ref_integer REF_COLON ref_integer
           {char *dcurrent_id_tmp;
            dcurrent_id_tmp = malloc(3*strlen(dcurrent_id)+4);
            dxref_id = malloc(strlen($2)+strlen($3)+strlen($5)+strlen(dcurrent_id)+6);
            strcpy(dxref_id, $2);
            strcat(dxref_id, ":");
            if (!is_standard){  //for standard.xref parsing we ignore line and column info because they will not be xrefed in foo.xref
                strcat(dxref_id, $3);
                strcat(dxref_id, ":");
                strcat(dxref_id, $5);
                strcat(dxref_id, ":");
            }
            strcat(dxref_id, dcurrent_id);
            if (is_standard){   //keeping track of the predefined identifiers
                 if (!predefined_ids_root) predefined_ids_root = add_filename(predefined_ids_root, predefined_ids_root, create_filename_node(dcurrent_id));
                 else add_filename(predefined_ids_root, predefined_ids_root, create_filename_node(dcurrent_id));
            }
			//fprintf(stdout, "DEBUG: dxref_id within ref_decl is %s\n", dxref_id);
            strcpy(dcurrent_id_tmp, dcurrent_id); //i.e. unchanged

            //e.g. decl_insert_start("a.ads:10:12:speed", "speed")
            //dcurrent_id_tmp ("speed" or "op_42") will be used to create the unique_id
            //when entering twice the same dxref_id the unique_id is returned as the same (see double_tst.c)
            current_decl = decl_insert_start(dxref_id, dcurrent_id_tmp); //added to the TST of declarations
            if (is_new_intrinsic($2, dcurrent_id)) fprintf(parsed, "is_intrinsic(%s, %s),\n", dcurrent_id, current_decl->unique_id);
            //checking for subprograms

            if (!strncmp(original_package_name, $2, strlen(original_package_name)) &&  $2[strlen(original_package_name)] == '.') {
              switch (dcurrent_kind) {
                case 0: break;
                case 1: // a function
                        fprintf(subprograms, "function\n%s\n%s\n", dcurrent_id, $3);
                        break;
                case 2: // a procedure
                        fprintf(subprograms, "procedure\n%s\n%s\n", dcurrent_id, $3);
                        break;
              }
            }
            free($2);
            free($3);
            free($5);
           }
         ;

//change with gnat distributed with gcc3.4.1: a declaration can have bodies in different files
// we proceed as usual: by inserting them to the ref TST
ref_body_opt : /*empty */
             | REF_BODY ref_body_list
             ;

ref_body_list : ref_body
              | ref_body_list ref_body
              ;

// slightly different than ref_reference because we assume only one line:column for a body
ref_body : ref_filename {tmp_filename_s = $1; } ref_line_column {free(tmp_filename_s);}
         ;

//treated the same way as references via the non-terminal ref_modified_or_referenced_list
ref_modi_opt : /* empty */
             | REF_MODI ref_modified_or_referenced_list
             ;

ref_ref_opt : /* empty */
            | REF_REF ref_modified_or_referenced_list
            ;

ref_modified_or_referenced_list : ref_reference
                                | ref_modified_or_referenced_list ref_reference
                                ;

ref_reference : ref_filename {tmp_filename_s = $1; } ref_line_column_list {free(tmp_filename_s);}
              ;

ref_line_column_list : ref_line_column
                     | ref_line_column_list ref_line_column
                     ;

ref_line_column : ref_integer REF_COLON ref_integer
                  {dxref_id = malloc((strlen(tmp_filename_s)+strlen($1)+strlen($3)+strlen(dcurrent_id)+4) );
                   strcpy(dxref_id, tmp_filename_s);
                   strcat(dxref_id, ":");
                   if (!is_standard){  //for standard.xref parsing we ignore line and column info
                        strcat(dxref_id, $1);
                        strcat(dxref_id, ":");
                        strcat(dxref_id, $3);
                        strcat(dxref_id, ":");
                   }
                   strcat(dxref_id, dcurrent_id);
                   free($1);
                   free($3);
                   ref_insert_start(dxref_id, current_decl); //e.g. positionvalidation-testhighdemandcurrent.adb:30:10
                  }
                ;

/********************* PAC ***************************/
/* There is nothing special to foo.bind parsing it   */
/*  is all straightforward: the files mentionned     */
/*  in foo.bind are added to queue (see queue.c)     */
/*  unless they are not mentionned in the binary     */
/*  tree structure (see binary_filename.c) created   */
/*  during the parsing of the filenames mentionned   */
/*  in foo.xref (see ada.l)                          */
/*****************************************************/
pac_elaboration_order : PAC_START_ELABORATION pac_package_list
                      ;

pac_package_list : pac_package
                 | pac_package_list pac_package
                 ;

//add elaboration package names to a queue (queue.c used later in yywrap)
pac_package : pac_name pac_spec_or_body
              {
               char *filename = NULL;
               char *path = NULL;
               find_filename_path($1, $2, &filename, &path);
               if (debugMode) fprintf(stdout, "Mika DEBUG after find_filename_path found for %s%s the filename: %s on the path: %s\n", $1, $2, !filename ? "NULL" : filename, !path ? "NULL" : path);
               if (filename != NULL) { //to see if it was mentionned in foo.xref
                 if (!path) {
                   fprintf(stdout, "Mika ERROR: a needed source file (%s%s) could not be found for the unit %s. Use the -c switch of ada_parser to pass on include files to gnat ls.\n", filename, $2, $1);
                   fflush(stdout);
                   my_exit(23);
                 }
                 else add_tail($1, filename, $2, path);   //add package name at the back of the queue (see queue.c)
               }
               else {
                    if (debugMode) {
                        fprintf(stdout, "Mika DEBUG Not added to queue %s%s\n", $1, $2); //not added because it is not referenced in foo.ref
                        fflush(stdout);
                    }
               }
               free($1);
               free($2);
              }
            ;

pac_name : pac_package_name pac_dot_opt
           {$$ = malloc((strlen($1)+strlen($2)+1) );
            strcpy($$, $1);
            strcat($$, $2);
            free($1);
            free($2);
           }
           ;

pac_dot_opt : /* empty */
              {$$ = malloc(1 ); strcpy($$, "");}
            | PAC_DOT pac_package_name pac_dot_opt
              {$$ = malloc((strlen($2)+strlen($3)+2) );
               strcpy($$, "-");         //according to GNAT file naming convention
               strcat($$, $2);
               strcat($$, $3);
               free($2);
               free($3);
              }
            ;

pac_spec_or_body : PAC_SPEC
                   {$$ = malloc(5);
                    strcpy($$, ".ads");
                   }
                 | PAC_BODY
                   {$$ = malloc(5);
                    strcpy($$, ".adb");
                   }
                 ;

/********************* ADA ***************************/
/* Chapter 4 */
pragma : PRAGMA identifier ';'
          {$$ = malloc(SAFETY+strlen($2.id)+17);
           strcpy($$, "atomic_pragma(");
           strcat($$, $2.id);
           strcat($$, ")");
           free($2.id);
          }
       | PRAGMA identifier '('
           {in_a_pragma = 1;}   //changes the behaviour of handle_identifier (if unxrefed return id 'as is', e.g. 'C' calling convention will be returned as 'C' within a pragma)
           pragma_argument_association_list
           ')'
           {in_a_pragma = 0;}
           ';'
          {$$ = malloc(SAFETY+strlen($2.id)+strlen($5)+14);
           strcpy($$, "pragma(");
           strcat($$, $2.id);
           strcat($$, ", [");
           strcat($$, $5);
           strcat($$, "])");
           free($2.id);
           free($5);
          }
       ;

pragma_argument_association_list : pragma_argument_association {$$ = $1;}
                                 | pragma_argument_association_list ',' pragma_argument_association
                                   {$$ = malloc(SAFETY+strlen($1)+strlen($3)+3);
                                    strcpy($$, $1);
                                    strcat($$, ", ");
                                    strcat($$, $3);
                                    free($1);
                                    free($3);
                                   }
                                 ;

pragma_argument_association : expression_2 {$$ = $1.id;}
                            | identifier RIGHT_SHAFT expression_2 {$$ = $3.id;} //we ignore identifiers (arguments are always in the same order for pragmas)
                              /*{$$ = malloc(SAFETY+strlen($1.id)+strlen($3)+5);
                               strcpy($$, "(");
                               strcat($$, $1.id);
                               strcat($$, ", ");
                               strcat($$, $3);
                               strcat($$, ")");
                               free($1.id);
                               free($3);
                              }*/
                            ;

pragma_s : /* empty */          {$$ = malloc(1); strcpy($$, "");}
         | pragma_s pragma
           {if (strcmp($1, "")) {
              $$ = malloc(SAFETY+strlen($1)+strlen($2)+3);
              strcpy($$, $1);
              strcat($$, ",\n");
              strcat($$, $2);
            }
            else {
              $$ = malloc(strlen($2)+1);
              strcpy($$, $2);
            }
            free($1);
            free($2);
           }
         ;

/* Chapter 5 */
// For Ada Rule 3.3.1
object_declaration : identifier_list ':' object_qualifier_opt object_subtype_definition init_opt ';'
                     {$$ = malloc(SAFETY+strlen($3)+strlen($1)+strlen($4)+strlen($5)+19);
                      strcpy($$, "  object(");
                      strcat($$, $3);
                      strcat($$, ", [");
                      strcat($$, $1);
                      strcat($$, "], ");
                      strcat($$, $4);
                      strcat($$, ", ");
                      strcat($$, $5);
                      strcat($$, ")");
                      free($3);
                      free($1);
                      free($4);
                      free($5);
                     }
                   ;

identifier_list : identifier_rule    {$$ = $1;}
                | identifier_list ',' identifier_rule
                  {$$ = malloc((SAFETY+strlen($1)+strlen($3)+3) );
                   strcpy($$, $1);
                   strcat($$, ", ");
                   strcat($$, $3);
                   free($1);
                   free($3);
                  }
                ;

identifier_rule : identifier
                  {if (is_ascii) {
                     char key1[1000];         /* used for searching double_tst structure */
                     strcpy(key1, "standard.ads:");
                     strcat(key1, $1.id);
                     strcat(key1, "_ascii");
                     found_decl = decl_search_start(key1);
                     if (!found_decl) found_decl = ref_search_start(key1);
                     if (!found_decl) {
                       fprintf(stdout, "Mika ERROR: unknown predefined identifier %s \n", $1.id); // could also print out key1
                       fflush(stdout);
                       my_exit(26);
                     }
                     $$ = malloc((strlen(found_decl->unique_id)+1) );
                     strcpy($$, found_decl->unique_id);
                   }
                   else $$ = handle_identifiers($1, 0); //0 because not a string
                  }
                | error new_ada_2005_reserved_words
                  {yyerrok;     //Bison macro to recover from the error
                   recover_keyword($2);                            //sets up error_recovery_identifier with column and line number
                   $$ = handle_identifiers(error_recovery_identifier, 0); //0 because not a string
                  }
                | error new_ada_95_reserved_words
                  {yyerrok;     //Bison macro to recover from the error
                   recover_keyword($2);                            //sets up error_recovery_identifier with column and line number
                   $$ = handle_identifiers(error_recovery_identifier, 0); //0 because not a string
                  }
                ;

new_ada_2005_reserved_words : INTERFACE
                              {$$ = malloc(10);
                               strcpy($$, "interface");
                              }
                            | OVERRIDING
                              {$$ = malloc(11);
                               strcpy($$, "overriding");
                              }
                            | SYNCHRONIZED
                              {$$ = malloc(13);
                               strcpy($$, "synchronized");
                              }
                            ;

new_ada_95_reserved_words : ABSTRACT
                              {$$ = malloc(10);
                               strcpy($$, "abstract");
                              }
                            | ALIASED
                              {$$ = malloc(11);
                               strcpy($$, "aliased");
                              }
                            | PROTECTED
                              {$$ = malloc(13);
                               strcpy($$, "protected");
                              }
                            | REQUEUE
                              {$$ = malloc(10);
                               strcpy($$, "requeue");
                              }
                            | TAGGED
                              {$$ = malloc(11);
                               strcpy($$, "tagged");
                              }
                            | UNTIL
                              {$$ = malloc(13);
                               strcpy($$, "until");
                              }
                            ;

object_qualifier_opt : /* empty */      {$$ = malloc(14); strcpy($$, "not_qualified");}
                     | ALIASED                       {$$ = malloc(8); strcpy($$, "aliased");}
                     | CONSTANT                      {$$ = malloc(9); strcpy($$, "constant");}
                     | ALIASED CONSTANT              {$$ = malloc(17); strcpy($$, "aliased_constant");}
                     ;

//changed in Ada 2005
object_subtype_definition : subtype_indication          {$$ = $1;}
                          | null_exclusion_opt access_definition
                            {$$ = malloc(SAFETY+strlen($1)+strlen($2)+22);
                             strcpy($$, "access_definition(");
                             strcat($$, $1);
                             strcat($$, ", ");
                             strcat($$, $2);
                             strcat($$, ")");
                             free($1);
                             free($2);
                            }
                          | array_type_definition       {$$ = $1;}
                          ;

init_opt : /* empty */
           {$$ = malloc((8) );
            strcpy($$, "no_init");
           }
         | IS_ASSIGNED <true_line_column> {$$.line = yylineno; $$.column = column+1;} expression_2
           {char *expression;
            build_expression($3, &expression, current_unit, $2.line, $2.column);
            $$ = malloc(SAFETY+strlen(expression)+1);
            strcpy($$, expression);
            free($3.id);
           }
         ;

type_declaration : TYPE identifier_rule discriminant_part_opt type_completion ';'
                   {$$ = malloc((SAFETY+strlen($2)+strlen($3)+strlen($4)+12) );
                    strcpy($$, " type(");
                    strcat($$, $2);
                    strcat($$, ", ");
                    strcat($$, $3);
                    strcat($$, ", ");
                    strcat($$, $4);
                    strcat($$, ")");
                    free($2);
                    free($3);
                    free($4);
                   }
                 ;

discriminant_part_opt : /* empty */             {$$ = malloc(SAFETY+16); strcpy($$, "no_discriminant");}
                      | discriminant_part       {$$ = $1;}
                      | '(' BOX ')'             {$$ = malloc(4); strcpy($$, "box");}
                      ;

type_definition : enumeration_type_definition   {$$ = $1;}
                | integer_type_definition       {$$ = $1;}
                | real_type_definition          {$$ = $1;}
                | array_type_definition         {$$ = $1;}
                | access_type_definition        {$$ = $1;}
                | ABSTRACT derived_type_definition              //rule 3.4 (derived_type_definition) or rule 7.3 (private_extension_declaration)
                  {$$ = malloc(SAFETY+strlen($2)+14);
                   strcpy($$, "abstract_new(");
                   strcat($$, $2);
                   strcat($$, ")");
                   free($2);
                  }
                | ABSTRACT TAGGED record_type_definition        //rule 3.8 record_type_definition
                  {$$ = malloc(SAFETY+strlen($3)+17);
                   strcpy($$, "abstract_tagged, ");
                   strcat($$, $3);
                   free($3);
                  }
                | ABSTRACT TAGGED private_type_definition      //rule 7.3 private_type_declaration
                  {$$ = malloc(SAFETY+strlen($3)+27);
                   strcpy($$, "private, abstract_tagged, ");
                   strcat($$, $3);
                   free($3);
                  }
                | TAGGED record_type_definition                 //rule 3.8 record_type_definition
                  {$$ = malloc(SAFETY+strlen($2)+9);
                   strcpy($$, "tagged, ");
                   strcat($$, $2);
                   free($2);
                  }
                | TAGGED private_type_definition               //rule 7.3 private_type_declaration
                  {$$ = malloc(SAFETY+strlen($2)+27);
                   strcpy($$, "private, tagged, ");
                   strcat($$, $2);
                   free($2);
                  }
                | record_type_definition                        //rule 3.8 record_type_definition
                  {$$ = malloc(SAFETY+strlen($1)+13);
                   strcpy($$, "not_tagged, ");
                   strcat($$, $1);
                   free($1);
                  }
                | derived_type_definition                       //rule 3.4 (derived_type_definition) or rule 7.3 (private_extension_declaration)
                  {$$ = malloc(SAFETY+strlen($1)+6);
                   strcpy($$, "new(");
                   strcat($$, $1);
                   strcat($$, ")");
                   free($1);
                  }
                | private_type_definition                      //rule 7.3 private_type_declaration
                  {$$ = malloc(SAFETY+strlen($1)+27);
                   strcpy($$, "private, not_tagged, ");
                   strcat($$, $1);
                   free($1);
                  }
                | interface_type_definition     {$$ = $1;}
                ;

//changed in Ada 2005
type_completion : /* empty */           {$$ = malloc(SAFETY+22); strcpy($$, "empty_type_completion");} //incomplete declaration occurs for access type declaration (Barnes 10.2)
                | IS TAGGED             {$$ = malloc(SAFETY+10); strcpy($$, "is_tagged");}             //new in Ada 2005 (incomplete type declaration)
                | IS type_definition    {$$ = $2;}
                ;

// For Ada Rule 3.2.2
subtype_declaration : SUBTYPE identifier_rule IS subtype_indication ';'
                      {$$ = malloc((SAFETY+strlen($2)+strlen($4)+13) );
                       strcpy($$, " subtype(");
                       strcat($$, $2);
                       strcat($$, ", ");
                       strcat($$, $4);
                       strcat($$, ")");
                       free($2);
                       free($4);
                      }
                    ;

//For Ada Rule 3.2.2
//changed in Ada 2005
subtype_indication : name constraint
                     {$$ = malloc(SAFETY+strlen($1)+strlen($2)+47);
                      strcpy($$, "subtype_indication(may_be_null,");
                      strcat($$, $1);
                      strcat($$, ", constraint(");
                      strcat($$, $2);
                      strcat($$, "))");
                      free($1);
                      free($2);
                     }
                   | name
                     {$$ = malloc(SAFETY+strlen($1)+49);
                      strcpy($$, "subtype_indication(may_be_null,");
                      strcat($$, $1);
                      strcat($$, ", no_constraint)");
                      free($1);
                     }
                   | NOT NuLL name constraint
                     {$$ = malloc(SAFETY+strlen($3)+strlen($4)+44);
                      strcpy($$, "subtype_indication(not_null,");
                      strcat($$, $3);
                      strcat($$, ", constraint(");
                      strcat($$, $4);
                      strcat($$, "))");
                      free($3);
                      free($4);
                     }
                   | NOT NuLL name
                     {$$ = malloc(SAFETY+strlen($3)+46);
                      strcpy($$, "subtype_indication(not_null,");
                      strcat($$, $3);
                      strcat($$, ", no_constraint)");
                      free($3);
                     }
                   ;

// For Ada rule 3.2.2 (the index_constraint and discriminant_constraint are handled via name within the subtype_indication
constraint : range_constraint           {$$ = $1;}
           | digits_constraint          {$$ = $1;}
           | delta_constraint           {$$ = $1;} //ommitted in our original grammar but part of the standard
           ;

// For Ada rule 3.5.9
digits_constraint : DIGITS simple_expression range_constraint_opt     //changed 28/03/09 was expression (consequences [very limited] using overloaded and to return an integer is not longer parsed properly)
                    {$$ = malloc((SAFETY+strlen($2.id)+strlen($3)+13) );
                     strcpy($$, "digits(");
                     strcat($$, $2.id);
                     strcat($$, ", ");
                     strcat($$, $3);
                     strcat($$, ")");
                     free($2.id);
                     free($3);
                    }
                  ;

// For Ada rule J.3
// added 02-11-04
delta_constraint : DELTA simple_expression range_constraint_opt     //changed 28/03/09 was expression (consequences [very limited] using overloaded and to return an integer is not longer parsed properly)
                    {$$ = malloc((SAFETY+strlen($2.id)+strlen($3)+13) );
                     strcpy($$, "delta(");
                     strcat($$, $2.id);
                     strcat($$, ", ");
                     strcat($$, $3);
                     strcat($$, ")");
                     free($2.id);
                     free($3);
                    }
                  ;

// For Ada rule 3.4 & 7.3
derived_type_definition : private_type_kind NEW subtype_indication                //For Ada rule 3.4 (derived_type_definition)
                          {$$ = malloc(SAFETY+strlen($1)+strlen($3)+27);
                           strcpy($$, "derived_type_definition(");
                           strcat($$, $1);
                           strcat($$, ", ");
                           strcat($$, $3);
                           strcat($$, ")");
                           free($1);
                           free($3);
                          }
                        | private_type_kind NEW subtype_indication interface_list_item_s WITH PRIVATE   //For Ada rule 7.3 (private_extension_declaration)
                          {$$ = malloc(SAFETY+strlen($1)+strlen($3)+strlen($4)+44);
                           strcpy($$, "derived_type_definition(");
                           strcat($$, $1);
                           strcat($$, ", ");
                           strcat($$, $3);
                           strcat($$, ", ");
                           strcat($$, $4);
                           strcat($$, ", with_private");
                           strcat($$, ")");
                           free($1);
                           free($3);
                           free($4);
                          }
                        | private_type_kind NEW subtype_indication interface_list_item_s WITH record_definition //For Ada rule 3.4 (derived_type_definition)
                          {$$ = malloc(SAFETY+strlen($1)+strlen($3)+strlen($4)+strlen($6)+32);
                           strcpy($$, "derived_type_definition(");
                           strcat($$, $1);
                           strcat($$, ", ");
                           strcat($$, $3);
                           strcat($$, ", ");
                           strcat($$, $4);
                           strcat($$, ", ");
                           strcat($$, $6);
                           strcat($$, ")");
                           free($1);
                           free($3);
                           free($4);
                           free($6);
                          }
                        ;

private_type_kind : /* empty */     {$$ = malloc(SAFETY+27); strcpy($$, "not_limited_nor_synchronized");}
                  | LIMITED         {$$ = malloc(SAFETY+8); strcpy($$, "limited");}
                  | SYNCHRONIZED    {$$ = malloc(SAFETY+13); strcpy($$, "synchronized");}
                  ;

//For Ada Rule 3.5
range_constraint : RANGE range
                   {$$ = malloc((SAFETY+strlen($2)+8) );
                    strcpy($$, "range(");
                    strcat($$, $2);
                    strcat($$, ")");
                    free($2);
                   }
                 ;

//Ada Rule 3.5
range : simple_expression DOT_DOT simple_expression     {$$ = malloc((SAFETY+strlen($1.id)+strlen($3.id)+5) );
                                                         strcpy($$, "[");
                                                         strcat($$, $1.id);
                                                         strcat($$, ", ");
                                                         strcat($$, $3.id);
                                                         strcat($$, "]");
                                                         free($1.id);
                                                         free($3.id);
                                                        }
      | name TIC RANGE                                  {$$ = malloc(SAFETY+strlen($1)+13);
                                                         strcpy($$, "tic(");
                                                         strcat($$, $1);
                                                         strcat($$, ", ");
                                                         strcat($$, "range)");
                                                         free($1);
                                                        }
      | name TIC RANGE '(' <true_line_column> {$$.line = yylineno; $$.column = column+1;} expression_2 ')'
                                                        {char *expression;
                                                         build_expression($6, &expression, current_unit, $5.line, $5.column);
                                                         $$ = malloc(SAFETY+strlen($1)+strlen(expression)+15);
                                                         strcpy($$, "tic(");
                                                         strcat($$, $1);
                                                         strcat($$, ", ");
                                                         strcat($$, "range, ");
                                                         strcat($$, expression);
                                                         strcat($$, ")");
                                                         free($1);
                                                         free($6.id);
                                                        }
      ;

//for Ada rule 3.5.1
enumeration_type_definition : '(' enum_identifier_list ')'
                              {$$ = malloc((SAFETY+strlen($2)+16) );
                               strcpy($$, "enumeration, [");
                               strcat($$, $2);
                               strcat($$, "]");
                               free($2);
                              }
                            ;

//for Ada rule 3.5.1
enum_identifier_list : enum_identifier  {$$ = $1;}
                     | enum_identifier_list ',' enum_identifier
                       {$$ = malloc((SAFETY+strlen($1)+strlen($3)+3) );
                        strcpy($$, $1);
                        strcat($$, ", ");
                        strcat($$, $3);
                        free($1);
                        free($3);
                       }
                     ;

//for Ada rule 3.5.1
enum_identifier : identifier_rule       {$$ = $1;}
                | character_literal             {$$ = handle_identifiers($1, 0);}//0 because not a string
                ;

/* Integer Type */
integer_type_definition : range_specification
                          {char key1[1000];         /* used for searching double_tst structure */
                           strcpy(key1, "standard.ads:integer");
                           found_decl = decl_search_start(key1); //retrieving standard.ads:integer's actual xrefed name
                           if (!found_decl) {
                             fprintf(stdout, "Mika ERROR: cannot find a cross referenced standard.ads:integer");
                             fflush(stdout);
                             my_exit(27);
                           }
                           $$ = malloc((SAFETY+strlen($1)+12+strlen(found_decl->unique_id)) );
                           strcpy($$, "integer, ");
                           strcat($$, found_decl->unique_id);
                           strcat($$, ", ");
                           strcat($$, $1);
                           free($1);
                          }
                        | MOD <true_line_column> {$$.line = yylineno; $$.column = column+1;} expression_2
                          {char key1[1000];         /* used for searching double_tst structure */
                           char *expression;
                           build_expression($3, &expression, current_unit, $2.line, $2.column);
                           strcpy(key1, "standard.ads:integer");
                           found_decl = decl_search_start(key1); //retrieving standard.ads:integer's actual xrefed name
                           if (!found_decl) {
                             fprintf(stdout, "Mika ERROR: cannot find a cross referenced standard.ads:integer");
                             fflush(stdout);
                             my_exit(28);
                           }
                           $$ = malloc((SAFETY+strlen(expression)+17+strlen(found_decl->unique_id)) );
                           strcpy($$, "integer, ");
                           strcat($$, found_decl->unique_id);
                           strcat($$, ", mod(");
                           strcat($$, expression);
                           strcat($$, ")");
                           free($3.id);
                          }
                        ;

/* Real Type */
real_type_definition : floating_point_definition
                       {char key1[1000];         /* used for searching double_tst structure */
                        strcpy(key1, "standard.ads:float");
                        found_decl = decl_search_start(key1); //retrieving standard.ads:float's actual xrefed name
                        if (!found_decl) {
                          fprintf(stdout, "Mika ERROR: cannot find a cross referenced standard.ads:float");
                          fflush(stdout);
                          my_exit(29);
                        }
                        $$ = malloc((SAFETY+strlen($1)+10+strlen(found_decl->unique_id)) );
                        strcpy($$, "float, ");
                        strcat($$, found_decl->unique_id);
                        strcat($$, ", ");
                        strcat($$, $1);
                        free($1);
                       }
                     | fixed_point_definition
                       {char key1[1000];         /* used for searching double_tst structure */
                        strcpy(key1, "standard.ads:float");
                        found_decl = decl_search_start(key1); //retrieving standard.ads:float's actual xrefed name
                        if (!found_decl) {
                          fprintf(stdout, "Mika ERROR: cannot find a cross referenced standard.ads:float");
                          fflush(stdout);
                          my_exit(30);
                        }
                        $$ = malloc((SAFETY+strlen($1)+10+strlen(found_decl->unique_id)) );
                        strcpy($$, "fixed, ");
                        strcat($$, found_decl->unique_id);
                        strcat($$, ", ");
                        strcat($$, $1);
                        free($1);
                       }
                     ;

//for Ada rule 3.5.7
floating_point_definition : DIGITS <true_line_column> {$$.line = yylineno; $$.column = column+1;} expression_2 range_specification_opt
                            {char *expression;
                             build_expression($3, &expression, current_unit, $2.line, $2.column);
                             $$ = malloc(SAFETY+strlen(expression)+strlen($4)+11);
                             strcpy($$, "digits(");
                             strcat($$, expression);
                             strcat($$, ", ");
                             strcat($$, $4);
                             strcat($$, ")");
                             free($3.id);
                             free($4);
                            }
                          ;

range_specification_opt : /* empty */
                          {$$ = malloc((12) );
                           strcpy($$, "empty_range");
                          }
                        | range_specification              {$$ = $1;}
                        ;

range_specification : range_constraint {$$ = $1;}
                    ;

range_constraint_opt : /* empty */
                       {$$ = malloc((12) );
                        strcpy($$, "empty_range");
                       }
                     | range_constraint
                               {$$ = malloc((2+strlen($1)) );
                                    strcpy($$, $1);
                                    free($1);
                               }
                     ;

/* the precision is ignored : the solver uses infinite precision rationals */
fixed_point_definition : DELTA expression range_specification   //will always have a range(..., ...)
                         //would like to use expression_2 but cannot find a good line or column ... so the column will be very approximate in case an expression that is also a deci
                         {$$ = malloc((SAFETY+strlen($2)+strlen($3)+10) );
                          strcpy($$, "delta(");
                          strcat($$, $2);
                          strcat($$, ", ");
                          strcat($$, $3);
                          strcat($$, ")");
                          free($2);
                          free($3);
                         }
                       | DELTA expression DIGITS expression range_specification_opt  //would like to use expression_2 but cannot find a good line or column ... so the column will be very approximate in case an expression that is also a deci
                         {$$ = malloc((SAFETY+strlen($2)+strlen($4)+strlen($5)+19) );
                          strcpy($$, "delta_digits(");
                          strcat($$, $2);
                          strcat($$, ", ");
                          strcat($$, $4);
                          strcat($$, ", ");
                          strcat($$, $5);
                          strcat($$, ")");
                          free($2);
                          free($4);
                          free($5);
                         }
                       ;

/* Record Type */
record_type_definition : limited_opt record_definition
                         {$$ = malloc(SAFETY+strlen($1)+strlen($2)+5);
                          strcpy($$, $1);
                          strcat($$, ", ");
                          strcat($$, $2);
                          free($1);
                          free($2);
                         }
                       ;

record_definition : RECORD pragma_s component_list END RECORD
                    {$$ = malloc((SAFETY+strlen($3)+11) );
                     strcpy($$, "record([");
                     strcat($$, $3);
                     strcat($$, "])");
                     free($2);
                     free($3);
                    }
                  | NuLL RECORD
                    {$$ = malloc((20) );
                     strcpy($$, "record(null_record)");
                    }
                  ;

component_list : component_declaration_list variant_part_opt
                 {$$ = malloc((SAFETY+strlen($1)+strlen($2)+3) );
                  strcpy($$, $1);
                  strcat($$, ", ");
                  strcat($$, $2);
                  free($1);
                  free($2);
                 }
               | variant_part pragma_s
                 {$$ = $1;
                  free($2);
                 }
               | NuLL ';' pragma_s
                 {$$ = malloc((5) );
                  strcpy($$, "null");
                  free($3);
                 }
               ;

component_declaration_list : component_declaration      {$$ = $1;}
                           | component_declaration_list pragma_s component_declaration
                             {$$ = malloc((SAFETY+strlen($1)+strlen($3)+3) );
                              strcpy($$, $1);
                              strcat($$, ", \n");
                              strcat($$, $3);
                              free($1);
                              free($2);
                              free($3);
                             }
                           ;

variant_part_opt : pragma_s
                   {$$ = malloc(11);
                    strcpy($$, "no_variant");
                    free($1);
                   }
                 | pragma_s variant_part pragma_s
                   {$$ = $2;
                    free($1);
                   }
                 ;

component_declaration : identifier_list ':' component_definition init_opt ';'
                        {$$ = malloc((SAFETY+strlen($1)+strlen($3)+strlen($4)+9) );
                         strcpy($$, "([");
                         strcat($$, $1);
                         strcat($$, "], ");
                         strcat($$, $3);
                         strcat($$, ", ");
                         strcat($$, $4);
                         strcat($$, ")");
                         free($1);
                         free($3);
                         free($4);
                        }
                      ;

discriminant_part : '(' discriminant_spec_s ')'
                     {$$ = malloc(17+strlen($2));
                      strcpy($$, "discriminant([");
                      strcat($$, $2);
                      strcat($$, "])");
                      free($2);
                     }
                  ;

discriminant_spec_s : discriminant_spec         {$$ = $1;}
                    | discriminant_spec_s ';' discriminant_spec
                      {$$ = malloc(3+strlen($1)+strlen($3));
                       strcpy($$, $1);
                       strcat($$, ", ");
                       strcat($$, $3);
                       free($1);
                       free($3);
                      }
                    ;

//changed in Ada 2005
discriminant_spec : identifier_list ':' access_or_subtype_disc init_opt    //changed in Ada 2005
                    {$$ = malloc(28+strlen($1)+strlen($3)+strlen($4));
                     strcpy($$, "discriminant_spec([");
                     strcat($$, $1);
                     strcat($$, "], ");
                     strcat($$, $3);
                     strcat($$, ", ");
                     strcat($$, $4);
                     strcat($$, ")");
                     free($1);
                     free($3);
                     free($4);
                    }
                  ;

//more permissive than the Ada 2005 grammar!!!
access_or_subtype_disc : null_exclusion_opt access_definition
                         {$$ = malloc(SAFETY+strlen($1)+strlen($2)+22);
                          strcpy($$, "access_definition(");
                          strcat($$, $1);
                          strcat($$, ", ");
                          strcat($$, $2);
                          strcat($$, ")");
                          free($1);
                          free($2);
                         }
                       | subtype_indication     //will always have no_constraint (due to grammar rule 3.7)
                         {$$ = $1;}
                       ;

variant_part : CASE direct_name IS pragma_s variant_s END CASE ';'
               {$$ = malloc((8) );
                strcpy($$, "variant");
                free($4);
               }
             ;

variant_s : variant             {;}
          | variant_s variant   {;}
          ;

variant : WHEN choice_s RIGHT_SHAFT pragma_s component_list
          {free($2);
           free($4);
           free($5);
          }
        ;

/* Array Type */
// For Ada Rule 3.6
array_type_definition : unconstrained_array_definition        {$$ = $1;}
                      | constrained_array_definition          {$$ = $1;}
                      ;

unconstrained_array_definition : ARRAY '(' index_subtype_definition_list ')' OF component_definition
                           {$$ = malloc((SAFETY+strlen($3)+strlen($6)+20) );
                            strcpy($$, "unconst_array, [");
                            strcat($$, $3);
                            strcat($$, "], ");
                            strcat($$, $6);
                            free($3);
                            free($6);
                           }
                         ;

index_subtype_definition_list : index_subtype_definition {$$ = $1;}
                              | index_subtype_definition_list ',' index_subtype_definition
                                {$$ = malloc((SAFETY+strlen($1)+strlen($3)+3) );
                                 strcpy($$, $1);
                                 strcat($$, ", ");
                                 strcat($$, $3);
                                 free($1);
                                 free($3);
                                }
                              ;

index_subtype_definition : name RANGE BOX
                           {$$ = malloc((SAFETY+strlen($1)+12) );
                            strcpy($$, "range(");
                            strcat($$, $1);
                            strcat($$, ", ");
                            strcat($$, "box)");
                            free($1);
                           }
                         ;

//For Ada Rule 3.6
constrained_array_definition : ARRAY '(' discrete_range_list ')' OF component_definition
                         {$$ = malloc((SAFETY+strlen($3)+strlen($6)+12) );
                          strcpy($$, "array, [");
                          strcat($$, $3);
                          strcat($$, "], ");
                          strcat($$, $6);
                          free($3);
                          free($6);
                         }
                       ;

//For Ada Rule 3.6
discrete_range_list : discrete_range {$$ = $1;}
                    | discrete_range_list ',' discrete_range
                      {$$ = malloc((SAFETY+strlen($1)+strlen($3)+3) );
                       strcpy($$, $1);
                       strcat($$, ", ");
                       strcat($$, $3);
                       free($1);
                       free($3);
                      }
                    ;

//For Ada Rule 3.6
//changed in Ada 2005
component_definition : aliased_opt subtype_indication
                       {$$ = malloc(SAFETY+strlen($1)+strlen($2)+26);
                        strcpy($$, "component_definition(");
                        strcat($$, $1);
                        strcat($$, ", ");
                        strcat($$, $2);
                        strcat($$, ")");
                        free($1);
                        free($2);
                       }
                     | aliased_opt null_exclusion_opt access_definition
                       {$$ = malloc(SAFETY+strlen($1)+strlen($2)+46);
                        strcpy($$, "component_definition(");
                        strcat($$, $1);
                        strcat($$, ", access_definition(");
                        strcat($$, $2);
                        strcat($$, ", ");
                        strcat($$, $3);
                        strcat($$, ")");
                        strcat($$, ")");
                        free($1);
                        free($2);
                        free($3);
                       }
                     ;

aliased_opt : /* empty */     {$$ = malloc((SAFETY+12) ); strcpy($$, "not_aliased");}
            | ALIASED         {$$ = malloc((SAFETY+8) ); strcpy($$, "aliased");}
            ;

/* Aggregate */
aggregate : '(' comp_assoc ')'  {$$ = malloc(SAFETY+strlen($2)+3);   //named aggregate with a single choice
                                 strcpy($$, "[");
                                 strcat($$, $2);
                                 strcat($$, "]");
                                 free($2);
                                }
          | '(' value_s_2 ')'   {$$ = malloc(SAFETY+strlen($2)+3);   //aggregate (could be mixed named and positional)
                                 strcpy($$, "[");
                                 strcat($$, $2);
                                 strcat($$, "]");
                                 free($2);
                                }
          | '(' expression WITH value_list ')'      //would like to use expression_2 but cannot find a good line or column ... so the column will be very approximate in case an expression that is also a deci
            {$$ = malloc((SAFETY+strlen($2)+strlen($4)+9) );
             strcpy($$, "with(");
             strcat($$, $2);
             strcat($$, ", ");
             strcat($$, $4);
             strcat($$, ")");
             free($2);
             free($4);
            }
          | '(' expression WITH NuLL RECORD ')' //would like to use expression_2 but cannot find a good line or column ... so the column will be very approximate in case an expression that is also a deci
            {$$ = malloc((SAFETY+strlen($2)+20) );
             strcpy($$, "with(");
             strcat($$, $2);
             strcat($$, ", null_record)");
             free($2);
            }
          | '(' NuLL RECORD ')'
            {$$ = malloc((SAFETY+12) );
             strcpy($$, "null_record");
            }
          ;

/*at least two*/
value_s_2 : value ',' value     {$$ = malloc((SAFETY+strlen($1)+strlen($3)+4) );
                                 strcpy($$, $1);
                                 strcat($$, ",\n");
                                 strcat($$, $3);
                                 free($1);
                                 free($3);
                                }
          | value_s_2 ',' value {$$ = malloc((SAFETY+strlen($1)+strlen($3)+4) );
                                 strcpy($$, $1);
                                 strcat($$, ",\n");
                                 strcat($$, $3);
                                 free($1);
                                 free($3);
                                }
          ;

comp_assoc : choice_s RIGHT_SHAFT <true_line_column> {$$.line = yylineno; $$.column = column+1;} expression_2
             {char *expression;
              build_expression($4, &expression, current_unit, $3.line, $3.column);
              $$ = malloc(strlen($1)+strlen(expression)+12);
              strcpy($$, "named([");
              strcat($$, $1);
              strcat($$, "], ");
              strcat($$, expression);
              strcat($$, ")");
              free($1);
              free($4.id);
             }
           | choice_s RIGHT_SHAFT BOX                       //new in Ada 2005
             {$$ = malloc(strlen($1)+15);
              strcpy($$, "named([");
              strcat($$, $1);
              strcat($$, "], box)");
              free($1);
             }
           ;

//for aggregates only
choice_s : choice               {$$ = $1;}
         | choice_s '|' choice  {$$ = malloc((SAFETY+strlen($1)+strlen($3)+3) );
                                 strcpy($$, $1);
                                 strcat($$, ", ");
                                 strcat($$, $3);
                                 free($1);
                                 free($3);
                                }
         ;

choice : expression             {$$ = $1;}  //would like to use expression_2 but cannot find a good line or column ... so the column will be very approximate in case an expression that is also a deci
       | discrete_with_range    {$$ = $1;}
       | OTHERS                 {$$ = malloc((SAFETY+7) ); strcpy($$, "others");}
       ;

//For Ada Rule 3.6.1
discrete_range : subtype_indication             {$$ = $1;}
               | range                          {$$ = $1;}
               ;

//For Ada Rule 3.6.1 where subtype_indication is actually 'discrete_'subtype_indication
discrete_with_range : name range_constraint     {$$ = malloc((SAFETY+strlen($1)+strlen($2)+3) );
                                                 strcpy($$, $1);
                                                 strcat($$, ", ");
                                                 strcat($$, $2);
                                                 free($1);
                                                 free($2);
                                                } /* can be min..max, or range function call */
                    | range                     {$$ = $1;} /* can be min..max, or range function call */
                    ;

//For Ada Rule 3.9.4
//new in Ada 2005
interface_type_definition : kind_opt INTERFACE interface_list_item_s
                            {$$ = malloc(SAFETY+strlen($1)+strlen($3)+14);
                             strcpy($$, "interface, ");
                             strcat($$, $1);
                             strcat($$, ", ");
                             strcat($$, $3);
                             free($1);
                             free($3);
                            }
                          ;

kind_opt : /* empty */      {$$ = malloc(SAFETY+6); strcpy($$, "plain");}
         | LIMITED          {$$ = malloc(SAFETY+8); strcpy($$, "limited");}
         | TASK             {$$ = malloc(SAFETY+5); strcpy($$, "task");}
         | PROTECTED        {$$ = malloc(SAFETY+10); strcpy($$, "protected");}
         | SYNCHRONIZED     {$$ = malloc(SAFETY+6); strcpy($$, "plain");}
         ;

interface_list_item_s : /* empty */
                        {$$ = malloc(SAFETY+3);
                         strcpy($$, "[]");
                        }
                      | AND interface_list_item_sl
                        {$$ = malloc(SAFETY+strlen($2)+3);
                         strcpy($$, "[");
                         strcat($$, $2);
                         strcat($$, "]");
                         free($1.id);
                         free($2);
                        }
                      ;

interface_list_item_sl : name {$$ = $1;}
                       | interface_list_item_sl AND name
                         {$$ = malloc(strlen($1)+strlen($3)+3);
                          strcpy($$, $1);
                          strcat($$, ", ");
                          strcat($$, $3);
                          free($1);
                          free($2.id);
                          free($3);
                         }
                       ;

//For Ada Rule 3.10
//changed in Ada 2005
access_type_definition : null_exclusion_opt access_type_definition_part
                         {$$ = malloc(SAFETY+3+strlen($1)+strlen($2));
                          strcpy($$, $1);
                          strcat($$, ", ");
                          strcat($$, $2);
                          free($1);
                          free($2);
                         }
                       ;

access_type_definition_part : ACCESS subtype_indication
                         {$$ = malloc(9+strlen($2));
                          strcpy($$, "access, ");
                          strcat($$, $2);
                          free($2);
                         }
                       | ACCESS CONSTANT subtype_indication
                         {$$ = malloc((18+strlen($3)) );
                          strcpy($$, "access_constant, ");
                          strcat($$, $3);
                          free($3);
                         }
                       | ACCESS ALL subtype_indication
                        {$$ = malloc((13+strlen($3)) );
                         strcpy($$, "access_all, ");
                         strcat($$, $3);
                         free($3);
                        }
                       | ACCESS protected_opt PROCEDURE formal_part_opt
                         {$$ = malloc((35+strlen($2)+strlen($4)) );
                          strcpy($$, "access_procedure, ");
                          strcat($$, $2);
                          strcat($$, ", parameters([");
                          strcat($$, $4);
                          strcat($$, "])");
                          free($2);
                          free($4);
                         }
                       | ACCESS protected_opt FUNCTION formal_part_opt RETURN access_or_subtype_disc    //changed in Ada 2005
                         {$$ = malloc(36+strlen($2)+strlen($4)+strlen($6));
                          strcpy($$, "access_function, ");
                          strcat($$, $2);
                          strcat($$, ", parameters([");
                          strcat($$, $4);
                          strcat($$, "]), ");
                          strcat($$, $6),
                          free($2);
                          free($4);
                          free($6);
                         }
                       ;

protected_opt : /* empty */     {$$ = malloc(SAFETY+14); strcpy($$, "not_protected");}
              | PROTECTED       {$$ = malloc(SAFETY+10); strcpy($$, "protected");}
              ;

//new in Ada 2005
null_exclusion_opt : /* empty */        {$$ = malloc(SAFETY+12); strcpy($$, "may_be_null");}
                   | NOT NuLL           {$$ = malloc(SAFETY+9); strcpy($$, "not_null");}
                   ;

//changed in Ada 2005
access_definition : ACCESS name
                    {$$ = malloc(SAFETY+strlen($2)+9);
                     strcpy($$, "access, ");
                     strcat($$, $2);
                     free($2);
                    }
                  | ACCESS CONSTANT name
                    {$$ = malloc(SAFETY+strlen($3)+18);
                     strcpy($$, "access_constant, ");
                     strcat($$, $3);
                     free($3);
                    }
                  | ACCESS protected_opt PROCEDURE formal_part_opt
                    {$$ = malloc((35+strlen($2)+strlen($4)) );
                     strcpy($$, "access_procedure, ");
                     strcat($$, $2);
                     strcat($$, ", parameters([");
                     strcat($$, $4);
                     strcat($$, "])");
                     free($2);
                     free($4);
                    }
                  | ACCESS protected_opt FUNCTION formal_part_opt RETURN access_or_subtype_disc     //changed in Ada 2005
                    {$$ = malloc(36+strlen($2)+strlen($4)+strlen($6));
                     strcpy($$, "access_function, ");
                     strcat($$, $2);
                     strcat($$, ", parameters([");
                     strcat($$, $4);
                     strcat($$, "]), ");
                     strcat($$, $6),
                     free($2);
                     free($4);
                     free($6);
                    }
                  ;

/* to clarify : a name can denote a function or procedure call through a direct_name or an indexed_component */
name : direct_name                  {$$ = $1;}
     | indexed_component            {$$ = $1;} /* can be a array access, a function/procedure call, a type conversion, or a subtype_indication with index_constraint or discriminant_constraint*/
     | selected_component           {$$ = $1;}
     | attribute_reference          {$$ = $1;}
     | operator_symbol_or_string    {$$ = $1;}
     | qualified_expression         {$$ = $1;}
     ;

//compound_name replaces defining_program_unit_name in the standard grammar which are more general but don't know why (09/11/04)
//For Ada Rule 6.1
compound_name : identifier_rule {$$ = $1;}
              | compound_name '.' identifier_rule
                {$$ = $3;
                 free($1);
                }
              ;

/*could be a function call without parameters or with parameters (in which case will also go afterwards through indexed_component */
direct_name : identifier_rule
              {$$ = $1;}
            ;

operator_symbol_or_string : string_literal
                            {$$ = handle_identifiers($1, 1); //1 because it is a string
                             if (!strncmp($$, "string(", 7)) { //not a cross referenced operator
                               strcpy(not_cross_referenced_operator, $$);
                               if (debugMode) fprintf(stdout, "not_cross_referenced_operator is: %s\n", not_cross_referenced_operator);
                             }
                            }
                          ;

/*can be a array access, a function/procedure call, a type conversion, or a subtype_indication with index_contraint */
/*we will have to differentiate between many things*/
indexed_component : name '(' value_list ')'
                    {$$ = malloc(SAFETY+strlen(tmp_s)+strlen($1)+strlen($1)+strlen($1)+strlen($3)+strlen($3)+strlen($3)+56);
                     itoa(runtime_nb++, tmp_s, 10);
                     print_coverage_details(RUNE, tmp_s, current_unit, yylineno, column+1); 
                     strcpy($$, "indexed(");
                     strcat($$, $1);
                     strcat($$, ", [");
                     strcat($$, "rune(");
                     strcat($$, tmp_s);
                     strcat($$, ", ");
                     strcat($$, $3);
                     strcat($$, " > ");
                     strcat($$, "tic(");
                     strcat($$, $1);
                     strcat($$, ", last) || ");
                     strcat($$, $3);
                     strcat($$, " < ");
                     strcat($$, "tic(");
                     strcat($$, $1);
                     strcat($$, ", first) ,");
                     strcat($$, $3);
                     strcat($$, ")");
                     strcat($$, "])");
                     free($1);
                     free($3);
                    }
                  ;

value_list : value                      {$$ = $1;}
           | value_list ',' value       {$$ = malloc(SAFETY+strlen($1)+strlen($3)+3);
                                         strcpy($$, $1);
                                         strcat($$, ", ");
                                         strcat($$, $3);
                                         free($1);
                                         free($3);
                                        }
           ;

value : expression          {$$ = $1;} //would like to use expression_2 but cannot find a good line or column ... so the column will be very approximate in case an expression that is also a deci
      | comp_assoc          {$$ = $1;}
      | discrete_with_range {$$ = $1;}
      ;

selected_component : name '.' {
                 if (!strcmp($1, "Ascii_98")) { //total hack see 28/09/04
                     is_ascii = 1;
                  }
                 }
                 direct_name
                     {is_ascii = 0;
                      $$ = malloc(SAFETY+strlen($1)+strlen($4)+13);
                      strcpy($$, "selected(");
                      strcat($$, $1);
                      strcat($$, ", ");
                      strcat($$, $4);
                      strcat($$, ")");
                      free($1);
                      free($4);
                     }
                   | name '.' character_literal
                     {$$ = malloc(SAFETY+strlen($1)+strlen($3.id)+33);
                      strcpy($$, "selected(");
                      strcat($$, $1);
                      strcat($$, ", ");
                      strcat($$, handle_identifiers($3, 0));    //0 because not a string
                      strcat($$, ")");
                      free($1);
                     }
                   | name '.' operator_symbol_or_string
                     {$$ = malloc(SAFETY+strlen($1)+strlen($3)+13);
                      strcpy($$, "selected(");
                      strcat($$, $1);
                      strcat($$, ", ");
                      strcat($$, $3);
                      strcat($$, ")");
                      free($1);
                      free($3);
                     }
                   | name '.' ALL
                     {$$ = malloc(SAFETY+strlen($1)+16);
                      strcpy($$, "selected(");
                      strcat($$, $1);
                      strcat($$, ", all)");
                      free($1);
                     }
                   ;

attribute_reference : name TIC attribute_id
                      {$$ = malloc(SAFETY+strlen($3)+strlen($1)+8);
                       strcpy($$, "tic(");
                       strcat($$, $1);
                       strcat($$, ", ");
                       strcat($$, $3);      //never cross referenced
                       strcat($$, ")");
                       free($3);
                       free($1);
                      }
                    ;

// attributes of the form X'attribute_id(static_expression) are treated as function calls
// for example Colour'Succ(Red) is treated as a function call ie. as an indexed_component with Colour'Succ the name of the function !!!
attribute_id : identifier       {$$ = $1.id;} //correct (not cross referenced)
             | DELTA            {$$ = malloc(6); strcpy($$, "delta");}
             | DIGITS           {$$ = malloc(7); strcpy($$, "digits");}
             | ACCESS           {$$ = malloc(7); strcpy($$, "access");}
             | MOD              {$$ = malloc(4); strcpy($$, "mod");}
             ;

/*********************************     EXPRESSION     *********************************/
expression : expression_2
             {if ($1.is_a_decision)
                {itoa(decision_nb++, tmp_s, 10);
                 $$ = malloc(SAFETY+strlen(tmp_s)+strlen($1.id)+15);
                 print_coverage_details(DECI, tmp_s, current_unit, yylineno, column);
                 strcpy($$, "deci(");
                 strcat($$, tmp_s);
                 strcat($$, ", ");
                 strcat($$, $1.id);
                 strcat($$, ")");
                 free($1.id);
                }
              else $$ = $1.id;
             }
           ;

//uses 'struct id_decision'
expression_2 : relation
               {$$ = $1;}
             | relation boolean_operator relation
               {if (!strncmp($2.id, "String_", 7)) {        //it is a user defined operator
                  $$.id = malloc(SAFETY+strlen($1.id)+strlen($2.id)+strlen($3.id)+15);
                  strcpy($$.id, "indexed(");
                  strcat($$.id, $2.id);
                  strcat($$.id, ",[");
                  strcat($$.id, $1.id);
                  strcat($$.id, ", ");
                  strcat($$.id, $3.id);
                  strcat($$.id, "])");
                  $$.is_a_decision = 0;
                }
                else {
                  char gate_nb_s[10];
                  char *le_arg = NULL;
                  char *ri_arg = NULL;
                  int line_int = atoi($2.line);
                  int column_int = atoi($2.column);
                  itoa(gate_nb++, gate_nb_s, 10);
                  print_coverage_details(GATE, gate_nb_s, current_unit, line_int, column_int);    //perfect : column indicated is just on the boolean operator
                  build_condition($1, &le_arg, current_unit, line_int, column_int);
                  build_condition($3, &ri_arg, current_unit, line_int, column_int);
                  $$.id = malloc(SAFETY+strlen($2.id)+strlen(gate_nb_s)+strlen(le_arg)+strlen(ri_arg)+7);
                  strcpy($$.id, $2.id);
                  strcat($$.id, "(");
                  strcat($$.id, gate_nb_s),
                  strcat($$.id, ", ");
                  strcat($$.id, le_arg);
                  strcat($$.id, ", ");
                  strcat($$.id, ri_arg);
                  strcat($$.id, ")");
                  free(le_arg);
                  free(ri_arg);
                  $$.is_a_decision = 1;
                }
                free($1.id);
                free($2.id);
                free($3.id);
               }
             | expression_2 boolean_operator relation
               {if (!strncmp($2.id, "String_", 7)) {       //it is a user defined operator
                  $$.id = malloc(SAFETY+strlen($1.id)+strlen($2.id)+strlen($3.id)+15);
                  strcpy($$.id, "indexed(");
                  strcat($$.id, $2.id);
                  strcat($$.id, ",[");
                  strcat($$.id, $1.id);
                  strcat($$.id, ", ");
                  strcat($$.id, $3.id);
                  strcat($$.id, "])");
                  $$.is_a_decision = 0;
                }
                else {
                  char gate_nb_s[10];
                  char *le_arg = NULL;
                  char *ri_arg = NULL;
                  int line_int = atoi($2.line);
                  int column_int = atoi($2.column);
                  itoa(gate_nb++, gate_nb_s, 10);
                  print_coverage_details(GATE, gate_nb_s, current_unit, line_int, column_int);    //perfect : column indicated is just on the boolean operator
                  build_condition($1, &le_arg, current_unit, line_int, column_int);
                  build_condition($3, &ri_arg, current_unit, line_int, column_int);
                  $$.id = malloc(SAFETY+strlen($2.id)+strlen(gate_nb_s)+strlen(le_arg)+strlen(ri_arg)+7);
                  strcpy($$.id, $2.id);
                  strcat($$.id, "(");
                  strcat($$.id, gate_nb_s),
                  strcat($$.id, ", ");
                  strcat($$.id, le_arg);
                  strcat($$.id, ", ");
                  strcat($$.id, ri_arg);
                  strcat($$.id, ")");
                  free(le_arg);
                  free(ri_arg);
                  $$.is_a_decision = 1;
                }
                free($1.id);
                free($2.id);
                free($3.id);
               }
             ;

boolean_operator : AND
                   {$$.id = handle_operator_calls($1);
                    if (!$$.id) {
                      $$.id = malloc(4);
                      strcpy($$.id, "and");
                    }
                    strcpy($$.line, $1.line);
                    strcpy($$.column, $1.column);
                   }
                 | OR
                   {$$.id = handle_operator_calls($1);
                    if (!$$.id)
                      {$$.id = malloc(3);
                       strcpy($$.id, "or");
                      }
                    strcpy($$.line, $1.line);
                    strcpy($$.column, $1.column);
                   }
                 | XOR
                   {$$.id = handle_operator_calls($1);
                    if (!$$.id)
                      {$$.id = malloc(4);
                       strcpy($$.id, "xor");
                      }
                    strcpy($$.line, $1.line);
                    strcpy($$.column, $1.column);
                   }
                 | AND THEN
                   {$$.id = malloc(9);
                    strcpy($$.id, "and_then");    //cannot be overloaded. But could it be renamed?
                    strcpy($$.line, $1.line);
                    strcpy($$.column, $1.column);
                   }
                 | OR ELSE
                   {$$.id = malloc(8);
                    strcpy($$.id, "or_else");     //cannot be overloaded. But could it be renamed?
                    strcpy($$.line, $1.line);
                    strcpy($$.column, $1.column);
                   }
                 ;

//uses 'struct id_decision'
relation : simple_expression    {$$ = $1;}
         | simple_expression relational_operator simple_expression
           {int line_int = atoi($2.line);
            int column_int = atoi($2.column);
            itoa(condition_nb++, tmp_s, 10);
            print_coverage_details(COND, tmp_s, current_unit, line_int, column_int);  //perfect : column indicated is just on the relational operator
            {if (!strncmp($2.id, "String_", 7))        //it is a user defined operator : maybe because overloaded or renamed
               {$$.id = malloc(SAFETY+strlen(tmp_s)+strlen($1.id)+strlen($2.id)+strlen($3.id)+23); // 20/05/08 was $$.id = malloc(SAFETY+strlen($1.id)+strlen($2)+strlen($3.id)+23);
                strcpy($$.id, "cond("); // 20/05/08 was commented out
                // 20/05/08 old comment : if it is  overloaded (and we cannot know this during parsing) then it may not return a Boolean (hence we do not surround the call with 'cond()')
                strcat($$.id, tmp_s);   // 20/05/08 was commented out
                strcat($$.id, ", ");    // 20/05/08 was commented out
                strcat($$.id, "indexed("); // 20/05/08 strcpy
                strcat($$.id, $2.id);
                strcat($$.id, ",[");
                strcat($$.id, $1.id);
                strcat($$.id, ", ");
                strcat($$.id, $3.id);
                strcat($$.id, "])");
                strcat($$.id, ")");     // 20/05/08 was commented out
                $$.is_a_decision = 1;   // 20/05/08 was 0
               }
             else
               {$$.id = malloc(SAFETY+strlen(tmp_s)+strlen($1.id)+strlen($2.id)+strlen($3.id)+9);
                strcpy($$.id, "cond(");
                strcat($$.id, tmp_s);
                strcat($$.id, ", ");
                strcat($$.id, $1.id);
                strcat($$.id, $2.id);
                strcat($$.id, $3.id);
                strcat($$.id, ")");
                $$.is_a_decision = 1;
               }
            }
            free($1.id);
            free($2.id);
            free($3.id);
           }
         | simple_expression membership range_or_name
           {int line_int = atoi($2.line);
            int column_int = atoi($2.column);
            itoa(condition_nb++, tmp_s, 10);
            $$.id = malloc(SAFETY+strlen(tmp_s)+strlen($2.id)+strlen($1.id)+strlen($3)+23);
            print_coverage_details(COND, tmp_s, current_unit, line_int, column_int);    //perfect : column indicated is just on the membership operator
            strcpy($$.id, "cond(");
            strcat($$.id, tmp_s);
            strcat($$.id, ", ");
            strcat($$.id, $2.id);
            strcat($$.id, "(");
            strcat($$.id, $1.id);
            strcat($$.id, ", ");
            strcat($$.id, $3);
            strcat($$.id, ")");
            strcat($$.id, ")");
            free($2.id);
            free($1.id);
            free($3);
            $$.is_a_decision = 1;
           }
         ;

//only used above in one rule
range_or_name : range   {$$ = $1;}
              | name    {$$ = $1;}
              ;

membership : IN                 {$$.id = malloc(6);
                                 strcpy($$.id, "is_in");
                                 strcpy($$.line, $1.line);
                                 strcpy($$.column, $1.column);
                                }
           | NOT IN             {$$.id = malloc(10);
                                 strcpy($$.id, "is_not_in");
                                 strcpy($$.line, $1.line);
                                 strcpy($$.column, $1.column);
                                }
           ;

//uses 'struct id_decision'
simple_expression : unary_adding_operator term
                    {if (!strncmp($1, "String_", 7))        //it is a user defined operator
                       {$$.id = malloc(SAFETY+strlen($1)+strlen($2.id)+13);
                        strcpy($$.id, "indexed(");
                        strcat($$.id, $1);
                        strcat($$.id, ",[");
                        strcat($$.id, $2.id);
                        strcat($$.id, "])");
                       }
                     else
                       {$$.id = malloc(SAFETY+strlen($1)+strlen($2.id)+1);
                        strcpy($$.id, $1);
                        strcat($$.id, $2.id);
                       }
                     free($1);
                     free($2.id);
                     $$.is_a_decision = 0;
                    }
                  | term        {$$ = $1;}
                  | simple_expression binary_adding_operator term
                    {if (!strncmp($2, "String_", 7))        //it is a user defined operator
                       {$$.id = malloc(SAFETY+strlen($1.id)+strlen($2)+strlen($3.id)+15);
                        strcpy($$.id, "indexed(");
                        strcat($$.id, $2);
                        strcat($$.id, ",[");
                        strcat($$.id, $1.id);
                        strcat($$.id, ", ");
                        strcat($$.id, $3.id);
                        strcat($$.id, "])");
                       }
                     else
                       {$$.id = malloc(SAFETY+strlen($1.id)+strlen($2)+strlen($3.id)+1);
                        strcpy($$.id, $1.id);
                        strcat($$.id, $2);
                        strcat($$.id, $3.id);
                       }
                     free($1.id);
                     free($2);
                     free($3.id);
                     $$.is_a_decision = 0;
                    }
                  ;

unary_adding_operator : PLUS    {$$ = handle_operator_calls($1);
                                 if (!$$)
                                   {$$ = malloc(4);
                                    strcpy($$, " + ");
                                   }
                                }
                      | MINUS   {$$ = handle_operator_calls($1);
                                 if (!$$)
                                   {$$ = malloc(4);
                                    strcpy($$, " - ");
                                   }
                                }
                      ;

binary_adding_operator : PLUS   {$$ = handle_operator_calls($1);
                                 if (!$$)
                                   {$$ = malloc(4);
                                    strcpy($$, " + ");
                                   }
                                }
                       | MINUS  {$$ = handle_operator_calls($1);
                                 if (!$$)
                                   {$$ = malloc(4);
                                    strcpy($$, " - ");
                                   }
                                }
                       | CONC   {$$ = handle_operator_calls($1);
                                 if (!$$)
                                   {$$ = malloc(4);
                                    strcpy($$, " & ");
                                   }
                                }
                       ;

//uses 'struct id_decision'
term    : factor                {$$ = $1;}
        | term multiplying_operator factor
                                {if (!strncmp($2, "String_", 7))        //it is a user defined operator
                                   {$$.id = malloc(SAFETY+strlen($1.id)+strlen($2)+strlen($3.id)+15);
                                    strcpy($$.id, "indexed(");
                                    strcat($$.id, $2);
                                    strcat($$.id, ",[");
                                    strcat($$.id, $1.id);
                                    strcat($$.id, ", ");
                                    strcat($$.id, $3.id);
                                    strcat($$.id, "])");
                                   }
                                  else if(!strncmp($2, " / ", 3))
                                  {
                                    $$.id = malloc(SAFETY+strlen(tmp_s)+strlen($1.id)+strlen($2)+strlen($3.id)+strlen($3.id)+14);
                                    itoa(runtime_nb++, tmp_s, 10);
                                    print_coverage_details(RUNE, tmp_s, current_unit, yylineno, column+1); 
                                    strcpy($$.id, $1.id);
                                    strcat($$.id, $2);
                                    strcat($$.id, "rune(");
                                    strcat($$.id, tmp_s);
                                    strcat($$.id, ", ");
                                    strcat($$.id, $3.id);
                                    strcat($$.id, " = 0, ");
                                    strcat($$.id, $3.id);
                                    strcat($$.id, ")");
                                  }
                                 else
                                   {$$.id = malloc(SAFETY+strlen($1.id)+strlen($2)+strlen($3.id)+1);
                                    strcpy($$.id, $1.id);
                                    strcat($$.id, $2);
                                    strcat($$.id, $3.id);
                                   }
                                 free($1.id);
                                 free($2);
                                 free($3.id);
                                 $$.is_a_decision = 0;
                                }
        ;  //runtime Error check, counter for this,

multiplying_operator : MULT     {$$ = handle_operator_calls($1);
                                 if (!$$)
                                   {$$ = malloc(4);
                                    strcpy($$, " * ");
                                   }
                                }
                     | DIV      {$$ = handle_operator_calls($1);
                                 
                                 if (!$$)
                                   {
                                    $$ = malloc(4);
                                    strcpy($$, " / ");
                                   }
                                }
                     | MOD      {$$ = handle_operator_calls($1);
                                 if (!$$)
                                   {$$ = malloc(6);
                                    strcpy($$, " mod ");
                                   }
                                }
                     | REM      {$$ = handle_operator_calls($1);
                                 if (!$$)
                                   {$$ = malloc(6);
                                    strcpy($$, " rem ");
                                   }
                                }
                     ;

//uses 'struct id_decision'
factor : primary                {$$ = $1;}
       | NOT primary
         {char *op;
          op = handle_operator_calls($1);
          if (!op) {    //it is not a user defined operator
            op = malloc(4);
            strcpy(op, "not");
          }
          if (!strncmp(op, "String_", 7)) {        //it is a user defined operator
            $$.id = malloc(SAFETY+strlen(op)+strlen($2.id)+13);
            strcpy($$.id, "indexed(");
            strcat($$.id, op);
            strcat($$.id, ",[");
            strcat($$.id, $2.id);
            strcat($$.id, "])");
            $$.is_a_decision = 0;
          }
          else {
            char gate_nb_s[10];
            char *le_arg = NULL;
            int line_int = atoi($1.line);
            int column_int = atoi($1.column);
            itoa(gate_nb++, gate_nb_s, 10);
            print_coverage_details(GATE, gate_nb_s, current_unit, line_int, column_int);    //perfect : column indicated is just on the boolean operator
            build_condition($2, &le_arg, current_unit, line_int, column_int);
            $$.id = malloc(SAFETY+strlen(op)+strlen(gate_nb_s)+strlen(le_arg)+5);
            strcpy($$.id, op);
            strcat($$.id, "(");
            strcat($$.id, gate_nb_s),
            strcat($$.id, ", ");
            strcat($$.id, le_arg);
            strcat($$.id, ")");
            free(le_arg);
            $$.is_a_decision = 1;
          }
          free($1.id);
          free($2.id);
          free(op);
         }
        | ABS primary
          {char *op;
           op = handle_operator_calls($1);
           if (!op)
           {op = malloc(5);
            strcpy(op, "abs ");
           }
           if (!strncmp(op, "String_", 7))        //it is a user defined operator
             {$$.id = malloc(SAFETY+strlen($2.id)+strlen(op)+13);
              strcpy($$.id, "indexed(");
              strcat($$.id, op);
              strcat($$.id, ",[");
              strcat($$.id, $2.id);
              strcat($$.id, "])");
             }
           else
             {$$.id = malloc(SAFETY+strlen($2.id)+strlen(op)+5);
              strcpy($$.id, op);
              strcat($$.id, $2.id);
             }
           free($2.id);
           $$.is_a_decision = 0;
          }
        | primary EXPON primary
          {char *op;
           op = handle_operator_calls($2);
           if (!op)
             {op = malloc(5);
              strcpy(op, " ** ");
             }
           if (!strncmp(op, "String_", 7))        //it is a user defined operator
             {$$.id = malloc(SAFETY+strlen($1.id)+strlen(op)+strlen($3.id)+15);
              strcpy($$.id, "indexed(");
              strcat($$.id, op);
              strcat($$.id, ",[");
              strcat($$.id, $1.id);
              strcat($$.id, ", ");
              strcat($$.id, $3.id);
              strcat($$.id, "])");
             }
           else
             {$$.id = malloc(SAFETY+strlen($1.id)+strlen(op)+strlen($3.id)+1);
              strcpy($$.id, $1.id);
              strcat($$.id, op);
              strcat($$.id, $3.id);
             }
           free($1.id);
           free($3.id);
           $$.is_a_decision = 0;
          }
        ;

/* a type conversion is treated as a name -> indexed component */
//uses 'struct id_decision'
primary : literal               {$$.id = $1; $$.is_a_decision = 0;}
        | name                  {$$.id = $1; $$.is_a_decision = 0;}
        | allocator             {$$.id = $1; $$.is_a_decision = 0;}
        | parenthesized_primary {$$ = $1;}  /* include aggregates !*/
        ;

literal : numeric_literal       {$$ = $1;}
        | character_literal     {$$ = handle_identifiers($1, 0);}//0 because not a string
        | NuLL                  {$$ = malloc(5 ); strcpy($$, "null");}
        ;

relational_operator : EQUAL     {$$.id = handle_operator_calls($1);
                                 if (!$$.id)
                                   {$$.id = malloc(4);
                                    strcpy($$.id, " = ");
                                   }
                                 strcpy($$.line, $1.line);
                                 strcpy($$.column, $1.column);
                                }
                    | NE        {$$.id = handle_operator_calls($1);
                                 if (!$$.id)
                                   {$$.id = malloc(5);
                                    strcpy($$.id, " <> ");
                                   }
                                 strcpy($$.line, $1.line);
                                 strcpy($$.column, $1.column);
                                }
                    | LT        {$$.id = handle_operator_calls($1);
                                 if (!$$.id)
                                   {$$.id = malloc(4);
                                    strcpy($$.id, " < ");
                                   }
                                 strcpy($$.line, $1.line);
                                 strcpy($$.column, $1.column);
                                }
                    | LT_EQ     {$$.id = handle_operator_calls($1);
                                 if (!$$.id)
                                   {$$.id = malloc(5);
                                    strcpy($$.id, " <= ");
                                   }
                                 strcpy($$.line, $1.line);
                                 strcpy($$.column, $1.column);
                                }
                    | GT        {$$.id = handle_operator_calls($1);
                                 if (!$$.id)
                                   {$$.id = malloc(4);
                                    strcpy($$.id, " > ");
                                   }
                                 strcpy($$.line, $1.line);
                                 strcpy($$.column, $1.column);
                                }
                    | GE        {$$.id = handle_operator_calls($1);
                                 if (!$$.id)
                                   {$$.id = malloc(5);
                                    strcpy($$.id, " >= ");
                                   }
                                 strcpy($$.line, $1.line);
                                 strcpy($$.column, $1.column);
                                }
                    ;

qualified_expression : name TIC parenthesized_primary
                       {$$ = malloc((SAFETY+strlen($1)+strlen($3.id)+8) );
                        strcpy($$, "tic(");
                        strcat($$, $1);
                        strcat($$, ", ");
                        strcat($$, $3.id);
                        strcat($$, ")");
                        free($1);
                        free($3.id);
                       }
                     ;

//uses 'struct id_decision'
parenthesized_primary : aggregate
                        {$$.id = malloc(SAFETY+strlen($1)+6);
                         strcpy($$.id, "agg(");
                         strcat($$.id, $1);
                         strcat($$.id, ")");
                         free($1);
                         $$.is_a_decision = 0;
                        }
                      | '(' expression_2 ')'
                        {$$.id = malloc(SAFETY+strlen($2.id)+3);
                         strcpy($$.id, "(");
                         strcat($$.id, $2.id);
                         strcat($$.id, ")");
                         $$.is_a_decision = $2.is_a_decision;
                         free($2.id);
                        }
                      | '(' IF cond_expression_list else_expression_opt ')'
                         {$$.id = malloc(SAFETY+strlen($3)+strlen($4)+14);
                          strcpy($$.id, "if_expr([");
                          strcat($$.id, $3);
                          strcat($$.id, "], ");
                          strcat($$.id, $4);
                          strcat($$.id, ")");
                          $$.is_a_decision = 0; //unsure about this one
                          free($3);
                          free($4);
                         }
                      ;

cond_expression_list : cond_expression          {$$ = $1;}
                     | cond_expression_list ELSIF cond_expression
                       {$$ = malloc(SAFETY+strlen($1)+strlen($3)+3);
                        strcpy($$, $1);
                        strcat($$, ", ");
                        strcat($$, $3);
                        free($1);
                        free($3);
                       }
                     ;

cond_expression : cond_part <true_line_column> {$$.line = yylineno; $$.column = column+1;} expression_2
                  {char *expression;
                   build_expression($3, &expression, current_unit, $2.line, $2.column);
                   $$ = malloc(SAFETY+strlen($1)+strlen(expression)+19);
                   strcpy($$, "if_expr_clause(");
                   strcat($$, $1);
                   strcat($$, ", ");
                   strcat($$, expression);
                   strcat($$, ")");
                   free($1);
                   free($3.id);
                  }
                ;

else_expression_opt : /* empty */
                      {$$ = malloc(SAFETY+22);
                       strcpy($$, "else_expression(true)"); //should evaluate to true
                      }
                    | ELSE <true_line_column> {$$.line = yylineno; $$.column = column+1;} expression_2
                      {char *expression;
                       build_expression($3, &expression, current_unit, $2.line, $2.column);
                       $$ = malloc(SAFETY+strlen(expression)+18);
                       strcpy($$, "else_expression(");
                       strcat($$, expression);
                       strcat($$, ")");
                       free($3.id);
                      }
                    ;

//Ada Rule 4.8
//changed for Ada 2005 : loads of difficulties ... but finally managed: name contsraint is not necessary ...
//not needed see Ada RM-4-8.html note 4 : "a constraint is permitted only if the subtype_mark denotes an unconstrained composite subtype;"
//therefore scalar constraints are not needed
allocator : NEW name    {$$ = malloc(SAFETY+strlen($2)+12);
                         strcpy($$, "allocator(");
                         strcat($$, $2);
                         strcat($$, ")");
                         free($2);
                        }
          ;

number_declaration : identifier_list ':' CONSTANT IS_ASSIGNED <true_line_column> {$$.line = yylineno; $$.column = column+1;} expression_2 ';'
                     {char *expression;
                      build_expression($6, &expression, current_unit, $5.line, $5.column);
                      $$ = malloc(SAFETY+strlen($1)+strlen(expression)+28);
                      strcpy($$, "\n             number([");
                      strcat($$, $1);
                      strcat($$, "], ");
                      strcat($$, expression);
                      strcat($$, ")");
                      free($1);
                      free($6.id);
                     }
        ;

/* Chapter 6 */
sequence_of_statements : statement
                         {$$ = malloc((SAFETY+strlen($1)+17) );
                          strcpy($$, "\n              ");
                          strcat($$, $1);
                          free($1);
                         }
                       | sequence_of_statements statement
                         {$$ = malloc((SAFETY+strlen($1)+strlen($2)+18) );
                          strcpy($$, $1);
                          strcat($$, ",\n              ");
                          strcat($$, $2);
                          free($1);
                          free($2);
                         }
                       ;

statement : unlabeled_statement {$$ = $1;}
          | label statement
            {$$ = malloc(SAFETY+strlen($1)+strlen($2)+22);
             strcpy($$, "labeled_statement(");
             strcat($$, $1);
             strcat($$, ", ");
             strcat($$, $2);
             strcat($$, ")");
             free($1);
             free($2);
            }
          ;

unlabeled_statement : simple_statement          {$$ = $1;}
                    | compound_statement        {$$ = $1;}
                    | pragma                    {$$ = $1;}
                    ;

simple_statement : null_statement       {$$ = $1;}
        | assignement_statement         {$$ = $1;}
        | exit_statement                {$$ = $1;}
        | simple_return_statement       {$$ = $1;}
        | goto_statement                {$$ = $1;}
        | procedure_call_statement      {$$ = $1;}  //including code_statement change in ada 2005
        | delay_statement               {$$ = $1;}
        | abort_statement               {$$ = $1;}
        | raise_statement               {$$ = $1;}
        | requeue_statement             {$$ = $1;}
        ;

compound_statement : if_statement       {$$ = $1;}
        | case_statement                {$$ = $1;}
        | loop_statement                {$$ = $1;}
        | block                         {$$ = $1;}
        | accept_statement              {$$ = $1;}
        | select_statement              {$$ = $1;}
        | extended_return_statement     {$$ = $1;} //new in Ada 2005
        ;

label : LT_LT identifier_rule GT_GT     {$$ = $2;}
      ;

null_statement : NuLL ';'       {$$ = malloc(5 ); strcpy($$, "null");}
        ;

assignement_statement : name IS_ASSIGNED <true_line_column> {$$.line = yylineno; $$.column = column+1;} expression_2 ';'
                        {char *expression;
                         build_expression($4, &expression, current_unit, $3.line, $3.column);
                         $$ = malloc(SAFETY+strlen($1)+strlen(expression)+11);
                         strcpy($$, "assign(");
                         strcat($$, $1);
                         strcat($$, ", ");
                         strcat($$, expression);
                         strcat($$, ")");
                         free($1);
                         free($4.id);
                        }
                      ;

if_statement : IF cond_clause_list else_opt END IF ';'
               {$$ = malloc((SAFETY+strlen($2)+strlen($3)+14) );
                strcpy($$, "if_stmt([");
                strcat($$, $2);
                strcat($$, "], ");
                strcat($$, $3);
                strcat($$, ")");
                free($2);
                free($3);
               }
             ;

cond_clause_list : cond_clause          {$$ = $1;}
                 | cond_clause_list ELSIF cond_clause
                   {$$ = malloc((SAFETY+strlen($1)+strlen($3)+3) );
                    strcpy($$, $1);
                    strcat($$, ", ");
                    strcat($$, $3);
                    free($1);
                    free($3);
                   }
                 ;

cond_clause : cond_part sequence_of_statements
              {$$ = malloc((SAFETY+strlen($1)+strlen($2)+69) );
               strcpy($$, "if_clause(");
               strcat($$, $1);
               strcat($$, ",\n              stmts([");
               strcat($$, $2);
               strcat($$, "\n                   ]))");
               free($1);
               free($2);
              }
            ;

cond_part : decision THEN       {$$ = $1;}
          ;

decision : <true_line_column> {$$.line = yylineno; $$.column = column+1;}
           expression_2
           {itoa(branch_nb++, tmp_s, 10);
            itoa(decision_nb++, tmp_s2, 10);
            print_coverage_details(BRAN, tmp_s, current_unit, $1.line, $1.column);  //ok : column indicated is just before the expression
            print_coverage_details(DECI, tmp_s2, current_unit, $1.line, $1.column); //ok : column indicated is just before the expression
            if ($2.is_a_decision)
              {$$ = malloc(SAFETY+strlen(tmp_s)+strlen(tmp_s2)+strlen($2.id)+30);
               strcpy($$, "bran(");
               strcat($$, tmp_s);
               strcat($$, ", deci(");
               strcat($$, tmp_s2);
               strcat($$, ", ");
               strcat($$, $2.id);
               strcat($$, "))");
              }
            else
              {itoa(condition_nb++, tmp_s3, 10);
               $$ = malloc(SAFETY+strlen(tmp_s)+strlen(tmp_s2)+strlen(tmp_s3)+strlen($2.id)+45);
               print_coverage_details(COND, tmp_s3, current_unit, $1.line, $1.column);  //ok : column indicated is just before the expression
               strcpy($$, "bran(");
               strcat($$, tmp_s);
               strcat($$, ", deci(");
               strcat($$, tmp_s2);
               strcat($$, ", cond(");
               strcat($$, tmp_s3);
               strcat($$, ", ");
               strcat($$, $2.id);
               strcat($$, ")))");
              }
            free($2.id);
           }
         ;

else_opt : /* empty */                  {$$ = malloc((SAFETY+32) ); strcpy($$, "\n              else(stmts([]))");}
         | ELSE sequence_of_statements
           {$$ = malloc((SAFETY+strlen($2)+32) );
            strcpy($$, "\n              else(stmts([");
            strcat($$, $2);
            strcat($$, "]))");
            free($2);
           }
         ;

case_statement : case_hdr pragma_s alternative_list END CASE ';'
                 {$$ = malloc((SAFETY+strlen($1)+strlen($3)+6) );
                  strcpy($$, $1);
                  strcat($$, "[");
                  strcat($$, $3);
                  strcat($$, "])");
                  free($1);
                  free($2);
                  free($3);
                 }
               ;

case_hdr : CASE <true_line_column> {$$.line = yylineno; $$.column = column+1;} expression_2 IS
           {char *expression;
            build_expression($3, &expression, current_unit, $2.line, $2.column);
            $$ = malloc(SAFETY+strlen(expression)+13);
            strcpy($$, "case_stmt(");
            strcat($$, expression);
            strcat($$, ", ");
            free($3.id);
           }
         ;
//only used in case_statement
alternative_list : alternative {$$ = $1;}
                 | alternative_list alternative
                   {$$ = malloc(SAFETY+strlen($1)+strlen($2)+4);
                    strcpy($$, $1);
                    strcat($$, ",\n");
                    strcat($$, $2);
                    free($1);
                    free($2);
                   }
                 ;

//15/02/06
//only used in case_statement
//this indirectly leads to the creation of a branch, a decision and a condition
// expression = choice_s : 1 condition per choice
alternative : WHEN <true_line_column> {$$.line = yylineno; $$.column = column+1;} choice_s2 RIGHT_SHAFT sequence_of_statements
              {if (!strcmp($3, "others")) {
                 $$ = malloc(SAFETY+strlen($5)+31);
                 strcpy($$, "alternative(others, stmts([");
                 strcat($$, $5);
                 strcat($$, "]))");
               }
               else {
                 itoa(branch_nb++, tmp_s, 10);
                 itoa(decision_nb++, tmp_s2, 10);
                 print_coverage_details(BRAN, tmp_s, current_unit, $2.line, $2.column);
                 print_coverage_details(DECI, tmp_s2, current_unit, $2.line, $2.column);
                 $$ = malloc((SAFETY+strlen(tmp_s)+strlen(tmp_s2)+strlen($3)+strlen($5)+44) );
                 strcpy($$, "alternative(bran(");
                 strcat($$, tmp_s);
                 strcat($$, ", ");
                 strcat($$, "deci(");
                 strcat($$, tmp_s2);
                 strcat($$, ", [");
                 strcat($$, $3);
                 strcat($$, "])), stmts([");
                 strcat($$, $5);
                 strcat($$, "]))");
               }
               free($3);
               free($5);
              }
            ;

//only used in case_statement
//is of the form cond(id, expr), ..., cond(id, expr)
//althought not obvious from the syntax : the choice "others" must appear alone and last
choice_s2 : choice2
            {$$ = $1;}
          | choice_s2 '|' choice2
            {$$ = malloc((SAFETY+strlen($1)+strlen($3)+3) );
             strcpy($$, $1);
             strcat($$, ", ");
             strcat($$, $3);
             free($1);
             free($3);
            }
         ;

//only used in case_statement
//will be a cond(...)
choice2 : expression_2
          {if ($1.is_a_decision)    //it would already contain cond(...)
             $$ = $1.id;
           else
             {itoa(condition_nb++, tmp_s, 10);
              $$ = malloc(SAFETY+strlen(tmp_s)+strlen($1.id)+9);
              print_coverage_details(COND, tmp_s, current_unit, yylineno, column);
              strcpy($$, "cond(");
              strcat($$, tmp_s);
              strcat($$, ", ");
              strcat($$, $1.id);
              strcat($$, ")");
              free($1.id);
             }
          }
        | discrete_with_range
          {itoa(condition_nb++, tmp_s, 10);
           $$ = malloc((SAFETY+strlen(tmp_s)+strlen($1)+31) );
           print_coverage_details(COND, tmp_s, current_unit, yylineno, column);
           strcpy($$, "cond(");
           strcat($$, tmp_s);
           strcat($$, ", discrete_with_range(");
           strcat($$, $1);
           strcat($$, "))");
           free($1);
          }
        | OTHERS
          {$$ = malloc(7);
           strcpy($$, "others");
          }
        ;

loop_statement : label_opt iteration_opt basic_loop id_opt ';'
                 {$$ = malloc(SAFETY+strlen($1)+strlen($2)+strlen($3)+16);
                  strcpy($$, "loop_stmt(");
                  strcat($$, $1);
                  strcat($$, ", ");
                  strcat($$, $2);
                  strcat($$, ", ");
                  strcat($$, $3);
                  strcat($$, ")");
                  free($1);
                  free($2);
                  free($3);
                 }
               ;

label_opt : /* empty */                 {$$ = malloc((9) ); strcpy($$, "no_label");}
          | statement_identifier ':'    {$$ = $1;}
          ;

statement_identifier : direct_name      {$$ = $1;}
                     ;
//where it all happens : 3 forms of loops
iteration_opt : /* empty : a simple loop */
                {$$ = malloc((SAFETY+6) );
                 strcpy($$, "loop");
                }
              | /* a while loop */
                WHILE decision
                {$$ = malloc((SAFETY+strlen($2)+10) );
                 strcpy($$, "while, ");
                 strcat($$, $2);
                 free($2);
                }
              | /* a for loop */
                iter_part reverse_opt discrete_range
                {$$ = malloc(SAFETY+strlen($1)+strlen($2)+strlen($3)+1);
                 strcpy($$, $1);
                 strcat($$, $2);
                 strcat($$, $3);
                 free($1);
                 free($2);
                 free($3);
                }
              ;

iter_part : FOR <true_line_column> {$$.line = yylineno; $$.column = column+1;} identifier_rule  IN
            {itoa(branch_nb++, tmp_s, 10);
             itoa(decision_nb++, tmp_s2, 10);
             itoa(condition_nb++, tmp_s3, 10);
             print_coverage_details(BRAN, tmp_s, current_unit, $2.line, $2.column);     //ok : column indicated is just after the FOR keyword
             print_coverage_details(DECI, tmp_s2, current_unit, $2.line, $2.column);    //ok : column indicated is just after the FOR keyword
             print_coverage_details(COND, tmp_s3, current_unit, $2.line, $2.column);    //ok : column indicated is just after the FOR keyword
             $$ = malloc(SAFETY+strlen(tmp_s)+strlen(tmp_s2)+strlen(tmp_s3)+strlen($3)+34);
             strcpy($$, "for, bran(");
             strcat($$, tmp_s);
             strcat($$, ", deci(");
             strcat($$, tmp_s2);
             strcat($$, ", cond(");
             strcat($$, tmp_s3);
             strcat($$, ", ");
             strcat($$, $3);
             strcat($$, "))), ");
             free($3);
            }
          ;

reverse_opt : /* empty */       {$$ = malloc(9); strcpy($$, "normal, ");}
            | REVERSE           {$$ = malloc(10); strcpy($$, "reverse, ");}
            ;

basic_loop : LOOP sequence_of_statements END LOOP
             {$$ = malloc((SAFETY+strlen($2)+10) );
              strcpy($$, "stmts([");
              strcat($$, $2);
              strcat($$, "])");
              free($2);
             }
           ;

/* ignored */
id_opt : /* empty */ {}
       | designator {}
       ;
//End of LOOPS

block_body : BEGiN handled_statement_s
             {$$ = malloc(SAFETY+strlen($2)+9);
              strcpy($$, "body(");
              strcat($$, $2);
              strcat($$, ")\n");
              free($2);
             }
           ;

block : label_opt block_declaration block_body END id_opt ';'
        {$$ = malloc(SAFETY+strlen($1)+strlen($2)+strlen($3)+12);
         strcpy($$, "block(");
         strcat($$, $1);
         strcat($$, ", ");
         strcat($$, $2);        //the declare part
         strcat($$, ", ");
         strcat($$, $3);
         strcat($$, ")");
         free($1);
         free($2);
         free($3);
        }
      ;

block_declaration : /* empty */
                    {$$ = malloc(15);
                     strcpy($$, "local_decl([])");
                    }
                  | DECLARE declarative_part
                    {$$ = malloc(SAFETY+strlen($2)+15);
                     strcpy($$, "local_decl([");
                     strcat($$, $2);
                     strcat($$, "])");
                     free($2);
                    }
                  ;

//for ada_rule 11.2
handled_statement_s : sequence_of_statements exception_handler_part_opt
                      {$$ = malloc(SAFETY+strlen($1)+strlen($2)+25);
                       strcpy($$, "\nstmts([");
                       strcat($$, $1);
                       strcat($$, "\n      ]),\n  ");
                       strcat($$, $2);
                       free($1);
                       free($2);
                      }
                    ;

exception_handler_part_opt : /* empty */
                             {$$ = malloc(14 );
                              strcpy($$, "no_exceptions");
                             }
                           | exception_handler_part
                             {$$ = malloc((SAFETY+strlen($1)+22) );
                              strcpy($$, "exception_handler([");
                              strcat($$, $1);
                              strcat($$, "])");
                              free($1);
                             }
                           ;

exit_statement : EXIT name_opt when_opt ';'
                 {$$ = malloc(SAFETY+strlen($2)+strlen($3)+14);
                  strcpy($$, "exit_when(");
                  strcat($$, $2);
                  strcat($$, ", ");
                  strcat($$, $3);
                  strcat($$, ")");
                  free($2);
                  free($3);
                 }
               ;

name_opt : /* Empty */  {$$ = malloc(8); strcpy($$, "no_name");}
         | name         {$$ = $1;}  /*for named loops (see page 112 of Barnes)*/
         ;

when_opt : /* empty */  {$$ = malloc(8 ); strcpy($$, "no_when");}
         | WHEN decision {$$ = malloc((SAFETY+strlen($2)+1) );
                          strcpy($$, $2);
                          free($2);
                         }
         ;

simple_return_statement : RETURN ';'
                   {$$ = malloc(SAFETY+strlen(current_subprogram_name)+16);
                    strcpy($$, "return(");
                    strcat($$, "Return_");
                    strcat($$, current_subprogram_name);
                    strcat($$, ")");
                   }
                 |
                   RETURN expression_2 ';'
                   {char *expression;
                    int line_int = atoi($1.line);
                    int column_int = atoi($1.column);
                    build_expression($2, &expression, current_unit, line_int, column_int);
                    $$ = malloc(SAFETY+strlen(current_subprogram_name)+strlen(expression)+18);
                    strcpy($$, "return(");
                    strcat($$, "Return_");
                    strcat($$, current_subprogram_name);
                    strcat($$, ", ");
                    strcat($$, expression);
                    strcat($$, ")");
                    free($2.id);
                   }
                 ;

//new in Ada 2005
extended_return_statement : RETURN identifier_rule ':' constant_opt access_or_subtype_disc init_opt opt_handled_statement_s ';'
                            {$$ = malloc(SAFETY+strlen($2)+strlen($4)+strlen($5)+strlen($6)+strlen($7)+26);
                             strcpy($$, "extended_return(");
                             strcat($$, $2);
                             strcat($$, ", ");
                             strcat($$, $4);
                             strcat($$, ", ");
                             strcat($$, $5);
                             strcat($$, ", ");
                             strcat($$, $6);
                             strcat($$, ", ");
                             strcat($$, $7);
                             strcat($$, ")");
                             free($2);
                             free($4);
                             free($5);
                             free($6);
                             free($7);
                            }
                          ;

constant_opt : /* empty */      {$$ = malloc(SAFETY+14); strcpy($$, "not_constant");}
             | CONSTANT         {$$ = malloc(SAFETY+9); strcpy($$, "constant");}
             ;

opt_handled_statement_s : /* empty */ {$$ = malloc((SAFETY+13) ); strcpy($$, "nothing");}
                        | DO handled_statement_s END RETURN
                          {$$ = $2;}
                        ;

goto_statement : GOTO name ';'
                 {$$ = malloc(strlen($2)+7);
                  strcpy($$, "goto(");
                  strcat($$, $2);
                  strcat($$, ")");
                  free($2);
                 }
               ;

//For Ada Rule 6.4
//may also be a 'code statement'  or an 'entry call statement' if it does not evaluate to a subprogram
procedure_call_statement : name ';'
                           {$$ = malloc(strlen($1)+17);
                            strcpy($$, "procedure_call(");
                            strcat($$, $1);
                            strcat($$, ")");
                            free($1);
                           }
                         ;

/*********************************     SUBPROGRAMS     *********************************/
subprogram_declaration : overriding_indicator_opt subprogram_specification ';'                              //changed in Ada 2005
                         {$$ = malloc(SAFETY+strlen($1)+strlen($2.id)+28);
                          strcpy($$, "subprogram_declaration(");
                          strcat($$, $1);
                          strcat($$, ", ");
                          strcat($$, $2.id);
                          strcat($$, "))");     //one ')' to clause the subprogram specification)
                          free($1);
                          free($2.id);
                          free($2.xref_subprogram_name);
                         }
                       | overriding_indicator_opt generic_subp_inst ';'                                     //changed in Ada 2005
                         {$$ = malloc(SAFETY+strlen($1)+strlen($2)+37);
                          strcpy($$, "generic_subprogram_instantiation(");
                          strcat($$, $1);
                          strcat($$, ", ");
                          strcat($$, $2);
                          strcat($$, ")");
                          free($1);
                          free($2);
                         }
                       | overriding_indicator_opt subprogram_specification_is_push ABSTRACT ';'             //changed in Ada 2005
                         {$$ = malloc(SAFETY+strlen($1)+strlen($2.id)+38);
                          strcpy($$, "abstract_subprogram_declaration(");
                          strcat($$, $1);
                          strcat($$, ", ");
                          strcat($$, $2.id);
                          strcat($$, "))");     //one ')' to clause the subprogram specification)
                          free($1);
                          free($2.id);
                          free($2.xref_subprogram_name);
                         }
                       | overriding_indicator_opt subprogram_specification_is_push NuLL ';'                 //new in Ada 2005 rule 6.7
                         {$$ = malloc(SAFETY+strlen($1)+strlen($2.id)+29);
                          strcpy($$, "null_prodecure_declaration(");
                          strcat($$, $1);
                          strcat($$, ", ");
                          strcat($$, $2.id);
                          strcat($$, "))");     //one ')' to clause the subprogram specification)
                          free($1);
                          free($2.id);
                          free($2.xref_subprogram_name);
                         }
                       ;

//For Ada Rule 6.1
subprogram_specification : PROCEDURE compound_name formal_part_opt
                                {$$.xref_subprogram_name = malloc(strlen($2) + 1);
                                 strcpy($$.xref_subprogram_name, $2);
                                 $$.id = malloc(SAFETY+strlen($2)+strlen($3)+100);
                                 strcpy($$.id, "\nprocedure_body(\n  ");
                                 strcat($$.id, $2);
                                 strcat($$.id, ", no_return");
                                 strcat($$.id, ",\n  parameters([");
                                 strcat($$.id, $3);
                                 strcat($$.id, "])\n");
                                 free($2);
                                 free($3);
                                 //printf("hi from %s\n", $2);
                                }
                         | FUNCTION designator formal_part_opt RETURN access_or_subtype_disc    //changed in Ada 2005
                                {$$.xref_subprogram_name = malloc(strlen($2) + 1);
                                 strcpy($$.xref_subprogram_name, $2);
                                 $$.id = malloc(SAFETY+strlen($2)+strlen($2)+strlen($3)+strlen($5)+100);
                                 strcpy($$.id, "\nfunction_body(\n  ");
                                 strcat($$.id, $2);
                                 strcat($$.id, ", Return_");
                                 strcat($$.id, $2);
                                 strcat($$.id, ", ");
                                 strcat($$.id, $5);
                                 strcat($$.id, ",\n  parameters([");
                                 strcat($$.id, $3);
                                 strcat($$.id, "])\n");
                                 free($2);
                                 free($3);
                                 free($5);
                                }
                         ;

designator : compound_name      {$$ = $1;}
           | string_literal
             {$$ = handle_identifiers($1, 0);//0 because although it is a string, it should always be crossreferenced by gnatxref (since it is designator), so it is really an error if it is not found
             }
           ;

//For Ada Rule 6.1
formal_part_opt : /* empty */         {$$ = malloc(1); strcpy($$, "");}
                | formal_part         {$$ = $1;}
                ;

formal_part : '(' parameter_specification_list ')'
                {$$ = malloc((SAFETY+strlen($2)+1) );
                 strcpy($$, $2);
                 free($2);
                }
            ;

parameter_specification_list : parameter_specification                  {$$ = $1;}
        | parameter_specification_list ';' parameter_specification      {$$ = malloc((SAFETY+strlen($1)+strlen($3)+3) );
                                                                         strcpy($$, $1);
                                                                         strcat($$, ", \n");
                                                                         strcat($$, $3);
                                                                         free($1);
                                                                         free($3);
                                                                        }
        ;

parameter_specification : identifier_list ':' mode access_or_subtype_disc init_opt     //changed in Ada 2005
        {$$ = malloc((SAFETY+strlen($1)+strlen($3)+strlen($4)+strlen($5)+16) );
         strcpy($$, "param([");
         strcat($$, $1);
         strcat($$, "], ");
         strcat($$, $3);
         strcat($$, ", ");
         strcat($$, $4);
         strcat($$, ", ");
         strcat($$, $5);
         strcat($$, ")");
         free($1);
         free($3);
         free($4);
         free($5);
        }
        ;

mode : /* empty */      {$$ = malloc(3 ); strcpy($$, "in");}    //for functions parameters
        | IN            {$$ = malloc(3 ); strcpy($$, "in");}
        | OUT           {$$ = malloc(4 ); strcpy($$, "out");}
        | IN OUT        {$$ = malloc(7 ); strcpy($$, "in_out");}
       // | ACCESS        {$$ = malloc(7 ); strcpy($$, "access");}  //commented out in Ada 2005
        ;

subprogram_specification_is_push : subprogram_specification IS {$$ = $1;}
                                 ;

subprogram_body : overriding_indicator_opt subprogram_specification_is_push declarative_part {strcpy(current_subprogram_name, $2.xref_subprogram_name);} block_body END id_opt ';'
                  {$$ = malloc(SAFETY+strlen($1)+strlen($2.id)+strlen($3)+strlen($5)+72);
                   strcpy($$, "subprogram_body(");
                   strcat($$, $1);
                   strcat($$, ", ");
                   strcat($$, $2.id);
                   strcat($$, ",\n  local_decl([");
                   strcat($$, $3);
                   strcat($$, "\n            ]),");
                   strcat($$, $5);
                   strcat($$, "\n             ))\n");    //one ')' to clause the subprogram specification)
                   free($1);
                   free($2.id);
                   free($2.xref_subprogram_name);
                   free($3);
                   free($5);
                  }
                ;

/* Chapter 7 */
/* follows Ada rather than SPARK */

declarative_part : /* empty */          {$$ = malloc((SAFETY+23) ); strcpy($$, "empty_declarative_part");}
                 | decl_item_or_body_sl {$$ = $1;}
                 ;

decl_item_or_body_sl : decl_item_or_body        {$$ = $1;}
                     | decl_item_or_body_sl decl_item_or_body
                       {$$ = malloc((SAFETY+strlen($1)+strlen($2)+5) );
                        strcpy($$, $1);
                        strcat($$, ",\n ");
                        strcat($$, $2);
                        free($1);
                        free($2);
                       }
                     ;

decl_item_or_body : body        {$$ = $1;}
                  | decl_item   {$$ = $1;}
                  ;

decl_item_s : /* empty */   {$$ = malloc(1); strcpy($$, "");}
            | decl_item_s1  {$$ = $1;}
            ;

decl_item_s1 : decl_item    {$$ = $1;}
             | decl_item_s1 decl_item   {$$ = malloc((SAFETY+strlen($1)+strlen($2)+5) );
                                         strcpy($$, $1);
                                         strcat($$, ",\n");
                                         strcat($$, $2);
                                         free($1);
                                         free($2);
                                        }
             ;

decl_item : decl                        {$$ = $1;}
          | use_clause                  {$$ = malloc(11 ); strcpy($$, "use_clause");}
          | aspect_clause               {$$ = $1;}
          | pragma                      {$$ = $1;}
          ;

decl : object_declaration          {$$ = $1;}
     | number_declaration          {$$ = $1;}
     | type_declaration            {$$ = $1;}
     | subtype_declaration         {$$ = $1;}
     | subprogram_declaration      {$$ = $1;}
     | package_declaration         {$$ = $1;}
     | task_declaration            {$$ = $1;}
     | protected_declaration       {$$ = $1;}
     | exception_declaration       {$$ = $1;}
     | rename_declaration          {$$ = $1;}
     | generic_declaration         {$$ = $1;}
     | body_stub
       {char *name;
        char *path = NULL;
        char *filename = NULL;
        int copy_length;
        FILE *tmp_stream;
        $$ = malloc(strlen($1) + 17);
        strcpy($$, "body_stub(");
        strcat($$, $1);         //the full xref name of the body_stub e.g. Speed_42
        strcat($$, "_stub");
        strcat($$, ")");
        name = malloc(strlen(current_unit->name)+strlen($1)+2);
        strcpy(name, current_unit->name);
        strcat(name, "-");
        //transforms Speed_32 into 'speed' see electronic diary  08/23/09
        copy_length = strlen($1) - strlen(strrchr ($1, '_'));
        strncpy(tmp_s, $1, copy_length); //strrchr : last occurence of character
        tmp_s[copy_length] ='\0';       //padding for strncpy
        tmp_s[0] += ('a' - 'A') ;   //put first character in lower case
        strcat(name, tmp_s);
        find_filename_path(name, ".adb", &filename, &path);         //path will be NULL because it is a subunit
        if (filename != NULL) {      //to see if it was mentionned in foo.xref
          strcpy(tmp_s, cwd);   //try the current working directory first
          strcat(tmp_s, "\\");
          strcat(tmp_s, filename);
          strcat(tmp_s, ".adb");
          if (!(tmp_stream = fopen(tmp_s, "r"))) {
            strcpy(tmp_s, current_unit->path);   //try the current working directory first 07/12/10
            strcat(tmp_s, "\\");
            strcat(tmp_s, filename);
            strcat(tmp_s, ".adb");
            if (!(tmp_stream = fopen(tmp_s, "r"))) {
              fprintf(stdout, "Mika ERROR: subunit could not be found : %s%s\n", name, ".adb");
              fflush(stdout);
              my_exit(9898);
            }
            if (debugMode)
              fprintf(stdout, "Mika DEBUG found subunit %s as a file name %s on path %s\n", name, filename, current_unit->path);
            fclose(tmp_stream); //subunit was found
            add_head(name, filename, ".adb", current_unit->path);     //use the same path as the current unit because it is a subunit
          }
          else {
            fclose(tmp_stream);   //subunit was found
            if (debugMode)
              fprintf(stdout, "Mika DEBUG found subunit %s as a file name %s on path %s\n", name, filename, cwd);
            add_head(name, filename, ".adb", cwd);     //use the cwd
          }
        }
        else {
          if (debugMode) {
            fprintf(stdout, "Not added to queue name%s\n", name); //not added because it is not referenced in foo.ref
            fflush(stdout);
          }
        }
       }
     ;

body : subprogram_body          {$$ = $1;}
     | package_body             {$$ = $1;}
     | task_body                {$$ = malloc(10); strcpy($$, "task_body");}
     | protected_body           {$$ = malloc(15); strcpy($$, "protected_body");}
     ;

package_declaration : package_specification ';' {$$ = $1;}
                    | generic_pkg_inst ';'      {$$ = $1;}
                    ;

/* as in Ada */
package_specification : PACKAGE compound_name IS
                        {if (!is_standard || debugMode) {
                            if (debugMode) {
                                fprintf(stdout, "Parsing package specification %s\n", $2);
                            }
                            else {
                                fprintf(stdout, ".");
                                //printout_shortened($2);  //we do not want to print the unique identifier
                                //fprintf(stdout, "\n");
                            }
                            fflush(stdout);
                         }
                        }
                         decl_item_s private_part_opt END c_id_opt
                        {$$ = malloc((strlen($2)+strlen($5)+strlen($6)+73) );
                         strcpy($$, "\npackage_specification(\n ");
                         strcat($$, $2);
                         strcat($$, ",\n local_decl([");
                         strcat($$, $5);
                         strcat($$, "])\n");
                         strcat($$, ", ");
                         strcat($$, $6);
                         strcat($$, "\n                     )\n");
                         free($2);
                         free($5);
                         free($6);
                         }
                      ;

private_part_opt : /* empty */          {$$ = malloc(11 ); strcpy($$, "no_private");}
                 | PRIVATE decl_item_s
                   {$$ = malloc((strlen($2)+12) );
                    strcpy($$, "private([");
                    strcat($$, $2);
                    strcat($$, "])");
                    free($2);
                   }
                 ;

c_id_opt : /* empty */
         | compound_name        {free($1);}
         ;

package_body : PACKAGE BODY compound_name IS
               {if (debugMode) fprintf(stdout, "Parsing package body %s\n", $3);
                else fprintf(stdout, ".");
                fflush(stdout);
               }
               declarative_part body_opt END c_id_opt ';'
               {strcpy(current_package_name, $3);
                $$ = malloc((SAFETY+strlen($3)+strlen($6)+strlen($7)+83) );
                strcpy($$, "\npackage_body(\n  ");
                strcat($$, $3);
                strcat($$, ", local_decl([");
                strcat($$, $6);
                strcat($$, "]),\n");
                strcat($$, $7);
                strcat($$, ")\n");
                free($3);
                free($6);
                free($7);
               }
             ;

body_opt : /* empty */  {$$ = malloc(11 ); strcpy($$, "empty_body");}
         | block_body   {$$ = $1;}
         ;

limited_opt : /* empty */       {$$ = malloc(12 ); strcpy($$, "not_limited");}
            | LIMITED           {$$ = malloc(8 ); strcpy($$, "limited");}
            ;

//For Ada rule 8.5.1 and 8.5.2
//!!!!note made 09/11/04
//!!!!for parsing reasons the rule for object renaming and exception renaming are more flexible below than what they are in the ARM
//!!!! for object renaming the ARM rule is object_renaming_declaration::= defining_identifier : subtype_mark renames name; (where subtype_mark is a name)
//!!!! for exception renaming the ARM rule is exception_renaming_declaration::= defining_identifier : exception renames name;
rename_declaration : identifier_list ':' object_qualifier_opt object_subtype_definition renames ';' //for Ada rule 8.5.1   //changed in Ada 2005
                     {$$ = malloc((SAFETY+strlen($1)+strlen($4)+strlen($5)+16) );
                      strcpy($$, " rename(");
                      strcat($$, $1);       //always a single defining_identifier, see !!!! above
                      strcat($$, ", ");
                                            //the object_qualifier_opt is always ignored as it will always be 'not_qualified', see !!!! above
                      strcat($$, $4);       //always a subtype_indication with no constraint or an access_type, see !!!! above
                      strcat($$, ", ");
                      strcat($$, $5);
                      strcat($$, ")");
                      free($1);
                      free($3);
                      free($4);
                      free($5);
                     }
                   | identifier_list ':' EXCEPTION renames ';' //for Ada rule 8.5.2
                     {$$ = malloc((SAFETY+strlen($1)+strlen($4)+22) );
                      strcpy($$, " rename_exception(");
                      strcat($$, $1);       //always a single defining_identifier, see !!!! above
                      strcat($$, ", ");
                      strcat($$, $4);
                      strcat($$, ")");
                      free($1);
                      free($4);
                     }
                   | rename_unit
                     {$$ = $1;}
                   ;

//For Ada rule 8.5.3
rename_unit : PACKAGE compound_name renames ';'
              {$$ = malloc((SAFETY+strlen($2)+strlen($3)+19) );
               strcpy($$, " package_rename(");
               strcat($$, $2);
               strcat($$, ", ");
               strcat($$, $3);
               strcat($$, ")");
               free($2);
               free($3);
              }
            | overriding_indicator_opt subprogram_specification {strcpy(not_cross_referenced_operator, "");} renames ';'
              {if (strcmp(not_cross_referenced_operator, "")) {//$2.id is not a user defined operator but is predefined, so instead of printing $2.id on reference we should print "op" not indexed()
                 add_operator($2.xref_subprogram_name); //will be checked in handle_operator_calls
                 if (debugMode) fprintf(stdout, "ADDED operator: %s\n", $2.xref_subprogram_name);
                 $$ = malloc(8);
                 strcpy($$, "nothing"); //because, in this case, the subprogram_rename() will never be needed
               }
               else {
                 $$ = malloc(SAFETY+strlen($1)+strlen($2.id)+strlen($4)+26);
                 strcpy($$, " subprogram_rename(");
                 strcat($$, $1);
                 strcat($$, ", ");
                 strcat($$, $2.id);
                 strcat($$, "),  ");      //one ')' to clause the subprogram specification
                 strcat($$, $4);
                 strcat($$, ")");
               }
               free($1);
               free($2.id);
               free($2.xref_subprogram_name);
               free($4);
              }
            | generic_formal_part PACKAGE compound_name renames ';' //generic_formal_part can only be 'generic' here but we keep generic_formal_part for parsing reasons
              {$$ = malloc(SAFETY+strlen($3)+strlen($4)+32);
               strcpy($$, " generic_package_rename(");
               strcat($$, $3);
               strcat($$, ", ");
               strcat($$, $4);
               strcat($$, ")");
               free($1);
               free($3);
               free($4);
              }
            | generic_formal_part FUNCTION designator renames ';' //generic_formal_part can only be 'generic' here but we keep generic_formal_part for parsing reasons
              {$$ = malloc(SAFETY+strlen($3)+strlen($4)+38);
               strcpy($$, " generic_subprogram_rename(");
               strcat($$, $3);
               strcat($$, ", ");
               strcat($$, $4);
               strcat($$, ")");
               free($1);
               free($3);
               free($4);
              }
             | generic_formal_part PROCEDURE designator renames ';' //generic_formal_part can only be 'generic' here but we keep generic_formal_part for parsing reasons
              {$$ = malloc(SAFETY+strlen($1)+strlen($3)+strlen($4)+38);
               strcpy($$, " generic_subprogram_rename(");
               strcat($$, $3);
               strcat($$, ", ");
               strcat($$, $4);
               strcat($$, ")");
               free($1);
               free($3);
               free($4);
              }
            ;

//04-03-08
//was 'RENAMES name' changed to 'RENAMES designator' but then the syntax is wrong because we cannot parse Real."+"
//reverting to 'RENAMES name' implies that if name contains an operator it is not properly identified as such but is left as string([43]) for "+"
// unless it is properly cross referenced by gnatxref (e.g. ok for RR code)
// instead of String_43_371, but then again gnatxref does not seem to cross reference it properly anyway.
renames : RENAMES name_or_character_literal {$$ = $2;}
        ;

//added 13/01/09
name_or_character_literal : name                {$$ = $1;}
                          | character_literal   {$$ = handle_identifiers($1, 0);}
                          ;

//TASKING IS IGNORED, TOP LEVEL RULES ONLY ARE PARSED : IGNORED DURING EXECUTION
//For Ada Rule 9.1 : ignored (tasking)
task_declaration : task_spec ';'
                   {$$ = malloc((SAFETY+17) );
                    strcpy($$, "task_declaration");
                   }
                 ;

task_spec : TASK direct_name task_def
          | TASK TYPE direct_name discriminant_part_opt task_def
          ;

task_def : /* empty */
         | IS interface_list_opt entry_decl_s rep_spec_s task_private_opt END id_opt
           {free($2);}
         ;


interface_list_opt : /* empty */
                     {$$ = malloc(3);
                      strcpy($$, "[]");
                     }
                   | NEW interface_list_item_sl WITH
                     {$$ = malloc(strlen($2)+3);
                      strcpy($$, "[");
                      strcat($$, $2);
                      strcat($$, "]");
                      free($2);
                     }
                   ;

task_private_opt : /* empty */
                 | PRIVATE entry_decl_s rep_spec_s
                 ;

task_body : TASK BODY direct_name IS declarative_part block_body END id_opt ';'
          ;

//For Ada Rule 9.4 : ignored (tasking)
protected_declaration : protected_spec ';'
                        {$$ = malloc((SAFETY+22) );
                         strcpy($$, "protected_declaration");
                        }
                      ;

//For Ada Rule 9.4
protected_spec : PROTECTED identifier protected_def
               | PROTECTED TYPE direct_name discriminant_part_opt protected_def
               ;

protected_def : IS interface_list_opt protected_op_decl_s protected_private_opt END id_opt
                {free($2);}
              ;

protected_private_opt : /* empty */
                 | PRIVATE protected_elem_decl_s
                 ;

protected_op_decl_s : /* empty */
               | protected_op_decl_s protected_op_decl
               ;

protected_op_decl : entry_decl
                  | subprogram_specification ';'        {;}
                  | aspect_clause       {;}
                  | pragma {free($1);}
                  ;

protected_elem_decl_s : /* empty */
                 | protected_elem_decl_s protected_elem_decl
                 ;

protected_elem_decl : protected_op_decl
                    | component_declaration     {;}
                    ;

protected_body : PROTECTED BODY direct_name IS protected_op_item_s END id_opt ';'
               ;

protected_op_item_s : pragma_s  {free($1);}
                    | protected_op_item_s protected_op_item pragma_s
                      {free($3);
                      }
                    ;

protected_op_item : entry_body
                  | subprogram_body     {free($1);}
                  | overriding_indicator_opt subprogram_specification ';'        {free($1); free($2.id); free($2.xref_subprogram_name);}
                  | aspect_clause       {free($1);}
                  ;

entry_decl_s : pragma_s {free($1);}
             | entry_decl_s entry_decl pragma_s
               {free($3);
               }
             ;

entry_decl : overriding_indicator_opt ENTRY identifier formal_part_opt ';'
             {free($1);}
           | overriding_indicator_opt ENTRY identifier '(' discrete_range ')' formal_part_opt ';'
             {free($1);}
           ;

entry_body : ENTRY identifier formal_part_opt WHEN decision entry_body_part
           | ENTRY identifier '(' iter_part discrete_range ')' formal_part_opt WHEN decision entry_body_part
           ;

entry_body_part : ';'
        | IS declarative_part block_body END id_opt ';'
        ;

rep_spec_s :
           | rep_spec_s aspect_clause pragma_s {free($3);}
           ;

entry_call : procedure_call_statement   {free($1);}
           ;

accept_statement : accept_hdr ';'
                   {$$ = malloc((17) );
                    strcpy($$, "accept_statement");
                   }
                 | accept_hdr DO handled_statement_s END id_opt ';'
                   {$$ = malloc((20) );
                    strcpy($$, "accept_do_statement");
                   }
                 ;

accept_hdr : ACCEPT entry_name formal_part_opt
           ;

entry_name : direct_name        {free($1);}
           | entry_name '(' expression ')'
           ;

delay_statement : DELAY expression ';'
                  {$$ = malloc((16) );
                   strcpy($$, "delay_statement");
                  }
                | DELAY UNTIL expression ';'
                  {$$ = malloc((22) );
                   strcpy($$, "delay_until_statement");
                  }
                ;

select_statement : select_wait
                   {$$ = malloc((17) );
                   strcpy($$, "select_statement");
                   }
                 | async_select
                   {$$ = malloc((17) );
                   strcpy($$, "select_statement");
                   }
                 | timed_entry_call
                   {$$ = malloc((17) );
                   strcpy($$, "select_statement");
                   }
                 | cond_entry_call
                   {$$ = malloc((17) );
                   strcpy($$, "select_statement");
                   }
                 ;

select_wait : SELECT guarded_select_alt or_select else_opt END SELECT ';'
            ;

guarded_select_alt : select_alt
                   | WHEN decision RIGHT_SHAFT select_alt
                   ;

or_select : /* empty */
          | or_select OR guarded_select_alt
          ;

select_alt : accept_statement stmts_opt {;}
           | delay_statement stmts_opt  {;}
           | TERMINATE ';'
           ;

delay_or_entry_alt : delay_statement stmts_opt {free($1);}
                   | entry_call stmts_opt
                   ;

async_select : SELECT delay_or_entry_alt
               THEN ABORT sequence_of_statements
               END SELECT ';'
             ;

timed_entry_call : SELECT entry_call stmts_opt OR delay_statement stmts_opt END SELECT ';'
                 ;

cond_entry_call : SELECT entry_call stmts_opt ELSE sequence_of_statements END SELECT ';'
                ;

stmts_opt : /* empty */
          | sequence_of_statements      {free($1);}
          ;

abort_statement : ABORT name_list ';'
                  {$$ = malloc((16) );
                   strcpy($$, "abort_statement");
                  }
                ;

//ayacc 3 warnings due to 'goal_symbol' top rule: does not cause a problem (always parsed correctly)
compilation : /* empty */ {fprintf(parsed, " nothing\n");}
            | compilation {fprintf(parsed, ",");} compilation_unit
            | pragma pragma_s
              {fprintf(parsed, "%s", $1);
               if (strcmp($2, "")) fprintf(parsed, ", %s", $2);
               free($1);
               free($2);
              }
            ;

compilation_unit : context_specification private_opt unit pragma_s
                   {if (strcmp($4, "")) fprintf(parsed, ", %s", $4);
                    free($4);
                    total_line_no = total_line_no + old_lineno;
                    if (debugMode) {
                      fprintf(stdout, "DEBUG: %i new lines with %i lines in total analised\n", old_lineno, total_line_no);
                      fflush(stdout);
                    }
                   }
                 | private_opt unit pragma_s
                   {if (strcmp($3, "")) fprintf(parsed, ",%s", $3);
                    free($3);
                    total_line_no = total_line_no + old_lineno;
                    if (debugMode) {
                      fprintf(stdout, "DEBUG: %i new lines with %i lines in total analised\n", old_lineno, total_line_no);
                      fflush(stdout);
                    }
                   }
                 ;

private_opt : /* empty */
            | PRIVATE   //is this ok ? private child units??
            ;

context_specification : with_clause use_clause_opt
                      | context_specification with_clause use_clause_opt
                      | context_specification pragma {free($2);}
                      ;
// with clause are not used: we rely on the bind information produced by gnat to analysed withed files
// (and then again only if some identifiers have been cross referenced to avoid having to deal with
// pre-defined packages that are not actually used)
// BUT (16/01/08) some units are not mentionned in foo.bind so we check ...
with_clause : WITH with_list ';'
            | PRIVATE WITH with_list ';'
            | LIMITED WITH with_list ';'
            | LIMITED PRIVATE WITH with_list ';'
            ;

with_list : file_name
          | with_list ',' file_name
          ;

file_name : compound_name
            {free($1);}
          ;

use_clause_opt : /* empty */
               | use_clause_opt use_clause
               ;

unit : package_declaration      {fprintf(parsed, "%s", $1);
                                 fflush(parsed);
                                 free($1);
                                }
     | package_body             {fprintf(parsed, "%s", $1);
                                 fflush(parsed);
                                 free($1);
                                }
     | subprogram_declaration   {fprintf(parsed, "%s", $1);
                                 fflush(parsed);
                                 free($1);
                                }
     | subprogram_body          {fprintf(parsed, "%s", $1);
                                 fflush(parsed);
                                 free($1);
                                }
     | subunit                  {}                      //already outputed
     | generic_declaration      {fprintf(parsed, "%s", $1);
                                 fflush(parsed);
                                 free($1);
                                }
     | rename_unit              {fprintf(parsed, "%s", $1);
                                 fflush(parsed);
                                 free($1);
                                }
     ;

//non generic private type
private_type_definition : limited_opt PRIVATE
                           {$$ = $1;}
                         ;

overriding_indicator_opt : /* empty */
                           {$$ = malloc(8);
                            strcpy($$, "nothing");
                           }
                         | overriding_indicator
                           {$$ = $1;}
                         ;

overriding_indicator : NOT OVERRIDING
                       {$$ = malloc(15);
                        strcpy($$, "not_overriding");
                       }
                     | OVERRIDING
                       {$$ = malloc(11);
                        strcpy($$, "overriding");
                       }
                     ;


use_clause : USE name_list ';'
           | USE TYPE name_list ';'
           ;

name_list : name                {free($1);}
          | name_list ',' name  {free($3);}
          ;

//we return the fully xrefed name of the stub
body_stub : TASK BODY direct_name IS SEPARATE ';'           {$$ = $3;}
          | PACKAGE BODY compound_name  IS SEPARATE ';'     {$$ = $3;}
          | overriding_indicator_opt subprogram_specification IS SEPARATE ';'           {$$ = $2.xref_subprogram_name; free($1); free($2.id);}
          | PROTECTED BODY direct_name IS SEPARATE ';'      {$$ = $3;}
          ;

subunit : SEPARATE '(' compound_name ')'
          {if (debugMode) {
             fprintf(stdout, "Parsing subunit %s%s\n", current_unit->filename, current_unit->suffix);
             fflush(stdout);
           }
           else fprintf(stdout, ".");   //for progress feedback
          }
          proper_body2
        ;

//follows a SEPARATE subunit
proper_body2 : subprogram_body          {fprintf(parsed, "match_body(%s_stub, %s)", current_subprogram_name, $1);
                                         fflush(parsed);
                                         free($1);
                                        }
             | package_body             {fprintf(parsed, "match_body(%s_stub, %s)", current_package_name, $1);
                                         fflush(parsed);
                                         free($1);
                                        }
             | task_body
             | protected_body
             ;

//Exceptions
//%for Ada rule 11.1
exception_declaration : identifier_list ':' EXCEPTION ';'
                        {$$ = malloc((SAFETY+strlen($1)+28) );
                         strcpy($$, "exception_declaration([");
                         strcat($$, $1);
                         strcat($$, "])");
                         free($1);
                        }
                      ;

exception_handler_part : EXCEPTION exception_handler
                         {$$ = $2;}
                       | exception_handler_part exception_handler
                         {$$ = malloc((SAFETY+strlen($1)+strlen($2)+3) );
                          strcpy($$, $1);
                          strcat($$, ", ");
                          strcat($$, $2);
                          free($1);
                          free($2);
                         }
                       ;

exception_handler : WHEN exception_choice_s RIGHT_SHAFT sequence_of_statements
                    {$$ = malloc((SAFETY+strlen($2)+strlen($4)+33) );
                         strcpy($$, "exception_handler([");
                         strcat($$, $2);
                         strcat($$, "], stmts([");
                         strcat($$, $4);
                         strcat($$, "]))");
                         free($2);
                         free($4);
                    }
                  | WHEN identifier_rule ':' exception_choice_s RIGHT_SHAFT sequence_of_statements
                    {$$ = malloc((SAFETY+strlen($2)+strlen($4)+strlen($6)+34) );
                     strcpy($$, "exception_handler(");
                     strcat($$, $2);
                     strcat($$, ", [");
                         strcat($$, $4);
                         strcat($$, "], stmts([");
                         strcat($$, $6);
                         strcat($$, "]))");
                         free($2);
                         free($4);
                     free($6);
                    }
                  ;

exception_choice_s : exception_choice   {$$ = $1;}
                   | exception_choice_s '|' exception_choice
                     {$$ = malloc((SAFETY+strlen($1)+strlen($3)+2) );
                      strcpy($$, $1);
                      strcat($$, ", ");
                      strcat($$, $3);
                      free($1);
                      free($3);
                     }
                   ;

exception_choice : name         {$$ = $1;}
                 | OTHERS
                   {$$ = malloc((SAFETY+7) );
                    strcpy($$, "others");
                   }
                 ;

// for Ada rule 11.3
raise_statement : RAISE ';'
                  {$$ = malloc(36);
                   strcpy($$, "raise_statement(no_name, no_string)");
                  }
                | RAISE name ';'
                  {$$ = malloc(SAFETY+strlen($2)+29);
                   strcpy($$, "raise_statement(");
                   strcat($$, $2);
                   strcat($$, ", no_string)");
                   free($2);
                  }
                | RAISE name WITH <true_line_column> {$$.line = yylineno; $$.column = column+1;} expression_2 ';'
                  {char *expression;
                   build_expression($5, &expression, current_unit, $4.line, $4.column);
                   $$ = malloc(SAFETY+strlen($2)+strlen(expression)+20);
                   strcpy($$, "raise_statement(");
                   strcat($$, $2);
                   strcat($$, ", ");
                   strcat($$, expression);
                   strcat($$, ")");
                   free($2);
                   free($5.id);
                  }
                ;

requeue_statement : REQUEUE name ';'
                    {$$ = malloc((SAFETY+18) );
                     strcpy($$, "requeue_statement");
                    }
                  | REQUEUE name WITH ABORT ';'
                    {$$ = malloc((SAFETY+24) );
                     strcpy($$, "requeue_abort_statement");
                    }
                  ;

generic_declaration : generic_formal_part subprogram_specification ';'
                      {$$ = malloc((SAFETY+strlen($1)+strlen($2.id)+40) );
                       strcpy($$, "generic_subprogram_specification([");
                       strcat($$, $1),
                       strcat($$, "], ");
                       strcat($$, $2.id);
                       strcat($$, "))");        //one ')' to clause the subprogram specification
                       free($1);
                       free($2.id);
                       free($2.xref_subprogram_name);
                      }
                    | generic_formal_part package_specification ';'
                      {$$ = malloc((SAFETY+strlen($1)+strlen($2)+36) );
                       strcpy($$, "generic_package_specification([");
                       strcat($$, $1),
                       strcat($$, "], ");
                       strcat($$, $2);
                       strcat($$, ")");
                       free($1);
                       free($2);
                      }
                    ;

generic_formal_part : GENERIC
                      {$$ = malloc(8);
                       strcpy($$, "generic");
                      }
                    | generic_formal_part generic_formal
                      {$$ = malloc(SAFETY+strlen($1)+strlen($2)+3);
                       strcpy($$, $1);
                       strcat($$, ", ");
                       strcat($$, $2);
                       free($1);
                       free($2);
                      }
                    ;

generic_formal : pragma {$$ = $1;}      //added because of s-htable.ads (see ADA RM)
               | parameter_specification ';' {$$ = $1;}     //changed in Ada 2005               (stands for formal_object_declaration)
               | TYPE direct_name generic_discriminant_part_opt IS generic_type_definition ';'
                 {$$ = malloc(strlen($2)+strlen($3)+strlen($5)+11);
                  strcpy($$, "type(");
                  strcat($$, $2);
                  strcat($$, ", ");
                  strcat($$, $3);
                  strcat($$, ", ");
                  strcat($$, $5);
                  strcat($$, ")");
                  free($2);
                  free($3);
                  free($5);
                 }
               | WITH subprogram_specification subp_default ';'
                 {$$ = malloc(SAFETY+strlen($2.id)+strlen($3)+21);
                  strcpy($$, "with_subprogram(");
                  strcat($$, $2.id);
                  strcat($$, ")");     //one ')' to clause the subprogram specification)
                  strcat($$, ", ");
                  strcat($$, $3);
                  strcat($$, ")");
                  free($2.id);
                  free($2.xref_subprogram_name);
                  free($3);
                 }
               | WITH PACKAGE direct_name IS NEW name '(' BOX ')' ';'
                 {$$ = malloc((SAFETY+strlen($3)+strlen($6)+22) );
                  strcpy($$, "with_package(");
                  strcat($$, $3);
                  strcat($$, ", ");
                  strcat($$, $6);
                  strcat($$, ", box)");
                  free($3);
                  free($6);
                 }
               | WITH PACKAGE direct_name IS generic_inst ';'       //generic_inst is NEW name and eats everything (I think)
                 {$$ = malloc((SAFETY+strlen($3)+strlen($5)+17) );
                  strcpy($$, "with_package(");
                  strcat($$, $3);
                  strcat($$, ", ");
                  strcat($$, $5);
                  strcat($$, ")");
                  free($3);
                  free($5);
                 }
               | use_clause
                 {$$ = malloc(11 );
                  strcpy($$, "use_clause");
                 }
               ;

generic_discriminant_part_opt : /* empty */             {$$ = malloc((SAFETY+16) ); strcpy($$, "no_discriminant");}
                              | discriminant_part       {$$ = $1;}
                              | '(' BOX ')'             {$$ = malloc(4); strcpy($$, "box");}
                              ;

subp_default : /* empty */      {$$ = malloc(11); strcpy($$, "empty_subp");}
             | IS name          {$$ = $2;}
             | IS BOX           {$$ = malloc(4); strcpy($$, "box");}
             | IS NuLL          {$$ = malloc(5); strcpy($$, "null");}
             | IS ABSTRACT name
               {$$ = malloc(SAFETY+strlen($3)+11);
                strcpy($$, "abstract, ");
                strcat($$, $3);
                free($3);
               }
             | IS ABSTRACT BOX
               {$$ = malloc(SAFETY+14);
                strcpy($$, "abstract, box");
               }
             | IS ABSTRACT NuLL
               {$$ = malloc(SAFETY+15);
                strcpy($$, "abstract, null");
               }
             ;

generic_type_definition : '(' BOX ')'           {$$ = malloc((4) ); strcpy($$, "box");}         //rule 12.5.2 formal_discrete_type_definition
                        | RANGE BOX             {$$ = malloc((10) ); strcpy($$, "range_box");}  //rule 12.5.2 formal_signed_integer_type_definition
                        | MOD BOX               {$$ = malloc(8); strcpy($$, "mod_box");}        //rule 12.5.2 formal_modular_type_definition
                        | DELTA BOX             {$$ = malloc((10) ); strcpy($$, "delta_box");}  //rule 12.5.2 formal_ordinary_fixed_point_definition
                        | DELTA BOX DIGITS BOX  {$$ = malloc((17) ); strcpy($$, "delta_digits_box");} //rule 12.5.2 formal_decimal_fixed_point_definition
                        | DIGITS BOX            {$$ = malloc((11) ); strcpy($$, "digits_box");} //rule 12.5.2 formal_floating_point_definition
                        | array_type_definition         {$$ = $1;}                              //rule 12.5.2 formal_array_type_definition
                        | access_type_definition        {$$ = $1;}                              //rule 12.5.2 formal_access_type_definition
                        | ABSTRACT TAGGED private_type_definition      //rule 12.5.1 formal_private_type_definition
                          {$$ = malloc(SAFETY+strlen($3)+27);
                           strcpy($$, "private, abstract_tagged, ");
                           strcat($$, $3);
                           free($3);
                          }
                        | TAGGED private_type_definition               //rule 12.5.1 formal_private_type_definition
                          {$$ = malloc(SAFETY+strlen($2)+27);
                           strcpy($$, "private, tagged, ");
                           strcat($$, $2);
                           free($2);
                          }
                        | private_type_definition                      //rule 12.5.1 formal_private_type_definition
                          {$$ = malloc(SAFETY+strlen($1)+27);
                           strcpy($$, "private, not_tagged, ");
                           strcat($$, $1);
                           free($1);
                          }
                        | derived_type_definition                       //rule 12.5.1 formal_derived_type_definition
                          {$$ = malloc(SAFETY+strlen($1)+6);
                           strcpy($$, "new(");
                           strcat($$, $1);
                           strcat($$, ")");
                           free($1);
                          }
                        | ABSTRACT derived_type_definition              //rule 12.5.1 formal_derived_type_definition
                          {$$ = malloc(SAFETY+strlen($2)+14);
                           strcpy($$, "abstract_new(");
                           strcat($$, $2);
                           strcat($$, ")");
                           free($2);
                          }
                        | interface_type_definition {$$ = $1;}          //rule 12.5.2 formal_interface_type_definition
                        ;

generic_subp_inst : subprogram_specification IS generic_inst
//21/01/04
// must use subprogram_specification here because replcaing by PROCEDURE compound_name gives rise to a conflict with subprogram_specification
//In the above subprogram_specification is equivalent to PROCEDURE compound_name with an empty formal_part_opt
                    {$$ = malloc(SAFETY+strlen($1.id)+strlen($3)+4);
                     strcpy($$, $1.id);
                     strcat($$, "), "); //one ')' to clause the subprogram specification
                     strcat($$, $3);
                     free($1.id);
                     free($1.xref_subprogram_name);
                     free($3);
                    }
                  | //this is still needed
                    FUNCTION designator IS generic_inst
                    {$$ = malloc(SAFETY+strlen($2)+strlen($4)+3);
                     strcpy($$, $2);
                     strcat($$, ", ");
                     strcat($$, $4);
                     free($2);
                     free($4);
                    }
                  ;

generic_pkg_inst : PACKAGE compound_name IS generic_inst
                   {$$ = malloc((SAFETY+strlen($2)+strlen($4)+34) );
                    strcpy($$, "generic_package_instantiation(");
                    strcat($$, $2);
                    strcat($$, ", ");
                    strcat($$, $4);
                    strcat($$, ")");
                    free($2);
                    free($4);
                   }
                 ;

generic_inst : NEW name {$$ = $2;} //includes everything e.g. 'Ada.Unchecked_Deallocation (Element_Wrapper, Elmt_Ptr);' similar to a procedure_call_statement
             ;

/* Chapter 8 */
//for Ada rule 13.1
aspect_clause : enumeration_or_attribute_definition_clause     {$$ = $1;}
              | record_representation_clause    {$$ = $1;}
              | at_clause                       {$$ = $1;}
              ;

//for Ada rule 13.3 (name is included in expression here)
enumeration_or_attribute_definition_clause : FOR name USE <true_line_column> {$$.line = yylineno; $$.column = column+1;} expression_2 ';'
                              {char *expression;
                               build_expression($5, &expression, current_unit, $4.line, $4.column);
                               $$ = malloc(SAFETY+strlen($2)+strlen(expression)+26);
                               strcpy($$, "representation_clause(");
                               strcat($$, $2);
                               strcat($$, ", ");
                               strcat($$, expression);
                               strcat($$, ")");
                               free($2);
                               free($5.id);
                              }
                            ;

record_representation_clause : FOR name USE RECORD mod_clause_opt component_clause END RECORD ';'
                               {$$ = malloc(SAFETY+strlen($2)+strlen($5)+strlen($6)+37);
                                strcpy($$, "record_representation_clause(");
                                strcat($$, $2);
                                strcat($$, ", ");
                                strcat($$, $5);
                                strcat($$, ", [");
                                strcat($$, $6);
                                strcat($$, "])");
                                free($2);
                                free($5);
                                free($6);
                              }
                             ;

mod_clause_opt : /* empty */
                 {$$ = malloc(10);
                  strcpy($$, "empty_mod");
                 }
               | AT MOD <true_line_column> {$$.line = yylineno; $$.column = column+1;} expression_2 ';'
                 {char *expression;
                  build_expression($4, &expression, current_unit, $3.line, $3.column);
                  $$ = malloc(SAFETY+strlen(expression)+1);
                  strcpy($$, expression);
                  free($4.id);
                 }
               ;

component_clause : /* empty */          {$$ = malloc(1); strcpy($$, "");}
                 | component_clause_sl  {$$ = $1;}
                 ;

component_clause_sl : component_clause_item     {$$ = $1;}
                    | component_clause_sl component_clause_item
                      {$$ = malloc(3+strlen($1)+strlen($2));
                       strcpy($$, $1);
                       strcat($$, ", ");
                       strcat($$, $2);
                       free($1);
                       free($2);
                      }
                    ;

component_clause_item : name AT <true_line_column> {$$.line = yylineno; $$.column = column+1;} expression_2 RANGE range ';'
                        {char *expression;
                         build_expression($4, &expression, current_unit, $3.line, $3.column);
                         $$ = malloc(13+strlen($1)+strlen(expression)+strlen($6));
                         strcpy($$, "clause(");
                         strcat($$, $1);
                         strcat($$, ", ");
                         strcat($$, expression);
                         strcat($$, ", ");
                         strcat($$, $6);
                         strcat($$, ")");
                         free($1);
                         free($4.id);
                         free($6);
                        }
                      ;

at_clause : FOR name USE AT <true_line_column> {$$.line = yylineno; $$.column = column+1;} expression_2 ';'
            {char *expression;
             build_expression($6, &expression, current_unit, $5.line, $5.column);
             $$ = malloc(SAFETY+strlen($2)+strlen(expression)+29);
             strcpy($$, "at_representation_clause(");
             strcat($$, $2);
             strcat($$, ", ");
             strcat($$, expression);
             strcat($$, ")");
             free($2);
             free($6.id);
            }
          ;


%%
//foo.ads or foo.adb must be in the current directory
int main(int argc, char *argv[])    //argc is the total number of strings in the argv array
{
    extern int error_count; //defined in ada.l
    extern int yyparse();
    extern int yylineno;
    int i;
    int time_stamp = 0;                     //time stamp is not required (enabled by -t option)
    char user_gnatmake_call_str[_MAX_PATH*10];
    char user_gnatbind_call_str[_MAX_PATH*10];
    char target_dir[_MAX_PATH];             //directory where all mika files (i.e. excluding .ali) will be created.
    char initial_dir[_MAX_PATH];            //directory of initial file (either of the file under test or of the project file)
    char gnat_dir[_MAX_PATH];               //path to the bin directory of gnat
    char *ada_include_path_env;             //the value of the ADA_INCLUDE_PATH env var
    char *path_env;                         //the value of the PATH env var
    char put_env_ada_include_path[_MAX_PATH*100];       //for setting the ADA_INCLUDE_PATH env var, contains 'ADA_INCLUDE_PATH= ...'
    char put_env_path[_MAX_PATH*100];       //for setting the PATH env var, contains 'PATH= ...'
    char ch_tmp;

    int ourxref_exit_code;
    FILE *tmp_stream;
    char *path_original_unit;

    _getcwd(cwd, 256);
    strcpy(target_dir, cwd);            //overwrite using -w
    strcpy(user_gnatmake_call_str, "");
    strcpy(user_gnatbind_call_str, "");
    strcpy(user_gnatls_call_str, "");
    strcpy(user_gnatproject_call_str, "");
    strcpy(install_dir, "");  //must be provided using -M
    strcpy(initial_dir, cwd);       //overwrite using -o
    strcpy(gnat_dir, "");       //overwrite using -f
//We SHOULD check the gcc version with gcc -v
    fprintf(stdout, "Mika START Parsing\n");
    fflush(stdout);
/*** Start checking line command input ***/
    strcpy(gnatkrExe, "gnatkr");                //default name of gnatkr executable; overwrite using -k
    strcpy(gnatlsExe, "gnat ls");                //default name of gnat ls executable; overwrite using -l
    strcpy(gnatmakeExe, "gnatmake ");            //default name of gnatmake executable; overwrite using -m
    strcpy(gnatbindExe, "gnat bind");                //default name of gnat bind executable; overwrite using -n
        for(i=1; i<argc-1; i++) //processing switches
        {
                if(argv[i][0] == '-')
                {
					switch(argv[i][1])
					{
                    case 'M' :  //specifies the install directory of the parser (i.e. Mika) executable
						if (_access(&argv[i][2], 0) == -1) {    //checks if &argv[i][2] is a valid directory
							fprintf(stdout, "Mika ERROR: indicated install directory (via -M switch) : %s , cannot be accessed\n", &argv[i][2]);
                            fflush(stdout);
                            my_exit(9);
						}
						strcpy(install_dir, &argv[i][2]);
						break;
                    case 'o' :	//specifies the full path to the file under test
                        if (_access(&argv[i][2], 0) == -1) {    //checks if &argv[i][2] is a valid directory
							fprintf(stdout, "Mika ERROR: indicated directory of initial file or of project file (via -o switch) : %s , cannot be accessed\n", &argv[i][2]);
                            fflush(stdout);
                            my_exit(9);
						}
						strcpy(initial_dir, &argv[i][2]);
						break;
                    case 'f' :	//specifies the full path to the bin directory of GNAT
                        if (_access(&argv[i][2], 0) == -1) {    //checks if &argv[i][2] is a valid directory
							fprintf(stdout, "Mika ERROR: indicated bin directory of gnat (via -f switch) : %s , cannot be accessed\n", &argv[i][2]);
                            fflush(stdout);
                            my_exit(9);
						}
						strcpy(gnat_dir, &argv[i][2]);
						break;
                    case 'w' :  //specifies the target directory for the output
						if (_access(&argv[i][2], 0) == -1) {    //checks if &argv[i][2] is a valid directory
						    if (_mkdir(&argv[i][2]) != 0) {
                                fprintf(stdout, "Mika parser ERROR: indicated target directory (via -w switch) : %s , does not exist and cannot be created\n", &argv[i][2]);
                                fflush(stdout);
                                my_exit(9);
                            }
						}
						strcpy(target_dir, &argv[i][2]);
						break;
                    case 'g' :	//specify the Ada version of the code under consideration
						if (!strcmp(argv[i], "-gnat83") || !strcmp(argv[i], "-gnat95") || !strcmp(argv[i], "-gnat05")) {
							strcpy(ada_version, argv[i]);
						}
						else {
							fprintf(stdout, "Mika Warning: unknown option is ignored : %s\n", argv[i]);
                            fflush(stdout);
						}
						break;
					case 't' :
						time_stamp = 1; //-t indicates that a time stamp will be appended to the name of the directory that the parser will create to contain the parsing phase main output. If this switch is omitted the name of new directory created will simply be the name of the file under test followed by '_mika'. This new directory is created in the target directory specified by �N or, by default, the current directory.
						break;
					case 'd' :
						debugMode = 1;  //we are in debug mode : will affect output of warnings amongst other things
						break;
					case 'a' :
						strcpy(user_gnatmake_call_str, &argv[i][2]);
						break;
                    case 'b' :
						strcpy(user_gnatbind_call_str, &argv[i][2]);
						break;
                    case 'c' :
						strcpy(user_gnatls_call_str, &argv[i][2]);
						break;
                    case 'e' :
						strcpy(user_gnatproject_call_str, &argv[i][2]);
						break;
                    case 'k' :
                        strcpy(gnatkrExe, &argv[i][2]);     //the caller supplied string executable for gnatkr
                        break;
                    case 'l' :
                        strcpy(gnatlsExe, &argv[i][2]);     //the caller supplied string executable for gnat ls
                        break;
                    case 'm' :
                        strcpy(gnatmakeExe, &argv[i][2]);   //the caller supplied string executable for gnatmake
                        break;
                    case 'n' :
                        strcpy(gnatbindExe, &argv[i][2]);   //the caller supplied string executable for gnat bind
                        break;
					default :
                        {fprintf(stdout, "Mika Warning: unknown option is ignored : %s\n", argv[i]);
                         fflush(stdout);
                        }
					}
                }
        else {
          fprintf(stdout, "Mika Warning: unknown parameter is ignored : %s\n", argv[i]);
          fflush(stdout);
        }
      }//end processing switches

    strcpy(original_package_name, argv[argc-1]);    //getting the filename which should always be the last argument
	_strlwr_s(original_package_name, strlen(original_package_name)+1);	//to lower case
    if (!strcmp(ada_version, "")) {
      fprintf(stdout, "Mika ERROR: the Ada version has not been set (e.g. to -gnat95)\n"); //because we need it to retrieve resource files
      fflush(stdout);
      my_exit(8);
    }
    if (!strcmp(install_dir, "")) {
      fprintf(stdout, "Mika ERROR: the installation directory of the parser tool has not been set: use -M switch on the command line\n");
      fflush(stdout);
      my_exit(8);
    }
    if (!strcmp(initial_dir, "")) {
      fprintf(stdout, "Mika ERROR: the directory of the file under test or of the project file has not been set: use -O switch on the command line\n");
      fflush(stdout);
      my_exit(8);
    }
    ada_include_path_env = getenv("ADA_INCLUDE_PATH");  //ADA_INCLUDE_PATH can be set initially by the user for Gnat specific directories
    strcpy(put_env_ada_include_path, "ADA_INCLUDE_PATH=");
    strcat(put_env_ada_include_path, (!ada_include_path_env ? "" : ada_include_path_env));
    strcat(put_env_ada_include_path, ";");
    strcat(put_env_ada_include_path, initial_dir);
    if (_putenv(put_env_ada_include_path) == -1) {  //necessary so that the initial file or project file can be found by gnatmake (may not be on the cwd)
      fprintf(stdout, "Mika ERROR: setting environment variable failed : %s\n", put_env_ada_include_path);
      fflush(stdout);
      exit(11);
    }
    if (strcmp(gnat_dir, "")) {
      path_env = getenv("PATH");
      strcpy(put_env_path, "PATH=");
      strcat(put_env_path, gnat_dir);
      strcat(put_env_path, ";");
      strcat(put_env_path, (!path_env ? "" : path_env));
      if (debugMode) fprintf(stdout, "PATH will be set to %s\n", put_env_path);
      if (_putenv(put_env_path) == -1) {  //necessary so the user can choose which version of gnat to use
        fprintf(stdout, "Mika ERROR: setting environment variable failed : %s\n", put_env_path);
        fflush(stdout);
        exit(11);
      }
    }
    fprintf(stdout, "Mika START Compilation\n");
    strcpy(tmp_s, gnatmakeExe); //gnatmakeExe is the name of the gnatmake executable (could be "dotnet-gnatmake.exe")
    strcat(tmp_s, " ");
    strcat(tmp_s, user_gnatproject_call_str);
    strcat(tmp_s, " ");
    if (!strcmp(user_gnatproject_call_str, "")) {   //we use the version to compile if no project switches are specified
      strcat(tmp_s, ada_version);
      strcat(tmp_s, " ");
    }
    strcat(tmp_s, user_gnatmake_call_str);
    strcat(tmp_s, " ");
   // strcat(tmp_s, " -z ");      //i.e. from GNAT User Manual "No main subprogram. Bind and link the program even if the unit name given on the command line is a package name. The resulting executable will execute the elaboration routines of the package and its closure, then the finalization routines."
   // strcat(tmp_s, " -c ");      //Compile only. Do not perform binding, except when `-b' is also specified. Do not perform linking, except if both `-b' and `-l' are also specified.
    strcat(tmp_s, original_package_name);
    if (debugMode) fprintf(stdout, "Mika DEBUG: call to gnat make: %s\n", tmp_s);
    if (system(tmp_s) != 0 ) {            //gnatmake called
      fprintf(stdout, "Mika ERROR: gnatmake generated an error withe following call: %s\n", tmp_s);
      fflush(stdout);
      my_exit(10);
    }

    if (!(path_original_unit = find_path(original_package_name, ".ads", " -s ", gnatlsExe, user_gnatproject_call_str, user_gnatls_call_str, debugMode)))
      if (!(path_original_unit = find_path(original_package_name, ".adb", " -s ", gnatlsExe, user_gnatproject_call_str, user_gnatls_call_str, debugMode))) {
        fprintf(stdout, "Mika ERROR: cannot find desired unit (%s).ads nor .adb \n", original_package_name);
        fflush(stdout);
        my_exit(10);
      }

    strcpy(new_dir, target_dir);
    strcat(new_dir, "\\");
    strcat(new_dir, original_package_name);
    strcat(new_dir, "_mika");
    if (time_stamp) {//time stamp is required
      strcat(new_dir, "_");
      strcat(new_dir, getdatetime());   //in include file getdatetime.c
    }
    //creating new directory with optional time stamp for all outputs
    //this : if(_mkdir( new_dir )!=0 && (time_stamp || (!time_stamp && errno != EEXIST))) {
    // is weird
    if(_mkdir( new_dir )!=0 && (time_stamp || (!time_stamp && errno != EEXIST))) {
      fprintf(stdout, "Mika ERROR: Problem creating directory %s because of errno : %i\n", new_dir, errno );
      fflush(stdout);
      my_exit(14);
    }
/*** END ***/

/*** Start initialise data structures ***/
    decl_init_start();          //for declaration of identifiers data structure
    unxrefed_init_start();      //for un crossreferenced variables (should not exist but does for dead code and some gnat run time libraries variables)
    ref_init_start();           //for reference of identifiers data structure
    init_queue();                   //for binding information data structure
    init_list_operators();      //for list of non user defined operators
/*** END ***/
    strcpy(subprograms_file, target_dir);
    strcat(subprograms_file, "\\");
    strcat(subprograms_file, original_package_name);
    strcat(subprograms_file, ".subprograms");
    subprograms = fopen(subprograms_file, "w");
    if (!subprograms) {
      fprintf(stdout, "Mika ERROR: Can't create %s\n", subprograms_file);
      fflush(stdout);
      my_exit(17);
    }
    fprintf(subprograms, "%s\n", new_dir);
    strcpy(tmp_s, path_original_unit);
    strcat(tmp_s, "\\");
    strcat(tmp_s, original_package_name);
    strcat(tmp_s, ".ads");
    tmp_stream = fopen(tmp_s, "r");
    if (!tmp_stream && !strstr(original_package_name, "-")) ;     //it is a subprogram only unit: i.e. foo.ads does not exist and foo does not contain '-'
    else fprintf(subprograms, "procedure\nelaboration\n0\n");    //it is a package: elaboration makes sense
    if (tmp_stream) fclose(tmp_stream);
/*** start initialise streams especially creating the .pl output file ***/
    strcpy(pl_file, new_dir);
    strcat(pl_file, "\\");
    strcat(pl_file, original_package_name);
    strcat(pl_file, ".pl");
    parsed = fopen(pl_file, "w");  //parsed = stdout;
    if (!parsed) {
      fprintf(stdout, "Mika ERROR: Can't create internal temp file");
      fflush(stdout);
      my_exit(18);
    }
    strcpy(tmp_s, new_dir);
    strcat(tmp_s, "\\");
    strcat(tmp_s, original_package_name);
    strcat(tmp_s, ".cond_ids");
    Fcond_ids = fopen(tmp_s, "w");
    if (!Fcond_ids) {
        fprintf(stdout, "Mika ERROR: Can't create internal temp file"); //%s\n", tmp_s);
        fflush(stdout);
        my_exit(19);
    }
    fprintf(Fcond_ids, "%%LIMITATIONS OF THE PARSING PHASE:\n");
    fprintf(Fcond_ids, "%%    'atomic conditions not within a decision or a branch are not labeled'\n");
    fprintf(Fcond_ids, "%%    in other words they are not considered as conditions at the moment\n");
    fprintf(Fcond_ids, "%%    e.g. x := A  where A is a Boolean; or x := f(y,z); where f returns a Boolean\n");
    fprintf(Fcond_ids, "%%OTHER REMARKS:\n");
    fprintf(Fcond_ids, "%%    Redundancy could be greatly reduced in this file!!\n");
    fprintf(Fcond_ids, "%%    file names could be referenced once and for all\n");

    fprintf(parsed, "mika_body([");
/*** END ***/

    fprintf(stdout, "Mika START Generating elaboration order\n");
    strcpy(bind_file, new_dir);
    strcat(bind_file, "\\");
    strcat(bind_file, original_package_name);
    strcat(bind_file, ".bind");

    strcpy(tmp_s, gnatbindExe);
    strcat(tmp_s, " ");
    strcat(tmp_s, user_gnatproject_call_str);
    strcat(tmp_s, " ");
    strcat(tmp_s, user_gnatbind_call_str);
    strcat(tmp_s, " -z ");      //i.e. from GNAT User Manual "No main subprogram."
    strcat(tmp_s, " -c ");      //i.e. from GNAT User Manual "Check only, no generation of binder output file."
    strcat(tmp_s, " -l ");      //i.e. from GNAT User Manual "Output chosen elaboration order."
    strcat(tmp_s, original_package_name);
    strcat(tmp_s, " > \"");
    strcat(tmp_s, bind_file);
    strcat(tmp_s, "\"");
    if (debugMode) fprintf(stdout, "Mika DEBUG: call to gnat bind: %s\n", tmp_s);
    if (system(tmp_s) != 0) {            //gnat bind called
      fprintf(stdout, "Mika ERROR: call to gnat bind failed: %s\n", tmp_s);
      fflush(stdout);
      my_exit(10);
    }
    fflush(stdout);

    fprintf(stdout, "Mika START Referencing\n");
    strcpy(xref_file, new_dir);
    strcat(xref_file, "\\");
    strcat(xref_file, original_package_name);
    strcat(xref_file, ".xref");

    fflush(stdout);
    strcpy(tmp_s, "\"\"");              //the entire string is surrounded with "" because of spaces in install_dir
    strcat(tmp_s, install_dir);
    strcat(tmp_s, "\\mika_ada_xref.exe\" ");
    if (debugMode) strcat(tmp_s, "-d ");
    //if (strcmp(user_gnatls_call_str, "") ){ //not equal to "" then we pass it on, otherwise the default will be used which is ""
      strcat(tmp_s, " -c\"");
      strcat(tmp_s, user_gnatls_call_str);
      strcat(tmp_s, "\" ");
    //}
    strcat(tmp_s, " -e\"");
    strcat(tmp_s, user_gnatproject_call_str);
    strcat(tmp_s, "\" ");
    strcat(tmp_s, " -l\"");
    strcat(tmp_s, gnatlsExe);
    strcat(tmp_s, "\" ");
    strcat(tmp_s, original_package_name);
    strcat(tmp_s, " > \"");
    strcat(tmp_s, xref_file);
    strcat(tmp_s, "\"\"");
    if (debugMode) {fprintf(stdout, "Mika DEBUG: call to mika_ada_xref: %s\n", tmp_s); fflush(stdout);}
    ourxref_exit_code = system(tmp_s);      //mika_ada_xref called
    if (ourxref_exit_code != 0) {
      fprintf(stdout, "Mika ERROR: Error in cross referencer");
      if (debugMode) fprintf(stdout, "; call was: %s", tmp_s);
      fflush(stdout);
      my_exit(ourxref_exit_code);
    }
/*** END ***/

/*** Start setting up initial file: standard.xref generated manually using standard.ads which is hand crafted***/
    strcpy(tmp_s, install_dir);
    strcat(tmp_s, "\\..\\resources\\");
    if (!strcmp(ada_version, "-gnat83"))
      strcat(tmp_s, "standard83\\standard.xref");  //because they are different (e.g. characters are only from 0 to 127)
    else if (!strcmp(ada_version, "-gnat95"))
      strcat(tmp_s, "standard95\\standard.xref");
    else if (!strcmp(ada_version, "-gnat05"))
      strcat(tmp_s, "standard05\\standard.xref");
    else {
      fprintf(stdout, "Mika ERROR: Ada version is not valid somehow: %s\n", ada_version);
      fflush(stdout);
      my_exit(7);
    }
    yyin = fopen(tmp_s, "r");
    if (!yyin) {
      fprintf(stdout, "Mika ERROR: %s:  Can't open internal file", tmp_s);
      fflush(stdout);
      my_exit(21);
    }
/*** END ***/

    yylineno = 1;
    column = 0;
    callBEGIN_REF();  //lex starts with a REF file
    if (debugMode) {
        fprintf(stdout, "\nMika START xref file: %s\n", tmp_s);
        fflush(stdout);
    }

    fprintf(stdout, "Mika START Parsing\n");

    yyparse();          //to initially parse standard.xref, then on yywrap, standard.ads, foo.xref, foo.bind and then and the ada code files as per dictated elaboration order

    if (!debugMode) fprintf(stdout, "\n");
    fprintf(stdout, "Mika START post-parsing\n");
    fflush(stdout);
    fprintf(parsed, ",\nmika_ref([\n");
    decl_print_start();         //prints all xrefed declarations for debug
    unxrefed_print_start();     //prints all un-crossreferenced variables for debug
    fprintf(parsed, "\n])\n]).\n");
    //additional information is added to the parsed file
    transform_path_to_prolog(path_original_unit, &tmp_chr_ptr);
    fprintf(parsed, "orig_dir('%s').\n", tmp_chr_ptr);
    transform_path_to_prolog(install_dir, &tmp_chr_ptr);
    fprintf(parsed, "install_dir('%s').\n", tmp_chr_ptr);   //still needed in gage because we need to call gage itself first
    transform_path_to_prolog(new_dir, &tmp_chr_ptr);
    fprintf(parsed, "target_dir('%s').\n", tmp_chr_ptr);    //still needed in gage because we need to load the parser file

    fclose(Fcond_ids);
    strcpy(tmp_s, new_dir);
    strcat(tmp_s, "\\");
    strcat(tmp_s, original_package_name);
    strcat(tmp_s, ".cond_ids");
    Fcond_ids = fopen(tmp_s, "r");
    if (!Fcond_ids) {
      fprintf(stdout, "Mika ERROR: Can't open internal temp file"); //%s\n", tmp_s);
      fflush(stdout);
      my_exit(45);
    }
    while ((ch_tmp=fgetc(Fcond_ids))!=EOF) {  //copies the contents of foo.cond_ids to the end of foo.pl
      fputc(ch_tmp, parsed);
    }
    fclose(Fcond_ids);
    if (!debugMode) remove(tmp_s);    //part of stdio.h : delete the foo.cond_ids file

    fclose(parsed);

    if (debugMode) {
      fprintf(stdout, "All files present in foo.xref follow\n");
      fflush(stdout);
      print_filename(filenames_in_xref);
      print_operators_queue();
      fflush(stdout);
    }
    if (error_count) {
      fprintf(stdout, "Mika ERROR: %d syntax errors%s detected\n", error_count, error_count == 1 ? "": "s");
      fprintf(stdout, "%s\\%s", new_dir, original_package_name);
      fflush(stdout);
      my_exit(22);
    }
    else {
      fprintf(stdout, "Mika %d lines analised\n", total_line_no);
      fprintf(stdout, "Mika No syntax errors detected\n");
      fprintf(stdout, "Mika END Parsing\n");
      if (debugMode) {
        fprintf(stdout, "Mika DEBUG: newly created directory is : %s \n", new_dir);
      }
      fflush(stdout);
      if (!debugMode) {
        strcpy(po_file, new_dir);
        strcat(po_file, "\\");
        strcat(po_file, original_package_name);
        strcat(po_file, ".po");
        chdir(new_dir);
        strcpy(tmp_s, "\"\"");              //the entire string is surrounded with "" because of spaces in install_dir
        strcat(tmp_s, install_dir);
        strcat(tmp_s, "\\encoder.exe\" ");
        strcat(tmp_s, original_package_name);
        strcat(tmp_s, ".pl ");
        strcat(tmp_s, original_package_name);
        strcat(tmp_s, ".po ");
        strcat(tmp_s, "\"");
        if (system(tmp_s) != 0) {            //encoder.exe called
          fprintf(stdout, "Mika ERROR: call to encoder.exe failed: %s\n", tmp_s);
          fflush(stdout);
          my_exit(10);
        }
        strcpy(tmp_s, original_package_name);
        strcat(tmp_s, ".pl ");
        remove(tmp_s);    //part of stdio.h : delete the foo.pl file
      }
      my_exit(EXIT_SUCCESS);
    }

} //end main

//searches the binary_filename(.c) DT created during parsing of foo.xref and standard.xref, which records the actual filenames as used by gnat,
// to retrieve the name of the files that have to be parsed (added to the queue(.c) during foo.bind parsing (add_tail) and body_stub parsing (add_head))
void find_filename_path(char *name, char *suffix, char **filename, char **path)
{
   char tmp_s2[_MAX_PATH];

   strcpy(tmp_s2, name);
   strcat(tmp_s2, suffix);
   if (search_filename(filenames_in_xref, tmp_s2)) {  //to see if it was mentionned in foo.xref
     *filename = name;                                  //it is the same i.e. unkrunched
   }
   else {
     char krunch[_MAX_PATH];
     FILE *tmp_stream_krunch;
     char tmp_s[_MAX_PATH+40];        /* temporary string holder */
     strcpy(tmp_s, gnatkrExe);       //gnatkrExe is the name of the gnatkr executable (could be "dotnet-gnatkr.exe")
     strcat(tmp_s, " ");
     strcat(tmp_s, name);
     strcat(tmp_s, " > mika_tmp.txt");
     if (system(tmp_s) !=0) {           //gnatkr called
       fprintf(stdout, "Mika ERROR: Can't call file shortener");
       fflush(stdout);
       my_exit(23);
     }
     tmp_stream_krunch = fopen("mika_tmp.txt", "r");
     fgets(krunch, 256, tmp_stream_krunch);
     fclose(tmp_stream_krunch);
     krunch[strlen(krunch)-1] = '\0';
     strcpy(tmp_s2, krunch);
     strcat(tmp_s2, suffix);
     if (search_filename(filenames_in_xref, tmp_s2)) {  //to see if it was mentionned in foo.xref
       *filename = malloc(strlen(krunch) + 1);
       strcpy(*filename, krunch);
     }
     else *filename = NULL;        //not added because it is not referenced in foo.ref
   }
   if (*filename)
     *path = find_path(*filename, suffix, " -a -s ", gnatlsExe, user_gnatproject_call_str, user_gnatls_call_str, debugMode);
   else *path = NULL;
   if (debugMode)
     fprintf(stdout, "Mika DEBUG within find_filename_path found for %s%s the filename: %s on the path: %s\n", name, suffix, !*filename ? "NULL" : *filename, !*path ? "NULL" : *path);
}//find_filename_path


char *handle_operator_calls(struct id_ref_t id_param)
{   char *return_string;
    key2 = malloc(strlen(current_unit->filename)+strlen(current_unit->suffix)+strlen(id_param.line)+strlen(id_param.column)+strlen(id_param.id)+4);
    strcpy(key2, current_unit->filename);
    strcat(key2, current_unit->suffix);
    strcat(key2, ":");
    strcat(key2, id_param.line);
    strcat(key2, ":");
    strcat(key2, id_param.column);
    strcat(key2, ":");
    strcat(key2, id_param.id);
    found_decl = ref_search_start(key2);
    if (found_decl) {   //is cross-referenced : i.e. has been redefined by the user
      if (absent_operator(found_decl->unique_id)) {   //actually defined
        return_string = malloc(strlen(found_decl->unique_id)+1);
        strcpy(return_string, found_decl->unique_id);
        return return_string;
      }
      else return NULL; //treat a normal operator -> use the predefined operator
    }
    else return NULL;   //not used defined : i.e. is the default behaviour
}

char *handle_identifiers(struct id_ref_t id_param, int is_a_string)
{   char *return_string;
    key2 = malloc(strlen(current_unit->filename)+strlen(current_unit->suffix)+strlen(id_param.line)+strlen(id_param.column)+strlen(id_param.id)+4);
    strcpy(key2, current_unit->filename);
    strcat(key2, current_unit->suffix);
    strcat(key2, ":");
    strcat(key2, id_param.line);
    strcat(key2, ":");
    strcat(key2, id_param.column);
    strcat(key2, ":");
    strcat(key2, id_param.id);
    //used to be search decls first and then refs but changing it 02/02/10
    found_decl = ref_search_start(key2);
    if (!found_decl)
      found_decl = decl_search_start(key2);
    if (found_decl) {
        return_string = malloc(strlen(found_decl->unique_id)+1);
        strcpy(return_string, found_decl->unique_id);
    }
    else { // can happen for standard defined identifiers
           // e.g. we are looking for 'stvr.adb:5:7:boolean' but in fact we should look for 'standard:boolean'
           if (is_a_string)
           {//really a string (not a designator)
            //a simple string (not actually an operator symbol) does not appear in foo.xref
            //14-06-07 (was strcpy(return_string, id_param.id);)
            //         now so e.g. string__12_55_55 is transformed to string([12,55,55])
              if (!strcmp(id_param.id, "string"))
              {
                return_string = malloc(SAFETY+strlen(id_param.id)+11);
                strcpy(return_string, "string([])");
              }
              else
              { int i = 7;
                return_string = malloc(SAFETY+strlen(id_param.id)+11);
                strcpy(return_string, "string([");
                while (id_param.id[i] != '\0')
                {
                  if (id_param.id[i] == '_')
                    return_string[i+1] = ',';
                  else
                    return_string[i+1] = id_param.id[i];
                  i++;
                }
                return_string[i+1] = '\0';
                strcat(return_string, "])");
              }
           }
           else { //not really a string (i.e. an identifier, a character_literal or a designator string!)
            if (is_predefined_id(id_param.id)) {
                key2 = malloc(strlen(id_param.id)+14);
                strcpy(key2, "standard.ads:");
                strcat(key2, id_param.id);
                found_decl = decl_search_start(key2);               //but [18/03/08] we may catch dead code entities that share the same name...
                if (!found_decl)
                    found_decl = ref_search_start(key2);
                if (!found_decl) {
                    fprintf(stdout, "Mika ERROR: unknown predefined identifier %s\n", id_param.id); //could also print out key2);
                    fflush(stdout);
                    my_exit(24);
                }
                //do not change. do not understand why this cannot be simply $$ = (found_decl->unique_id)
                return_string = malloc(strlen(found_decl->unique_id)+1);
                strcpy(return_string, found_decl->unique_id);
                free(key2);
            }
            else { //this should only happen for dead code (see 21/09/04) but may happen for run time libraries due to non standard Ada (27/10/07), and also within pragmas (e.g. 'C' calling convention) (13/03/08)
              if (in_a_pragma) {
                return_string = malloc(strlen(id_param.id)+1);
                strcpy(return_string, id_param.id);
              }
              else {
                if (debugMode) {
                  fprintf(stdout, "Un-crossreferenced variable %s with key2 :%s\n", id_param.id, key2);   //will be outputed as unxrefed(..., ...) in foo.pl later (see double_tst.c)
                  fflush(stdout);
                }
                current_unxrefed = unxrefed_insert_start(key2, id_param.id);
                return_string = malloc(strlen(current_unxrefed->unique_id)+1);
                strcpy(return_string, current_unxrefed->unique_id);
              }
            }
        }//not a string
    }//was not found
    return return_string;
}// handle_identifiers

int is_predefined_id(char *id)
{
  if (search_filename(predefined_ids_root, id)) //constructed while parsing standard.xref
    return 1;
  else return 0;
}//is_predefined_id

//print out shortened identifier without the unique final number e.g. "my_package_365" will be printed as "my_package"
//usefful for feedback messages
void printout_shortened(char *str)
{
    char *p = strrchr(str, '_');  //pointer to the last '_' within str
    char *i = str;
    while (i != p && (*i != '\0')) {
      fprintf(stdout, "%c", *i);
      i++;
    }
}

//check if we encounter the declaration of an intrinsic subprogram
int is_new_intrinsic(char *filename, char *id)
{
  int i;
  for (i = 0; i < NUM_INTRINSIC; i++) {
    if (!intrinsic_tab[i].printed && !strcmp(filename, intrinsic_tab[i].filename) && !strcmp(id, intrinsic_tab[i].id)) {
      intrinsic_tab[i].printed = 1;     //won't be printed again
      return 1;
    }
  }
  return 0;
}

//build condition string
void build_condition(struct id_decision relation, char **arg, struct unit_type *unit, int line, int column)
{
  if (relation.is_a_decision) {
    *arg = malloc(strlen(relation.id)+1);
    strcpy(*arg, relation.id);
  }
  else {
    itoa(condition_nb++, tmp_s, 10);
    print_coverage_details(COND, tmp_s, unit, line, column);    //perfect : column indicated is just on the boolean operator
    *arg = malloc(strlen(relation.id)+strlen(tmp_s)+9);
    strcpy(*arg, "cond(");
    strcat(*arg, tmp_s);
    strcat(*arg, ", ");
    strcat(*arg, relation.id);
    strcat(*arg, ")");
  }
}

//build expression string
void build_expression(struct id_decision expression2, char **expression, struct unit_type *unit, int line, int column)
{
  if (expression2.is_a_decision) {
    itoa(decision_nb++, tmp_s, 10);
    *expression = malloc(SAFETY+strlen(tmp_s)+strlen(expression2.id)+15);
    print_coverage_details(DECI, tmp_s, unit, line, column);
    strcpy(*expression, "deci(");
    strcat(*expression, tmp_s);
    strcat(*expression, ", ");
    strcat(*expression, expression2.id);
    strcat(*expression, ")");
  }
  else {
    *expression = malloc(strlen(expression2.id)+1);
    strcpy(*expression, expression2.id);
  }
}

//handles parsing errors : since the Ada code has already passed GNAT's compiler, this is not very useful...
//in fact it is only useful for bind or ali files or when our Ada syntax rules are wrong.
void yyerror(const char *s)
{
        extern int yychar;
        if (yychar == INTERFACE) { //new Ada 2005 keyword : can we recover? : should be identifier interface rather than keyword
          return; //return from yyerror : will be recovered by error handling rule for identifiers
        }
        error_count++;

        //fprintf(stdout,"  %s", s);
        if (yylineno) {
          fprintf(stdout,"Mika ERROR: in file %s%s on line %d and column %d,", current_unit->filename, current_unit->suffix, yylineno, column);
          fflush(stdout);
        }
        fprintf(stdout," on input: ");
        fflush(stdout);
        if (yychar >= 0400) {
                if ((yychar >= ABORT) && (yychar <= XOR)) { //a keyword
                    fprintf(stdout, "(keyword) %s, number #%d\n", key_tab[yychar-ABORT].kw, yychar);
                    fflush(stdout);
                } else switch (yychar) {
                        case PAC_START_ELABORATION : fprintf(stdout, "(remark : IN BIND FILE!) ELABORATION ORDER\n");
                                break;
                        case pac_package_name : fprintf(stdout, "(remark : IN BIND FILE!) %s, a package name\n", yytext);
                                break;
                        case PAC_SPEC : fprintf(stdout, "(remark : IN BIND FILE!) (spec)\n");
                                break;
                        case PAC_BODY : fprintf(stdout, "(remark : IN BIND FILE!) (body)\n");
                                break;
                        case PAC_DOT : fprintf(stdout, "(remark : IN BIND FILE!) .\n");
                                break;
                        case REF_COLON : fprintf(stdout, "(remark : IN ALI FILE!) :\n");
                                break;
                        case REF_DECL : fprintf(stdout, "(remark : IN ALI FILE!) Decl:\n");
                                break;
                        case REF_BODY : fprintf(stdout, "(remark : IN ALI FILE!) Body:\n");
                                break;
                        case REF_REF : fprintf(stdout, "(remark : IN ALI FILE!) Ref:\n");
                                break;
                        case REF_MODI : fprintf(stdout, "(remark : IN ALI FILE!) Modi:\n");
                                break;
                        case ref_string : fprintf(stdout, "(remark : IN ALI FILE!) %s, a string\n", yytext);
                                break;
                        case ref_filename : fprintf(stdout, "(remark : IN ALI FILE!) %s, a filename\n", yytext);
                                break;
                        case ref_identifier : fprintf(stdout, "(remark : IN ALI FILE!) %s, an identifier\n", yytext);
                                break;
                        case ref_character_literal : fprintf(stdout, "(remark : IN ALI FILE!) %s, a character literal\n", yytext);
                                break;
                        case ref_integer : fprintf(stdout, "(remark : IN ALI FILE!) %s, an integer\n", yytext);
                                break;
                        case TIC : fprintf(stdout, "single-quote\n");
                                break;
                        case DOT_DOT : fprintf(stdout, "..\n");
                                break;
                        case LT_LT : fprintf(stdout, "<<\n");
                                break;
                        case BOX : fprintf(stdout, "<>\n");
                                break;
                        case GT_GT : fprintf(stdout, ">>\n");
                                break;
                        case IS_ASSIGNED : fprintf(stdout, ":=\n");
                                break;
                        case RIGHT_SHAFT : fprintf(stdout, "=>\n");
                                break;
                        case PLUS : fprintf(stdout, "+\n");
                                break;
                        case MINUS : fprintf(stdout, "-\n");
                                break;
                        case MULT : fprintf(stdout, "*\n");
                                break;
                        case DIV : fprintf(stdout, "/\n");
                                break;
                        case EXPON : fprintf(stdout, "**\n");
                                break;
                        case CONC : fprintf(stdout, "&\n");
                                break;
                        case EQUAL : fprintf(stdout, "=\n");
                                break;
                        case NE : fprintf(stdout, "/=\n");
                                break;
                        case LT : fprintf(stdout, "<\n");
                                break;
                        case LT_EQ : fprintf(stdout, "<=\n");
                                break;
                        case GT : fprintf(stdout, ">\n");
                                break;
                        case GE : fprintf(stdout, ">=\n");
                                break;
                        case character_literal : fprintf(stdout, "character literal\n");
                                break;
                        case identifier : fprintf(stdout, "%s an identifier\n", yytext);
                                break;
                        case string_literal : fprintf(stdout, "string\n");
                                break;
                        case numeric_literal : fprintf(stdout, "numeric literal\n");
                                break;
                        default :
                                fprintf(stdout, "(token) %s\n", yytext);
                }
                fflush(stdout);
        } else {
                switch (yychar) {
                        case '\t': fprintf(stdout,"horizontal-tab\n"); break;
                        case '\n': fprintf(stdout,"newline\n"); break;
                        case '\0': fprintf(stdout,"end\n"); break;
                        case ' ': fprintf(stdout, "(blank)\n"); break;
                        default : fprintf(stdout,"(char) %c\n", yychar);
                }
                fflush(stdout);
        }
  my_exit(25);
}// yyerror function

//recover from error due to new keywords
void recover_keyword(char *str)
{
  char lineno[10];     //see fill_id_ref
  char columnno[10];
  int correct_column;

  error_recovery_identifier.id = malloc((strlen(str)+1)*sizeof(char));
  strcpy(error_recovery_identifier.id, str);
  itoa(yylineno, lineno, 10);                              //see fill_id_ref
  strcpy(error_recovery_identifier.line, lineno);
  correct_column = column - strlen(str) + 1;
  itoa(correct_column, columnno, 10);
  strcpy(error_recovery_identifier.column, columnno);
}// recover_keyword

//print cond, gate, deci or bran information to Fcond_ids file
void print_coverage_details(int type, char * number, struct unit_type *unit, int line, int column)
{
  switch (type) {
    case COND : fprintf(Fcond_ids, "cond(");
                break;
    case GATE : fprintf(Fcond_ids, "gate(");
                break;
    case DECI : fprintf(Fcond_ids, "deci(");
                break;
    case BRAN : fprintf(Fcond_ids, "bran(");
                break;
    case RUNE : fprintf(Fcond_ids, "rune(");
                break;
    default : fprintf(stdout, "Mika ERROR: unknown type of coverage %i in print_coverage_details", type);
              fflush(stdout);
              my_exit(31);
  }
  fprintf(Fcond_ids, "%s, '%s', '%s', '%s', '%s', %i, %i).\n", number, unit->name, unit->filename, unit->suffix, unit->path, line, column);
}

//exits and performs some tidying up if not in debug mode
void my_exit(int exit_code)
{
  if (!debugMode) {
    if (yyin) fclose(yyin);
    if (parsed) fclose(parsed);
    if (subprograms) fclose(subprograms);
    if (_access(xref_file, 0) != -1) remove(xref_file);
    if (_access(bind_file, 0) != -1) remove(bind_file);
    if (_access(pl_file, 0) != -1) remove(pl_file);
    strcpy(tmp_s, cwd);
    strcat(tmp_s, "\\mika_tmp.txt");
    if ((_access_s( tmp_s, 0 )) == 0 ) remove(tmp_s);
    if (exit_code != EXIT_SUCCESS) {
        if (_access(po_file, 0) != -1) remove(po_file);
        if (_access(subprograms_file, 0) != -1) remove(subprograms_file);
    }
  }
  exit(exit_code);
}

/**************************************END OF ADA.Y FILE *********************************************/