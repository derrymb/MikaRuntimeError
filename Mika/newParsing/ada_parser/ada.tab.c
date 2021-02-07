/* A Bison parser, made by GNU Bison 3.7.1.  */

/* Bison implementation for Yacc-like parsers in C

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

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.7.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 16 "ada.y"

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
int gate_nb = 1;        //counter for individual gate (i.e. boolean operators : and, or, xor, not and_then, or_else, not)
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

#line 213 "ada.tab.c"

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

#include "ada.tab.h"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_PAC_START_ELABORATION = 3,      /* PAC_START_ELABORATION  */
  YYSYMBOL_pac_package_name = 4,           /* pac_package_name  */
  YYSYMBOL_PAC_SPEC = 5,                   /* PAC_SPEC  */
  YYSYMBOL_PAC_BODY = 6,                   /* PAC_BODY  */
  YYSYMBOL_PAC_DOT = 7,                    /* PAC_DOT  */
  YYSYMBOL_REF_COLON = 8,                  /* REF_COLON  */
  YYSYMBOL_REF_DECL = 9,                   /* REF_DECL  */
  YYSYMBOL_REF_BODY = 10,                  /* REF_BODY  */
  YYSYMBOL_REF_REF = 11,                   /* REF_REF  */
  YYSYMBOL_REF_MODI = 12,                  /* REF_MODI  */
  YYSYMBOL_ref_string = 13,                /* ref_string  */
  YYSYMBOL_ref_filename = 14,              /* ref_filename  */
  YYSYMBOL_ref_identifier = 15,            /* ref_identifier  */
  YYSYMBOL_ref_character_literal = 16,     /* ref_character_literal  */
  YYSYMBOL_ref_integer = 17,               /* ref_integer  */
  YYSYMBOL_TIC = 18,                       /* TIC  */
  YYSYMBOL_DOT_DOT = 19,                   /* DOT_DOT  */
  YYSYMBOL_LT_LT = 20,                     /* LT_LT  */
  YYSYMBOL_BOX = 21,                       /* BOX  */
  YYSYMBOL_GT_GT = 22,                     /* GT_GT  */
  YYSYMBOL_IS_ASSIGNED = 23,               /* IS_ASSIGNED  */
  YYSYMBOL_RIGHT_SHAFT = 24,               /* RIGHT_SHAFT  */
  YYSYMBOL_ABORT = 25,                     /* ABORT  */
  YYSYMBOL_ABS = 26,                       /* ABS  */
  YYSYMBOL_ABSTRACT = 27,                  /* ABSTRACT  */
  YYSYMBOL_ACCEPT = 28,                    /* ACCEPT  */
  YYSYMBOL_ACCESS = 29,                    /* ACCESS  */
  YYSYMBOL_ALIASED = 30,                   /* ALIASED  */
  YYSYMBOL_ALL = 31,                       /* ALL  */
  YYSYMBOL_AND = 32,                       /* AND  */
  YYSYMBOL_ARRAY = 33,                     /* ARRAY  */
  YYSYMBOL_AT = 34,                        /* AT  */
  YYSYMBOL_BEGiN = 35,                     /* BEGiN  */
  YYSYMBOL_BODY = 36,                      /* BODY  */
  YYSYMBOL_CASE = 37,                      /* CASE  */
  YYSYMBOL_CONSTANT = 38,                  /* CONSTANT  */
  YYSYMBOL_DECLARE = 39,                   /* DECLARE  */
  YYSYMBOL_DELAY = 40,                     /* DELAY  */
  YYSYMBOL_DELTA = 41,                     /* DELTA  */
  YYSYMBOL_DIGITS = 42,                    /* DIGITS  */
  YYSYMBOL_DO = 43,                        /* DO  */
  YYSYMBOL_ELSE = 44,                      /* ELSE  */
  YYSYMBOL_ELSIF = 45,                     /* ELSIF  */
  YYSYMBOL_END = 46,                       /* END  */
  YYSYMBOL_ENTRY = 47,                     /* ENTRY  */
  YYSYMBOL_EXCEPTION = 48,                 /* EXCEPTION  */
  YYSYMBOL_EXIT = 49,                      /* EXIT  */
  YYSYMBOL_FOR = 50,                       /* FOR  */
  YYSYMBOL_FUNCTION = 51,                  /* FUNCTION  */
  YYSYMBOL_GENERIC = 52,                   /* GENERIC  */
  YYSYMBOL_GOTO = 53,                      /* GOTO  */
  YYSYMBOL_IF = 54,                        /* IF  */
  YYSYMBOL_IN = 55,                        /* IN  */
  YYSYMBOL_INTERFACE = 56,                 /* INTERFACE  */
  YYSYMBOL_IS = 57,                        /* IS  */
  YYSYMBOL_LIMITED = 58,                   /* LIMITED  */
  YYSYMBOL_LOOP = 59,                      /* LOOP  */
  YYSYMBOL_MOD = 60,                       /* MOD  */
  YYSYMBOL_NEW = 61,                       /* NEW  */
  YYSYMBOL_NOT = 62,                       /* NOT  */
  YYSYMBOL_NuLL = 63,                      /* NuLL  */
  YYSYMBOL_OF = 64,                        /* OF  */
  YYSYMBOL_OR = 65,                        /* OR  */
  YYSYMBOL_OTHERS = 66,                    /* OTHERS  */
  YYSYMBOL_OUT = 67,                       /* OUT  */
  YYSYMBOL_OVERRIDING = 68,                /* OVERRIDING  */
  YYSYMBOL_PACKAGE = 69,                   /* PACKAGE  */
  YYSYMBOL_PRAGMA = 70,                    /* PRAGMA  */
  YYSYMBOL_PRIVATE = 71,                   /* PRIVATE  */
  YYSYMBOL_PROCEDURE = 72,                 /* PROCEDURE  */
  YYSYMBOL_PROTECTED = 73,                 /* PROTECTED  */
  YYSYMBOL_RAISE = 74,                     /* RAISE  */
  YYSYMBOL_RANGE = 75,                     /* RANGE  */
  YYSYMBOL_RECORD = 76,                    /* RECORD  */
  YYSYMBOL_REM = 77,                       /* REM  */
  YYSYMBOL_RENAMES = 78,                   /* RENAMES  */
  YYSYMBOL_REQUEUE = 79,                   /* REQUEUE  */
  YYSYMBOL_RETURN = 80,                    /* RETURN  */
  YYSYMBOL_REVERSE = 81,                   /* REVERSE  */
  YYSYMBOL_SELECT = 82,                    /* SELECT  */
  YYSYMBOL_SEPARATE = 83,                  /* SEPARATE  */
  YYSYMBOL_SUBTYPE = 84,                   /* SUBTYPE  */
  YYSYMBOL_SYNCHRONIZED = 85,              /* SYNCHRONIZED  */
  YYSYMBOL_TAGGED = 86,                    /* TAGGED  */
  YYSYMBOL_TASK = 87,                      /* TASK  */
  YYSYMBOL_TERMINATE = 88,                 /* TERMINATE  */
  YYSYMBOL_THEN = 89,                      /* THEN  */
  YYSYMBOL_TYPE = 90,                      /* TYPE  */
  YYSYMBOL_UNTIL = 91,                     /* UNTIL  */
  YYSYMBOL_USE = 92,                       /* USE  */
  YYSYMBOL_WHEN = 93,                      /* WHEN  */
  YYSYMBOL_WHILE = 94,                     /* WHILE  */
  YYSYMBOL_WITH = 95,                      /* WITH  */
  YYSYMBOL_XOR = 96,                       /* XOR  */
  YYSYMBOL_PLUS = 97,                      /* PLUS  */
  YYSYMBOL_MINUS = 98,                     /* MINUS  */
  YYSYMBOL_MULT = 99,                      /* MULT  */
  YYSYMBOL_DIV = 100,                      /* DIV  */
  YYSYMBOL_EXPON = 101,                    /* EXPON  */
  YYSYMBOL_CONC = 102,                     /* CONC  */
  YYSYMBOL_EQUAL = 103,                    /* EQUAL  */
  YYSYMBOL_NE = 104,                       /* NE  */
  YYSYMBOL_LT = 105,                       /* LT  */
  YYSYMBOL_LT_EQ = 106,                    /* LT_EQ  */
  YYSYMBOL_GT = 107,                       /* GT  */
  YYSYMBOL_GE = 108,                       /* GE  */
  YYSYMBOL_character_literal = 109,        /* character_literal  */
  YYSYMBOL_identifier = 110,               /* identifier  */
  YYSYMBOL_string_literal = 111,           /* string_literal  */
  YYSYMBOL_numeric_literal = 112,          /* numeric_literal  */
  YYSYMBOL_113_ = 113,                     /* ';'  */
  YYSYMBOL_114_ = 114,                     /* '('  */
  YYSYMBOL_115_ = 115,                     /* ')'  */
  YYSYMBOL_116_ = 116,                     /* ','  */
  YYSYMBOL_117_ = 117,                     /* ':'  */
  YYSYMBOL_118_ = 118,                     /* '|'  */
  YYSYMBOL_119_ = 119,                     /* '.'  */
  YYSYMBOL_YYACCEPT = 120,                 /* $accept  */
  YYSYMBOL_goal_symbol = 121,              /* goal_symbol  */
  YYSYMBOL_122_1 = 122,                    /* $@1  */
  YYSYMBOL_ref_xref_list = 123,            /* ref_xref_list  */
  YYSYMBOL_ref_xref = 124,                 /* ref_xref  */
  YYSYMBOL_ref_identifier_or_string_or_character_literal = 125, /* ref_identifier_or_string_or_character_literal  */
  YYSYMBOL_ref_decl = 126,                 /* ref_decl  */
  YYSYMBOL_ref_body_opt = 127,             /* ref_body_opt  */
  YYSYMBOL_ref_body_list = 128,            /* ref_body_list  */
  YYSYMBOL_ref_body = 129,                 /* ref_body  */
  YYSYMBOL_130_2 = 130,                    /* $@2  */
  YYSYMBOL_ref_modi_opt = 131,             /* ref_modi_opt  */
  YYSYMBOL_ref_ref_opt = 132,              /* ref_ref_opt  */
  YYSYMBOL_ref_modified_or_referenced_list = 133, /* ref_modified_or_referenced_list  */
  YYSYMBOL_ref_reference = 134,            /* ref_reference  */
  YYSYMBOL_135_3 = 135,                    /* $@3  */
  YYSYMBOL_ref_line_column_list = 136,     /* ref_line_column_list  */
  YYSYMBOL_ref_line_column = 137,          /* ref_line_column  */
  YYSYMBOL_pac_elaboration_order = 138,    /* pac_elaboration_order  */
  YYSYMBOL_pac_package_list = 139,         /* pac_package_list  */
  YYSYMBOL_pac_package = 140,              /* pac_package  */
  YYSYMBOL_pac_name = 141,                 /* pac_name  */
  YYSYMBOL_pac_dot_opt = 142,              /* pac_dot_opt  */
  YYSYMBOL_pac_spec_or_body = 143,         /* pac_spec_or_body  */
  YYSYMBOL_pragma = 144,                   /* pragma  */
  YYSYMBOL_145_4 = 145,                    /* $@4  */
  YYSYMBOL_146_5 = 146,                    /* $@5  */
  YYSYMBOL_pragma_argument_association_list = 147, /* pragma_argument_association_list  */
  YYSYMBOL_pragma_argument_association = 148, /* pragma_argument_association  */
  YYSYMBOL_pragma_s = 149,                 /* pragma_s  */
  YYSYMBOL_object_declaration = 150,       /* object_declaration  */
  YYSYMBOL_identifier_list = 151,          /* identifier_list  */
  YYSYMBOL_identifier_rule = 152,          /* identifier_rule  */
  YYSYMBOL_new_ada_2005_reserved_words = 153, /* new_ada_2005_reserved_words  */
  YYSYMBOL_new_ada_95_reserved_words = 154, /* new_ada_95_reserved_words  */
  YYSYMBOL_object_qualifier_opt = 155,     /* object_qualifier_opt  */
  YYSYMBOL_object_subtype_definition = 156, /* object_subtype_definition  */
  YYSYMBOL_init_opt = 157,                 /* init_opt  */
  YYSYMBOL_158_6 = 158,                    /* @6  */
  YYSYMBOL_type_declaration = 159,         /* type_declaration  */
  YYSYMBOL_discriminant_part_opt = 160,    /* discriminant_part_opt  */
  YYSYMBOL_type_definition = 161,          /* type_definition  */
  YYSYMBOL_type_completion = 162,          /* type_completion  */
  YYSYMBOL_subtype_declaration = 163,      /* subtype_declaration  */
  YYSYMBOL_subtype_indication = 164,       /* subtype_indication  */
  YYSYMBOL_constraint = 165,               /* constraint  */
  YYSYMBOL_digits_constraint = 166,        /* digits_constraint  */
  YYSYMBOL_delta_constraint = 167,         /* delta_constraint  */
  YYSYMBOL_derived_type_definition = 168,  /* derived_type_definition  */
  YYSYMBOL_private_type_kind = 169,        /* private_type_kind  */
  YYSYMBOL_range_constraint = 170,         /* range_constraint  */
  YYSYMBOL_range = 171,                    /* range  */
  YYSYMBOL_172_7 = 172,                    /* @7  */
  YYSYMBOL_enumeration_type_definition = 173, /* enumeration_type_definition  */
  YYSYMBOL_enum_identifier_list = 174,     /* enum_identifier_list  */
  YYSYMBOL_enum_identifier = 175,          /* enum_identifier  */
  YYSYMBOL_integer_type_definition = 176,  /* integer_type_definition  */
  YYSYMBOL_177_8 = 177,                    /* @8  */
  YYSYMBOL_real_type_definition = 178,     /* real_type_definition  */
  YYSYMBOL_floating_point_definition = 179, /* floating_point_definition  */
  YYSYMBOL_180_9 = 180,                    /* @9  */
  YYSYMBOL_range_specification_opt = 181,  /* range_specification_opt  */
  YYSYMBOL_range_specification = 182,      /* range_specification  */
  YYSYMBOL_range_constraint_opt = 183,     /* range_constraint_opt  */
  YYSYMBOL_fixed_point_definition = 184,   /* fixed_point_definition  */
  YYSYMBOL_record_type_definition = 185,   /* record_type_definition  */
  YYSYMBOL_record_definition = 186,        /* record_definition  */
  YYSYMBOL_component_list = 187,           /* component_list  */
  YYSYMBOL_component_declaration_list = 188, /* component_declaration_list  */
  YYSYMBOL_variant_part_opt = 189,         /* variant_part_opt  */
  YYSYMBOL_component_declaration = 190,    /* component_declaration  */
  YYSYMBOL_discriminant_part = 191,        /* discriminant_part  */
  YYSYMBOL_discriminant_spec_s = 192,      /* discriminant_spec_s  */
  YYSYMBOL_discriminant_spec = 193,        /* discriminant_spec  */
  YYSYMBOL_access_or_subtype_disc = 194,   /* access_or_subtype_disc  */
  YYSYMBOL_variant_part = 195,             /* variant_part  */
  YYSYMBOL_variant_s = 196,                /* variant_s  */
  YYSYMBOL_variant = 197,                  /* variant  */
  YYSYMBOL_array_type_definition = 198,    /* array_type_definition  */
  YYSYMBOL_unconstrained_array_definition = 199, /* unconstrained_array_definition  */
  YYSYMBOL_index_subtype_definition_list = 200, /* index_subtype_definition_list  */
  YYSYMBOL_index_subtype_definition = 201, /* index_subtype_definition  */
  YYSYMBOL_constrained_array_definition = 202, /* constrained_array_definition  */
  YYSYMBOL_discrete_range_list = 203,      /* discrete_range_list  */
  YYSYMBOL_component_definition = 204,     /* component_definition  */
  YYSYMBOL_aliased_opt = 205,              /* aliased_opt  */
  YYSYMBOL_aggregate = 206,                /* aggregate  */
  YYSYMBOL_value_s_2 = 207,                /* value_s_2  */
  YYSYMBOL_comp_assoc = 208,               /* comp_assoc  */
  YYSYMBOL_209_10 = 209,                   /* @10  */
  YYSYMBOL_choice_s = 210,                 /* choice_s  */
  YYSYMBOL_choice = 211,                   /* choice  */
  YYSYMBOL_discrete_range = 212,           /* discrete_range  */
  YYSYMBOL_discrete_with_range = 213,      /* discrete_with_range  */
  YYSYMBOL_interface_type_definition = 214, /* interface_type_definition  */
  YYSYMBOL_kind_opt = 215,                 /* kind_opt  */
  YYSYMBOL_interface_list_item_s = 216,    /* interface_list_item_s  */
  YYSYMBOL_interface_list_item_sl = 217,   /* interface_list_item_sl  */
  YYSYMBOL_access_type_definition = 218,   /* access_type_definition  */
  YYSYMBOL_access_type_definition_part = 219, /* access_type_definition_part  */
  YYSYMBOL_protected_opt = 220,            /* protected_opt  */
  YYSYMBOL_null_exclusion_opt = 221,       /* null_exclusion_opt  */
  YYSYMBOL_access_definition = 222,        /* access_definition  */
  YYSYMBOL_name = 223,                     /* name  */
  YYSYMBOL_compound_name = 224,            /* compound_name  */
  YYSYMBOL_direct_name = 225,              /* direct_name  */
  YYSYMBOL_operator_symbol_or_string = 226, /* operator_symbol_or_string  */
  YYSYMBOL_indexed_component = 227,        /* indexed_component  */
  YYSYMBOL_value_list = 228,               /* value_list  */
  YYSYMBOL_value = 229,                    /* value  */
  YYSYMBOL_selected_component = 230,       /* selected_component  */
  YYSYMBOL_231_11 = 231,                   /* $@11  */
  YYSYMBOL_attribute_reference = 232,      /* attribute_reference  */
  YYSYMBOL_attribute_id = 233,             /* attribute_id  */
  YYSYMBOL_expression = 234,               /* expression  */
  YYSYMBOL_expression_2 = 235,             /* expression_2  */
  YYSYMBOL_boolean_operator = 236,         /* boolean_operator  */
  YYSYMBOL_relation = 237,                 /* relation  */
  YYSYMBOL_range_or_name = 238,            /* range_or_name  */
  YYSYMBOL_membership = 239,               /* membership  */
  YYSYMBOL_simple_expression = 240,        /* simple_expression  */
  YYSYMBOL_unary_adding_operator = 241,    /* unary_adding_operator  */
  YYSYMBOL_binary_adding_operator = 242,   /* binary_adding_operator  */
  YYSYMBOL_term = 243,                     /* term  */
  YYSYMBOL_multiplying_operator = 244,     /* multiplying_operator  */
  YYSYMBOL_factor = 245,                   /* factor  */
  YYSYMBOL_primary = 246,                  /* primary  */
  YYSYMBOL_literal = 247,                  /* literal  */
  YYSYMBOL_relational_operator = 248,      /* relational_operator  */
  YYSYMBOL_qualified_expression = 249,     /* qualified_expression  */
  YYSYMBOL_parenthesized_primary = 250,    /* parenthesized_primary  */
  YYSYMBOL_cond_expression_list = 251,     /* cond_expression_list  */
  YYSYMBOL_cond_expression = 252,          /* cond_expression  */
  YYSYMBOL_253_12 = 253,                   /* @12  */
  YYSYMBOL_else_expression_opt = 254,      /* else_expression_opt  */
  YYSYMBOL_255_13 = 255,                   /* @13  */
  YYSYMBOL_allocator = 256,                /* allocator  */
  YYSYMBOL_number_declaration = 257,       /* number_declaration  */
  YYSYMBOL_258_14 = 258,                   /* @14  */
  YYSYMBOL_sequence_of_statements = 259,   /* sequence_of_statements  */
  YYSYMBOL_statement = 260,                /* statement  */
  YYSYMBOL_unlabeled_statement = 261,      /* unlabeled_statement  */
  YYSYMBOL_simple_statement = 262,         /* simple_statement  */
  YYSYMBOL_compound_statement = 263,       /* compound_statement  */
  YYSYMBOL_label = 264,                    /* label  */
  YYSYMBOL_null_statement = 265,           /* null_statement  */
  YYSYMBOL_assignement_statement = 266,    /* assignement_statement  */
  YYSYMBOL_267_15 = 267,                   /* @15  */
  YYSYMBOL_if_statement = 268,             /* if_statement  */
  YYSYMBOL_cond_clause_list = 269,         /* cond_clause_list  */
  YYSYMBOL_cond_clause = 270,              /* cond_clause  */
  YYSYMBOL_cond_part = 271,                /* cond_part  */
  YYSYMBOL_decision = 272,                 /* decision  */
  YYSYMBOL_273_16 = 273,                   /* @16  */
  YYSYMBOL_else_opt = 274,                 /* else_opt  */
  YYSYMBOL_case_statement = 275,           /* case_statement  */
  YYSYMBOL_case_hdr = 276,                 /* case_hdr  */
  YYSYMBOL_277_17 = 277,                   /* @17  */
  YYSYMBOL_alternative_list = 278,         /* alternative_list  */
  YYSYMBOL_alternative = 279,              /* alternative  */
  YYSYMBOL_280_18 = 280,                   /* @18  */
  YYSYMBOL_choice_s2 = 281,                /* choice_s2  */
  YYSYMBOL_choice2 = 282,                  /* choice2  */
  YYSYMBOL_loop_statement = 283,           /* loop_statement  */
  YYSYMBOL_label_opt = 284,                /* label_opt  */
  YYSYMBOL_statement_identifier = 285,     /* statement_identifier  */
  YYSYMBOL_iteration_opt = 286,            /* iteration_opt  */
  YYSYMBOL_iter_part = 287,                /* iter_part  */
  YYSYMBOL_288_19 = 288,                   /* @19  */
  YYSYMBOL_reverse_opt = 289,              /* reverse_opt  */
  YYSYMBOL_basic_loop = 290,               /* basic_loop  */
  YYSYMBOL_id_opt = 291,                   /* id_opt  */
  YYSYMBOL_block_body = 292,               /* block_body  */
  YYSYMBOL_block = 293,                    /* block  */
  YYSYMBOL_block_declaration = 294,        /* block_declaration  */
  YYSYMBOL_handled_statement_s = 295,      /* handled_statement_s  */
  YYSYMBOL_exception_handler_part_opt = 296, /* exception_handler_part_opt  */
  YYSYMBOL_exit_statement = 297,           /* exit_statement  */
  YYSYMBOL_name_opt = 298,                 /* name_opt  */
  YYSYMBOL_when_opt = 299,                 /* when_opt  */
  YYSYMBOL_simple_return_statement = 300,  /* simple_return_statement  */
  YYSYMBOL_extended_return_statement = 301, /* extended_return_statement  */
  YYSYMBOL_constant_opt = 302,             /* constant_opt  */
  YYSYMBOL_opt_handled_statement_s = 303,  /* opt_handled_statement_s  */
  YYSYMBOL_goto_statement = 304,           /* goto_statement  */
  YYSYMBOL_procedure_call_statement = 305, /* procedure_call_statement  */
  YYSYMBOL_subprogram_declaration = 306,   /* subprogram_declaration  */
  YYSYMBOL_subprogram_specification = 307, /* subprogram_specification  */
  YYSYMBOL_designator = 308,               /* designator  */
  YYSYMBOL_formal_part_opt = 309,          /* formal_part_opt  */
  YYSYMBOL_formal_part = 310,              /* formal_part  */
  YYSYMBOL_parameter_specification_list = 311, /* parameter_specification_list  */
  YYSYMBOL_parameter_specification = 312,  /* parameter_specification  */
  YYSYMBOL_mode = 313,                     /* mode  */
  YYSYMBOL_subprogram_specification_is_push = 314, /* subprogram_specification_is_push  */
  YYSYMBOL_subprogram_body = 315,          /* subprogram_body  */
  YYSYMBOL_316_20 = 316,                   /* $@20  */
  YYSYMBOL_declarative_part = 317,         /* declarative_part  */
  YYSYMBOL_decl_item_or_body_sl = 318,     /* decl_item_or_body_sl  */
  YYSYMBOL_decl_item_or_body = 319,        /* decl_item_or_body  */
  YYSYMBOL_decl_item_s = 320,              /* decl_item_s  */
  YYSYMBOL_decl_item_s1 = 321,             /* decl_item_s1  */
  YYSYMBOL_decl_item = 322,                /* decl_item  */
  YYSYMBOL_decl = 323,                     /* decl  */
  YYSYMBOL_body = 324,                     /* body  */
  YYSYMBOL_package_declaration = 325,      /* package_declaration  */
  YYSYMBOL_package_specification = 326,    /* package_specification  */
  YYSYMBOL_327_21 = 327,                   /* $@21  */
  YYSYMBOL_private_part_opt = 328,         /* private_part_opt  */
  YYSYMBOL_c_id_opt = 329,                 /* c_id_opt  */
  YYSYMBOL_package_body = 330,             /* package_body  */
  YYSYMBOL_331_22 = 331,                   /* $@22  */
  YYSYMBOL_body_opt = 332,                 /* body_opt  */
  YYSYMBOL_limited_opt = 333,              /* limited_opt  */
  YYSYMBOL_rename_declaration = 334,       /* rename_declaration  */
  YYSYMBOL_rename_unit = 335,              /* rename_unit  */
  YYSYMBOL_336_23 = 336,                   /* $@23  */
  YYSYMBOL_renames = 337,                  /* renames  */
  YYSYMBOL_name_or_character_literal = 338, /* name_or_character_literal  */
  YYSYMBOL_task_declaration = 339,         /* task_declaration  */
  YYSYMBOL_task_spec = 340,                /* task_spec  */
  YYSYMBOL_task_def = 341,                 /* task_def  */
  YYSYMBOL_interface_list_opt = 342,       /* interface_list_opt  */
  YYSYMBOL_task_private_opt = 343,         /* task_private_opt  */
  YYSYMBOL_task_body = 344,                /* task_body  */
  YYSYMBOL_protected_declaration = 345,    /* protected_declaration  */
  YYSYMBOL_protected_spec = 346,           /* protected_spec  */
  YYSYMBOL_protected_def = 347,            /* protected_def  */
  YYSYMBOL_protected_private_opt = 348,    /* protected_private_opt  */
  YYSYMBOL_protected_op_decl_s = 349,      /* protected_op_decl_s  */
  YYSYMBOL_protected_op_decl = 350,        /* protected_op_decl  */
  YYSYMBOL_protected_elem_decl_s = 351,    /* protected_elem_decl_s  */
  YYSYMBOL_protected_elem_decl = 352,      /* protected_elem_decl  */
  YYSYMBOL_protected_body = 353,           /* protected_body  */
  YYSYMBOL_protected_op_item_s = 354,      /* protected_op_item_s  */
  YYSYMBOL_protected_op_item = 355,        /* protected_op_item  */
  YYSYMBOL_entry_decl_s = 356,             /* entry_decl_s  */
  YYSYMBOL_entry_decl = 357,               /* entry_decl  */
  YYSYMBOL_entry_body = 358,               /* entry_body  */
  YYSYMBOL_entry_body_part = 359,          /* entry_body_part  */
  YYSYMBOL_rep_spec_s = 360,               /* rep_spec_s  */
  YYSYMBOL_entry_call = 361,               /* entry_call  */
  YYSYMBOL_accept_statement = 362,         /* accept_statement  */
  YYSYMBOL_accept_hdr = 363,               /* accept_hdr  */
  YYSYMBOL_entry_name = 364,               /* entry_name  */
  YYSYMBOL_delay_statement = 365,          /* delay_statement  */
  YYSYMBOL_select_statement = 366,         /* select_statement  */
  YYSYMBOL_select_wait = 367,              /* select_wait  */
  YYSYMBOL_guarded_select_alt = 368,       /* guarded_select_alt  */
  YYSYMBOL_or_select = 369,                /* or_select  */
  YYSYMBOL_select_alt = 370,               /* select_alt  */
  YYSYMBOL_delay_or_entry_alt = 371,       /* delay_or_entry_alt  */
  YYSYMBOL_async_select = 372,             /* async_select  */
  YYSYMBOL_timed_entry_call = 373,         /* timed_entry_call  */
  YYSYMBOL_cond_entry_call = 374,          /* cond_entry_call  */
  YYSYMBOL_stmts_opt = 375,                /* stmts_opt  */
  YYSYMBOL_abort_statement = 376,          /* abort_statement  */
  YYSYMBOL_compilation = 377,              /* compilation  */
  YYSYMBOL_378_24 = 378,                   /* $@24  */
  YYSYMBOL_compilation_unit = 379,         /* compilation_unit  */
  YYSYMBOL_private_opt = 380,              /* private_opt  */
  YYSYMBOL_context_specification = 381,    /* context_specification  */
  YYSYMBOL_with_clause = 382,              /* with_clause  */
  YYSYMBOL_with_list = 383,                /* with_list  */
  YYSYMBOL_file_name = 384,                /* file_name  */
  YYSYMBOL_use_clause_opt = 385,           /* use_clause_opt  */
  YYSYMBOL_unit = 386,                     /* unit  */
  YYSYMBOL_private_type_definition = 387,  /* private_type_definition  */
  YYSYMBOL_overriding_indicator_opt = 388, /* overriding_indicator_opt  */
  YYSYMBOL_overriding_indicator = 389,     /* overriding_indicator  */
  YYSYMBOL_use_clause = 390,               /* use_clause  */
  YYSYMBOL_name_list = 391,                /* name_list  */
  YYSYMBOL_body_stub = 392,                /* body_stub  */
  YYSYMBOL_subunit = 393,                  /* subunit  */
  YYSYMBOL_394_25 = 394,                   /* $@25  */
  YYSYMBOL_proper_body2 = 395,             /* proper_body2  */
  YYSYMBOL_exception_declaration = 396,    /* exception_declaration  */
  YYSYMBOL_exception_handler_part = 397,   /* exception_handler_part  */
  YYSYMBOL_exception_handler = 398,        /* exception_handler  */
  YYSYMBOL_exception_choice_s = 399,       /* exception_choice_s  */
  YYSYMBOL_exception_choice = 400,         /* exception_choice  */
  YYSYMBOL_raise_statement = 401,          /* raise_statement  */
  YYSYMBOL_402_26 = 402,                   /* @26  */
  YYSYMBOL_requeue_statement = 403,        /* requeue_statement  */
  YYSYMBOL_generic_declaration = 404,      /* generic_declaration  */
  YYSYMBOL_generic_formal_part = 405,      /* generic_formal_part  */
  YYSYMBOL_generic_formal = 406,           /* generic_formal  */
  YYSYMBOL_generic_discriminant_part_opt = 407, /* generic_discriminant_part_opt  */
  YYSYMBOL_subp_default = 408,             /* subp_default  */
  YYSYMBOL_generic_type_definition = 409,  /* generic_type_definition  */
  YYSYMBOL_generic_subp_inst = 410,        /* generic_subp_inst  */
  YYSYMBOL_generic_pkg_inst = 411,         /* generic_pkg_inst  */
  YYSYMBOL_generic_inst = 412,             /* generic_inst  */
  YYSYMBOL_aspect_clause = 413,            /* aspect_clause  */
  YYSYMBOL_enumeration_or_attribute_definition_clause = 414, /* enumeration_or_attribute_definition_clause  */
  YYSYMBOL_415_27 = 415,                   /* @27  */
  YYSYMBOL_record_representation_clause = 416, /* record_representation_clause  */
  YYSYMBOL_mod_clause_opt = 417,           /* mod_clause_opt  */
  YYSYMBOL_418_28 = 418,                   /* @28  */
  YYSYMBOL_component_clause = 419,         /* component_clause  */
  YYSYMBOL_component_clause_sl = 420,      /* component_clause_sl  */
  YYSYMBOL_component_clause_item = 421,    /* component_clause_item  */
  YYSYMBOL_422_29 = 422,                   /* @29  */
  YYSYMBOL_at_clause = 423,                /* at_clause  */
  YYSYMBOL_424_30 = 424                    /* @30  */
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
typedef yytype_int16 yy_state_t;

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
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
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
#define YYFINAL  8
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   2827

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  120
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  305
/* YYNRULES -- Number of rules.  */
#define YYNRULES  646
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1176

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   367


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
     114,   115,     2,     2,   116,     2,   119,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   117,   113,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,   118,     2,     2,     2,     2,     2,
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
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,   548,   548,   548,   552,   553,   556,   562,   566,   570,
     578,   624,   625,   628,   629,   633,   633,   637,   638,   641,
     642,   645,   646,   649,   649,   652,   653,   656,   682,   685,
     686,   690,   715,   725,   726,   736,   740,   748,   756,   759,
     755,   772,   773,   783,   784,   796,   797,   815,   833,   834,
     844,   862,   867,   874,   878,   882,   888,   892,   896,   900,
     904,   908,   914,   915,   916,   917,   921,   922,   932,   936,
     939,   939,   948,   963,   964,   965,   968,   969,   970,   971,
     972,   973,   980,   986,   992,   998,  1004,  1010,  1017,  1023,
    1027,  1028,  1029,  1033,  1047,  1057,  1064,  1074,  1084,  1085,
    1086,  1090,  1104,  1117,  1127,  1141,  1159,  1160,  1161,  1165,
    1175,  1184,  1191,  1191,  1207,  1217,  1218,  1229,  1230,  1234,
    1250,  1250,  1272,  1288,  1307,  1307,  1322,  1325,  1328,  1332,
    1335,  1343,  1354,  1370,  1380,  1388,  1394,  1402,  1406,  1413,
    1414,  1425,  1430,  1436,  1451,  1460,  1461,  1472,  1488,  1498,
    1502,  1509,  1510,  1513,  1522,  1523,  1526,  1537,  1538,  1548,
    1559,  1571,  1572,  1584,  1594,  1610,  1611,  1615,  1621,  1627,
    1637,  1644,  1651,  1658,  1667,  1667,  1679,  1689,  1690,  1699,
    1700,  1701,  1705,  1706,  1710,  1717,  1722,  1733,  1734,  1735,
    1736,  1737,  1741,  1744,  1754,  1755,  1768,  1778,  1784,  1790,
    1796,  1806,  1820,  1821,  1825,  1826,  1830,  1836,  1842,  1852,
    1867,  1868,  1869,  1870,  1871,  1872,  1877,  1878,  1885,  1889,
    1900,  1912,  1913,  1922,  1923,  1924,  1927,  1927,  1943,  1952,
    1962,  1971,  1985,  1986,  1987,  1988,  1989,  1993,  2010,  2012,
    2051,  2092,  2101,  2110,  2119,  2125,  2134,  2135,  2172,  2196,
    2197,  2200,  2205,  2213,  2231,  2232,  2256,  2262,  2270,  2276,
    2282,  2291,  2292,  2331,  2337,  2345,  2351,  2360,  2361,  2399,
    2422,  2453,  2454,  2455,  2456,  2459,  2460,  2461,  2464,  2472,
    2480,  2488,  2496,  2504,  2514,  2527,  2535,  2543,  2556,  2557,
    2567,  2567,  2582,  2585,  2585,  2600,  2608,  2608,  2623,  2629,
    2639,  2640,  2652,  2653,  2654,  2657,  2658,  2659,  2660,  2661,
    2662,  2663,  2664,  2665,  2666,  2669,  2670,  2671,  2672,  2673,
    2674,  2675,  2678,  2681,  2684,  2684,  2698,  2710,  2711,  2721,
    2733,  2736,  2736,  2770,  2771,  2780,  2792,  2792,  2803,  2804,
    2818,  2818,  2850,  2852,  2864,  2879,  2890,  2896,  2911,  2912,
    2915,  2919,  2923,  2930,  2941,  2941,  2962,  2963,  2966,  2976,
    2977,  2981,  2990,  3006,  3009,  3019,  3031,  3034,  3043,  3055,
    3056,  3059,  3060,  3066,  3074,  3091,  3112,  3113,  3116,  3117,
    3121,  3132,  3142,  3153,  3163,  3174,  3188,  3202,  3221,  3222,
    3228,  3229,  3232,  3239,  3240,  3249,  3267,  3268,  3269,  3270,
    3274,  3277,  3277,  3299,  3300,  3303,  3304,  3314,  3315,  3318,
    3319,  3322,  3323,  3332,  3333,  3334,  3335,  3338,  3339,  3340,
    3341,  3342,  3343,  3344,  3345,  3346,  3347,  3348,  3349,  3406,
    3407,  3408,  3409,  3412,  3413,  3418,  3417,  3446,  3447,  3456,
    3457,  3461,  3460,  3481,  3482,  3485,  3486,  3494,  3509,  3519,
    3524,  3534,  3534,  3556,  3567,  3578,  3596,  3600,  3601,  3606,
    3612,  3613,  3616,  3617,  3623,  3626,  3635,  3636,  3639,  3643,
    3650,  3651,  3654,  3658,  3659,  3662,  3663,  3666,  3667,  3668,
    3669,  3672,  3673,  3676,  3677,  3680,  3683,  3684,  3689,  3690,
    3691,  3692,  3695,  3696,  3701,  3703,  3707,  3708,  3711,  3712,
    3715,  3716,  3719,  3722,  3726,  3732,  3735,  3736,  3739,  3743,
    3749,  3753,  3757,  3761,  3767,  3770,  3771,  3774,  3775,  3778,
    3779,  3780,  3783,  3784,  3787,  3792,  3795,  3798,  3799,  3802,
    3809,  3810,  3810,  3811,  3819,  3828,  3839,  3840,  3843,  3844,
    3845,  3851,  3852,  3853,  3854,  3857,  3858,  3861,  3865,  3866,
    3869,  3873,  3877,  3881,  3885,  3886,  3890,  3897,  3902,  3905,
    3909,  3913,  3920,  3921,  3924,  3925,  3929,  3930,  3931,  3932,
    3936,  3935,  3946,  3950,  3954,  3955,  3960,  3969,  3971,  3981,
    3991,  4006,  4007,  4017,  4018,  4025,  4029,  4036,  4036,  4050,
    4054,  4060,  4071,  4083,  4087,  4097,  4098,  4099,  4112,  4124,
    4134,  4144,  4150,  4151,  4152,  4155,  4156,  4157,  4158,  4159,
    4165,  4169,  4175,  4176,  4177,  4178,  4179,  4180,  4181,  4182,
    4183,  4189,  4195,  4201,  4208,  4215,  4218,  4231,  4241,  4253,
    4258,  4259,  4260,  4264,  4264,  4278,  4294,  4297,  4297,  4306,
    4307,  4310,  4311,  4321,  4321,  4338,  4338
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
  "\"end of file\"", "error", "\"invalid token\"",
  "PAC_START_ELABORATION", "pac_package_name", "PAC_SPEC", "PAC_BODY",
  "PAC_DOT", "REF_COLON", "REF_DECL", "REF_BODY", "REF_REF", "REF_MODI",
  "ref_string", "ref_filename", "ref_identifier", "ref_character_literal",
  "ref_integer", "TIC", "DOT_DOT", "LT_LT", "BOX", "GT_GT", "IS_ASSIGNED",
  "RIGHT_SHAFT", "ABORT", "ABS", "ABSTRACT", "ACCEPT", "ACCESS", "ALIASED",
  "ALL", "AND", "ARRAY", "AT", "BEGiN", "BODY", "CASE", "CONSTANT",
  "DECLARE", "DELAY", "DELTA", "DIGITS", "DO", "ELSE", "ELSIF", "END",
  "ENTRY", "EXCEPTION", "EXIT", "FOR", "FUNCTION", "GENERIC", "GOTO", "IF",
  "IN", "INTERFACE", "IS", "LIMITED", "LOOP", "MOD", "NEW", "NOT", "NuLL",
  "OF", "OR", "OTHERS", "OUT", "OVERRIDING", "PACKAGE", "PRAGMA",
  "PRIVATE", "PROCEDURE", "PROTECTED", "RAISE", "RANGE", "RECORD", "REM",
  "RENAMES", "REQUEUE", "RETURN", "REVERSE", "SELECT", "SEPARATE",
  "SUBTYPE", "SYNCHRONIZED", "TAGGED", "TASK", "TERMINATE", "THEN", "TYPE",
  "UNTIL", "USE", "WHEN", "WHILE", "WITH", "XOR", "PLUS", "MINUS", "MULT",
  "DIV", "EXPON", "CONC", "EQUAL", "NE", "LT", "LT_EQ", "GT", "GE",
  "character_literal", "identifier", "string_literal", "numeric_literal",
  "';'", "'('", "')'", "','", "':'", "'|'", "'.'", "$accept",
  "goal_symbol", "$@1", "ref_xref_list", "ref_xref",
  "ref_identifier_or_string_or_character_literal", "ref_decl",
  "ref_body_opt", "ref_body_list", "ref_body", "$@2", "ref_modi_opt",
  "ref_ref_opt", "ref_modified_or_referenced_list", "ref_reference", "$@3",
  "ref_line_column_list", "ref_line_column", "pac_elaboration_order",
  "pac_package_list", "pac_package", "pac_name", "pac_dot_opt",
  "pac_spec_or_body", "pragma", "$@4", "$@5",
  "pragma_argument_association_list", "pragma_argument_association",
  "pragma_s", "object_declaration", "identifier_list", "identifier_rule",
  "new_ada_2005_reserved_words", "new_ada_95_reserved_words",
  "object_qualifier_opt", "object_subtype_definition", "init_opt", "@6",
  "type_declaration", "discriminant_part_opt", "type_definition",
  "type_completion", "subtype_declaration", "subtype_indication",
  "constraint", "digits_constraint", "delta_constraint",
  "derived_type_definition", "private_type_kind", "range_constraint",
  "range", "@7", "enumeration_type_definition", "enum_identifier_list",
  "enum_identifier", "integer_type_definition", "@8",
  "real_type_definition", "floating_point_definition", "@9",
  "range_specification_opt", "range_specification", "range_constraint_opt",
  "fixed_point_definition", "record_type_definition", "record_definition",
  "component_list", "component_declaration_list", "variant_part_opt",
  "component_declaration", "discriminant_part", "discriminant_spec_s",
  "discriminant_spec", "access_or_subtype_disc", "variant_part",
  "variant_s", "variant", "array_type_definition",
  "unconstrained_array_definition", "index_subtype_definition_list",
  "index_subtype_definition", "constrained_array_definition",
  "discrete_range_list", "component_definition", "aliased_opt",
  "aggregate", "value_s_2", "comp_assoc", "@10", "choice_s", "choice",
  "discrete_range", "discrete_with_range", "interface_type_definition",
  "kind_opt", "interface_list_item_s", "interface_list_item_sl",
  "access_type_definition", "access_type_definition_part", "protected_opt",
  "null_exclusion_opt", "access_definition", "name", "compound_name",
  "direct_name", "operator_symbol_or_string", "indexed_component",
  "value_list", "value", "selected_component", "$@11",
  "attribute_reference", "attribute_id", "expression", "expression_2",
  "boolean_operator", "relation", "range_or_name", "membership",
  "simple_expression", "unary_adding_operator", "binary_adding_operator",
  "term", "multiplying_operator", "factor", "primary", "literal",
  "relational_operator", "qualified_expression", "parenthesized_primary",
  "cond_expression_list", "cond_expression", "@12", "else_expression_opt",
  "@13", "allocator", "number_declaration", "@14",
  "sequence_of_statements", "statement", "unlabeled_statement",
  "simple_statement", "compound_statement", "label", "null_statement",
  "assignement_statement", "@15", "if_statement", "cond_clause_list",
  "cond_clause", "cond_part", "decision", "@16", "else_opt",
  "case_statement", "case_hdr", "@17", "alternative_list", "alternative",
  "@18", "choice_s2", "choice2", "loop_statement", "label_opt",
  "statement_identifier", "iteration_opt", "iter_part", "@19",
  "reverse_opt", "basic_loop", "id_opt", "block_body", "block",
  "block_declaration", "handled_statement_s", "exception_handler_part_opt",
  "exit_statement", "name_opt", "when_opt", "simple_return_statement",
  "extended_return_statement", "constant_opt", "opt_handled_statement_s",
  "goto_statement", "procedure_call_statement", "subprogram_declaration",
  "subprogram_specification", "designator", "formal_part_opt",
  "formal_part", "parameter_specification_list", "parameter_specification",
  "mode", "subprogram_specification_is_push", "subprogram_body", "$@20",
  "declarative_part", "decl_item_or_body_sl", "decl_item_or_body",
  "decl_item_s", "decl_item_s1", "decl_item", "decl", "body",
  "package_declaration", "package_specification", "$@21",
  "private_part_opt", "c_id_opt", "package_body", "$@22", "body_opt",
  "limited_opt", "rename_declaration", "rename_unit", "$@23", "renames",
  "name_or_character_literal", "task_declaration", "task_spec", "task_def",
  "interface_list_opt", "task_private_opt", "task_body",
  "protected_declaration", "protected_spec", "protected_def",
  "protected_private_opt", "protected_op_decl_s", "protected_op_decl",
  "protected_elem_decl_s", "protected_elem_decl", "protected_body",
  "protected_op_item_s", "protected_op_item", "entry_decl_s", "entry_decl",
  "entry_body", "entry_body_part", "rep_spec_s", "entry_call",
  "accept_statement", "accept_hdr", "entry_name", "delay_statement",
  "select_statement", "select_wait", "guarded_select_alt", "or_select",
  "select_alt", "delay_or_entry_alt", "async_select", "timed_entry_call",
  "cond_entry_call", "stmts_opt", "abort_statement", "compilation", "$@24",
  "compilation_unit", "private_opt", "context_specification",
  "with_clause", "with_list", "file_name", "use_clause_opt", "unit",
  "private_type_definition", "overriding_indicator_opt",
  "overriding_indicator", "use_clause", "name_list", "body_stub",
  "subunit", "$@25", "proper_body2", "exception_declaration",
  "exception_handler_part", "exception_handler", "exception_choice_s",
  "exception_choice", "raise_statement", "@26", "requeue_statement",
  "generic_declaration", "generic_formal_part", "generic_formal",
  "generic_discriminant_part_opt", "subp_default",
  "generic_type_definition", "generic_subp_inst", "generic_pkg_inst",
  "generic_inst", "aspect_clause",
  "enumeration_or_attribute_definition_clause", "@27",
  "record_representation_clause", "mod_clause_opt", "@28",
  "component_clause", "component_clause_sl", "component_clause_item",
  "@29", "at_clause", "@30", YY_NULLPTR
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
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,    59,    40,    41,    44,    58,   124,    46
};
#endif

#define YYPACT_NINF (-955)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-641)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    1088,  -955,  -955,  -955,    80,   800,  -955,    97,  -955,   134,
    -955,  -955,  1106,    98,   259,   854,   274,  1088,   907,   363,
     418,   398,  -955,  -955,  -955,  1017,   482,   425,   112,  -955,
     956,   538,  -955,   536,  -955,   418,  -955,   557,   570,  1152,
     641,   274,   617,   112,   112,  1075,  -955,  -955,   633,   840,
    -955,  -955,   686,  -955,    93,   644,  -955,  -955,  -955,   679,
    -955,  -955,  -955,   610,  -955,  -955,  -955,   867,   696,  -955,
     956,  -955,   702,   805,   807,  -955,  -955,   557,  -955,   557,
    -955,    99,   230,    99,  -955,  -955,  -955,  -955,   814,  -955,
    -955,   107,   661,  -955,  -955,  -955,   447,  -955,  -955,  -955,
    -955,  -955,   437,   437,  1511,   721,   737,  -955,   759,  -955,
    -955,  -955,  -955,   857,   641,  -955,  1094,   877,   112,   866,
     870,  -955,  -955,  -955,  -955,  -955,  -955,  -955,  -955,  -955,
    -955,  -955,   112,  -955,   112,  -955,   112,    36,   112,  -955,
     274,   317,   112,    32,  2470,   778,   317,   112,   317,   112,
      73,   663,  -955,   978,  -955,   789,   796,   802,  -955,  -955,
    -955,  -955,   702,  -955,  -955,   891,  -955,   807,  -955,   557,
    -955,   447,  -955,  1184,  -955,   837,  -955,  -955,   992,   810,
     224,  -955,  1010,   481,   801,   568,   634,  1237,  -955,  1152,
     545,   310,   240,   838,   905,  -955,  1184,  1184,  -955,   880,
    -955,  -955,  -955,  -955,  -955,  -955,  -955,  -955,  -955,  1184,
     721,  1184,   737,  -955,  -955,  -955,  -955,   721,    99,   970,
    -955,  -955,  -955,  -955,  -955,   881,  -955,  -955,  -955,  -955,
      52,   893,   146,   847,   411,  -955,   633,   352,   607,   893,
    -955,   914,   888,   230,   890,   176,    42,   112,   214,   112,
    -955,  -955,  1011,  -955,  -955,  -955,  -955,  -955,  -955,  2517,
    -955,  -955,  -955,  -955,  -955,  -955,  -955,  -955,  -955,   918,
    -955,  -955,   924,  -955,   610,  -955,  -955,  -955,  -955,  -955,
    -955,  -955,  -955,  -955,   -11,    40,   931,   914,   927,   230,
     447,   950,   317,   112,   965,   112,   543,  -955,  -955,  -955,
     274,  1064,   807,  -955,   437,  1096,  -955,  -955,   963,  1184,
     974,  -955,   310,  -955,  1070,   310,   656,  1184,  -955,   310,
     745,  -955,  1184,  1003,  -955,  -955,  -955,  -955,  -955,  -955,
    -955,  -955,  -955,  1041,  -955,  1049,   437,  -955,  -955,  -955,
     112,  -955,  -955,  -955,  -955,  -955,  -955,  1214,  -955,   691,
     737,   643,  -955,  -955,   857,  -955,  -955,   230,  2526,  -955,
    -955,   447,  -955,  -955,  -955,   893,   112,  1043,  -955,  -955,
    -955,  1021,  -955,   417,  -955,   112,   112,   112,  1081,  1111,
     112,   112,  1113,  1063,    57,  1144,  -955,  -955,  -955,   359,
    1068,  -955,  1082,  1084,    90,  -955,  1126,   958,  -955,   230,
    1074,  1145,   237,  1091,  -955,  1149,  -955,   252,  -955,  -955,
    -955,  -955,  1114,  1184,  -955,   437,  -955,  -955,  -955,  1184,
    -955,  -955,  -955,  1108,  -955,   470,  -955,  1154,  1057,   643,
    -955,  -955,   310,  -955,  -955,  2573,   447,   357,   284,   351,
    1157,  2582,  -955,   610,   991,  -955,   522,  -955,   252,  -955,
      65,   256,  1174,  1063,  1177,  -955,   272,  1185,  1063,  1177,
    -955,   278,  1186,  -955,  1203,  1221,   320,   496,  2437,  1202,
     514,  -955,  -955,  -955,  1136,  1059,   552,  -955,  2691,  -955,
     447,  1191,  -955,   491,  -955,   447,  -955,  -955,  1190,  -955,
    1232,  1228,   786,  1184,  -955,  -955,   437,   437,  -955,  1143,
    -955,  -955,  1144,   112,   112,   112,  2625,  1219,  -955,   483,
    1224,  1231,  1236,  -955,  -955,  -955,  -955,   687,  -955,   112,
    -955,  -955,  -955,  1242,  1184,  1194,  1201,  1081,   230,  -955,
    1222,  1173,  1485,  1113,  -955,  1175,  2713,  1187,  -955,  -955,
    -955,  1188,  1183,   490,  -955,  -955,  -955,  -955,  1228,   112,
     230,   112,  -955,   937,   130,   230,  -955,  1189,   157,   230,
     784,   449,  -955,   367,  1192,  1870,  -955,  -955,  -955,  -955,
    2437,  -955,  -955,  -955,  -955,  -955,  -955,   894,  1193,  -955,
    -955,  -955,  -955,  -955,  -955,  -955,  -955,    29,  -955,  -955,
    -955,  -955,  -955,  -955,  -955,  -955,  -955,   174,  1210,  -955,
     252,   112,  -955,   533,  1282,  1283,   886,  1286,  1258,  -955,
    1303,  1269,  1268,  -955,  1309,  -955,  1270,  -955,  -955,  1280,
    -955,  1317,  1267,  -955,  1234,   230,  1235,  -955,  -955,   447,
     178,  -955,  -955,   380,  -955,  1184,  1184,  -955,  -955,  -955,
    -955,   437,  1184,  -955,  -955,  1304,   321,  1292,  1295,  -955,
      78,   112,   112,  1296,  2638,  -955,  1184,  1298,   315,   659,
    1247,  1248,   274,  1008,  -955,    87,   447,  1336,   230,  -955,
    1249,  1144,  -955,   274,   718,  -955,   715,  1184,  -955,  -955,
     459,   219,  -955,  -955,  -955,  -955,  -955,  -955,  -955,  -955,
    -955,  -955,  -955,  -955,  -955,   758,  -955,  -955,  1184,  -955,
    1208,  1250,  1251,  -955,  1343,   972,  -955,  1253,  1184,  1184,
    1259,   447,  1281,   567,  1146,  -955,  2437,  -955,  -955,   450,
     558,  -955,  1256,   793,  1263,  -955,   604,  -955,  1611,  1714,
    1331,  -955,  -955,  1289,  -955,  -955,  1299,  -955,  -955,  1299,
    -955,     5,  2638,  -955,  -955,  1320,  1301,  1144,  -955,  2437,
    -955,  1275,  -955,  -955,  1232,  -955,  -955,  -955,  1268,  -955,
    1347,  -955,  -955,  -955,  -955,  -955,  -955,  1276,   272,  1361,
     292,  -955,  -955,  -955,   614,  -955,   786,  1184,   230,  -955,
     727,   447,   742,   742,   683,    78,  1194,  1201,  1312,   633,
    -955,  1340,  1342,  -955,   856,  -955,   436,  1354,   349,  -955,
    -955,  -955,  -955,   174,  1293,  -955,  -955,  -955,   687,  -955,
     230,  -955,  -955,  -955,  1302,  1356,  -955,  -955,  1362,  -955,
    -955,  1366,  -955,   791,  1268,  -955,   409,  1184,  1184,  -955,
    -955,  -955,  -955,  1078,  -955,  1338,  -955,  -955,   899,   166,
    -955,  -955,  1103,  -955,  1105,  -955,  1164,  -955,  -955,  -955,
    -955,  1184,  -955,   525,  1310,  -955,  -955,  1311,  -955,  2437,
    -955,  1370,  1803,  -955,  -955,  1392,  -955,  1384,  -955,  -955,
    1402,  1434,   819,  -955,  1339,   839,  1404,  1184,   196,  -955,
    -955,  -955,    64,  -955,  -955,   112,  -955,  2437,   174,  -955,
    1208,  1381,  1388,  -955,  -955,  -955,  1415,  -955,   242,   230,
    -955,   272,   272,  -955,   835,   517,  -955,   437,   447,  1074,
    1074,  -955,  -955,  -955,  -955,  1325,  -955,  2638,  -955,  1184,
    -955,  1363,  -955,  1327,  1329,   274,   457,   447,   657,  -955,
     174,  1334,   174,   274,  -955,  1399,  -955,  -955,  -955,  1184,
    -955,   579,   437,  -955,   219,  -955,   245,  -955,   396,  1383,
     230,  1386,  1208,  1035,  1095,  1333,  -955,  -955,  -955,  -955,
    2122,  -955,  1397,  1184,  1344,  -955,   252,   673,  2437,  1413,
     882,  1409,  2437,   908,  -955,  1346,   447,   228,  -955,  1089,
    1419,  -955,  1410,  2185,  1353,  -955,  1121,   174,   174,  -955,
    1372,  1436,  -955,  -955,  1074,  1074,  1355,  1395,  -955,  -955,
     944,  1184,  1359,  -955,   114,  1389,  -955,  1107,  -955,  -955,
    -955,  -955,  1365,  1364,   718,   174,   274,  1401,  -955,  -955,
    -955,   112,  1368,  1439,  -955,  -955,  -955,  1459,  -955,   620,
    1459,  -955,  -955,  -955,  1377,   973,  -955,  1232,  1714,  -955,
    2248,  2311,  -955,  1412,  2374,  -955,   196,  2437,   196,  -955,
    -955,   437,   266,  -955,  1378,  -955,  1433,  -955,  1382,  1385,
     909,  1416,  -955,  1387,   252,  -955,   997,  -955,  1208,  -955,
    1459,  1208,  1390,  -955,  1451,  -955,  -955,  1445,  -955,  1429,
     650,  -955,   274,  -955,  -955,   252,  1489,  -955,  -955,  -955,
    1463,  -955,  1435,  1469,  1405,  1440,   286,  1933,  -955,  2437,
    1089,  -955,  -955,  -955,  -955,  -955,  -955,   252,  -955,  -955,
    1184,  1406,   510,  1232,  1411,  -955,  -955,   274,  -955,  -955,
    -955,  -955,  1228,  2437,  1417,  1420,  1442,  -955,  1421,  2437,
    1996,  -955,  -955,  1426,  1074,  2638,  -955,  -955,  1427,  1074,
     167,   274,  -955,  1483,  -955,  -955,  1428,  -955,  2059,  -955,
    1450,  1144,  -955,  1437,   310,   454,  -955,  1452,  -955,  -955,
    1500,  -955,   311,  1512,  -955,  -955,   510,   174,  -955,  1438,
    -955,  1443,   245,  -955,  -955,  -955
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       0,     8,     7,     9,     0,   530,     4,     0,     1,     0,
       5,    45,   531,     0,    11,     0,   533,     0,   536,     0,
       0,    17,    37,    38,    46,     0,     0,   537,     0,   532,
     558,   536,   548,     0,    15,    12,    13,     0,    19,     0,
       0,   530,     0,     0,     0,     0,    50,   216,   547,     0,
     545,   593,     0,   561,     0,     0,   552,   553,   550,     0,
     551,   556,    45,     0,   559,   554,   555,     0,     0,   540,
     558,   548,   538,     0,     0,    14,    23,    18,    21,     0,
       6,     0,     0,     0,   277,   256,   257,   276,    50,   219,
     275,     0,     0,    41,   218,   285,   272,   210,   214,   211,
     212,   213,    43,   238,   246,     0,   254,   261,   267,   271,
     215,   274,   273,    33,    28,    29,     0,   531,     0,     0,
       0,    56,    57,    53,    54,    58,    59,    55,    60,    61,
      51,    52,     0,   541,     0,   560,     0,     0,     0,   433,
     535,     0,     0,   451,     0,     0,     0,     0,     0,     0,
       0,     0,   595,     0,    48,     0,     0,     0,   601,   594,
     434,    45,   539,   549,    10,     0,    16,     0,    22,    20,
     269,   295,   268,     0,   331,   277,   181,   185,     0,   224,
       0,   177,   180,   272,     0,   179,   237,   246,    39,     0,
       0,     0,   226,   241,   242,   243,     0,     0,   251,     0,
     258,   259,   260,   278,   279,   280,   281,   282,   283,     0,
       0,     0,   253,   265,   266,   263,   264,     0,     0,     0,
      32,    30,    35,    36,    31,     0,   543,   542,   217,   546,
       0,   435,     0,     0,     0,   389,   388,   390,   390,   400,
     382,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     416,   417,     0,   419,   420,   418,   421,   429,   401,     0,
     405,   408,   413,   407,   422,   430,   426,   449,   423,     0,
     431,   424,     0,   432,     0,   414,   428,   425,   427,   415,
     630,   631,   632,   383,   390,     0,   388,     0,   602,     0,
     564,     0,     0,     0,   605,     0,   396,   591,   596,   592,
     534,     0,    24,    25,    44,   292,   288,   290,     0,     0,
       0,   168,     0,   167,   174,     0,     0,     0,   184,     0,
       0,   286,     0,     0,    42,   235,   233,   234,   236,   232,
     231,   284,   224,     0,   221,   179,   237,   230,   228,   229,
       0,   244,   245,   240,   239,   252,   249,   250,   248,     0,
     255,   247,   262,   270,    33,   544,   441,     0,     0,   628,
     458,   457,   456,   450,   570,     0,     0,     0,   391,   386,
     626,     0,   384,     0,   385,     0,     0,     0,     0,     0,
       0,     0,   462,    73,    62,     0,   406,   459,   469,   451,
       0,   435,     0,     0,     0,   603,     0,     0,   562,     0,
     390,     0,     0,     0,    49,   397,   398,     0,    27,    26,
     293,   331,     0,     0,   330,   332,   171,   173,   176,     0,
     178,   180,   179,   111,   109,   272,   172,   277,     0,   110,
      40,   220,     0,   227,    34,     0,   629,     0,     0,     0,
     437,     0,   411,     0,   558,   627,     0,   393,     0,   452,
     633,     0,     0,    73,   464,   470,     0,     0,    73,   464,
     460,     0,    90,    74,    63,    64,     0,     0,     0,     0,
     400,   454,   453,   455,     0,     0,     0,   145,   106,   563,
     565,     0,   607,     0,   608,   606,   598,   399,     0,   149,
      69,     0,    95,     0,   289,   287,   291,   175,   112,     0,
     169,   222,   443,     0,     0,     0,     0,     0,   412,     0,
       0,     0,     0,   572,   573,   574,   575,     0,   571,     0,
     392,   387,   645,   636,     0,   441,    45,     0,     0,   475,
       0,     0,     0,   462,    45,     0,   445,     0,    65,   296,
     576,     0,     0,    69,    66,    68,   154,   155,     0,     0,
       0,     0,   336,     0,     0,     0,   331,     0,     0,     0,
       0,     0,   304,     0,   210,     0,   298,   300,   302,   303,
       0,   305,   306,   315,   316,    45,   317,   351,     0,   318,
     361,   307,   308,   321,   309,   310,   319,     0,   311,   320,
     510,   511,   512,   513,   312,   313,   314,     0,     0,   604,
       0,     0,   144,   106,     0,     0,   446,     0,     0,   190,
       0,   108,   445,   189,     0,   623,     0,   618,   625,     0,
     619,     0,     0,   622,     0,     0,     0,   610,   611,   609,
       0,    70,   395,     0,   148,     0,     0,    94,    99,   100,
      98,   294,     0,   170,   444,     0,     0,     0,     0,   438,
       0,     0,     0,     0,     0,   394,     0,     0,     0,     0,
       0,     0,   486,   558,   471,     0,   194,   473,     0,    93,
       0,     0,   461,   492,   500,    75,   106,     0,   124,   120,
     445,     0,    92,    87,   128,    76,    77,    78,   122,   119,
     123,    86,    79,    89,    80,     0,    88,    72,     0,   448,
       0,     0,     0,    67,     0,     0,   506,   390,     0,     0,
       0,   370,   371,     0,   333,   327,     0,   323,   585,     0,
       0,   373,   218,     0,     0,   331,     0,   502,     0,     0,
       0,   517,   515,     0,   324,   381,     0,   299,   365,   367,
     301,     0,     0,   354,   331,     0,   356,     0,   349,     0,
     503,     0,   360,   568,    69,   146,   107,   108,   445,   624,
     615,   617,   614,   205,   613,   446,   621,     0,     0,   192,
       0,   196,   557,   597,   629,   600,    97,     0,     0,   203,
       0,   206,   129,   129,     0,     0,     0,     0,     0,   440,
     436,     0,     0,   400,     0,   637,     0,     0,     0,   641,
     634,   567,   569,     0,     0,   489,    45,   488,     0,   491,
       0,   465,   481,   480,     0,     0,   476,   477,     0,   479,
     566,     0,    45,   466,   445,    81,     0,     0,     0,    84,
      85,   118,   117,     0,   115,     0,    45,   133,     0,     0,
     182,   183,     0,   157,     0,   161,   272,    47,   447,   322,
     529,     0,   505,     0,     0,   508,   331,     0,   380,     0,
     331,     0,     0,   587,   586,     0,   589,   376,   374,   521,
       0,     0,   523,   519,   520,   333,     0,     0,     0,   577,
     578,   340,     0,   338,   364,     0,   352,     0,     0,   357,
       0,     0,     0,   402,   147,   620,     0,   612,   103,     0,
     186,     0,     0,   197,     0,     0,    96,    71,   207,   390,
     390,   130,   102,   101,   113,     0,    45,     0,   646,     0,
     643,     0,   642,     0,   390,   487,     0,   195,     0,   478,
       0,     0,     0,   493,    45,     0,    45,    82,    83,     0,
     131,   126,   121,   114,     0,   135,     0,   297,     0,     0,
       0,     0,     0,     0,   218,     0,   337,   509,   372,   368,
       0,   328,     0,     0,     0,   377,     0,     0,     0,     0,
       0,     0,     0,     0,   584,   218,   583,     0,   581,     0,
       0,   339,     0,     0,     0,   353,   272,     0,     0,   616,
       0,   193,   199,   198,   390,   390,     0,     0,   208,   442,
       0,     0,     0,   485,     0,     0,   490,     0,   484,   483,
     482,   472,   390,     0,   500,     0,   501,   126,   125,   127,
     116,     0,     0,     0,    45,   139,    45,   165,   158,     0,
     165,   162,   159,   507,     0,     0,   590,    69,     0,   516,
       0,     0,   518,     0,     0,   325,     0,     0,     0,   346,
     345,   344,     0,   342,     0,   355,     0,   347,     0,     0,
       0,     0,   200,     0,     0,   638,     0,   635,     0,   331,
     165,     0,     0,   468,   467,   463,   132,     0,    45,     0,
       0,   136,   137,   166,   156,     0,     0,   160,   326,   588,
     378,   520,     0,     0,     0,     0,     0,     0,   582,     0,
       0,   335,   358,   362,   504,   104,   105,     0,   599,   209,
       0,     0,     0,    69,     0,   494,    45,   138,   134,   140,
      45,   163,     0,     0,     0,     0,     0,   514,     0,     0,
       0,   343,   201,     0,   390,     0,   498,   496,     0,   390,
       0,   142,   164,     0,   375,   526,     0,   524,     0,   644,
       0,     0,   143,     0,     0,     0,   151,     0,   525,   331,
       0,   495,     0,     0,   152,   379,     0,     0,    45,     0,
     497,     0,     0,   150,   499,   153
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -955,  -955,  -955,  1531,   478,  -955,  -955,  -955,  -955,  1517,
    -955,  -955,  -955,  1480,     9,  -955,  -955,   -96,  -955,  -955,
    1446,  -955,  1207,  -955,    -5,  -955,  -955,  -955,  1374,   -10,
    -955,   -61,     2,  -955,  -955,  -955,  -955,  -509,  -955,  -955,
     492,  -955,  -955,  -955,  -379,   788,  -955,  -955,  -478,  -955,
    -166,  -196,  -955,  -955,  -955,   623,  -955,  -955,  -955,  -955,
    -955,   553,  -501,   795,  -955,  -623,   511,   404,  -955,  -955,
    -884,  1291,  -955,   979,  -434,   501,  -955,   427,  -419,  -955,
    -955,   635,  -955,  -955,  -954,  -955,  -955,  -955,  1492,  -955,
     430,  1271,  -829,  -305,  1051,  -955,   690,   692,  1053,  -955,
     822,  -446,  -529,   559,    38,   703,  1418,  -955,  1273,   -53,
    -955,  -955,  -955,  -955,   -89,    -8,  1491,  1029,  -955,  -955,
     -87,  -955,  -955,   -20,  -955,  1379,   -65,  -955,  -955,  -955,
    -117,  -955,  1200,  -955,  -955,  -955,  -955,  -955,  -955,  -453,
    -505,  -955,  -955,  -955,  -955,  -955,  -955,  -955,  -955,  -955,
     738,  -528,  -702,  -955,   726,  -955,  -955,  -955,  -955,   720,
    -955,  -955,   504,  -955,  -955,  -955,  -955,   616,  -955,  -955,
    -955,  -696,  -482,  -955,  -955,  -724,  -955,  -955,  -955,  -955,
    -955,  -955,  -955,  -955,  -955,  1060,   412,   -55,   -78,  -231,
    -955,  -955,   -45,  -955,  -416,   -21,  -955,  -430,  -955,  1367,
    1116,  -955,  -304,  -955,  -955,   594,  1557,  -955,  -955,   843,
      -6,  -955,  -955,  -485,  -955,   672,  -955,  -157,  -955,  -955,
    -955,  1092,  1170,  -955,  1196,  -955,  -955,  1110,  -955,  -955,
     704,  -955,  -955,  1197,  -955,  -955,   699,  -645,  -955,   468,
     621,  -955,  -521,  -955,  -955,  -522,  -955,  -955,   668,  -955,
     675,  -955,  -955,  -955,  -955,  -682,  -955,  1602,  -955,  -955,
    1614,  -955,  1616,   283,  1515,  1581,  1583,  -486,   -27,  -955,
     250,  -248,  -955,  -955,  -955,  -955,  -955,  -955,   915,   611,
     619,  -955,  -955,  -955,   681,  -955,  -955,  -955,  -955,  -955,
    -955,  -955,  -178,  -630,  -955,  -955,  -955,  -955,  -955,  -955,
    -955,   864,  -955,  -955,  -955
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     4,    17,     5,     6,     7,    14,    21,    35,    36,
      74,    38,    80,    77,    78,   167,   302,   166,    41,   114,
     115,   116,   220,   224,   562,    39,   323,    92,    93,   662,
     251,   252,    94,   130,   131,   467,   543,   632,   777,   253,
     462,   682,   537,   254,   489,   637,   638,   639,   615,   616,
     640,   177,   642,   685,   833,   834,   686,   828,   687,   688,
     827,  1018,  1019,   912,   690,   691,   837,  1023,  1024,  1081,
    1025,   463,   476,   477,   490,  1026,  1155,  1156,   545,   546,
     842,   843,   547,   844,  1084,  1085,    95,   178,   332,   419,
     180,   181,   845,   182,   618,   619,   900,   665,   620,   771,
     780,   491,   634,    96,   236,    97,    98,    99,   333,   334,
     100,   340,   101,   330,   335,   336,   196,   103,   348,   209,
     104,   105,   210,   106,   217,   107,   108,   109,   211,   110,
     111,   305,   306,   413,   412,   493,   112,   255,   698,   871,
     566,   567,   568,   569,   570,   571,   572,   877,   573,   714,
     715,   307,   308,   309,   861,   574,   575,   708,   882,   883,
     979,  1052,  1053,   576,   577,   578,   745,   746,   885,   890,
     888,   751,   469,   579,   747,   580,   738,   581,   712,   857,
     582,   583,   966,  1124,   584,   585,   256,   389,   752,   367,
     368,   446,   447,   407,   144,   257,   385,   258,   259,   260,
     440,   441,   261,   262,   263,   264,    59,   358,   507,   790,
     265,   435,   645,   622,   266,   267,   241,   233,   362,   268,
     269,   460,   529,   935,   270,   271,   272,   455,   815,   667,
     816,   928,  1010,   273,   663,   806,   674,   817,   807,  1137,
     823,   728,   586,   587,   707,   588,   589,   590,   731,   875,
     732,   733,   591,   592,   593,   872,   594,    12,    18,    29,
      30,    31,    32,    49,    50,    72,    62,   623,   274,    64,
     275,   291,   276,    65,   444,   518,   277,   739,   879,   977,
     978,   595,   963,   596,   278,    67,   159,   396,   403,   624,
     145,    68,   370,   279,   280,   524,   281,   658,   919,   797,
     798,   799,  1001,   282,   656
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
      11,    16,   185,    63,   187,   502,   153,   369,   143,    57,
     421,    24,   155,   346,   521,   565,   170,   318,   172,   703,
     644,   548,   156,   870,    60,   892,    69,   509,   716,   822,
      47,   102,   621,   809,   701,   689,    11,   819,   184,   730,
     729,   397,   886,    63,  1008,    47,    47,   873,   874,    57,
     696,   695,   140,   359,   442,   369,    47,   829,   683,   617,
     737,   985,   152,   237,    60,   740,    48,   232,   284,   154,
     287,   303,   749,   331,    45,     9,  1087,   531,   376,    45,
       8,    48,    48,   186,   371,   212,   168,   464,   544,   239,
     621,    45,   137,   231,    45,   465,   294,   391,   881,   522,
      45,   654,   671,   366,   187,   466,    13,   923,    45,   356,
     980,   474,    19,    45,   232,    45,  1113,   692,   232,   810,
      47,   424,   349,  1031,   351,   759,   766,   390,   392,   136,
     393,    45,   377,    81,   228,    24,    47,   508,    47,   250,
      47,   523,   750,    47,    47,   240,   154,    45,    47,    47,
      47,   300,   378,   353,   958,   132,    48,   881,    45,   132,
      82,   174,    84,   289,   743,   304,   754,    45,    82,    83,
     175,   132,    48,   176,   230,    45,   234,    45,   168,    45,
     238,   102,   811,    46,    89,   285,   286,   445,    46,   821,
     350,  -439,   984,   936,   830,   695,  1119,    45,   825,   331,
      46,   937,   442,    46,    85,    86,   409,  -205,    87,    46,
      89,    90,   375,    91,   400,    45,    87,    46,    89,    90,
      45,    91,    46,  -369,    46,   187,   422,    82,   187,   948,
     349,    45,   187,   187,  1011,   429,  1013,     9,    45,  1111,
      46,    89,  1114,  -369,    15,   894,    45,    47,   314,   379,
     380,   383,  1047,    45,   250,   360,    46,    89,   482,   417,
    1154,   154,   974,   862,   483,   891,   426,    46,    89,    20,
     718,   337,   895,    45,   899,    87,    46,    89,    90,    45,
      91,  -204,  1021,   137,    46,   235,    46,  -359,    46,    89,
    1099,  1058,  1059,    45,    47,    24,   565,   404,   819,   535,
     484,   415,   705,   626,   381,   153,    46,    89,  1022,   541,
    1129,    45,   884,   525,   488,     9,    45,   158,    45,  1075,
     504,   840,   163,   901,    46,   940,   119,   120,   831,    46,
     902,   443,   716,   475,   530,  1168,    81,  -192,   938,   695,
      46,    89,   315,  -202,     9,   187,  1048,    46,    89,   338,
      45,    89,    45,   250,   530,    46,  1091,   737,    45,  1093,
     154,  -639,    46,    89,  -202,   779,   737,  1112,   154,   822,
     684,    82,    83,    84,   377,   132,   176,    47,   786,   501,
      33,    45,    46,    89,  1100,   190,   702,   505,    46,   898,
     734,   903,   654,   503,   378,  -640,   154,    45,   232,  1143,
     475,   225,    46,    89,  1048,   496,   960,    85,    86,   365,
      37,   497,   163,   451,   443,  -277,   470,   517,   778,    87,
      46,    89,    90,   513,    91,    46,    89,    46,   235,   315,
     250,  -202,    34,   540,   983,   190,   250,   154,   514,    47,
     132,   381,    56,   154,   936,  1038,   729,  1041,  1038,   729,
      45,   939,  -202,   779,   190,   737,  -277,  1166,   153,    46,
      89,    46,   653,   154,   710,   190,   366,    46,   190,   193,
     920,  1171,   240,  -277,   655,   137,   852,   551,   737,   443,
     735,   191,    56,    10,   317,   641,   192,   671,   316,   553,
      46,    89,    45,  -277,  -277,  -277,  -277,    45,  -277,   316,
    1163,   250,   194,    10,   841,    47,    46,    89,   154,   450,
     242,   840,   627,   631,   793,  1040,   659,   765,    45,  1044,
      44,   154,   992,   993,   673,  -204,   364,   250,  1090,   542,
     132,   191,  1037,   195,   154,   737,   192,   724,   996,   737,
     475,   646,   725,    81,    73,   863,   244,  1154,   782,   783,
     191,   704,   723,    42,   628,   192,   317,   193,   488,    46,
      89,   191,   722,   864,   191,   741,   192,  1135,   232,   192,
    1006,    76,   -91,   840,   325,   357,   190,    43,    82,    83,
      84,    79,   956,   176,   191,   190,   326,   327,   826,   192,
     194,   756,   737,  1142,  1097,   191,    26,   598,   405,    47,
     192,    46,    89,   154,  1138,   328,    46,    89,     9,    27,
     406,   193,   814,   349,    85,    86,   911,   911,   757,   758,
     854,   195,   190,  1136,    58,   737,    87,    46,    89,    90,
    1109,    91,   190,    28,   784,   519,   808,   520,   190,  1122,
     818,   171,   805,   737,   194,   113,  1130,   818,   794,   250,
     183,    45,    47,   865,   317,   329,   154,    24,    45,    91,
     684,   141,   813,   320,    58,   601,   193,   602,    24,  1160,
     565,   866,   191,  1132,  1050,   195,  1148,   192,   997,   998,
     858,   191,   142,   832,  -223,   325,   192,  1021,   789,   840,
     838,   193,   840,  1005,   841,  1086,  -141,   326,   327,   194,
     853,   551,    61,  -474,  -558,  1151,  1121,   243,   292,   290,
     322,    66,   118,   553,   292,   193,   328,   735,   191,    52,
       9,   366,    45,   192,   194,    53,   132,     9,   905,   142,
     195,   423,   293,   192,   191,   142,    24,   250,   292,   192,
     200,   201,    61,  -141,   154,   202,    45,    81,   194,   321,
     183,    66,   132,   926,   135,   195,   841,   424,   138,   142,
      46,   724,   955,  1061,  1062,  -558,   329,    46,   347,   907,
      91,    81,   800,   756,   172,   684,   188,   189,   909,   195,
      52,  1072,    82,    83,    84,    45,    53,    47,   200,   201,
     153,   361,   139,   202,   150,  1050,   925,   213,   914,   910,
     757,   824,   373,   349,   190,    47,    82,    83,   427,   160,
      81,   176,   933,     1,   214,     2,     3,   317,   187,   941,
     942,   835,   164,   789,   165,   193,   946,   635,   636,   772,
      87,    46,    89,    90,   836,    91,   215,   216,   173,   200,
     201,   243,    85,    86,   202,    82,    83,    84,   290,   421,
    1017,   684,   288,   954,    87,    46,    89,    90,   194,    91,
     218,   317,   934,   968,   219,   349,   349,  1007,    45,   973,
       9,   183,   841,   814,   183,   841,   425,    -3,   183,   183,
     975,    85,    86,   859,   969,  1007,   994,   982,   193,   195,
      47,   283,   187,    87,    46,    89,    90,   721,    91,   301,
     191,   818,   297,  1150,   970,   192,   868,   995,  1153,   298,
     551,  1000,   250,   310,  1133,   299,   436,   319,   146,   154,
      24,   194,   553,   813,   673,   313,  1016,   341,    24,  -363,
     154,   193,    47,   742,    47,   345,   147,     9,    45,   148,
     193,    24,  -188,   153,   743,   527,   832,  -107,   154,   342,
     533,   382,   195,   133,   357,  1035,   134,   149,   480,   150,
     363,   485,   151,    81,   194,    26,   492,    22,    23,   918,
     724,  1051,   835,   194,   354,   725,   193,    46,    27,   226,
    1105,   349,   134,   227,   349,   836,   134,   818,   744,    47,
      47,   183,   232,  1066,   355,   195,   401,   134,    82,    83,
      84,   372,    28,   374,   195,   193,   154,   492,    51,   194,
     153,    24,   947,   187,  1080,   492,  1082,    47,    52,  1007,
      40,  1045,   402,   349,    53,    54,   492,   563,   709,   193,
       1,   387,     2,     3,    85,    86,    45,   388,   194,    55,
     195,   394,   629,   433,  -390,   366,    87,    46,    89,    90,
     132,    91,   414,    52,   803,   804,  1032,  1065,   243,    53,
     510,    81,   194,   398,   511,   422,   399,   187,  1117,   195,
      52,   479,  1110,   954,   399,    24,    53,    24,   512,   452,
     453,   408,   154,   457,   458,   850,  1089,   666,   399,   416,
      45,   418,  1051,   195,   295,   296,    82,    83,    84,   222,
     223,     1,   121,     2,     3,   122,  1140,   311,   312,   290,
    1141,  1007,    24,   711,   713,    81,   430,   719,   720,    -2,
     726,    -2,    -2,   448,   563,  -225,  -225,   295,   384,   563,
     250,   123,    85,    86,   449,    24,    24,   154,   454,   316,
     410,   411,   382,   124,    87,    46,    89,    90,   125,    91,
      82,    83,    84,    45,   126,  1049,   431,   432,  1172,   492,
     127,   128,   635,   636,  -223,  -223,   129,    24,   456,    47,
     459,   564,   500,   432,   154,   295,   600,   461,    81,   468,
     -95,   471,   316,   478,   774,    45,    85,    86,   366,   776,
     859,   860,   781,   943,   944,   472,   317,   473,    87,    46,
      89,    90,   481,    91,   486,   635,   636,   647,   648,    45,
      81,   -48,   -48,    82,    83,    84,   487,   796,   949,   950,
     951,   952,   498,   295,  1070,   343,   344,   776,   506,   495,
     499,   526,   316,  -272,    81,   191,   -95,   -95,   528,   953,
     192,   538,   532,   536,   539,    82,    83,    84,   597,    85,
      86,   599,   625,   630,   706,   631,   322,   633,   643,   846,
     136,    87,    88,    89,    90,   650,    91,   651,   564,    82,
     839,    84,   652,   564,  -272,   563,   657,   660,   191,   -95,
     -95,    85,    86,   192,   661,   668,   669,   563,   563,   563,
     675,  -272,   198,    87,    46,    89,    90,   700,    91,   199,
     697,   699,   717,   760,   761,    85,    86,   762,   563,  -350,
     748,  -272,  -272,  -272,  -272,  -272,  -272,    87,    46,    89,
      90,   763,    91,   753,   764,  -191,   765,   492,   191,   492,
     767,   768,    45,   192,   200,   201,   769,   908,   772,   202,
     203,   204,   205,   206,   207,   208,   770,   773,   775,   787,
     785,   549,   788,   793,   791,   792,   550,   796,   795,   551,
     801,   802,   820,   847,   848,   849,  -348,   851,   552,   927,
    -348,   553,   855,   867,   856,  -527,   869,  -527,   876,   887,
     554,  -348,   889,  -558,   555,   556,   243,   292,   893,   896,
    -348,   897,   878,   899,   557,   670,  -527,   916,    52,   917,
     921,     9,   930,   924,    53,   558,     9,   812,   142,   931,
     559,   560,   932,   561,   945,   929,   962,   964,   563,   564,
    -527,   563,   965,   957,   959,  -348,   967,   987,  -522,   972,
     563,   564,   564,   564,   988,    45,   989,   976,   999,  1002,
    1003,    46,    89,  1004,  1012,  1015,   563,  1027,  1033,   986,
    1030,  1034,   564,   553,   549,  1043,  1054,  1036,   666,   550,
     492,   492,   551,  1046,   183,  1055,  1057,  1060,   810,  -348,
    1063,   552,  1067,  -348,   553,  1064,   317,  1073,  -528,  1071,
    -528,  1078,  1069,   554,  -348,  1079,    45,   555,   556,  1083,
    1088,  1101,  1102,  -348,  1094,  1103,  1107,   557,  1104,  -528,
    1108,   243,  1116,  1115,     9,  1118,  1123,   776,   558,  1029,
    1032,   986,   425,   559,   560,  1126,   561,  1125,  1127,   563,
    -403,  1134,  1128,  -528,  1146,   492,  1139,   563,  -348,  1157,
    1144,   563,  1165,  1145,  1147,   243,  -558,    51,   183,  1149,
    1152,  1158,   563,  1159,    46,    89,  1167,    52,    25,  1169,
    1161,  1173,    75,    53,   245,     9,  1174,  -558,   246,   169,
     221,   434,   564,   324,   906,   564,   198,  1020,   670,   247,
    1076,  1106,   248,   199,   564,   249,  1175,   150,   913,   395,
     755,  1120,  1164,   179,  1162,  1028,   420,   693,   990,   694,
     564,   991,   904,   428,   197,    46,   352,   563,   961,   563,
     563,   971,   981,   563,  1131,   976,   563,   976,   200,   201,
     339,   494,    45,   202,   203,   204,   205,   206,   207,   208,
    1068,   727,   649,   492,   157,   672,   386,   986,   915,   534,
     986,   549,  1009,  1014,  1170,  1074,   550,   664,  1042,   551,
     515,   516,  1039,   117,   492,    70,  -348,    71,   552,   229,
    -348,   553,   162,   161,   880,  -527,   563,  1096,   563,   183,
     554,  -348,   922,   564,   555,   556,   492,  1098,     0,   425,
    -348,   564,     0,     0,   557,   564,  -527,     0,     0,     0,
       0,     9,   563,     0,     0,   558,   564,     0,   563,   563,
     559,   560,     0,   561,     0,     0,     0,     0,     0,     0,
    -527,     0,     0,     0,     0,  -348,     0,   563,     0,     0,
       0,     0,     0,   183,     0,    45,     0,     0,     0,     0,
       0,    46,    89,     0,  1077,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   549,     0,     0,     0,     0,   550,
       0,   564,   551,   564,   564,     0,     0,   564,     0,  -348,
     564,   552,     0,  -348,   553,     0,     0,     0,  -527,     0,
    -527,     0,     0,   554,  -348,     0,     0,   555,   556,     0,
       0,     0,     0,  -348,     0,     0,     0,   557,     0,  -527,
       0,     0,     0,     0,     9,     0,     0,     0,   558,     0,
       0,     0,     0,   559,   560,     0,   561,     0,     0,     0,
     564,     0,   564,     0,    45,     0,     0,     0,  -348,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   549,    46,    89,   564,     0,   550,     0,
       0,   551,   564,   564,     0,     0,     0,     0,  -348,     0,
     552,     0,  -348,   553,     0,     0,     0,  -329,  -329,  -329,
       0,   564,   554,  -348,     0,     0,   555,   556,     0,     0,
       0,     0,  -348,     0,     0,     0,   557,     0,     0,     0,
       0,    45,     0,     9,     0,     0,     0,   558,     0,     0,
       0,     0,   559,   560,     0,   561,     0,     0,     0,     0,
     549,     0,     0,     0,     0,   550,     0,  -348,   551,     0,
       0,     0,     0,     0,     0,  -348,     0,   552,     0,  -348,
     553,     0,     0,    46,    89,     0,  -366,     0,   736,   554,
    -348,     0,     0,   555,   556,     0,     0,     0,     0,  -348,
       0,     0,     0,   557,    45,     0,     0,     0,     0,     0,
       9,     0,     0,     0,   558,     0,     0,     0,     0,   559,
     560,     0,   561,   549,     0,     0,     0,     0,   550,     0,
       0,   551,     0,     0,  -348,     0,     0,     0,  -348,     0,
     552,     0,  -348,   553,     0,     0,     0,     0,     0,  -579,
      46,    89,   554,  -348,     0,     0,   555,   556,     0,     0,
       0,     0,  -348,     0,     0,     0,   557,    45,     0,     0,
       0,     0,     0,     9,     0,     0,     0,   558,     0,     0,
       0,     0,   559,   560,     0,   561,   549,     0,     0,     0,
       0,   550,     0,     0,   551,     0,  -579,  -348,     0,     0,
       0,  -348,     0,   552,     0,  -348,   553,     0,     0,     0,
       0,     0,  -341,    46,    89,   554,  -348,     0,     0,   555,
     556,     0,     0,     0,     0,  -348,     0,     0,     0,   557,
      45,     0,     0,     0,     0,     0,     9,     0,     0,     0,
     558,     0,     0,     0,     0,   559,   560,     0,   561,   549,
       0,     0,     0,     0,   550,     0,     0,   551,     0,  -341,
    -348,     0,     0,     0,  -348,     0,   552,     0,  -348,   553,
       0,     0,     0,     0,     0,  -580,    46,    89,   554,  -348,
       0,     0,   555,   556,     0,     0,     0,     0,  -348,     0,
       0,     0,   557,    45,     0,     0,     0,     0,     0,     9,
       0,     0,     0,   558,     0,     0,     0,     0,   559,   560,
       0,   561,   549,     0,     0,     0,     0,   550,     0,     0,
     551,     0,  -580,  -348,     0,     0,     0,  -348,     0,   552,
       0,  -348,   553,     0,     0,     0,     0,     0,  -334,    46,
      89,   554,  -348,     0,     0,   555,   556,     0,     0,     0,
       0,  -348,     0,     0,     0,   557,    45,     0,     0,     0,
       0,     0,     9,     0,     0,     0,   558,     0,     0,     0,
       0,   559,   560,     0,   561,   549,     0,     0,     0,     0,
     550,     0,     0,   551,     0,     0,  -348,     0,     0,     0,
    -348,     0,   552,     0,  -348,   553,     0,     0,     0,     0,
       0,  1056,    46,    89,   554,  -348,     0,     0,   555,   556,
       0,     0,     0,     0,  -348,     0,     0,     0,   557,    45,
       0,     0,     0,     0,     0,     9,     0,     0,     0,   558,
       0,     0,     0,     0,   559,   560,     0,   561,   549,     0,
       0,     0,     0,   550,     0,     0,   551,     0,     0,  -348,
       0,     0,     0,  -348,     0,   552,     0,  -348,   553,     0,
       0,     0,     0,     0,  1092,    46,    89,   554,  -348,     0,
       0,   555,   556,     0,     0,     0,     0,  -348,     0,     0,
       0,   557,    45,     0,     0,     0,     0,     0,     9,     0,
       0,     0,   558,     0,     0,     0,     0,   559,   560,     0,
     561,   549,     0,     0,     0,     0,   550,     0,     0,   551,
       0,     0,  -348,     0,     0,     0,  -348,     0,   552,     0,
    -348,   553,     0,     0,     0,     0,     0,  -527,    46,    89,
     554,  -348,     0,     0,   555,   556,     0,     0,     0,     0,
    -348,     0,     0,     0,   557,    45,     0,     0,     0,     0,
       0,     9,     0,     0,     0,   558,     0,     0,     0,     0,
     559,   560,     0,   561,   549,     0,     0,     0,     0,   550,
       0,     0,   551,     0,     0,  -348,     0,     0,     0,  -348,
       0,   552,     0,  -348,   553,     0,     0,     0,     0,     0,
    1095,    46,    89,   554,  -348,     0,     0,   555,   556,     0,
       0,     0,     0,  -348,     0,     0,     0,   557,    45,     0,
       0,     0,     0,     0,     9,     0,     0,     0,   558,     0,
       0,     0,     0,   559,   560,     0,   561,   549,     0,     0,
       0,     0,   550,     0,     0,   551,     0,     0,  -348,     0,
       0,    45,  -348,     0,   552,     0,  -348,   553,     0,     0,
       0,     0,     0,     0,    46,    89,   554,  -348,     0,     0,
     555,   556,     0,     0,     0,     0,  -348,   242,     0,     0,
     557,     0,     0,     0,     0,  -403,     0,     9,     0,     0,
       0,   558,     0,     0,     0,     0,   559,   560,    45,   561,
     243,  -558,    51,     0,     0,     0,     0,    45,     0,     0,
       0,  -348,    52,   244,     0,     0,     0,     0,    53,   245,
       9,     0,  -558,   246,     0,     0,     0,    46,    89,     0,
       0,     0,  -404,     0,   247,     0,     0,   248,     0,     0,
     249,     0,   150,  -404,     0,     0,     0,   243,  -558,    51,
       0,     0,  -409,     0,    45,     0,   243,  -558,    51,    52,
      46,     0,     0,    45,     0,    53,   245,     9,    52,  -558,
     246,     0,     0,     0,    53,   437,     9,  -409,  -558,   438,
       0,   247,     0,     0,   248,     0,     0,   249,  -403,   150,
     247,     0,     0,   439,     0,     0,   249,     0,   150,  -403,
       0,     0,     0,   243,  -558,    51,    45,    46,  -410,     0,
       0,     0,   243,  -558,    51,    52,    46,     0,     0,    45,
       0,    53,   245,     9,    52,  -558,   246,     0,     0,     0,
      53,   437,     9,  -410,  -558,   438,     0,   247,     0,     0,
     248,     0,     0,   249,     0,   150,   247,     0,     0,   439,
       0,  -409,   249,  -403,   150,   243,  -558,    51,     0,     0,
       0,     0,     0,    46,     0,     0,     0,    52,   243,  -558,
      51,     0,    46,    53,   437,     9,     0,  -558,   438,     0,
      52,     0,     0,     0,     0,     0,    53,   245,     9,   247,
    -558,   246,   439,     0,     0,   249,     0,   150,   603,     0,
    -204,     0,   247,     0,   542,   248,     0,     0,   249,     0,
     150,     0,   604,   605,     0,    46,     0,     0,     0,     0,
     676,     0,  -204,     0,     0,     0,   542,  -187,    46,   606,
       0,   607,     0,   608,   677,   678,     0,     0,     0,     0,
       0,     0,  -445,     0,   609,     0,   610,     0,     0,  -187,
       0,   606,     0,   679,  -106,   608,   611,   612,   613,     0,
       0,     0,     0,     0,     0,     0,   609,     0,   317,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   611,   680,
     613,     0,     0,     0,     0,   614,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   681
};

static const yytype_int16 yycheck[] =
{
       5,    11,    91,    30,    91,   435,    67,   238,    63,    30,
     315,    16,    67,   209,   448,   468,    81,   183,    83,   548,
     502,   467,    67,   725,    30,   749,    31,   443,   556,   674,
      28,    39,   478,   663,   543,   536,    41,   667,    91,   561,
     561,   289,   744,    70,   928,    43,    44,   729,   730,    70,
     536,   536,    62,   231,   358,   286,    54,   680,   536,   478,
     565,   890,    67,   141,    70,   570,    28,    78,   146,    67,
     148,   167,    43,   190,     1,    70,  1030,   456,    36,     1,
       0,    43,    44,    91,   241,   105,    77,    30,   467,    57,
     536,     1,    54,    57,     1,    38,   151,    57,    93,    34,
       1,   517,   532,   114,   191,    48,     9,   803,     1,    57,
      46,    21,    14,     1,    78,     1,  1070,   536,    78,    32,
     118,   317,   209,   952,   211,   603,   612,   284,   285,    36,
     287,     1,    90,    26,   132,   140,   134,   441,   136,   144,
     138,    76,   113,   141,   142,   113,   144,     1,   146,   147,
     148,   161,   110,   218,   856,   119,   118,    93,     1,   119,
      61,    54,    63,    90,    50,   173,   600,     1,    61,    62,
      63,   119,   134,    66,   136,     1,   138,     1,   169,     1,
     142,   189,    95,   110,   111,   147,   148,   365,   110,   671,
     210,   113,   888,   823,   680,   680,  1080,     1,   676,   316,
     110,   824,   506,   110,    97,    98,   302,    29,   109,   110,
     111,   112,    36,   114,   292,     1,   109,   110,   111,   112,
       1,   114,   110,    93,   110,   312,   315,    61,   315,    63,
     317,     1,   319,   320,   930,   322,   932,    70,     1,  1068,
     110,   111,  1071,   113,   110,   754,     1,   245,    24,   247,
      36,   249,    24,     1,   259,   109,   110,   111,    21,   312,
      93,   259,    66,   716,    27,   747,   319,   110,   111,    10,
     113,    31,   758,     1,    32,   109,   110,   111,   112,     1,
     114,    29,    37,   245,   110,   111,   110,   113,   110,   111,
      24,   987,   988,     1,   292,   300,   749,   295,   928,    21,
      63,   309,   550,   481,    90,   366,   110,   111,    63,   466,
      24,     1,   742,    57,    62,    70,     1,    67,     1,  1015,
      36,   700,    72,    31,   110,   826,    43,    44,   109,   110,
      38,   358,   860,   394,    62,    24,    26,    95,   824,   824,
     110,   111,   118,    51,    70,   432,   118,   110,   111,   109,
       1,   111,     1,   358,    62,   110,  1038,   862,     1,  1041,
     358,    46,   110,   111,    72,    73,   871,  1069,   366,  1014,
     536,    61,    62,    63,    90,   119,    66,   375,    57,   432,
      17,     1,   110,   111,   118,    18,   543,    36,   110,   768,
      23,   770,   808,    36,   110,    46,   394,     1,    78,  1123,
     461,   118,   110,   111,   118,   413,   859,    97,    98,    57,
      12,   419,   162,   375,   441,    19,    57,   444,    38,   109,
     110,   111,   112,   444,   114,   110,   111,   110,   111,   118,
     435,    51,    14,   113,   887,    18,   441,   435,   444,   437,
     119,    90,    30,   441,  1074,   967,   967,   969,   970,   970,
       1,    42,    72,    73,    18,   960,    60,  1159,   519,   110,
     111,   110,   517,   461,   553,    18,   114,   110,    18,    32,
      34,  1167,   113,    77,   519,   437,   707,    28,   983,   506,
     113,   114,    70,     5,    75,   493,   119,   917,    18,    40,
     110,   111,     1,    97,    98,    99,   100,     1,   102,    18,
      46,   506,    65,    25,   700,   503,   110,   111,   506,    92,
      27,   890,    21,    23,    57,   968,   524,    58,     1,   972,
      95,   519,   901,   902,   534,    29,   115,   532,  1037,    33,
     119,   114,   966,    96,   532,  1040,   119,    88,    21,  1044,
     601,   503,    93,    26,     8,    95,    63,    93,   635,   636,
     114,   549,   560,    71,    63,   119,    75,    32,    62,   110,
     111,   114,   560,   113,   114,   575,   119,    57,    78,   119,
     113,    14,   113,   952,    29,    61,    18,    95,    61,    62,
      63,    11,    57,    66,   114,    18,    41,    42,   677,   119,
      65,    58,  1097,  1122,  1047,   114,    58,    83,    55,   597,
     119,   110,   111,   601,  1113,    60,   110,   111,    70,    71,
      67,    32,   667,   700,    97,    98,   782,   783,    85,    86,
     709,    96,    18,   113,    30,  1130,   109,   110,   111,   112,
    1064,   114,    18,    95,   642,   113,   663,   115,    18,  1085,
     667,    82,   663,  1148,    65,     4,  1099,   674,   656,   654,
      91,     1,   650,    95,    75,   110,   654,   662,     1,   114,
     826,    51,   667,    95,    70,   113,    32,   115,   673,  1151,
    1123,   113,   114,  1107,   979,    96,  1129,   119,   909,   910,
     113,   114,    72,   681,   116,    29,   119,    37,   650,  1068,
     698,    32,  1071,   924,   890,    75,    46,    41,    42,    65,
     708,    28,    30,    46,    47,  1135,  1085,    50,    51,   150,
      19,    30,    95,    40,    51,    32,    60,   113,   114,    62,
      70,   114,     1,   119,    65,    68,   119,    70,   114,    72,
      96,    75,    69,   119,   114,    72,   741,   742,    51,   119,
      97,    98,    70,    93,   742,   102,     1,    26,    65,   115,
     191,    70,   119,   808,    68,    96,   952,   953,   114,    72,
     110,    88,   851,   994,   995,    47,   110,   110,   209,   777,
     114,    26,   113,    58,   839,   941,   115,   116,    51,    96,
      62,  1012,    61,    62,    63,     1,    68,   785,    97,    98,
     851,   232,   113,   102,    92,  1100,   806,    60,   115,    72,
      85,    86,   243,   890,    18,   803,    61,    62,    63,   113,
      26,    66,   822,    13,    77,    15,    16,    75,   905,   827,
     828,    63,    17,   785,    17,    32,   836,    41,    42,    71,
     109,   110,   111,   112,    76,   114,    99,   100,    24,    97,
      98,    50,    97,    98,   102,    61,    62,    63,   289,  1154,
     939,  1017,   149,   851,   109,   110,   111,   112,    65,   114,
     101,    75,    71,    44,     7,   952,   953,   928,     1,   877,
      70,   312,  1068,   928,   315,  1071,   317,     0,   319,   320,
     878,    97,    98,    44,    65,   946,    51,   885,    32,    96,
     888,   113,   979,   109,   110,   111,   112,   113,   114,     8,
     114,   928,   113,  1134,    65,   119,   113,    72,  1139,   113,
      28,   919,   917,    76,  1110,   113,   357,   116,    51,   917,
     925,    65,    40,   928,   934,   115,   936,    89,   933,    35,
     928,    32,   930,    39,   932,    55,    69,    70,     1,    72,
      32,   946,    56,  1004,    50,   453,   944,    61,   946,    44,
     458,   248,    96,   113,    61,   963,   116,    90,   399,    92,
     113,   402,    95,    26,    65,    58,   407,   113,   114,   113,
      88,   979,    63,    65,     4,    93,    32,   110,    71,   113,
      71,  1068,   116,   113,  1071,    76,   116,  1014,    94,   987,
     988,   432,    78,  1001,   113,    96,   293,   116,    61,    62,
      63,   113,    95,   113,    96,    32,  1004,   448,    52,    65,
    1071,  1016,   113,  1100,  1024,   456,  1026,  1015,    62,  1080,
       3,   113,    57,  1110,    68,    69,   467,   468,    91,    32,
      13,   113,    15,    16,    97,    98,     1,   113,    65,    83,
      96,   114,   483,   340,   113,   114,   109,   110,   111,   112,
     119,   114,    89,    62,    46,    47,    21,   113,    50,    68,
      69,    26,    65,   113,    73,  1154,   116,  1154,  1078,    96,
      62,   113,    75,  1071,   116,  1080,    68,  1082,    87,   376,
     377,    17,  1080,   380,   381,   113,   113,   528,   116,   115,
       1,    21,  1100,    96,   116,   117,    61,    62,    63,     5,
       6,    13,    27,    15,    16,    30,  1116,   115,   116,   550,
    1120,  1172,  1117,   554,   555,    26,   113,   558,   559,    13,
     561,    15,    16,    80,   565,   115,   116,   116,   117,   570,
    1135,    56,    97,    98,   113,  1140,  1141,  1135,    57,    18,
      44,    45,   439,    68,   109,   110,   111,   112,    73,   114,
      61,    62,    63,     1,    79,    66,   115,   116,  1168,   600,
      85,    86,    41,    42,   115,   116,    91,  1172,    57,  1167,
      57,   468,   115,   116,  1172,   116,   117,   114,    26,    35,
      59,   113,    18,    57,   625,     1,    97,    98,   114,   630,
      44,    45,   633,   115,   116,   113,    75,   113,   109,   110,
     111,   112,    57,   114,   113,    41,    42,   504,   505,     1,
      26,   116,   117,    61,    62,    63,    67,   658,   115,   116,
     115,   116,   114,   116,   117,   196,   197,   668,    71,   115,
      76,    57,    18,    19,    26,   114,   115,   116,    61,    75,
     119,    38,    57,    57,    23,    61,    62,    63,    46,    97,
      98,   115,    61,    63,   551,    23,    19,    29,   115,   700,
      36,   109,   110,   111,   112,    46,   114,    36,   565,    61,
      62,    63,    36,   570,    60,   716,    34,    83,   114,   115,
     116,    97,    98,   119,    83,    63,   113,   728,   729,   730,
     115,    77,    55,   109,   110,   111,   112,   114,   114,    62,
     113,   113,   113,    21,    21,    97,    98,    21,   749,   117,
     117,    97,    98,    99,   100,   101,   102,   109,   110,   111,
     112,    63,   114,   113,    21,    56,    58,   768,   114,   770,
      21,    61,     1,   119,    97,    98,    56,   778,    71,   102,
     103,   104,   105,   106,   107,   108,    29,   113,   113,    57,
      46,    20,    57,    57,   651,   652,    25,   798,    60,    28,
     113,   113,   113,   113,   113,    22,    35,   114,    37,   810,
      39,    40,   113,   117,    93,    44,   113,    46,    89,    59,
      49,    50,    81,    47,    53,    54,    50,    51,   113,    42,
      59,   115,    93,    32,    63,    83,    65,    57,    62,    57,
      46,    70,    46,   110,    68,    74,    70,    71,    72,    47,
      79,    80,    46,    82,    76,   113,    46,    25,   859,   716,
      89,   862,    38,   113,   113,    94,    24,    46,    89,    25,
     871,   728,   729,   730,    46,     1,    21,   878,   113,    76,
     113,   110,   111,   114,   110,    46,   887,    64,   115,   890,
      64,    54,   749,    40,    20,    46,    37,   113,   899,    25,
     901,   902,    28,   117,   905,    55,   113,    95,    32,    35,
     115,    37,   113,    39,    40,    80,    75,   113,    44,   114,
      46,   113,    93,    49,    50,    46,     1,    53,    54,    30,
     113,   113,    59,    59,    82,   113,    80,    63,   113,    65,
     113,    50,    57,   113,    70,    76,    43,   948,    74,   950,
      21,   952,   953,    79,    80,    46,    82,    82,   113,   960,
      35,   115,    82,    89,    82,   966,   115,   968,    94,    46,
     113,   972,    80,   113,   113,    50,    51,    52,   979,   113,
     113,   113,   983,    93,   110,   111,    46,    62,    17,    37,
     113,   113,    35,    68,    69,    70,   113,    72,    73,    79,
     114,   354,   859,   189,   776,   862,    55,   944,    83,    84,
    1017,  1060,    87,    62,   871,    90,  1172,    92,   783,   288,
     601,  1080,  1155,    91,  1154,   950,   315,   536,   898,   536,
     887,   899,   770,   320,   103,   110,   217,  1038,   860,  1040,
    1041,   875,   882,  1044,  1100,  1046,  1047,  1048,    97,    98,
     192,   411,     1,   102,   103,   104,   105,   106,   107,   108,
    1004,   561,   506,  1064,    67,   533,   259,  1068,   785,   459,
    1071,    20,   928,   934,  1166,  1014,    25,   527,   970,    28,
     444,   444,   967,    41,  1085,    31,    35,    31,    37,   134,
      39,    40,    71,    70,   739,    44,  1097,  1046,  1099,  1100,
      49,    50,   798,   960,    53,    54,  1107,  1048,    -1,  1110,
      59,   968,    -1,    -1,    63,   972,    65,    -1,    -1,    -1,
      -1,    70,  1123,    -1,    -1,    74,   983,    -1,  1129,  1130,
      79,    80,    -1,    82,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    -1,    -1,    94,    -1,  1148,    -1,    -1,
      -1,    -1,    -1,  1154,    -1,     1,    -1,    -1,    -1,    -1,
      -1,   110,   111,    -1,  1021,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    20,    -1,    -1,    -1,    -1,    25,
      -1,  1038,    28,  1040,  1041,    -1,    -1,  1044,    -1,    35,
    1047,    37,    -1,    39,    40,    -1,    -1,    -1,    44,    -1,
      46,    -1,    -1,    49,    50,    -1,    -1,    53,    54,    -1,
      -1,    -1,    -1,    59,    -1,    -1,    -1,    63,    -1,    65,
      -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    74,    -1,
      -1,    -1,    -1,    79,    80,    -1,    82,    -1,    -1,    -1,
    1097,    -1,  1099,    -1,     1,    -1,    -1,    -1,    94,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    20,   110,   111,  1123,    -1,    25,    -1,
      -1,    28,  1129,  1130,    -1,    -1,    -1,    -1,    35,    -1,
      37,    -1,    39,    40,    -1,    -1,    -1,    44,    45,    46,
      -1,  1148,    49,    50,    -1,    -1,    53,    54,    -1,    -1,
      -1,    -1,    59,    -1,    -1,    -1,    63,    -1,    -1,    -1,
      -1,     1,    -1,    70,    -1,    -1,    -1,    74,    -1,    -1,
      -1,    -1,    79,    80,    -1,    82,    -1,    -1,    -1,    -1,
      20,    -1,    -1,    -1,    -1,    25,    -1,    94,    28,    -1,
      -1,    -1,    -1,    -1,    -1,    35,    -1,    37,    -1,    39,
      40,    -1,    -1,   110,   111,    -1,    46,    -1,    48,    49,
      50,    -1,    -1,    53,    54,    -1,    -1,    -1,    -1,    59,
      -1,    -1,    -1,    63,     1,    -1,    -1,    -1,    -1,    -1,
      70,    -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,    79,
      80,    -1,    82,    20,    -1,    -1,    -1,    -1,    25,    -1,
      -1,    28,    -1,    -1,    94,    -1,    -1,    -1,    35,    -1,
      37,    -1,    39,    40,    -1,    -1,    -1,    -1,    -1,    46,
     110,   111,    49,    50,    -1,    -1,    53,    54,    -1,    -1,
      -1,    -1,    59,    -1,    -1,    -1,    63,     1,    -1,    -1,
      -1,    -1,    -1,    70,    -1,    -1,    -1,    74,    -1,    -1,
      -1,    -1,    79,    80,    -1,    82,    20,    -1,    -1,    -1,
      -1,    25,    -1,    -1,    28,    -1,    93,    94,    -1,    -1,
      -1,    35,    -1,    37,    -1,    39,    40,    -1,    -1,    -1,
      -1,    -1,    46,   110,   111,    49,    50,    -1,    -1,    53,
      54,    -1,    -1,    -1,    -1,    59,    -1,    -1,    -1,    63,
       1,    -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,
      74,    -1,    -1,    -1,    -1,    79,    80,    -1,    82,    20,
      -1,    -1,    -1,    -1,    25,    -1,    -1,    28,    -1,    93,
      94,    -1,    -1,    -1,    35,    -1,    37,    -1,    39,    40,
      -1,    -1,    -1,    -1,    -1,    46,   110,   111,    49,    50,
      -1,    -1,    53,    54,    -1,    -1,    -1,    -1,    59,    -1,
      -1,    -1,    63,     1,    -1,    -1,    -1,    -1,    -1,    70,
      -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,    79,    80,
      -1,    82,    20,    -1,    -1,    -1,    -1,    25,    -1,    -1,
      28,    -1,    93,    94,    -1,    -1,    -1,    35,    -1,    37,
      -1,    39,    40,    -1,    -1,    -1,    -1,    -1,    46,   110,
     111,    49,    50,    -1,    -1,    53,    54,    -1,    -1,    -1,
      -1,    59,    -1,    -1,    -1,    63,     1,    -1,    -1,    -1,
      -1,    -1,    70,    -1,    -1,    -1,    74,    -1,    -1,    -1,
      -1,    79,    80,    -1,    82,    20,    -1,    -1,    -1,    -1,
      25,    -1,    -1,    28,    -1,    -1,    94,    -1,    -1,    -1,
      35,    -1,    37,    -1,    39,    40,    -1,    -1,    -1,    -1,
      -1,    46,   110,   111,    49,    50,    -1,    -1,    53,    54,
      -1,    -1,    -1,    -1,    59,    -1,    -1,    -1,    63,     1,
      -1,    -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    74,
      -1,    -1,    -1,    -1,    79,    80,    -1,    82,    20,    -1,
      -1,    -1,    -1,    25,    -1,    -1,    28,    -1,    -1,    94,
      -1,    -1,    -1,    35,    -1,    37,    -1,    39,    40,    -1,
      -1,    -1,    -1,    -1,    46,   110,   111,    49,    50,    -1,
      -1,    53,    54,    -1,    -1,    -1,    -1,    59,    -1,    -1,
      -1,    63,     1,    -1,    -1,    -1,    -1,    -1,    70,    -1,
      -1,    -1,    74,    -1,    -1,    -1,    -1,    79,    80,    -1,
      82,    20,    -1,    -1,    -1,    -1,    25,    -1,    -1,    28,
      -1,    -1,    94,    -1,    -1,    -1,    35,    -1,    37,    -1,
      39,    40,    -1,    -1,    -1,    -1,    -1,    46,   110,   111,
      49,    50,    -1,    -1,    53,    54,    -1,    -1,    -1,    -1,
      59,    -1,    -1,    -1,    63,     1,    -1,    -1,    -1,    -1,
      -1,    70,    -1,    -1,    -1,    74,    -1,    -1,    -1,    -1,
      79,    80,    -1,    82,    20,    -1,    -1,    -1,    -1,    25,
      -1,    -1,    28,    -1,    -1,    94,    -1,    -1,    -1,    35,
      -1,    37,    -1,    39,    40,    -1,    -1,    -1,    -1,    -1,
      46,   110,   111,    49,    50,    -1,    -1,    53,    54,    -1,
      -1,    -1,    -1,    59,    -1,    -1,    -1,    63,     1,    -1,
      -1,    -1,    -1,    -1,    70,    -1,    -1,    -1,    74,    -1,
      -1,    -1,    -1,    79,    80,    -1,    82,    20,    -1,    -1,
      -1,    -1,    25,    -1,    -1,    28,    -1,    -1,    94,    -1,
      -1,     1,    35,    -1,    37,    -1,    39,    40,    -1,    -1,
      -1,    -1,    -1,    -1,   110,   111,    49,    50,    -1,    -1,
      53,    54,    -1,    -1,    -1,    -1,    59,    27,    -1,    -1,
      63,    -1,    -1,    -1,    -1,    35,    -1,    70,    -1,    -1,
      -1,    74,    -1,    -1,    -1,    -1,    79,    80,     1,    82,
      50,    51,    52,    -1,    -1,    -1,    -1,     1,    -1,    -1,
      -1,    94,    62,    63,    -1,    -1,    -1,    -1,    68,    69,
      70,    -1,    72,    73,    -1,    -1,    -1,   110,   111,    -1,
      -1,    -1,    35,    -1,    84,    -1,    -1,    87,    -1,    -1,
      90,    -1,    92,    46,    -1,    -1,    -1,    50,    51,    52,
      -1,    -1,    46,    -1,     1,    -1,    50,    51,    52,    62,
     110,    -1,    -1,     1,    -1,    68,    69,    70,    62,    72,
      73,    -1,    -1,    -1,    68,    69,    70,    71,    72,    73,
      -1,    84,    -1,    -1,    87,    -1,    -1,    90,    35,    92,
      84,    -1,    -1,    87,    -1,    -1,    90,    -1,    92,    46,
      -1,    -1,    -1,    50,    51,    52,     1,   110,    46,    -1,
      -1,    -1,    50,    51,    52,    62,   110,    -1,    -1,     1,
      -1,    68,    69,    70,    62,    72,    73,    -1,    -1,    -1,
      68,    69,    70,    71,    72,    73,    -1,    84,    -1,    -1,
      87,    -1,    -1,    90,    -1,    92,    84,    -1,    -1,    87,
      -1,    46,    90,    35,    92,    50,    51,    52,    -1,    -1,
      -1,    -1,    -1,   110,    -1,    -1,    -1,    62,    50,    51,
      52,    -1,   110,    68,    69,    70,    -1,    72,    73,    -1,
      62,    -1,    -1,    -1,    -1,    -1,    68,    69,    70,    84,
      72,    73,    87,    -1,    -1,    90,    -1,    92,    27,    -1,
      29,    -1,    84,    -1,    33,    87,    -1,    -1,    90,    -1,
      92,    -1,    41,    42,    -1,   110,    -1,    -1,    -1,    -1,
      27,    -1,    29,    -1,    -1,    -1,    33,    56,   110,    58,
      -1,    60,    -1,    62,    41,    42,    -1,    -1,    -1,    -1,
      -1,    -1,    71,    -1,    73,    -1,    75,    -1,    -1,    56,
      -1,    58,    -1,    60,    61,    62,    85,    86,    87,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    73,    -1,    75,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,    86,
      87,    -1,    -1,    -1,    -1,   114,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   114
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,    13,    15,    16,   121,   123,   124,   125,     0,    70,
     124,   144,   377,     9,   126,   110,   149,   122,   378,    14,
      10,   127,   113,   114,   144,   123,    58,    71,    95,   379,
     380,   381,   382,    17,    14,   128,   129,    12,   131,   145,
       3,   138,    71,    95,    95,     1,   110,   152,   224,   383,
     384,    52,    62,    68,    69,    83,   306,   315,   325,   326,
     330,   335,   386,   388,   389,   393,   404,   405,   411,   144,
     380,   382,   385,     8,   130,   129,    14,   133,   134,    11,
     132,    26,    61,    62,    63,    97,    98,   109,   110,   111,
     112,   114,   147,   148,   152,   206,   223,   225,   226,   227,
     230,   232,   235,   237,   240,   241,   243,   245,   246,   247,
     249,   250,   256,     4,   139,   140,   141,   377,    95,   383,
     383,    27,    30,    56,    68,    73,    79,    85,    86,    91,
     153,   154,   119,   113,   116,    68,    36,   224,   114,   113,
     149,    51,    72,   307,   314,   410,    51,    69,    72,    90,
      92,    95,   144,   151,   152,   307,   312,   326,   390,   406,
     113,   386,   385,   390,    17,    17,   137,   135,   134,   133,
     246,   223,   246,    24,    54,    63,    66,   171,   207,   208,
     210,   211,   213,   223,   229,   234,   235,   240,   115,   116,
      18,   114,   119,    32,    65,    96,   236,   236,    55,    62,
      97,    98,   102,   103,   104,   105,   106,   107,   108,   239,
     242,   248,   243,    60,    77,    99,   100,   244,   101,     7,
     142,   140,     5,     6,   143,   383,   113,   113,   152,   384,
     224,    57,    78,   337,   224,   111,   224,   308,   224,    57,
     113,   336,    27,    50,    63,    69,    73,    84,    87,    90,
     144,   150,   151,   159,   163,   257,   306,   315,   317,   318,
     319,   322,   323,   324,   325,   330,   334,   335,   339,   340,
     344,   345,   346,   353,   388,   390,   392,   396,   404,   413,
     414,   416,   423,   113,   308,   224,   224,   308,   225,    90,
     223,   391,    51,    69,   307,   116,   117,   113,   113,   113,
     149,     8,   136,   137,   235,   251,   252,   271,   272,   273,
      76,   115,   116,   115,    24,   118,    18,    75,   170,   116,
      95,   115,    19,   146,   148,    29,    41,    42,    60,   110,
     233,   250,   208,   228,   229,   234,   235,    31,   109,   226,
     231,    89,    44,   237,   237,    55,   171,   223,   238,   240,
     243,   240,   245,   246,     4,   113,    57,    61,   327,   412,
     109,   223,   338,   113,   115,    57,   114,   309,   310,   309,
     412,   337,   113,   223,   113,    36,    36,    90,   110,   152,
      36,    90,   225,   152,   117,   316,   319,   113,   113,   307,
     337,    57,   337,   337,   114,   191,   407,   391,   113,   116,
     308,   225,    57,   408,   152,    55,    67,   313,    17,   137,
      44,    45,   254,   253,    89,   235,   115,   229,    21,   209,
     211,   213,   234,    75,   171,   223,   229,    63,   228,   240,
     113,   115,   116,   225,   142,   331,   223,    69,    73,    87,
     320,   321,   322,   388,   394,   412,   311,   312,    80,   113,
      92,   224,   225,   225,    57,   347,    57,   225,   225,    57,
     341,   114,   160,   191,    30,    38,    48,   155,    35,   292,
      57,   113,   113,   113,    21,   151,   192,   193,    57,   113,
     223,    57,    21,    27,    63,   223,   113,    67,    62,   164,
     194,   221,   223,   255,   252,   115,   235,   235,   114,    76,
     115,   229,   317,    36,    36,    36,    71,   328,   322,   314,
      69,    73,    87,   315,   330,   344,   353,   388,   395,   113,
     115,   194,    34,    76,   415,    57,    57,   160,    61,   342,
      62,   164,    57,   160,   342,    21,    57,   162,    38,    23,
     113,   337,    33,   156,   164,   198,   199,   202,   221,    20,
      25,    28,    37,    40,    49,    53,    54,    63,    74,    79,
      80,    82,   144,   223,   225,   259,   260,   261,   262,   263,
     264,   265,   266,   268,   275,   276,   283,   284,   285,   293,
     295,   297,   300,   301,   304,   305,   362,   363,   365,   366,
     367,   372,   373,   374,   376,   401,   403,    46,    83,   115,
     117,   113,   115,    27,    41,    42,    58,    60,    62,    73,
      75,    85,    86,    87,   114,   168,   169,   198,   214,   215,
     218,   221,   333,   387,   409,    61,   412,    21,    63,   223,
      63,    23,   157,    29,   222,    41,    42,   165,   166,   167,
     170,   235,   172,   115,   292,   332,   224,   225,   225,   320,
      46,    36,    36,   307,   314,   312,   424,    34,   417,   235,
      83,    83,   149,   354,   347,   217,   223,   349,    63,   113,
      83,   317,   341,   149,   356,   115,    27,    41,    42,    60,
      86,   114,   161,   168,   170,   173,   176,   178,   179,   182,
     184,   185,   198,   214,   218,   333,   387,   113,   258,   113,
     114,   157,   337,   222,   152,   391,   225,   364,   277,    91,
     234,   223,   298,   223,   269,   270,   271,   113,   113,   223,
     223,   113,   152,   235,    88,    93,   223,   305,   361,   362,
     365,   368,   370,   371,    23,   113,    48,   260,   296,   397,
     260,   149,    39,    50,    94,   286,   287,   294,   117,    43,
     113,   291,   308,   113,   194,   193,    58,    85,    86,   168,
      21,    21,    21,    63,    21,    58,   387,    21,    61,    56,
      29,   219,    71,   113,   223,   113,   223,   158,    38,    73,
     220,   223,   240,   240,   235,    46,    57,    57,    57,   224,
     329,   225,   225,    57,   235,    60,   223,   419,   420,   421,
     113,   113,   113,    46,    47,   315,   355,   358,   388,   413,
      32,    95,    71,   144,   307,   348,   350,   357,   388,   413,
     113,   292,   357,   360,    86,   168,   234,   180,   177,   185,
     387,   109,   152,   174,   175,    63,    76,   186,   235,    62,
     164,   171,   200,   201,   203,   212,   223,   113,   113,    22,
     113,   114,   309,   235,   234,   113,    93,   299,   113,    44,
      45,   274,   259,    95,   113,    95,   113,   117,   113,   113,
     272,   259,   375,   375,   375,   369,    89,   267,    93,   398,
     398,    93,   278,   279,   317,   288,   272,    59,   290,    81,
     289,   292,   295,   113,   157,   387,    42,   115,   164,    32,
     216,    31,    38,   164,   220,   114,   165,   235,   223,    51,
      72,   170,   183,   183,   115,   329,    57,    57,   113,   418,
      34,    46,   421,   291,   110,   149,   307,   223,   351,   113,
      46,    47,    46,   149,    71,   343,   413,   185,   387,    42,
     182,   235,   235,   115,   116,    76,   149,   113,    63,   115,
     116,   115,   116,    75,   152,   234,    57,   113,   272,   113,
     259,   270,    46,   402,    25,    38,   302,    24,    44,    65,
      65,   274,    25,   235,    66,   152,   223,   399,   400,   280,
      46,   279,   152,   259,   291,   212,   223,    46,    46,    21,
     216,   217,   164,   164,    51,    72,    21,   309,   309,   113,
     235,   422,    76,   113,   114,   309,   113,   151,   190,   350,
     352,   291,   110,   291,   356,    46,   149,   234,   181,   182,
     175,    37,    63,   187,   188,   190,   195,    64,   201,   223,
      64,   212,    21,   115,    54,   235,   113,   194,   365,   370,
     259,   365,   368,    46,   259,   113,   117,    24,   118,    66,
     213,   235,   281,   282,    37,    55,    46,   113,   291,   291,
      95,   309,   309,   115,    80,   113,   235,   113,   287,    93,
     117,   114,   309,   113,   360,   291,   181,   225,   113,    46,
     149,   189,   149,    30,   204,   205,    75,   204,   113,   113,
     157,   375,    46,   375,    82,    46,   399,   259,   400,    24,
     118,   113,    59,   113,   113,    71,   186,    80,   113,   194,
      75,   212,   272,   204,   212,   113,    57,   149,    76,   190,
     195,   164,   221,    43,   303,    82,    46,   113,    82,    24,
     259,   282,   194,   171,   115,    57,   113,   359,   157,   115,
     149,   149,   222,   295,   113,   113,    82,   113,   259,   113,
     309,   317,   113,   309,    93,   196,   197,    46,   113,    93,
     292,   113,   210,    46,   197,    80,   272,    46,    24,    37,
     359,   291,   149,   113,   113,   187
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   120,   122,   121,   123,   123,   124,   125,   125,   125,
     126,   127,   127,   128,   128,   130,   129,   131,   131,   132,
     132,   133,   133,   135,   134,   136,   136,   137,   138,   139,
     139,   140,   141,   142,   142,   143,   143,   144,   145,   146,
     144,   147,   147,   148,   148,   149,   149,   150,   151,   151,
     152,   152,   152,   153,   153,   153,   154,   154,   154,   154,
     154,   154,   155,   155,   155,   155,   156,   156,   156,   157,
     158,   157,   159,   160,   160,   160,   161,   161,   161,   161,
     161,   161,   161,   161,   161,   161,   161,   161,   161,   161,
     162,   162,   162,   163,   164,   164,   164,   164,   165,   165,
     165,   166,   167,   168,   168,   168,   169,   169,   169,   170,
     171,   171,   172,   171,   173,   174,   174,   175,   175,   176,
     177,   176,   178,   178,   180,   179,   181,   181,   182,   183,
     183,   184,   184,   185,   186,   186,   187,   187,   187,   188,
     188,   189,   189,   190,   191,   192,   192,   193,   194,   194,
     195,   196,   196,   197,   198,   198,   199,   200,   200,   201,
     202,   203,   203,   204,   204,   205,   205,   206,   206,   206,
     206,   206,   207,   207,   209,   208,   208,   210,   210,   211,
     211,   211,   212,   212,   213,   213,   214,   215,   215,   215,
     215,   215,   216,   216,   217,   217,   218,   219,   219,   219,
     219,   219,   220,   220,   221,   221,   222,   222,   222,   222,
     223,   223,   223,   223,   223,   223,   224,   224,   225,   226,
     227,   228,   228,   229,   229,   229,   231,   230,   230,   230,
     230,   232,   233,   233,   233,   233,   233,   234,   235,   235,
     235,   236,   236,   236,   236,   236,   237,   237,   237,   238,
     238,   239,   239,   240,   240,   240,   241,   241,   242,   242,
     242,   243,   243,   244,   244,   244,   244,   245,   245,   245,
     245,   246,   246,   246,   246,   247,   247,   247,   248,   248,
     248,   248,   248,   248,   249,   250,   250,   250,   251,   251,
     253,   252,   254,   255,   254,   256,   258,   257,   259,   259,
     260,   260,   261,   261,   261,   262,   262,   262,   262,   262,
     262,   262,   262,   262,   262,   263,   263,   263,   263,   263,
     263,   263,   264,   265,   267,   266,   268,   269,   269,   270,
     271,   273,   272,   274,   274,   275,   277,   276,   278,   278,
     280,   279,   281,   281,   282,   282,   282,   283,   284,   284,
     285,   286,   286,   286,   288,   287,   289,   289,   290,   291,
     291,   292,   293,   294,   294,   295,   296,   296,   297,   298,
     298,   299,   299,   300,   300,   301,   302,   302,   303,   303,
     304,   305,   306,   306,   306,   306,   307,   307,   308,   308,
     309,   309,   310,   311,   311,   312,   313,   313,   313,   313,
     314,   316,   315,   317,   317,   318,   318,   319,   319,   320,
     320,   321,   321,   322,   322,   322,   322,   323,   323,   323,
     323,   323,   323,   323,   323,   323,   323,   323,   323,   324,
     324,   324,   324,   325,   325,   327,   326,   328,   328,   329,
     329,   331,   330,   332,   332,   333,   333,   334,   334,   334,
     335,   336,   335,   335,   335,   335,   337,   338,   338,   339,
     340,   340,   341,   341,   342,   342,   343,   343,   344,   345,
     346,   346,   347,   348,   348,   349,   349,   350,   350,   350,
     350,   351,   351,   352,   352,   353,   354,   354,   355,   355,
     355,   355,   356,   356,   357,   357,   358,   358,   359,   359,
     360,   360,   361,   362,   362,   363,   364,   364,   365,   365,
     366,   366,   366,   366,   367,   368,   368,   369,   369,   370,
     370,   370,   371,   371,   372,   373,   374,   375,   375,   376,
     377,   378,   377,   377,   379,   379,   380,   380,   381,   381,
     381,   382,   382,   382,   382,   383,   383,   384,   385,   385,
     386,   386,   386,   386,   386,   386,   386,   387,   388,   388,
     389,   389,   390,   390,   391,   391,   392,   392,   392,   392,
     394,   393,   395,   395,   395,   395,   396,   397,   397,   398,
     398,   399,   399,   400,   400,   401,   401,   402,   401,   403,
     403,   404,   404,   405,   405,   406,   406,   406,   406,   406,
     406,   406,   407,   407,   407,   408,   408,   408,   408,   408,
     408,   408,   409,   409,   409,   409,   409,   409,   409,   409,
     409,   409,   409,   409,   409,   409,   410,   410,   411,   412,
     413,   413,   413,   415,   414,   416,   417,   418,   417,   419,
     419,   420,   420,   422,   421,   424,   423
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     6,     1,     2,     5,     1,     1,     1,
       5,     0,     2,     1,     2,     0,     3,     0,     2,     0,
       2,     1,     2,     0,     3,     1,     2,     3,     2,     1,
       2,     2,     2,     0,     3,     1,     1,     3,     0,     0,
       8,     1,     3,     1,     3,     0,     2,     6,     1,     3,
       1,     2,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     0,     1,     1,     2,     1,     2,     1,     0,
       0,     3,     5,     0,     1,     3,     1,     1,     1,     1,
       1,     2,     3,     3,     2,     2,     1,     1,     1,     1,
       0,     2,     2,     5,     2,     1,     4,     3,     1,     1,
       1,     3,     3,     3,     6,     6,     0,     1,     1,     2,
       3,     3,     0,     7,     3,     1,     3,     1,     1,     1,
       0,     3,     1,     1,     0,     4,     0,     1,     1,     0,
       1,     3,     5,     2,     5,     2,     2,     2,     3,     1,
       3,     1,     3,     5,     3,     1,     3,     4,     2,     1,
       8,     1,     2,     5,     1,     1,     6,     1,     3,     3,
       6,     1,     3,     2,     3,     0,     1,     3,     3,     5,
       6,     4,     3,     3,     0,     4,     3,     1,     3,     1,
       1,     1,     1,     1,     2,     1,     3,     0,     1,     1,
       1,     1,     0,     2,     1,     3,     2,     2,     3,     3,
       4,     6,     0,     1,     0,     2,     2,     3,     4,     6,
       1,     1,     1,     1,     1,     1,     1,     3,     1,     1,
       4,     1,     3,     1,     1,     1,     0,     4,     3,     3,
       3,     3,     1,     1,     1,     1,     1,     1,     1,     3,
       3,     1,     1,     1,     2,     2,     1,     3,     3,     1,
       1,     1,     2,     2,     1,     3,     1,     1,     1,     1,
       1,     1,     3,     1,     1,     1,     1,     1,     2,     2,
       3,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     3,     1,     3,     5,     1,     3,
       0,     3,     0,     0,     3,     2,     0,     7,     1,     2,
       1,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     3,     2,     0,     5,     6,     1,     3,     2,
       2,     0,     2,     0,     2,     6,     0,     4,     1,     2,
       0,     5,     1,     3,     1,     1,     1,     5,     0,     2,
       1,     0,     2,     3,     0,     4,     0,     1,     4,     0,
       1,     2,     6,     0,     2,     2,     0,     1,     4,     0,
       1,     0,     2,     2,     3,     8,     0,     1,     0,     4,
       3,     2,     3,     3,     4,     4,     3,     5,     1,     1,
       0,     1,     3,     1,     3,     5,     0,     1,     1,     2,
       2,     0,     8,     0,     1,     1,     2,     1,     1,     0,
       1,     1,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     2,     2,     0,     8,     0,     2,     0,
       1,     0,    10,     0,     1,     0,     1,     6,     5,     1,
       4,     0,     5,     5,     5,     5,     2,     1,     1,     2,
       3,     5,     0,     7,     0,     3,     0,     3,     9,     2,
       3,     5,     6,     0,     2,     0,     2,     1,     2,     1,
       1,     0,     2,     1,     1,     8,     1,     3,     1,     1,
       3,     1,     1,     3,     5,     8,     6,    10,     1,     6,
       0,     3,     1,     2,     6,     3,     1,     4,     3,     4,
       1,     1,     1,     1,     7,     1,     4,     0,     3,     2,
       2,     2,     2,     2,     8,     9,     8,     0,     1,     3,
       0,     0,     3,     2,     4,     3,     0,     1,     2,     3,
       2,     3,     4,     4,     5,     1,     3,     1,     0,     2,
       1,     1,     1,     1,     1,     1,     1,     2,     0,     1,
       2,     1,     3,     4,     1,     3,     6,     6,     5,     6,
       0,     6,     1,     1,     1,     1,     4,     2,     2,     4,
       6,     1,     3,     1,     1,     2,     3,     0,     6,     3,
       5,     3,     3,     1,     2,     1,     2,     6,     4,    10,
       6,     1,     0,     1,     3,     0,     2,     2,     2,     3,
       3,     3,     3,     2,     2,     2,     4,     2,     1,     1,
       3,     2,     1,     1,     2,     1,     3,     4,     4,     2,
       1,     1,     1,     0,     6,     9,     0,     0,     5,     0,
       1,     1,     2,     0,     7,     0,     7
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
  YYUSE (yyoutput);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yykind < YYNTOKENS)
    YYPRINT (yyo, yytoknum[yykind], *yyvaluep);
# endif
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yykind);
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
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yykind);
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
  case 2: /* $@1: %empty  */
#line 548 "ada.y"
                                        {fprintf(parsed, ",");}
#line 2897 "ada.tab.c"
    break;

  case 6: /* ref_xref: ref_identifier_or_string_or_character_literal ref_decl ref_body_opt ref_modi_opt ref_ref_opt  */
#line 557 "ada.y"
            {
                 free(dcurrent_id);
            }
#line 2905 "ada.tab.c"
    break;

  case 7: /* ref_identifier_or_string_or_character_literal: ref_identifier  */
#line 563 "ada.y"
                                                {dcurrent_id = (yyvsp[0].id_kind).id;
                                                 dcurrent_kind = (yyvsp[0].id_kind).kind;
                                                }
#line 2913 "ada.tab.c"
    break;

  case 8: /* ref_identifier_or_string_or_character_literal: ref_string  */
#line 567 "ada.y"
                                                {dcurrent_id = (yyvsp[0].id_kind).id;
                                                 dcurrent_kind = (yyvsp[0].id_kind).kind;
                                                }
#line 2921 "ada.tab.c"
    break;

  case 9: /* ref_identifier_or_string_or_character_literal: ref_character_literal  */
#line 571 "ada.y"
                                                {dcurrent_id = (yyvsp[0].id_kind).id;
                                                 dcurrent_kind = (yyvsp[0].id_kind).kind;
                                                }
#line 2929 "ada.tab.c"
    break;

  case 10: /* ref_decl: REF_DECL ref_filename ref_integer REF_COLON ref_integer  */
#line 579 "ada.y"
           {char *dcurrent_id_tmp;
            dcurrent_id_tmp = malloc(3*strlen(dcurrent_id)+4);
            dxref_id = malloc(strlen((yyvsp[-3].id))+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id))+strlen(dcurrent_id)+6);
            strcpy(dxref_id, (yyvsp[-3].id));
            strcat(dxref_id, ":");
            if (!is_standard){  //for standard.xref parsing we ignore line and column info because they will not be xrefed in foo.xref
                strcat(dxref_id, (yyvsp[-2].id));
                strcat(dxref_id, ":");
                strcat(dxref_id, (yyvsp[0].id));
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
            if (is_new_intrinsic((yyvsp[-3].id), dcurrent_id)) fprintf(parsed, "is_intrinsic(%s, %s),\n", dcurrent_id, current_decl->unique_id);
            //checking for subprograms

            if (!strncmp(original_package_name, (yyvsp[-3].id), strlen(original_package_name)) &&  (yyvsp[-3].id)[strlen(original_package_name)] == '.') {
              switch (dcurrent_kind) {
                case 0: break;
                case 1: // a function
                        fprintf(subprograms, "function\n%s\n%s\n", dcurrent_id, (yyvsp[-2].id));
                        break;
                case 2: // a procedure
                        fprintf(subprograms, "procedure\n%s\n%s\n", dcurrent_id, (yyvsp[-2].id));
                        break;
              }
            }
            free((yyvsp[-3].id));
            free((yyvsp[-2].id));
            free((yyvsp[0].id));
           }
#line 2975 "ada.tab.c"
    break;

  case 15: /* $@2: %empty  */
#line 633 "ada.y"
                        {tmp_filename_s = (yyvsp[0].id); }
#line 2981 "ada.tab.c"
    break;

  case 16: /* ref_body: ref_filename $@2 ref_line_column  */
#line 633 "ada.y"
                                                                {free(tmp_filename_s);}
#line 2987 "ada.tab.c"
    break;

  case 23: /* $@3: %empty  */
#line 649 "ada.y"
                             {tmp_filename_s = (yyvsp[0].id); }
#line 2993 "ada.tab.c"
    break;

  case 24: /* ref_reference: ref_filename $@3 ref_line_column_list  */
#line 649 "ada.y"
                                                                          {free(tmp_filename_s);}
#line 2999 "ada.tab.c"
    break;

  case 27: /* ref_line_column: ref_integer REF_COLON ref_integer  */
#line 657 "ada.y"
                  {dxref_id = malloc((strlen(tmp_filename_s)+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id))+strlen(dcurrent_id)+4) );
                   strcpy(dxref_id, tmp_filename_s);
                   strcat(dxref_id, ":");
                   if (!is_standard){  //for standard.xref parsing we ignore line and column info
                        strcat(dxref_id, (yyvsp[-2].id));
                        strcat(dxref_id, ":");
                        strcat(dxref_id, (yyvsp[0].id));
                        strcat(dxref_id, ":");
                   }
                   strcat(dxref_id, dcurrent_id);
                   free((yyvsp[-2].id));
                   free((yyvsp[0].id));
                   ref_insert_start(dxref_id, current_decl); //e.g. positionvalidation-testhighdemandcurrent.adb:30:10
                  }
#line 3018 "ada.tab.c"
    break;

  case 31: /* pac_package: pac_name pac_spec_or_body  */
#line 691 "ada.y"
              {
               char *filename = NULL;
               char *path = NULL;
               find_filename_path((yyvsp[-1].id), (yyvsp[0].id), &filename, &path);
               if (debugMode) fprintf(stdout, "Mika DEBUG after find_filename_path found for %s%s the filename: %s on the path: %s\n", (yyvsp[-1].id), (yyvsp[0].id), !filename ? "NULL" : filename, !path ? "NULL" : path);
               if (filename != NULL) { //to see if it was mentionned in foo.xref
                 if (!path) {
                   fprintf(stdout, "Mika ERROR: a needed source file (%s%s) could not be found for the unit %s. Use the -c switch of ada_parser to pass on include files to gnat ls.\n", filename, (yyvsp[0].id), (yyvsp[-1].id));
                   fflush(stdout);
                   my_exit(23);
                 }
                 else add_tail((yyvsp[-1].id), filename, (yyvsp[0].id), path);   //add package name at the back of the queue (see queue.c)
               }
               else {
                    if (debugMode) {
                        fprintf(stdout, "Mika DEBUG Not added to queue %s%s\n", (yyvsp[-1].id), (yyvsp[0].id)); //not added because it is not referenced in foo.ref
                        fflush(stdout);
                    }
               }
               free((yyvsp[-1].id));
               free((yyvsp[0].id));
              }
#line 3045 "ada.tab.c"
    break;

  case 32: /* pac_name: pac_package_name pac_dot_opt  */
#line 716 "ada.y"
           {(yyval.id) = malloc((strlen((yyvsp[-1].id))+strlen((yyvsp[0].id))+1) );
            strcpy((yyval.id), (yyvsp[-1].id));
            strcat((yyval.id), (yyvsp[0].id));
            free((yyvsp[-1].id));
            free((yyvsp[0].id));
           }
#line 3056 "ada.tab.c"
    break;

  case 33: /* pac_dot_opt: %empty  */
#line 725 "ada.y"
              {(yyval.id) = malloc(1 ); strcpy((yyval.id), "");}
#line 3062 "ada.tab.c"
    break;

  case 34: /* pac_dot_opt: PAC_DOT pac_package_name pac_dot_opt  */
#line 727 "ada.y"
              {(yyval.id) = malloc((strlen((yyvsp[-1].id))+strlen((yyvsp[0].id))+2) );
               strcpy((yyval.id), "-");         //according to GNAT file naming convention
               strcat((yyval.id), (yyvsp[-1].id));
               strcat((yyval.id), (yyvsp[0].id));
               free((yyvsp[-1].id));
               free((yyvsp[0].id));
              }
#line 3074 "ada.tab.c"
    break;

  case 35: /* pac_spec_or_body: PAC_SPEC  */
#line 737 "ada.y"
                   {(yyval.id) = malloc(5);
                    strcpy((yyval.id), ".ads");
                   }
#line 3082 "ada.tab.c"
    break;

  case 36: /* pac_spec_or_body: PAC_BODY  */
#line 741 "ada.y"
                   {(yyval.id) = malloc(5);
                    strcpy((yyval.id), ".adb");
                   }
#line 3090 "ada.tab.c"
    break;

  case 37: /* pragma: PRAGMA identifier ';'  */
#line 749 "ada.y"
          {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-1].id_ref).id)+17);
           strcpy((yyval.id), "atomic_pragma(");
           strcat((yyval.id), (yyvsp[-1].id_ref).id);
           strcat((yyval.id), ")");
           free((yyvsp[-1].id_ref).id);
          }
#line 3101 "ada.tab.c"
    break;

  case 38: /* $@4: %empty  */
#line 756 "ada.y"
           {in_a_pragma = 1;}
#line 3107 "ada.tab.c"
    break;

  case 39: /* $@5: %empty  */
#line 759 "ada.y"
           {in_a_pragma = 0;}
#line 3113 "ada.tab.c"
    break;

  case 40: /* pragma: PRAGMA identifier '(' $@4 pragma_argument_association_list ')' $@5 ';'  */
#line 761 "ada.y"
          {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-6].id_ref).id)+strlen((yyvsp[-3].id))+14);
           strcpy((yyval.id), "pragma(");
           strcat((yyval.id), (yyvsp[-6].id_ref).id);
           strcat((yyval.id), ", [");
           strcat((yyval.id), (yyvsp[-3].id));
           strcat((yyval.id), "])");
           free((yyvsp[-6].id_ref).id);
           free((yyvsp[-3].id));
          }
#line 3127 "ada.tab.c"
    break;

  case 41: /* pragma_argument_association_list: pragma_argument_association  */
#line 772 "ada.y"
                                                               {(yyval.id) = (yyvsp[0].id);}
#line 3133 "ada.tab.c"
    break;

  case 42: /* pragma_argument_association_list: pragma_argument_association_list ',' pragma_argument_association  */
#line 774 "ada.y"
                                   {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id))+3);
                                    strcpy((yyval.id), (yyvsp[-2].id));
                                    strcat((yyval.id), ", ");
                                    strcat((yyval.id), (yyvsp[0].id));
                                    free((yyvsp[-2].id));
                                    free((yyvsp[0].id));
                                   }
#line 3145 "ada.tab.c"
    break;

  case 43: /* pragma_argument_association: expression_2  */
#line 783 "ada.y"
                                           {(yyval.id) = (yyvsp[0].id_deci).id;}
#line 3151 "ada.tab.c"
    break;

  case 44: /* pragma_argument_association: identifier RIGHT_SHAFT expression_2  */
#line 784 "ada.y"
                                                                  {(yyval.id) = (yyvsp[0].id_deci).id;}
#line 3157 "ada.tab.c"
    break;

  case 45: /* pragma_s: %empty  */
#line 796 "ada.y"
                                {(yyval.id) = malloc(1); strcpy((yyval.id), "");}
#line 3163 "ada.tab.c"
    break;

  case 46: /* pragma_s: pragma_s pragma  */
#line 798 "ada.y"
           {if (strcmp((yyvsp[-1].id), "")) {
              (yyval.id) = malloc(SAFETY+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id))+3);
              strcpy((yyval.id), (yyvsp[-1].id));
              strcat((yyval.id), ",\n");
              strcat((yyval.id), (yyvsp[0].id));
            }
            else {
              (yyval.id) = malloc(strlen((yyvsp[0].id))+1);
              strcpy((yyval.id), (yyvsp[0].id));
            }
            free((yyvsp[-1].id));
            free((yyvsp[0].id));
           }
#line 3181 "ada.tab.c"
    break;

  case 47: /* object_declaration: identifier_list ':' object_qualifier_opt object_subtype_definition init_opt ';'  */
#line 816 "ada.y"
                     {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-3].id))+strlen((yyvsp[-5].id))+strlen((yyvsp[-2].id))+strlen((yyvsp[-1].id))+19);
                      strcpy((yyval.id), "  object(");
                      strcat((yyval.id), (yyvsp[-3].id));
                      strcat((yyval.id), ", [");
                      strcat((yyval.id), (yyvsp[-5].id));
                      strcat((yyval.id), "], ");
                      strcat((yyval.id), (yyvsp[-2].id));
                      strcat((yyval.id), ", ");
                      strcat((yyval.id), (yyvsp[-1].id));
                      strcat((yyval.id), ")");
                      free((yyvsp[-3].id));
                      free((yyvsp[-5].id));
                      free((yyvsp[-2].id));
                      free((yyvsp[-1].id));
                     }
#line 3201 "ada.tab.c"
    break;

  case 48: /* identifier_list: identifier_rule  */
#line 833 "ada.y"
                                     {(yyval.id) = (yyvsp[0].id);}
#line 3207 "ada.tab.c"
    break;

  case 49: /* identifier_list: identifier_list ',' identifier_rule  */
#line 835 "ada.y"
                  {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id))+3) );
                   strcpy((yyval.id), (yyvsp[-2].id));
                   strcat((yyval.id), ", ");
                   strcat((yyval.id), (yyvsp[0].id));
                   free((yyvsp[-2].id));
                   free((yyvsp[0].id));
                  }
#line 3219 "ada.tab.c"
    break;

  case 50: /* identifier_rule: identifier  */
#line 845 "ada.y"
                  {if (is_ascii) {
                     char key1[1000];         /* used for searching double_tst structure */
                     strcpy(key1, "standard.ads:");
                     strcat(key1, (yyvsp[0].id_ref).id);
                     strcat(key1, "_ascii");
                     found_decl = decl_search_start(key1);
                     if (!found_decl) found_decl = ref_search_start(key1);
                     if (!found_decl) {
                       fprintf(stdout, "Mika ERROR: unknown predefined identifier %s \n", (yyvsp[0].id_ref).id); // could also print out key1
                       fflush(stdout);
                       my_exit(26);
                     }
                     (yyval.id) = malloc((strlen(found_decl->unique_id)+1) );
                     strcpy((yyval.id), found_decl->unique_id);
                   }
                   else (yyval.id) = handle_identifiers((yyvsp[0].id_ref), 0); //0 because not a string
                  }
#line 3241 "ada.tab.c"
    break;

  case 51: /* identifier_rule: error new_ada_2005_reserved_words  */
#line 863 "ada.y"
                  {yyerrok;     //Bison macro to recover from the error
                   recover_keyword((yyvsp[0].id));                            //sets up error_recovery_identifier with column and line number
                   (yyval.id) = handle_identifiers(error_recovery_identifier, 0); //0 because not a string
                  }
#line 3250 "ada.tab.c"
    break;

  case 52: /* identifier_rule: error new_ada_95_reserved_words  */
#line 868 "ada.y"
                  {yyerrok;     //Bison macro to recover from the error
                   recover_keyword((yyvsp[0].id));                            //sets up error_recovery_identifier with column and line number
                   (yyval.id) = handle_identifiers(error_recovery_identifier, 0); //0 because not a string
                  }
#line 3259 "ada.tab.c"
    break;

  case 53: /* new_ada_2005_reserved_words: INTERFACE  */
#line 875 "ada.y"
                              {(yyval.id) = malloc(10);
                               strcpy((yyval.id), "interface");
                              }
#line 3267 "ada.tab.c"
    break;

  case 54: /* new_ada_2005_reserved_words: OVERRIDING  */
#line 879 "ada.y"
                              {(yyval.id) = malloc(11);
                               strcpy((yyval.id), "overriding");
                              }
#line 3275 "ada.tab.c"
    break;

  case 55: /* new_ada_2005_reserved_words: SYNCHRONIZED  */
#line 883 "ada.y"
                              {(yyval.id) = malloc(13);
                               strcpy((yyval.id), "synchronized");
                              }
#line 3283 "ada.tab.c"
    break;

  case 56: /* new_ada_95_reserved_words: ABSTRACT  */
#line 889 "ada.y"
                              {(yyval.id) = malloc(10);
                               strcpy((yyval.id), "abstract");
                              }
#line 3291 "ada.tab.c"
    break;

  case 57: /* new_ada_95_reserved_words: ALIASED  */
#line 893 "ada.y"
                              {(yyval.id) = malloc(11);
                               strcpy((yyval.id), "aliased");
                              }
#line 3299 "ada.tab.c"
    break;

  case 58: /* new_ada_95_reserved_words: PROTECTED  */
#line 897 "ada.y"
                              {(yyval.id) = malloc(13);
                               strcpy((yyval.id), "protected");
                              }
#line 3307 "ada.tab.c"
    break;

  case 59: /* new_ada_95_reserved_words: REQUEUE  */
#line 901 "ada.y"
                              {(yyval.id) = malloc(10);
                               strcpy((yyval.id), "requeue");
                              }
#line 3315 "ada.tab.c"
    break;

  case 60: /* new_ada_95_reserved_words: TAGGED  */
#line 905 "ada.y"
                              {(yyval.id) = malloc(11);
                               strcpy((yyval.id), "tagged");
                              }
#line 3323 "ada.tab.c"
    break;

  case 61: /* new_ada_95_reserved_words: UNTIL  */
#line 909 "ada.y"
                              {(yyval.id) = malloc(13);
                               strcpy((yyval.id), "until");
                              }
#line 3331 "ada.tab.c"
    break;

  case 62: /* object_qualifier_opt: %empty  */
#line 914 "ada.y"
                                        {(yyval.id) = malloc(14); strcpy((yyval.id), "not_qualified");}
#line 3337 "ada.tab.c"
    break;

  case 63: /* object_qualifier_opt: ALIASED  */
#line 915 "ada.y"
                                                     {(yyval.id) = malloc(8); strcpy((yyval.id), "aliased");}
#line 3343 "ada.tab.c"
    break;

  case 64: /* object_qualifier_opt: CONSTANT  */
#line 916 "ada.y"
                                                     {(yyval.id) = malloc(9); strcpy((yyval.id), "constant");}
#line 3349 "ada.tab.c"
    break;

  case 65: /* object_qualifier_opt: ALIASED CONSTANT  */
#line 917 "ada.y"
                                                     {(yyval.id) = malloc(17); strcpy((yyval.id), "aliased_constant");}
#line 3355 "ada.tab.c"
    break;

  case 66: /* object_subtype_definition: subtype_indication  */
#line 921 "ada.y"
                                                        {(yyval.id) = (yyvsp[0].id);}
#line 3361 "ada.tab.c"
    break;

  case 67: /* object_subtype_definition: null_exclusion_opt access_definition  */
#line 923 "ada.y"
                            {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id))+22);
                             strcpy((yyval.id), "access_definition(");
                             strcat((yyval.id), (yyvsp[-1].id));
                             strcat((yyval.id), ", ");
                             strcat((yyval.id), (yyvsp[0].id));
                             strcat((yyval.id), ")");
                             free((yyvsp[-1].id));
                             free((yyvsp[0].id));
                            }
#line 3375 "ada.tab.c"
    break;

  case 68: /* object_subtype_definition: array_type_definition  */
#line 932 "ada.y"
                                                        {(yyval.id) = (yyvsp[0].id);}
#line 3381 "ada.tab.c"
    break;

  case 69: /* init_opt: %empty  */
#line 936 "ada.y"
           {(yyval.id) = malloc((8) );
            strcpy((yyval.id), "no_init");
           }
#line 3389 "ada.tab.c"
    break;

  case 70: /* @6: %empty  */
#line 939 "ada.y"
                                          {(yyval.true_line_column).line = yylineno; (yyval.true_line_column).column = column+1;}
#line 3395 "ada.tab.c"
    break;

  case 71: /* init_opt: IS_ASSIGNED @6 expression_2  */
#line 940 "ada.y"
           {char *expression;
            build_expression((yyvsp[0].id_deci), &expression, current_unit, (yyvsp[-1].true_line_column).line, (yyvsp[-1].true_line_column).column);
            (yyval.id) = malloc(SAFETY+strlen(expression)+1);
            strcpy((yyval.id), expression);
            free((yyvsp[0].id_deci).id);
           }
#line 3406 "ada.tab.c"
    break;

  case 72: /* type_declaration: TYPE identifier_rule discriminant_part_opt type_completion ';'  */
#line 949 "ada.y"
                   {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-3].id))+strlen((yyvsp[-2].id))+strlen((yyvsp[-1].id))+12) );
                    strcpy((yyval.id), " type(");
                    strcat((yyval.id), (yyvsp[-3].id));
                    strcat((yyval.id), ", ");
                    strcat((yyval.id), (yyvsp[-2].id));
                    strcat((yyval.id), ", ");
                    strcat((yyval.id), (yyvsp[-1].id));
                    strcat((yyval.id), ")");
                    free((yyvsp[-3].id));
                    free((yyvsp[-2].id));
                    free((yyvsp[-1].id));
                   }
#line 3423 "ada.tab.c"
    break;

  case 73: /* discriminant_part_opt: %empty  */
#line 963 "ada.y"
                                                {(yyval.id) = malloc(SAFETY+16); strcpy((yyval.id), "no_discriminant");}
#line 3429 "ada.tab.c"
    break;

  case 74: /* discriminant_part_opt: discriminant_part  */
#line 964 "ada.y"
                                                {(yyval.id) = (yyvsp[0].id);}
#line 3435 "ada.tab.c"
    break;

  case 75: /* discriminant_part_opt: '(' BOX ')'  */
#line 965 "ada.y"
                                                {(yyval.id) = malloc(4); strcpy((yyval.id), "box");}
#line 3441 "ada.tab.c"
    break;

  case 76: /* type_definition: enumeration_type_definition  */
#line 968 "ada.y"
                                                {(yyval.id) = (yyvsp[0].id);}
#line 3447 "ada.tab.c"
    break;

  case 77: /* type_definition: integer_type_definition  */
#line 969 "ada.y"
                                                {(yyval.id) = (yyvsp[0].id);}
#line 3453 "ada.tab.c"
    break;

  case 78: /* type_definition: real_type_definition  */
#line 970 "ada.y"
                                                {(yyval.id) = (yyvsp[0].id);}
#line 3459 "ada.tab.c"
    break;

  case 79: /* type_definition: array_type_definition  */
#line 971 "ada.y"
                                                {(yyval.id) = (yyvsp[0].id);}
#line 3465 "ada.tab.c"
    break;

  case 80: /* type_definition: access_type_definition  */
#line 972 "ada.y"
                                                {(yyval.id) = (yyvsp[0].id);}
#line 3471 "ada.tab.c"
    break;

  case 81: /* type_definition: ABSTRACT derived_type_definition  */
#line 974 "ada.y"
                  {(yyval.id) = malloc(SAFETY+strlen((yyvsp[0].id))+14);
                   strcpy((yyval.id), "abstract_new(");
                   strcat((yyval.id), (yyvsp[0].id));
                   strcat((yyval.id), ")");
                   free((yyvsp[0].id));
                  }
#line 3482 "ada.tab.c"
    break;

  case 82: /* type_definition: ABSTRACT TAGGED record_type_definition  */
#line 981 "ada.y"
                  {(yyval.id) = malloc(SAFETY+strlen((yyvsp[0].id))+17);
                   strcpy((yyval.id), "abstract_tagged, ");
                   strcat((yyval.id), (yyvsp[0].id));
                   free((yyvsp[0].id));
                  }
#line 3492 "ada.tab.c"
    break;

  case 83: /* type_definition: ABSTRACT TAGGED private_type_definition  */
#line 987 "ada.y"
                  {(yyval.id) = malloc(SAFETY+strlen((yyvsp[0].id))+27);
                   strcpy((yyval.id), "private, abstract_tagged, ");
                   strcat((yyval.id), (yyvsp[0].id));
                   free((yyvsp[0].id));
                  }
#line 3502 "ada.tab.c"
    break;

  case 84: /* type_definition: TAGGED record_type_definition  */
#line 993 "ada.y"
                  {(yyval.id) = malloc(SAFETY+strlen((yyvsp[0].id))+9);
                   strcpy((yyval.id), "tagged, ");
                   strcat((yyval.id), (yyvsp[0].id));
                   free((yyvsp[0].id));
                  }
#line 3512 "ada.tab.c"
    break;

  case 85: /* type_definition: TAGGED private_type_definition  */
#line 999 "ada.y"
                  {(yyval.id) = malloc(SAFETY+strlen((yyvsp[0].id))+27);
                   strcpy((yyval.id), "private, tagged, ");
                   strcat((yyval.id), (yyvsp[0].id));
                   free((yyvsp[0].id));
                  }
#line 3522 "ada.tab.c"
    break;

  case 86: /* type_definition: record_type_definition  */
#line 1005 "ada.y"
                  {(yyval.id) = malloc(SAFETY+strlen((yyvsp[0].id))+13);
                   strcpy((yyval.id), "not_tagged, ");
                   strcat((yyval.id), (yyvsp[0].id));
                   free((yyvsp[0].id));
                  }
#line 3532 "ada.tab.c"
    break;

  case 87: /* type_definition: derived_type_definition  */
#line 1011 "ada.y"
                  {(yyval.id) = malloc(SAFETY+strlen((yyvsp[0].id))+6);
                   strcpy((yyval.id), "new(");
                   strcat((yyval.id), (yyvsp[0].id));
                   strcat((yyval.id), ")");
                   free((yyvsp[0].id));
                  }
#line 3543 "ada.tab.c"
    break;

  case 88: /* type_definition: private_type_definition  */
#line 1018 "ada.y"
                  {(yyval.id) = malloc(SAFETY+strlen((yyvsp[0].id))+27);
                   strcpy((yyval.id), "private, not_tagged, ");
                   strcat((yyval.id), (yyvsp[0].id));
                   free((yyvsp[0].id));
                  }
#line 3553 "ada.tab.c"
    break;

  case 89: /* type_definition: interface_type_definition  */
#line 1023 "ada.y"
                                                {(yyval.id) = (yyvsp[0].id);}
#line 3559 "ada.tab.c"
    break;

  case 90: /* type_completion: %empty  */
#line 1027 "ada.y"
                                        {(yyval.id) = malloc(SAFETY+22); strcpy((yyval.id), "empty_type_completion");}
#line 3565 "ada.tab.c"
    break;

  case 91: /* type_completion: IS TAGGED  */
#line 1028 "ada.y"
                                        {(yyval.id) = malloc(SAFETY+10); strcpy((yyval.id), "is_tagged");}
#line 3571 "ada.tab.c"
    break;

  case 92: /* type_completion: IS type_definition  */
#line 1029 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 3577 "ada.tab.c"
    break;

  case 93: /* subtype_declaration: SUBTYPE identifier_rule IS subtype_indication ';'  */
#line 1034 "ada.y"
                      {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-3].id))+strlen((yyvsp[-1].id))+13) );
                       strcpy((yyval.id), " subtype(");
                       strcat((yyval.id), (yyvsp[-3].id));
                       strcat((yyval.id), ", ");
                       strcat((yyval.id), (yyvsp[-1].id));
                       strcat((yyval.id), ")");
                       free((yyvsp[-3].id));
                       free((yyvsp[-1].id));
                      }
#line 3591 "ada.tab.c"
    break;

  case 94: /* subtype_indication: name constraint  */
#line 1048 "ada.y"
                     {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id))+47);
                      strcpy((yyval.id), "subtype_indication(may_be_null,");
                      strcat((yyval.id), (yyvsp[-1].id));
                      strcat((yyval.id), ", constraint(");
                      strcat((yyval.id), (yyvsp[0].id));
                      strcat((yyval.id), "))");
                      free((yyvsp[-1].id));
                      free((yyvsp[0].id));
                     }
#line 3605 "ada.tab.c"
    break;

  case 95: /* subtype_indication: name  */
#line 1058 "ada.y"
                     {(yyval.id) = malloc(SAFETY+strlen((yyvsp[0].id))+49);
                      strcpy((yyval.id), "subtype_indication(may_be_null,");
                      strcat((yyval.id), (yyvsp[0].id));
                      strcat((yyval.id), ", no_constraint)");
                      free((yyvsp[0].id));
                     }
#line 3616 "ada.tab.c"
    break;

  case 96: /* subtype_indication: NOT NuLL name constraint  */
#line 1065 "ada.y"
                     {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id))+44);
                      strcpy((yyval.id), "subtype_indication(not_null,");
                      strcat((yyval.id), (yyvsp[-1].id));
                      strcat((yyval.id), ", constraint(");
                      strcat((yyval.id), (yyvsp[0].id));
                      strcat((yyval.id), "))");
                      free((yyvsp[-1].id));
                      free((yyvsp[0].id));
                     }
#line 3630 "ada.tab.c"
    break;

  case 97: /* subtype_indication: NOT NuLL name  */
#line 1075 "ada.y"
                     {(yyval.id) = malloc(SAFETY+strlen((yyvsp[0].id))+46);
                      strcpy((yyval.id), "subtype_indication(not_null,");
                      strcat((yyval.id), (yyvsp[0].id));
                      strcat((yyval.id), ", no_constraint)");
                      free((yyvsp[0].id));
                     }
#line 3641 "ada.tab.c"
    break;

  case 98: /* constraint: range_constraint  */
#line 1084 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 3647 "ada.tab.c"
    break;

  case 99: /* constraint: digits_constraint  */
#line 1085 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 3653 "ada.tab.c"
    break;

  case 100: /* constraint: delta_constraint  */
#line 1086 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 3659 "ada.tab.c"
    break;

  case 101: /* digits_constraint: DIGITS simple_expression range_constraint_opt  */
#line 1091 "ada.y"
                    {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-1].id_deci).id)+strlen((yyvsp[0].id))+13) );
                     strcpy((yyval.id), "digits(");
                     strcat((yyval.id), (yyvsp[-1].id_deci).id);
                     strcat((yyval.id), ", ");
                     strcat((yyval.id), (yyvsp[0].id));
                     strcat((yyval.id), ")");
                     free((yyvsp[-1].id_deci).id);
                     free((yyvsp[0].id));
                    }
#line 3673 "ada.tab.c"
    break;

  case 102: /* delta_constraint: DELTA simple_expression range_constraint_opt  */
#line 1105 "ada.y"
                    {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-1].id_deci).id)+strlen((yyvsp[0].id))+13) );
                     strcpy((yyval.id), "delta(");
                     strcat((yyval.id), (yyvsp[-1].id_deci).id);
                     strcat((yyval.id), ", ");
                     strcat((yyval.id), (yyvsp[0].id));
                     strcat((yyval.id), ")");
                     free((yyvsp[-1].id_deci).id);
                     free((yyvsp[0].id));
                    }
#line 3687 "ada.tab.c"
    break;

  case 103: /* derived_type_definition: private_type_kind NEW subtype_indication  */
#line 1118 "ada.y"
                          {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id))+27);
                           strcpy((yyval.id), "derived_type_definition(");
                           strcat((yyval.id), (yyvsp[-2].id));
                           strcat((yyval.id), ", ");
                           strcat((yyval.id), (yyvsp[0].id));
                           strcat((yyval.id), ")");
                           free((yyvsp[-2].id));
                           free((yyvsp[0].id));
                          }
#line 3701 "ada.tab.c"
    break;

  case 104: /* derived_type_definition: private_type_kind NEW subtype_indication interface_list_item_s WITH PRIVATE  */
#line 1128 "ada.y"
                          {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-5].id))+strlen((yyvsp[-3].id))+strlen((yyvsp[-2].id))+44);
                           strcpy((yyval.id), "derived_type_definition(");
                           strcat((yyval.id), (yyvsp[-5].id));
                           strcat((yyval.id), ", ");
                           strcat((yyval.id), (yyvsp[-3].id));
                           strcat((yyval.id), ", ");
                           strcat((yyval.id), (yyvsp[-2].id));
                           strcat((yyval.id), ", with_private");
                           strcat((yyval.id), ")");
                           free((yyvsp[-5].id));
                           free((yyvsp[-3].id));
                           free((yyvsp[-2].id));
                          }
#line 3719 "ada.tab.c"
    break;

  case 105: /* derived_type_definition: private_type_kind NEW subtype_indication interface_list_item_s WITH record_definition  */
#line 1142 "ada.y"
                          {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-5].id))+strlen((yyvsp[-3].id))+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id))+32);
                           strcpy((yyval.id), "derived_type_definition(");
                           strcat((yyval.id), (yyvsp[-5].id));
                           strcat((yyval.id), ", ");
                           strcat((yyval.id), (yyvsp[-3].id));
                           strcat((yyval.id), ", ");
                           strcat((yyval.id), (yyvsp[-2].id));
                           strcat((yyval.id), ", ");
                           strcat((yyval.id), (yyvsp[0].id));
                           strcat((yyval.id), ")");
                           free((yyvsp[-5].id));
                           free((yyvsp[-3].id));
                           free((yyvsp[-2].id));
                           free((yyvsp[0].id));
                          }
#line 3739 "ada.tab.c"
    break;

  case 106: /* private_type_kind: %empty  */
#line 1159 "ada.y"
                                    {(yyval.id) = malloc(SAFETY+27); strcpy((yyval.id), "not_limited_nor_synchronized");}
#line 3745 "ada.tab.c"
    break;

  case 107: /* private_type_kind: LIMITED  */
#line 1160 "ada.y"
                                    {(yyval.id) = malloc(SAFETY+8); strcpy((yyval.id), "limited");}
#line 3751 "ada.tab.c"
    break;

  case 108: /* private_type_kind: SYNCHRONIZED  */
#line 1161 "ada.y"
                                    {(yyval.id) = malloc(SAFETY+13); strcpy((yyval.id), "synchronized");}
#line 3757 "ada.tab.c"
    break;

  case 109: /* range_constraint: RANGE range  */
#line 1166 "ada.y"
                   {(yyval.id) = malloc((SAFETY+strlen((yyvsp[0].id))+8) );
                    strcpy((yyval.id), "range(");
                    strcat((yyval.id), (yyvsp[0].id));
                    strcat((yyval.id), ")");
                    free((yyvsp[0].id));
                   }
#line 3768 "ada.tab.c"
    break;

  case 110: /* range: simple_expression DOT_DOT simple_expression  */
#line 1175 "ada.y"
                                                        {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-2].id_deci).id)+strlen((yyvsp[0].id_deci).id)+5) );
                                                         strcpy((yyval.id), "[");
                                                         strcat((yyval.id), (yyvsp[-2].id_deci).id);
                                                         strcat((yyval.id), ", ");
                                                         strcat((yyval.id), (yyvsp[0].id_deci).id);
                                                         strcat((yyval.id), "]");
                                                         free((yyvsp[-2].id_deci).id);
                                                         free((yyvsp[0].id_deci).id);
                                                        }
#line 3782 "ada.tab.c"
    break;

  case 111: /* range: name TIC RANGE  */
#line 1184 "ada.y"
                                                        {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-2].id))+13);
                                                         strcpy((yyval.id), "tic(");
                                                         strcat((yyval.id), (yyvsp[-2].id));
                                                         strcat((yyval.id), ", ");
                                                         strcat((yyval.id), "range)");
                                                         free((yyvsp[-2].id));
                                                        }
#line 3794 "ada.tab.c"
    break;

  case 112: /* @7: %empty  */
#line 1191 "ada.y"
                                              {(yyval.true_line_column).line = yylineno; (yyval.true_line_column).column = column+1;}
#line 3800 "ada.tab.c"
    break;

  case 113: /* range: name TIC RANGE '(' @7 expression_2 ')'  */
#line 1192 "ada.y"
                                                        {char *expression;
                                                         build_expression((yyvsp[-1].id_deci), &expression, current_unit, (yyvsp[-2].true_line_column).line, (yyvsp[-2].true_line_column).column);
                                                         (yyval.id) = malloc(SAFETY+strlen((yyvsp[-6].id))+strlen(expression)+15);
                                                         strcpy((yyval.id), "tic(");
                                                         strcat((yyval.id), (yyvsp[-6].id));
                                                         strcat((yyval.id), ", ");
                                                         strcat((yyval.id), "range, ");
                                                         strcat((yyval.id), expression);
                                                         strcat((yyval.id), ")");
                                                         free((yyvsp[-6].id));
                                                         free((yyvsp[-1].id_deci).id);
                                                        }
#line 3817 "ada.tab.c"
    break;

  case 114: /* enumeration_type_definition: '(' enum_identifier_list ')'  */
#line 1208 "ada.y"
                              {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-1].id))+16) );
                               strcpy((yyval.id), "enumeration, [");
                               strcat((yyval.id), (yyvsp[-1].id));
                               strcat((yyval.id), "]");
                               free((yyvsp[-1].id));
                              }
#line 3828 "ada.tab.c"
    break;

  case 115: /* enum_identifier_list: enum_identifier  */
#line 1217 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 3834 "ada.tab.c"
    break;

  case 116: /* enum_identifier_list: enum_identifier_list ',' enum_identifier  */
#line 1219 "ada.y"
                       {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id))+3) );
                        strcpy((yyval.id), (yyvsp[-2].id));
                        strcat((yyval.id), ", ");
                        strcat((yyval.id), (yyvsp[0].id));
                        free((yyvsp[-2].id));
                        free((yyvsp[0].id));
                       }
#line 3846 "ada.tab.c"
    break;

  case 117: /* enum_identifier: identifier_rule  */
#line 1229 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 3852 "ada.tab.c"
    break;

  case 118: /* enum_identifier: character_literal  */
#line 1230 "ada.y"
                                                {(yyval.id) = handle_identifiers((yyvsp[0].id_ref), 0);}
#line 3858 "ada.tab.c"
    break;

  case 119: /* integer_type_definition: range_specification  */
#line 1235 "ada.y"
                          {char key1[1000];         /* used for searching double_tst structure */
                           strcpy(key1, "standard.ads:integer");
                           found_decl = decl_search_start(key1); //retrieving standard.ads:integer's actual xrefed name
                           if (!found_decl) {
                             fprintf(stdout, "Mika ERROR: cannot find a cross referenced standard.ads:integer");
                             fflush(stdout);
                             my_exit(27);
                           }
                           (yyval.id) = malloc((SAFETY+strlen((yyvsp[0].id))+12+strlen(found_decl->unique_id)) );
                           strcpy((yyval.id), "integer, ");
                           strcat((yyval.id), found_decl->unique_id);
                           strcat((yyval.id), ", ");
                           strcat((yyval.id), (yyvsp[0].id));
                           free((yyvsp[0].id));
                          }
#line 3878 "ada.tab.c"
    break;

  case 120: /* @8: %empty  */
#line 1250 "ada.y"
                                                 {(yyval.true_line_column).line = yylineno; (yyval.true_line_column).column = column+1;}
#line 3884 "ada.tab.c"
    break;

  case 121: /* integer_type_definition: MOD @8 expression_2  */
#line 1251 "ada.y"
                          {char key1[1000];         /* used for searching double_tst structure */
                           char *expression;
                           build_expression((yyvsp[0].id_deci), &expression, current_unit, (yyvsp[-1].true_line_column).line, (yyvsp[-1].true_line_column).column);
                           strcpy(key1, "standard.ads:integer");
                           found_decl = decl_search_start(key1); //retrieving standard.ads:integer's actual xrefed name
                           if (!found_decl) {
                             fprintf(stdout, "Mika ERROR: cannot find a cross referenced standard.ads:integer");
                             fflush(stdout);
                             my_exit(28);
                           }
                           (yyval.id) = malloc((SAFETY+strlen(expression)+17+strlen(found_decl->unique_id)) );
                           strcpy((yyval.id), "integer, ");
                           strcat((yyval.id), found_decl->unique_id);
                           strcat((yyval.id), ", mod(");
                           strcat((yyval.id), expression);
                           strcat((yyval.id), ")");
                           free((yyvsp[0].id_deci).id);
                          }
#line 3907 "ada.tab.c"
    break;

  case 122: /* real_type_definition: floating_point_definition  */
#line 1273 "ada.y"
                       {char key1[1000];         /* used for searching double_tst structure */
                        strcpy(key1, "standard.ads:float");
                        found_decl = decl_search_start(key1); //retrieving standard.ads:float's actual xrefed name
                        if (!found_decl) {
                          fprintf(stdout, "Mika ERROR: cannot find a cross referenced standard.ads:float");
                          fflush(stdout);
                          my_exit(29);
                        }
                        (yyval.id) = malloc((SAFETY+strlen((yyvsp[0].id))+10+strlen(found_decl->unique_id)) );
                        strcpy((yyval.id), "float, ");
                        strcat((yyval.id), found_decl->unique_id);
                        strcat((yyval.id), ", ");
                        strcat((yyval.id), (yyvsp[0].id));
                        free((yyvsp[0].id));
                       }
#line 3927 "ada.tab.c"
    break;

  case 123: /* real_type_definition: fixed_point_definition  */
#line 1289 "ada.y"
                       {char key1[1000];         /* used for searching double_tst structure */
                        strcpy(key1, "standard.ads:float");
                        found_decl = decl_search_start(key1); //retrieving standard.ads:float's actual xrefed name
                        if (!found_decl) {
                          fprintf(stdout, "Mika ERROR: cannot find a cross referenced standard.ads:float");
                          fflush(stdout);
                          my_exit(30);
                        }
                        (yyval.id) = malloc((SAFETY+strlen((yyvsp[0].id))+10+strlen(found_decl->unique_id)) );
                        strcpy((yyval.id), "fixed, ");
                        strcat((yyval.id), found_decl->unique_id);
                        strcat((yyval.id), ", ");
                        strcat((yyval.id), (yyvsp[0].id));
                        free((yyvsp[0].id));
                       }
#line 3947 "ada.tab.c"
    break;

  case 124: /* @9: %empty  */
#line 1307 "ada.y"
                                                      {(yyval.true_line_column).line = yylineno; (yyval.true_line_column).column = column+1;}
#line 3953 "ada.tab.c"
    break;

  case 125: /* floating_point_definition: DIGITS @9 expression_2 range_specification_opt  */
#line 1308 "ada.y"
                            {char *expression;
                             build_expression((yyvsp[-1].id_deci), &expression, current_unit, (yyvsp[-2].true_line_column).line, (yyvsp[-2].true_line_column).column);
                             (yyval.id) = malloc(SAFETY+strlen(expression)+strlen((yyvsp[0].id))+11);
                             strcpy((yyval.id), "digits(");
                             strcat((yyval.id), expression);
                             strcat((yyval.id), ", ");
                             strcat((yyval.id), (yyvsp[0].id));
                             strcat((yyval.id), ")");
                             free((yyvsp[-1].id_deci).id);
                             free((yyvsp[0].id));
                            }
#line 3969 "ada.tab.c"
    break;

  case 126: /* range_specification_opt: %empty  */
#line 1322 "ada.y"
                          {(yyval.id) = malloc((12) );
                           strcpy((yyval.id), "empty_range");
                          }
#line 3977 "ada.tab.c"
    break;

  case 127: /* range_specification_opt: range_specification  */
#line 1325 "ada.y"
                                                           {(yyval.id) = (yyvsp[0].id);}
#line 3983 "ada.tab.c"
    break;

  case 128: /* range_specification: range_constraint  */
#line 1328 "ada.y"
                                       {(yyval.id) = (yyvsp[0].id);}
#line 3989 "ada.tab.c"
    break;

  case 129: /* range_constraint_opt: %empty  */
#line 1332 "ada.y"
                       {(yyval.id) = malloc((12) );
                        strcpy((yyval.id), "empty_range");
                       }
#line 3997 "ada.tab.c"
    break;

  case 130: /* range_constraint_opt: range_constraint  */
#line 1336 "ada.y"
                               {(yyval.id) = malloc((2+strlen((yyvsp[0].id))) );
                                    strcpy((yyval.id), (yyvsp[0].id));
                                    free((yyvsp[0].id));
                               }
#line 4006 "ada.tab.c"
    break;

  case 131: /* fixed_point_definition: DELTA expression range_specification  */
#line 1345 "ada.y"
                         {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id))+10) );
                          strcpy((yyval.id), "delta(");
                          strcat((yyval.id), (yyvsp[-1].id));
                          strcat((yyval.id), ", ");
                          strcat((yyval.id), (yyvsp[0].id));
                          strcat((yyval.id), ")");
                          free((yyvsp[-1].id));
                          free((yyvsp[0].id));
                         }
#line 4020 "ada.tab.c"
    break;

  case 132: /* fixed_point_definition: DELTA expression DIGITS expression range_specification_opt  */
#line 1355 "ada.y"
                         {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-3].id))+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id))+19) );
                          strcpy((yyval.id), "delta_digits(");
                          strcat((yyval.id), (yyvsp[-3].id));
                          strcat((yyval.id), ", ");
                          strcat((yyval.id), (yyvsp[-1].id));
                          strcat((yyval.id), ", ");
                          strcat((yyval.id), (yyvsp[0].id));
                          strcat((yyval.id), ")");
                          free((yyvsp[-3].id));
                          free((yyvsp[-1].id));
                          free((yyvsp[0].id));
                         }
#line 4037 "ada.tab.c"
    break;

  case 133: /* record_type_definition: limited_opt record_definition  */
#line 1371 "ada.y"
                         {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id))+5);
                          strcpy((yyval.id), (yyvsp[-1].id));
                          strcat((yyval.id), ", ");
                          strcat((yyval.id), (yyvsp[0].id));
                          free((yyvsp[-1].id));
                          free((yyvsp[0].id));
                         }
#line 4049 "ada.tab.c"
    break;

  case 134: /* record_definition: RECORD pragma_s component_list END RECORD  */
#line 1381 "ada.y"
                    {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-2].id))+11) );
                     strcpy((yyval.id), "record([");
                     strcat((yyval.id), (yyvsp[-2].id));
                     strcat((yyval.id), "])");
                     free((yyvsp[-3].id));
                     free((yyvsp[-2].id));
                    }
#line 4061 "ada.tab.c"
    break;

  case 135: /* record_definition: NuLL RECORD  */
#line 1389 "ada.y"
                    {(yyval.id) = malloc((20) );
                     strcpy((yyval.id), "record(null_record)");
                    }
#line 4069 "ada.tab.c"
    break;

  case 136: /* component_list: component_declaration_list variant_part_opt  */
#line 1395 "ada.y"
                 {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id))+3) );
                  strcpy((yyval.id), (yyvsp[-1].id));
                  strcat((yyval.id), ", ");
                  strcat((yyval.id), (yyvsp[0].id));
                  free((yyvsp[-1].id));
                  free((yyvsp[0].id));
                 }
#line 4081 "ada.tab.c"
    break;

  case 137: /* component_list: variant_part pragma_s  */
#line 1403 "ada.y"
                 {(yyval.id) = (yyvsp[-1].id);
                  free((yyvsp[0].id));
                 }
#line 4089 "ada.tab.c"
    break;

  case 138: /* component_list: NuLL ';' pragma_s  */
#line 1407 "ada.y"
                 {(yyval.id) = malloc((5) );
                  strcpy((yyval.id), "null");
                  free((yyvsp[0].id));
                 }
#line 4098 "ada.tab.c"
    break;

  case 139: /* component_declaration_list: component_declaration  */
#line 1413 "ada.y"
                                                        {(yyval.id) = (yyvsp[0].id);}
#line 4104 "ada.tab.c"
    break;

  case 140: /* component_declaration_list: component_declaration_list pragma_s component_declaration  */
#line 1415 "ada.y"
                             {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id))+3) );
                              strcpy((yyval.id), (yyvsp[-2].id));
                              strcat((yyval.id), ", \n");
                              strcat((yyval.id), (yyvsp[0].id));
                              free((yyvsp[-2].id));
                              free((yyvsp[-1].id));
                              free((yyvsp[0].id));
                             }
#line 4117 "ada.tab.c"
    break;

  case 141: /* variant_part_opt: pragma_s  */
#line 1426 "ada.y"
                   {(yyval.id) = malloc(11);
                    strcpy((yyval.id), "no_variant");
                    free((yyvsp[0].id));
                   }
#line 4126 "ada.tab.c"
    break;

  case 142: /* variant_part_opt: pragma_s variant_part pragma_s  */
#line 1431 "ada.y"
                   {(yyval.id) = (yyvsp[-1].id);
                    free((yyvsp[-2].id));
                   }
#line 4134 "ada.tab.c"
    break;

  case 143: /* component_declaration: identifier_list ':' component_definition init_opt ';'  */
#line 1437 "ada.y"
                        {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-4].id))+strlen((yyvsp[-2].id))+strlen((yyvsp[-1].id))+9) );
                         strcpy((yyval.id), "([");
                         strcat((yyval.id), (yyvsp[-4].id));
                         strcat((yyval.id), "], ");
                         strcat((yyval.id), (yyvsp[-2].id));
                         strcat((yyval.id), ", ");
                         strcat((yyval.id), (yyvsp[-1].id));
                         strcat((yyval.id), ")");
                         free((yyvsp[-4].id));
                         free((yyvsp[-2].id));
                         free((yyvsp[-1].id));
                        }
#line 4151 "ada.tab.c"
    break;

  case 144: /* discriminant_part: '(' discriminant_spec_s ')'  */
#line 1452 "ada.y"
                     {(yyval.id) = malloc(17+strlen((yyvsp[-1].id)));
                      strcpy((yyval.id), "discriminant([");
                      strcat((yyval.id), (yyvsp[-1].id));
                      strcat((yyval.id), "])");
                      free((yyvsp[-1].id));
                     }
#line 4162 "ada.tab.c"
    break;

  case 145: /* discriminant_spec_s: discriminant_spec  */
#line 1460 "ada.y"
                                                {(yyval.id) = (yyvsp[0].id);}
#line 4168 "ada.tab.c"
    break;

  case 146: /* discriminant_spec_s: discriminant_spec_s ';' discriminant_spec  */
#line 1462 "ada.y"
                      {(yyval.id) = malloc(3+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id)));
                       strcpy((yyval.id), (yyvsp[-2].id));
                       strcat((yyval.id), ", ");
                       strcat((yyval.id), (yyvsp[0].id));
                       free((yyvsp[-2].id));
                       free((yyvsp[0].id));
                      }
#line 4180 "ada.tab.c"
    break;

  case 147: /* discriminant_spec: identifier_list ':' access_or_subtype_disc init_opt  */
#line 1473 "ada.y"
                    {(yyval.id) = malloc(28+strlen((yyvsp[-3].id))+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id)));
                     strcpy((yyval.id), "discriminant_spec([");
                     strcat((yyval.id), (yyvsp[-3].id));
                     strcat((yyval.id), "], ");
                     strcat((yyval.id), (yyvsp[-1].id));
                     strcat((yyval.id), ", ");
                     strcat((yyval.id), (yyvsp[0].id));
                     strcat((yyval.id), ")");
                     free((yyvsp[-3].id));
                     free((yyvsp[-1].id));
                     free((yyvsp[0].id));
                    }
#line 4197 "ada.tab.c"
    break;

  case 148: /* access_or_subtype_disc: null_exclusion_opt access_definition  */
#line 1489 "ada.y"
                         {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id))+22);
                          strcpy((yyval.id), "access_definition(");
                          strcat((yyval.id), (yyvsp[-1].id));
                          strcat((yyval.id), ", ");
                          strcat((yyval.id), (yyvsp[0].id));
                          strcat((yyval.id), ")");
                          free((yyvsp[-1].id));
                          free((yyvsp[0].id));
                         }
#line 4211 "ada.tab.c"
    break;

  case 149: /* access_or_subtype_disc: subtype_indication  */
#line 1499 "ada.y"
                         {(yyval.id) = (yyvsp[0].id);}
#line 4217 "ada.tab.c"
    break;

  case 150: /* variant_part: CASE direct_name IS pragma_s variant_s END CASE ';'  */
#line 1503 "ada.y"
               {(yyval.id) = malloc((8) );
                strcpy((yyval.id), "variant");
                free((yyvsp[-4].id));
               }
#line 4226 "ada.tab.c"
    break;

  case 151: /* variant_s: variant  */
#line 1509 "ada.y"
                                {;}
#line 4232 "ada.tab.c"
    break;

  case 152: /* variant_s: variant_s variant  */
#line 1510 "ada.y"
                                {;}
#line 4238 "ada.tab.c"
    break;

  case 153: /* variant: WHEN choice_s RIGHT_SHAFT pragma_s component_list  */
#line 1514 "ada.y"
          {free((yyvsp[-3].id));
           free((yyvsp[-1].id));
           free((yyvsp[0].id));
          }
#line 4247 "ada.tab.c"
    break;

  case 154: /* array_type_definition: unconstrained_array_definition  */
#line 1522 "ada.y"
                                                              {(yyval.id) = (yyvsp[0].id);}
#line 4253 "ada.tab.c"
    break;

  case 155: /* array_type_definition: constrained_array_definition  */
#line 1523 "ada.y"
                                                              {(yyval.id) = (yyvsp[0].id);}
#line 4259 "ada.tab.c"
    break;

  case 156: /* unconstrained_array_definition: ARRAY '(' index_subtype_definition_list ')' OF component_definition  */
#line 1527 "ada.y"
                           {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-3].id))+strlen((yyvsp[0].id))+20) );
                            strcpy((yyval.id), "unconst_array, [");
                            strcat((yyval.id), (yyvsp[-3].id));
                            strcat((yyval.id), "], ");
                            strcat((yyval.id), (yyvsp[0].id));
                            free((yyvsp[-3].id));
                            free((yyvsp[0].id));
                           }
#line 4272 "ada.tab.c"
    break;

  case 157: /* index_subtype_definition_list: index_subtype_definition  */
#line 1537 "ada.y"
                                                         {(yyval.id) = (yyvsp[0].id);}
#line 4278 "ada.tab.c"
    break;

  case 158: /* index_subtype_definition_list: index_subtype_definition_list ',' index_subtype_definition  */
#line 1539 "ada.y"
                                {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id))+3) );
                                 strcpy((yyval.id), (yyvsp[-2].id));
                                 strcat((yyval.id), ", ");
                                 strcat((yyval.id), (yyvsp[0].id));
                                 free((yyvsp[-2].id));
                                 free((yyvsp[0].id));
                                }
#line 4290 "ada.tab.c"
    break;

  case 159: /* index_subtype_definition: name RANGE BOX  */
#line 1549 "ada.y"
                           {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-2].id))+12) );
                            strcpy((yyval.id), "range(");
                            strcat((yyval.id), (yyvsp[-2].id));
                            strcat((yyval.id), ", ");
                            strcat((yyval.id), "box)");
                            free((yyvsp[-2].id));
                           }
#line 4302 "ada.tab.c"
    break;

  case 160: /* constrained_array_definition: ARRAY '(' discrete_range_list ')' OF component_definition  */
#line 1560 "ada.y"
                         {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-3].id))+strlen((yyvsp[0].id))+12) );
                          strcpy((yyval.id), "array, [");
                          strcat((yyval.id), (yyvsp[-3].id));
                          strcat((yyval.id), "], ");
                          strcat((yyval.id), (yyvsp[0].id));
                          free((yyvsp[-3].id));
                          free((yyvsp[0].id));
                         }
#line 4315 "ada.tab.c"
    break;

  case 161: /* discrete_range_list: discrete_range  */
#line 1571 "ada.y"
                                     {(yyval.id) = (yyvsp[0].id);}
#line 4321 "ada.tab.c"
    break;

  case 162: /* discrete_range_list: discrete_range_list ',' discrete_range  */
#line 1573 "ada.y"
                      {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id))+3) );
                       strcpy((yyval.id), (yyvsp[-2].id));
                       strcat((yyval.id), ", ");
                       strcat((yyval.id), (yyvsp[0].id));
                       free((yyvsp[-2].id));
                       free((yyvsp[0].id));
                      }
#line 4333 "ada.tab.c"
    break;

  case 163: /* component_definition: aliased_opt subtype_indication  */
#line 1585 "ada.y"
                       {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id))+26);
                        strcpy((yyval.id), "component_definition(");
                        strcat((yyval.id), (yyvsp[-1].id));
                        strcat((yyval.id), ", ");
                        strcat((yyval.id), (yyvsp[0].id));
                        strcat((yyval.id), ")");
                        free((yyvsp[-1].id));
                        free((yyvsp[0].id));
                       }
#line 4347 "ada.tab.c"
    break;

  case 164: /* component_definition: aliased_opt null_exclusion_opt access_definition  */
#line 1595 "ada.y"
                       {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[-1].id))+46);
                        strcpy((yyval.id), "component_definition(");
                        strcat((yyval.id), (yyvsp[-2].id));
                        strcat((yyval.id), ", access_definition(");
                        strcat((yyval.id), (yyvsp[-1].id));
                        strcat((yyval.id), ", ");
                        strcat((yyval.id), (yyvsp[0].id));
                        strcat((yyval.id), ")");
                        strcat((yyval.id), ")");
                        free((yyvsp[-2].id));
                        free((yyvsp[-1].id));
                        free((yyvsp[0].id));
                       }
#line 4365 "ada.tab.c"
    break;

  case 165: /* aliased_opt: %empty  */
#line 1610 "ada.y"
                              {(yyval.id) = malloc((SAFETY+12) ); strcpy((yyval.id), "not_aliased");}
#line 4371 "ada.tab.c"
    break;

  case 166: /* aliased_opt: ALIASED  */
#line 1611 "ada.y"
                              {(yyval.id) = malloc((SAFETY+8) ); strcpy((yyval.id), "aliased");}
#line 4377 "ada.tab.c"
    break;

  case 167: /* aggregate: '(' comp_assoc ')'  */
#line 1615 "ada.y"
                                {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-1].id))+3);   //named aggregate with a single choice
                                 strcpy((yyval.id), "[");
                                 strcat((yyval.id), (yyvsp[-1].id));
                                 strcat((yyval.id), "]");
                                 free((yyvsp[-1].id));
                                }
#line 4388 "ada.tab.c"
    break;

  case 168: /* aggregate: '(' value_s_2 ')'  */
#line 1621 "ada.y"
                                {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-1].id))+3);   //aggregate (could be mixed named and positional)
                                 strcpy((yyval.id), "[");
                                 strcat((yyval.id), (yyvsp[-1].id));
                                 strcat((yyval.id), "]");
                                 free((yyvsp[-1].id));
                                }
#line 4399 "ada.tab.c"
    break;

  case 169: /* aggregate: '(' expression WITH value_list ')'  */
#line 1628 "ada.y"
            {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-3].id))+strlen((yyvsp[-1].id))+9) );
             strcpy((yyval.id), "with(");
             strcat((yyval.id), (yyvsp[-3].id));
             strcat((yyval.id), ", ");
             strcat((yyval.id), (yyvsp[-1].id));
             strcat((yyval.id), ")");
             free((yyvsp[-3].id));
             free((yyvsp[-1].id));
            }
#line 4413 "ada.tab.c"
    break;

  case 170: /* aggregate: '(' expression WITH NuLL RECORD ')'  */
#line 1638 "ada.y"
            {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-4].id))+20) );
             strcpy((yyval.id), "with(");
             strcat((yyval.id), (yyvsp[-4].id));
             strcat((yyval.id), ", null_record)");
             free((yyvsp[-4].id));
            }
#line 4424 "ada.tab.c"
    break;

  case 171: /* aggregate: '(' NuLL RECORD ')'  */
#line 1645 "ada.y"
            {(yyval.id) = malloc((SAFETY+12) );
             strcpy((yyval.id), "null_record");
            }
#line 4432 "ada.tab.c"
    break;

  case 172: /* value_s_2: value ',' value  */
#line 1651 "ada.y"
                                {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id))+4) );
                                 strcpy((yyval.id), (yyvsp[-2].id));
                                 strcat((yyval.id), ",\n");
                                 strcat((yyval.id), (yyvsp[0].id));
                                 free((yyvsp[-2].id));
                                 free((yyvsp[0].id));
                                }
#line 4444 "ada.tab.c"
    break;

  case 173: /* value_s_2: value_s_2 ',' value  */
#line 1658 "ada.y"
                                {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id))+4) );
                                 strcpy((yyval.id), (yyvsp[-2].id));
                                 strcat((yyval.id), ",\n");
                                 strcat((yyval.id), (yyvsp[0].id));
                                 free((yyvsp[-2].id));
                                 free((yyvsp[0].id));
                                }
#line 4456 "ada.tab.c"
    break;

  case 174: /* @10: %empty  */
#line 1667 "ada.y"
                                                     {(yyval.true_line_column).line = yylineno; (yyval.true_line_column).column = column+1;}
#line 4462 "ada.tab.c"
    break;

  case 175: /* comp_assoc: choice_s RIGHT_SHAFT @10 expression_2  */
#line 1668 "ada.y"
             {char *expression;
              build_expression((yyvsp[0].id_deci), &expression, current_unit, (yyvsp[-1].true_line_column).line, (yyvsp[-1].true_line_column).column);
              (yyval.id) = malloc(strlen((yyvsp[-3].id))+strlen(expression)+12);
              strcpy((yyval.id), "named([");
              strcat((yyval.id), (yyvsp[-3].id));
              strcat((yyval.id), "], ");
              strcat((yyval.id), expression);
              strcat((yyval.id), ")");
              free((yyvsp[-3].id));
              free((yyvsp[0].id_deci).id);
             }
#line 4478 "ada.tab.c"
    break;

  case 176: /* comp_assoc: choice_s RIGHT_SHAFT BOX  */
#line 1680 "ada.y"
             {(yyval.id) = malloc(strlen((yyvsp[-2].id))+15);
              strcpy((yyval.id), "named([");
              strcat((yyval.id), (yyvsp[-2].id));
              strcat((yyval.id), "], box)");
              free((yyvsp[-2].id));
             }
#line 4489 "ada.tab.c"
    break;

  case 177: /* choice_s: choice  */
#line 1689 "ada.y"
                                {(yyval.id) = (yyvsp[0].id);}
#line 4495 "ada.tab.c"
    break;

  case 178: /* choice_s: choice_s '|' choice  */
#line 1690 "ada.y"
                                {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id))+3) );
                                 strcpy((yyval.id), (yyvsp[-2].id));
                                 strcat((yyval.id), ", ");
                                 strcat((yyval.id), (yyvsp[0].id));
                                 free((yyvsp[-2].id));
                                 free((yyvsp[0].id));
                                }
#line 4507 "ada.tab.c"
    break;

  case 179: /* choice: expression  */
#line 1699 "ada.y"
                                {(yyval.id) = (yyvsp[0].id);}
#line 4513 "ada.tab.c"
    break;

  case 180: /* choice: discrete_with_range  */
#line 1700 "ada.y"
                                {(yyval.id) = (yyvsp[0].id);}
#line 4519 "ada.tab.c"
    break;

  case 181: /* choice: OTHERS  */
#line 1701 "ada.y"
                                {(yyval.id) = malloc((SAFETY+7) ); strcpy((yyval.id), "others");}
#line 4525 "ada.tab.c"
    break;

  case 182: /* discrete_range: subtype_indication  */
#line 1705 "ada.y"
                                                {(yyval.id) = (yyvsp[0].id);}
#line 4531 "ada.tab.c"
    break;

  case 183: /* discrete_range: range  */
#line 1706 "ada.y"
                                                {(yyval.id) = (yyvsp[0].id);}
#line 4537 "ada.tab.c"
    break;

  case 184: /* discrete_with_range: name range_constraint  */
#line 1710 "ada.y"
                                                {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id))+3) );
                                                 strcpy((yyval.id), (yyvsp[-1].id));
                                                 strcat((yyval.id), ", ");
                                                 strcat((yyval.id), (yyvsp[0].id));
                                                 free((yyvsp[-1].id));
                                                 free((yyvsp[0].id));
                                                }
#line 4549 "ada.tab.c"
    break;

  case 185: /* discrete_with_range: range  */
#line 1717 "ada.y"
                                                {(yyval.id) = (yyvsp[0].id);}
#line 4555 "ada.tab.c"
    break;

  case 186: /* interface_type_definition: kind_opt INTERFACE interface_list_item_s  */
#line 1723 "ada.y"
                            {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id))+14);
                             strcpy((yyval.id), "interface, ");
                             strcat((yyval.id), (yyvsp[-2].id));
                             strcat((yyval.id), ", ");
                             strcat((yyval.id), (yyvsp[0].id));
                             free((yyvsp[-2].id));
                             free((yyvsp[0].id));
                            }
#line 4568 "ada.tab.c"
    break;

  case 187: /* kind_opt: %empty  */
#line 1733 "ada.y"
                            {(yyval.id) = malloc(SAFETY+6); strcpy((yyval.id), "plain");}
#line 4574 "ada.tab.c"
    break;

  case 188: /* kind_opt: LIMITED  */
#line 1734 "ada.y"
                            {(yyval.id) = malloc(SAFETY+8); strcpy((yyval.id), "limited");}
#line 4580 "ada.tab.c"
    break;

  case 189: /* kind_opt: TASK  */
#line 1735 "ada.y"
                            {(yyval.id) = malloc(SAFETY+5); strcpy((yyval.id), "task");}
#line 4586 "ada.tab.c"
    break;

  case 190: /* kind_opt: PROTECTED  */
#line 1736 "ada.y"
                            {(yyval.id) = malloc(SAFETY+10); strcpy((yyval.id), "protected");}
#line 4592 "ada.tab.c"
    break;

  case 191: /* kind_opt: SYNCHRONIZED  */
#line 1737 "ada.y"
                            {(yyval.id) = malloc(SAFETY+6); strcpy((yyval.id), "plain");}
#line 4598 "ada.tab.c"
    break;

  case 192: /* interface_list_item_s: %empty  */
#line 1741 "ada.y"
                        {(yyval.id) = malloc(SAFETY+3);
                         strcpy((yyval.id), "[]");
                        }
#line 4606 "ada.tab.c"
    break;

  case 193: /* interface_list_item_s: AND interface_list_item_sl  */
#line 1745 "ada.y"
                        {(yyval.id) = malloc(SAFETY+strlen((yyvsp[0].id))+3);
                         strcpy((yyval.id), "[");
                         strcat((yyval.id), (yyvsp[0].id));
                         strcat((yyval.id), "]");
                         free((yyvsp[-1].id_ref).id);
                         free((yyvsp[0].id));
                        }
#line 4618 "ada.tab.c"
    break;

  case 194: /* interface_list_item_sl: name  */
#line 1754 "ada.y"
                              {(yyval.id) = (yyvsp[0].id);}
#line 4624 "ada.tab.c"
    break;

  case 195: /* interface_list_item_sl: interface_list_item_sl AND name  */
#line 1756 "ada.y"
                         {(yyval.id) = malloc(strlen((yyvsp[-2].id))+strlen((yyvsp[0].id))+3);
                          strcpy((yyval.id), (yyvsp[-2].id));
                          strcat((yyval.id), ", ");
                          strcat((yyval.id), (yyvsp[0].id));
                          free((yyvsp[-2].id));
                          free((yyvsp[-1].id_ref).id);
                          free((yyvsp[0].id));
                         }
#line 4637 "ada.tab.c"
    break;

  case 196: /* access_type_definition: null_exclusion_opt access_type_definition_part  */
#line 1769 "ada.y"
                         {(yyval.id) = malloc(SAFETY+3+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id)));
                          strcpy((yyval.id), (yyvsp[-1].id));
                          strcat((yyval.id), ", ");
                          strcat((yyval.id), (yyvsp[0].id));
                          free((yyvsp[-1].id));
                          free((yyvsp[0].id));
                         }
#line 4649 "ada.tab.c"
    break;

  case 197: /* access_type_definition_part: ACCESS subtype_indication  */
#line 1779 "ada.y"
                         {(yyval.id) = malloc(9+strlen((yyvsp[0].id)));
                          strcpy((yyval.id), "access, ");
                          strcat((yyval.id), (yyvsp[0].id));
                          free((yyvsp[0].id));
                         }
#line 4659 "ada.tab.c"
    break;

  case 198: /* access_type_definition_part: ACCESS CONSTANT subtype_indication  */
#line 1785 "ada.y"
                         {(yyval.id) = malloc((18+strlen((yyvsp[0].id))) );
                          strcpy((yyval.id), "access_constant, ");
                          strcat((yyval.id), (yyvsp[0].id));
                          free((yyvsp[0].id));
                         }
#line 4669 "ada.tab.c"
    break;

  case 199: /* access_type_definition_part: ACCESS ALL subtype_indication  */
#line 1791 "ada.y"
                        {(yyval.id) = malloc((13+strlen((yyvsp[0].id))) );
                         strcpy((yyval.id), "access_all, ");
                         strcat((yyval.id), (yyvsp[0].id));
                         free((yyvsp[0].id));
                        }
#line 4679 "ada.tab.c"
    break;

  case 200: /* access_type_definition_part: ACCESS protected_opt PROCEDURE formal_part_opt  */
#line 1797 "ada.y"
                         {(yyval.id) = malloc((35+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id))) );
                          strcpy((yyval.id), "access_procedure, ");
                          strcat((yyval.id), (yyvsp[-2].id));
                          strcat((yyval.id), ", parameters([");
                          strcat((yyval.id), (yyvsp[0].id));
                          strcat((yyval.id), "])");
                          free((yyvsp[-2].id));
                          free((yyvsp[0].id));
                         }
#line 4693 "ada.tab.c"
    break;

  case 201: /* access_type_definition_part: ACCESS protected_opt FUNCTION formal_part_opt RETURN access_or_subtype_disc  */
#line 1807 "ada.y"
                         {(yyval.id) = malloc(36+strlen((yyvsp[-4].id))+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id)));
                          strcpy((yyval.id), "access_function, ");
                          strcat((yyval.id), (yyvsp[-4].id));
                          strcat((yyval.id), ", parameters([");
                          strcat((yyval.id), (yyvsp[-2].id));
                          strcat((yyval.id), "]), ");
                          strcat((yyval.id), (yyvsp[0].id)),
                          free((yyvsp[-4].id));
                          free((yyvsp[-2].id));
                          free((yyvsp[0].id));
                         }
#line 4709 "ada.tab.c"
    break;

  case 202: /* protected_opt: %empty  */
#line 1820 "ada.y"
                                {(yyval.id) = malloc(SAFETY+14); strcpy((yyval.id), "not_protected");}
#line 4715 "ada.tab.c"
    break;

  case 203: /* protected_opt: PROTECTED  */
#line 1821 "ada.y"
                                {(yyval.id) = malloc(SAFETY+10); strcpy((yyval.id), "protected");}
#line 4721 "ada.tab.c"
    break;

  case 204: /* null_exclusion_opt: %empty  */
#line 1825 "ada.y"
                                        {(yyval.id) = malloc(SAFETY+12); strcpy((yyval.id), "may_be_null");}
#line 4727 "ada.tab.c"
    break;

  case 205: /* null_exclusion_opt: NOT NuLL  */
#line 1826 "ada.y"
                                        {(yyval.id) = malloc(SAFETY+9); strcpy((yyval.id), "not_null");}
#line 4733 "ada.tab.c"
    break;

  case 206: /* access_definition: ACCESS name  */
#line 1831 "ada.y"
                    {(yyval.id) = malloc(SAFETY+strlen((yyvsp[0].id))+9);
                     strcpy((yyval.id), "access, ");
                     strcat((yyval.id), (yyvsp[0].id));
                     free((yyvsp[0].id));
                    }
#line 4743 "ada.tab.c"
    break;

  case 207: /* access_definition: ACCESS CONSTANT name  */
#line 1837 "ada.y"
                    {(yyval.id) = malloc(SAFETY+strlen((yyvsp[0].id))+18);
                     strcpy((yyval.id), "access_constant, ");
                     strcat((yyval.id), (yyvsp[0].id));
                     free((yyvsp[0].id));
                    }
#line 4753 "ada.tab.c"
    break;

  case 208: /* access_definition: ACCESS protected_opt PROCEDURE formal_part_opt  */
#line 1843 "ada.y"
                    {(yyval.id) = malloc((35+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id))) );
                     strcpy((yyval.id), "access_procedure, ");
                     strcat((yyval.id), (yyvsp[-2].id));
                     strcat((yyval.id), ", parameters([");
                     strcat((yyval.id), (yyvsp[0].id));
                     strcat((yyval.id), "])");
                     free((yyvsp[-2].id));
                     free((yyvsp[0].id));
                    }
#line 4767 "ada.tab.c"
    break;

  case 209: /* access_definition: ACCESS protected_opt FUNCTION formal_part_opt RETURN access_or_subtype_disc  */
#line 1853 "ada.y"
                    {(yyval.id) = malloc(36+strlen((yyvsp[-4].id))+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id)));
                     strcpy((yyval.id), "access_function, ");
                     strcat((yyval.id), (yyvsp[-4].id));
                     strcat((yyval.id), ", parameters([");
                     strcat((yyval.id), (yyvsp[-2].id));
                     strcat((yyval.id), "]), ");
                     strcat((yyval.id), (yyvsp[0].id)),
                     free((yyvsp[-4].id));
                     free((yyvsp[-2].id));
                     free((yyvsp[0].id));
                    }
#line 4783 "ada.tab.c"
    break;

  case 210: /* name: direct_name  */
#line 1867 "ada.y"
                                    {(yyval.id) = (yyvsp[0].id);}
#line 4789 "ada.tab.c"
    break;

  case 211: /* name: indexed_component  */
#line 1868 "ada.y"
                                    {(yyval.id) = (yyvsp[0].id);}
#line 4795 "ada.tab.c"
    break;

  case 212: /* name: selected_component  */
#line 1869 "ada.y"
                                    {(yyval.id) = (yyvsp[0].id);}
#line 4801 "ada.tab.c"
    break;

  case 213: /* name: attribute_reference  */
#line 1870 "ada.y"
                                    {(yyval.id) = (yyvsp[0].id);}
#line 4807 "ada.tab.c"
    break;

  case 214: /* name: operator_symbol_or_string  */
#line 1871 "ada.y"
                                    {(yyval.id) = (yyvsp[0].id);}
#line 4813 "ada.tab.c"
    break;

  case 215: /* name: qualified_expression  */
#line 1872 "ada.y"
                                    {(yyval.id) = (yyvsp[0].id);}
#line 4819 "ada.tab.c"
    break;

  case 216: /* compound_name: identifier_rule  */
#line 1877 "ada.y"
                                {(yyval.id) = (yyvsp[0].id);}
#line 4825 "ada.tab.c"
    break;

  case 217: /* compound_name: compound_name '.' identifier_rule  */
#line 1879 "ada.y"
                {(yyval.id) = (yyvsp[0].id);
                 free((yyvsp[-2].id));
                }
#line 4833 "ada.tab.c"
    break;

  case 218: /* direct_name: identifier_rule  */
#line 1886 "ada.y"
              {(yyval.id) = (yyvsp[0].id);}
#line 4839 "ada.tab.c"
    break;

  case 219: /* operator_symbol_or_string: string_literal  */
#line 1890 "ada.y"
                            {(yyval.id) = handle_identifiers((yyvsp[0].id_ref), 1); //1 because it is a string
                             if (!strncmp((yyval.id), "string(", 7)) { //not a cross referenced operator
                               strcpy(not_cross_referenced_operator, (yyval.id));
                               if (debugMode) fprintf(stdout, "not_cross_referenced_operator is: %s\n", not_cross_referenced_operator);
                             }
                            }
#line 4850 "ada.tab.c"
    break;

  case 220: /* indexed_component: name '(' value_list ')'  */
#line 1901 "ada.y"
                    {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-3].id))+strlen((yyvsp[-1].id))+14);
                     strcpy((yyval.id), "indexed(");
                     strcat((yyval.id), (yyvsp[-3].id));
                     strcat((yyval.id), ", [");
                     strcat((yyval.id), (yyvsp[-1].id));
                     strcat((yyval.id), "])");
                     free((yyvsp[-3].id));
                     free((yyvsp[-1].id));
                    }
#line 4864 "ada.tab.c"
    break;

  case 221: /* value_list: value  */
#line 1912 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 4870 "ada.tab.c"
    break;

  case 222: /* value_list: value_list ',' value  */
#line 1913 "ada.y"
                                        {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id))+3);
                                         strcpy((yyval.id), (yyvsp[-2].id));
                                         strcat((yyval.id), ", ");
                                         strcat((yyval.id), (yyvsp[0].id));
                                         free((yyvsp[-2].id));
                                         free((yyvsp[0].id));
                                        }
#line 4882 "ada.tab.c"
    break;

  case 223: /* value: expression  */
#line 1922 "ada.y"
                            {(yyval.id) = (yyvsp[0].id);}
#line 4888 "ada.tab.c"
    break;

  case 224: /* value: comp_assoc  */
#line 1923 "ada.y"
                            {(yyval.id) = (yyvsp[0].id);}
#line 4894 "ada.tab.c"
    break;

  case 225: /* value: discrete_with_range  */
#line 1924 "ada.y"
                            {(yyval.id) = (yyvsp[0].id);}
#line 4900 "ada.tab.c"
    break;

  case 226: /* $@11: %empty  */
#line 1927 "ada.y"
                              {
                 if (!strcmp((yyvsp[-1].id), "Ascii_98")) { //total hack see 28/09/04
                     is_ascii = 1;
                  }
                 }
#line 4910 "ada.tab.c"
    break;

  case 227: /* selected_component: name '.' $@11 direct_name  */
#line 1933 "ada.y"
                     {is_ascii = 0;
                      (yyval.id) = malloc(SAFETY+strlen((yyvsp[-3].id))+strlen((yyvsp[0].id))+13);
                      strcpy((yyval.id), "selected(");
                      strcat((yyval.id), (yyvsp[-3].id));
                      strcat((yyval.id), ", ");
                      strcat((yyval.id), (yyvsp[0].id));
                      strcat((yyval.id), ")");
                      free((yyvsp[-3].id));
                      free((yyvsp[0].id));
                     }
#line 4925 "ada.tab.c"
    break;

  case 228: /* selected_component: name '.' character_literal  */
#line 1944 "ada.y"
                     {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id_ref).id)+33);
                      strcpy((yyval.id), "selected(");
                      strcat((yyval.id), (yyvsp[-2].id));
                      strcat((yyval.id), ", ");
                      strcat((yyval.id), handle_identifiers((yyvsp[0].id_ref), 0));    //0 because not a string
                      strcat((yyval.id), ")");
                      free((yyvsp[-2].id));
                     }
#line 4938 "ada.tab.c"
    break;

  case 229: /* selected_component: name '.' operator_symbol_or_string  */
#line 1953 "ada.y"
                     {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id))+13);
                      strcpy((yyval.id), "selected(");
                      strcat((yyval.id), (yyvsp[-2].id));
                      strcat((yyval.id), ", ");
                      strcat((yyval.id), (yyvsp[0].id));
                      strcat((yyval.id), ")");
                      free((yyvsp[-2].id));
                      free((yyvsp[0].id));
                     }
#line 4952 "ada.tab.c"
    break;

  case 230: /* selected_component: name '.' ALL  */
#line 1963 "ada.y"
                     {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-2].id))+16);
                      strcpy((yyval.id), "selected(");
                      strcat((yyval.id), (yyvsp[-2].id));
                      strcat((yyval.id), ", all)");
                      free((yyvsp[-2].id));
                     }
#line 4963 "ada.tab.c"
    break;

  case 231: /* attribute_reference: name TIC attribute_id  */
#line 1972 "ada.y"
                      {(yyval.id) = malloc(SAFETY+strlen((yyvsp[0].id))+strlen((yyvsp[-2].id))+8);
                       strcpy((yyval.id), "tic(");
                       strcat((yyval.id), (yyvsp[-2].id));
                       strcat((yyval.id), ", ");
                       strcat((yyval.id), (yyvsp[0].id));      //never cross referenced
                       strcat((yyval.id), ")");
                       free((yyvsp[0].id));
                       free((yyvsp[-2].id));
                      }
#line 4977 "ada.tab.c"
    break;

  case 232: /* attribute_id: identifier  */
#line 1985 "ada.y"
                                {(yyval.id) = (yyvsp[0].id_ref).id;}
#line 4983 "ada.tab.c"
    break;

  case 233: /* attribute_id: DELTA  */
#line 1986 "ada.y"
                                {(yyval.id) = malloc(6); strcpy((yyval.id), "delta");}
#line 4989 "ada.tab.c"
    break;

  case 234: /* attribute_id: DIGITS  */
#line 1987 "ada.y"
                                {(yyval.id) = malloc(7); strcpy((yyval.id), "digits");}
#line 4995 "ada.tab.c"
    break;

  case 235: /* attribute_id: ACCESS  */
#line 1988 "ada.y"
                                {(yyval.id) = malloc(7); strcpy((yyval.id), "access");}
#line 5001 "ada.tab.c"
    break;

  case 236: /* attribute_id: MOD  */
#line 1989 "ada.y"
                                {(yyval.id) = malloc(4); strcpy((yyval.id), "mod");}
#line 5007 "ada.tab.c"
    break;

  case 237: /* expression: expression_2  */
#line 1994 "ada.y"
             {if ((yyvsp[0].id_deci).is_a_decision)
                {itoa(decision_nb++, tmp_s, 10);
                 (yyval.id) = malloc(SAFETY+strlen(tmp_s)+strlen((yyvsp[0].id_deci).id)+15);
                 print_coverage_details(DECI, tmp_s, current_unit, yylineno, column);
                 strcpy((yyval.id), "deci(");
                 strcat((yyval.id), tmp_s);
                 strcat((yyval.id), ", ");
                 strcat((yyval.id), (yyvsp[0].id_deci).id);
                 strcat((yyval.id), ")");
                 free((yyvsp[0].id_deci).id);
                }
              else (yyval.id) = (yyvsp[0].id_deci).id;
             }
#line 5025 "ada.tab.c"
    break;

  case 238: /* expression_2: relation  */
#line 2011 "ada.y"
               {(yyval.id_deci) = (yyvsp[0].id_deci);}
#line 5031 "ada.tab.c"
    break;

  case 239: /* expression_2: relation boolean_operator relation  */
#line 2013 "ada.y"
               {if (!strncmp((yyvsp[-1].id_ref).id, "String_", 7)) {        //it is a user defined operator
                  (yyval.id_deci).id = malloc(SAFETY+strlen((yyvsp[-2].id_deci).id)+strlen((yyvsp[-1].id_ref).id)+strlen((yyvsp[0].id_deci).id)+15);
                  strcpy((yyval.id_deci).id, "indexed(");
                  strcat((yyval.id_deci).id, (yyvsp[-1].id_ref).id);
                  strcat((yyval.id_deci).id, ",[");
                  strcat((yyval.id_deci).id, (yyvsp[-2].id_deci).id);
                  strcat((yyval.id_deci).id, ", ");
                  strcat((yyval.id_deci).id, (yyvsp[0].id_deci).id);
                  strcat((yyval.id_deci).id, "])");
                  (yyval.id_deci).is_a_decision = 0;
                }
                else {
                  char gate_nb_s[10];
                  char *le_arg = NULL;
                  char *ri_arg = NULL;
                  int line_int = atoi((yyvsp[-1].id_ref).line);
                  int column_int = atoi((yyvsp[-1].id_ref).column);
                  itoa(gate_nb++, gate_nb_s, 10);
                  print_coverage_details(GATE, gate_nb_s, current_unit, line_int, column_int);    //perfect : column indicated is just on the boolean operator
                  build_condition((yyvsp[-2].id_deci), &le_arg, current_unit, line_int, column_int);
                  build_condition((yyvsp[0].id_deci), &ri_arg, current_unit, line_int, column_int);
                  (yyval.id_deci).id = malloc(SAFETY+strlen((yyvsp[-1].id_ref).id)+strlen(gate_nb_s)+strlen(le_arg)+strlen(ri_arg)+7);
                  strcpy((yyval.id_deci).id, (yyvsp[-1].id_ref).id);
                  strcat((yyval.id_deci).id, "(");
                  strcat((yyval.id_deci).id, gate_nb_s),
                  strcat((yyval.id_deci).id, ", ");
                  strcat((yyval.id_deci).id, le_arg);
                  strcat((yyval.id_deci).id, ", ");
                  strcat((yyval.id_deci).id, ri_arg);
                  strcat((yyval.id_deci).id, ")");
                  free(le_arg);
                  free(ri_arg);
                  (yyval.id_deci).is_a_decision = 1;
                }
                free((yyvsp[-2].id_deci).id);
                free((yyvsp[-1].id_ref).id);
                free((yyvsp[0].id_deci).id);
               }
#line 5074 "ada.tab.c"
    break;

  case 240: /* expression_2: expression_2 boolean_operator relation  */
#line 2052 "ada.y"
               {if (!strncmp((yyvsp[-1].id_ref).id, "String_", 7)) {       //it is a user defined operator
                  (yyval.id_deci).id = malloc(SAFETY+strlen((yyvsp[-2].id_deci).id)+strlen((yyvsp[-1].id_ref).id)+strlen((yyvsp[0].id_deci).id)+15);
                  strcpy((yyval.id_deci).id, "indexed(");
                  strcat((yyval.id_deci).id, (yyvsp[-1].id_ref).id);
                  strcat((yyval.id_deci).id, ",[");
                  strcat((yyval.id_deci).id, (yyvsp[-2].id_deci).id);
                  strcat((yyval.id_deci).id, ", ");
                  strcat((yyval.id_deci).id, (yyvsp[0].id_deci).id);
                  strcat((yyval.id_deci).id, "])");
                  (yyval.id_deci).is_a_decision = 0;
                }
                else {
                  char gate_nb_s[10];
                  char *le_arg = NULL;
                  char *ri_arg = NULL;
                  int line_int = atoi((yyvsp[-1].id_ref).line);
                  int column_int = atoi((yyvsp[-1].id_ref).column);
                  itoa(gate_nb++, gate_nb_s, 10);
                  print_coverage_details(GATE, gate_nb_s, current_unit, line_int, column_int);    //perfect : column indicated is just on the boolean operator
                  build_condition((yyvsp[-2].id_deci), &le_arg, current_unit, line_int, column_int);
                  build_condition((yyvsp[0].id_deci), &ri_arg, current_unit, line_int, column_int);
                  (yyval.id_deci).id = malloc(SAFETY+strlen((yyvsp[-1].id_ref).id)+strlen(gate_nb_s)+strlen(le_arg)+strlen(ri_arg)+7);
                  strcpy((yyval.id_deci).id, (yyvsp[-1].id_ref).id);
                  strcat((yyval.id_deci).id, "(");
                  strcat((yyval.id_deci).id, gate_nb_s),
                  strcat((yyval.id_deci).id, ", ");
                  strcat((yyval.id_deci).id, le_arg);
                  strcat((yyval.id_deci).id, ", ");
                  strcat((yyval.id_deci).id, ri_arg);
                  strcat((yyval.id_deci).id, ")");
                  free(le_arg);
                  free(ri_arg);
                  (yyval.id_deci).is_a_decision = 1;
                }
                free((yyvsp[-2].id_deci).id);
                free((yyvsp[-1].id_ref).id);
                free((yyvsp[0].id_deci).id);
               }
#line 5117 "ada.tab.c"
    break;

  case 241: /* boolean_operator: AND  */
#line 2093 "ada.y"
                   {(yyval.id_ref).id = handle_operator_calls((yyvsp[0].id_ref));
                    if (!(yyval.id_ref).id) {
                      (yyval.id_ref).id = malloc(4);
                      strcpy((yyval.id_ref).id, "and");
                    }
                    strcpy((yyval.id_ref).line, (yyvsp[0].id_ref).line);
                    strcpy((yyval.id_ref).column, (yyvsp[0].id_ref).column);
                   }
#line 5130 "ada.tab.c"
    break;

  case 242: /* boolean_operator: OR  */
#line 2102 "ada.y"
                   {(yyval.id_ref).id = handle_operator_calls((yyvsp[0].id_ref));
                    if (!(yyval.id_ref).id)
                      {(yyval.id_ref).id = malloc(3);
                       strcpy((yyval.id_ref).id, "or");
                      }
                    strcpy((yyval.id_ref).line, (yyvsp[0].id_ref).line);
                    strcpy((yyval.id_ref).column, (yyvsp[0].id_ref).column);
                   }
#line 5143 "ada.tab.c"
    break;

  case 243: /* boolean_operator: XOR  */
#line 2111 "ada.y"
                   {(yyval.id_ref).id = handle_operator_calls((yyvsp[0].id_ref));
                    if (!(yyval.id_ref).id)
                      {(yyval.id_ref).id = malloc(4);
                       strcpy((yyval.id_ref).id, "xor");
                      }
                    strcpy((yyval.id_ref).line, (yyvsp[0].id_ref).line);
                    strcpy((yyval.id_ref).column, (yyvsp[0].id_ref).column);
                   }
#line 5156 "ada.tab.c"
    break;

  case 244: /* boolean_operator: AND THEN  */
#line 2120 "ada.y"
                   {(yyval.id_ref).id = malloc(9);
                    strcpy((yyval.id_ref).id, "and_then");    //cannot be overloaded. But could it be renamed?
                    strcpy((yyval.id_ref).line, (yyvsp[-1].id_ref).line);
                    strcpy((yyval.id_ref).column, (yyvsp[-1].id_ref).column);
                   }
#line 5166 "ada.tab.c"
    break;

  case 245: /* boolean_operator: OR ELSE  */
#line 2126 "ada.y"
                   {(yyval.id_ref).id = malloc(8);
                    strcpy((yyval.id_ref).id, "or_else");     //cannot be overloaded. But could it be renamed?
                    strcpy((yyval.id_ref).line, (yyvsp[-1].id_ref).line);
                    strcpy((yyval.id_ref).column, (yyvsp[-1].id_ref).column);
                   }
#line 5176 "ada.tab.c"
    break;

  case 246: /* relation: simple_expression  */
#line 2134 "ada.y"
                                {(yyval.id_deci) = (yyvsp[0].id_deci);}
#line 5182 "ada.tab.c"
    break;

  case 247: /* relation: simple_expression relational_operator simple_expression  */
#line 2136 "ada.y"
           {int line_int = atoi((yyvsp[-1].id_ref).line);
            int column_int = atoi((yyvsp[-1].id_ref).column);
            itoa(condition_nb++, tmp_s, 10);
            print_coverage_details(COND, tmp_s, current_unit, line_int, column_int);  //perfect : column indicated is just on the relational operator
            {if (!strncmp((yyvsp[-1].id_ref).id, "String_", 7))        //it is a user defined operator : maybe because overloaded or renamed
               {(yyval.id_deci).id = malloc(SAFETY+strlen(tmp_s)+strlen((yyvsp[-2].id_deci).id)+strlen((yyvsp[-1].id_ref).id)+strlen((yyvsp[0].id_deci).id)+23); // 20/05/08 was $$.id = malloc(SAFETY+strlen($1.id)+strlen($2)+strlen($3.id)+23);
                strcpy((yyval.id_deci).id, "cond("); // 20/05/08 was commented out
                // 20/05/08 old comment : if it is  overloaded (and we cannot know this during parsing) then it may not return a Boolean (hence we do not surround the call with 'cond()')
                strcat((yyval.id_deci).id, tmp_s);   // 20/05/08 was commented out
                strcat((yyval.id_deci).id, ", ");    // 20/05/08 was commented out
                strcat((yyval.id_deci).id, "indexed("); // 20/05/08 strcpy
                strcat((yyval.id_deci).id, (yyvsp[-1].id_ref).id);
                strcat((yyval.id_deci).id, ",[");
                strcat((yyval.id_deci).id, (yyvsp[-2].id_deci).id);
                strcat((yyval.id_deci).id, ", ");
                strcat((yyval.id_deci).id, (yyvsp[0].id_deci).id);
                strcat((yyval.id_deci).id, "])");
                strcat((yyval.id_deci).id, ")");     // 20/05/08 was commented out
                (yyval.id_deci).is_a_decision = 1;   // 20/05/08 was 0
               }
             else
               {(yyval.id_deci).id = malloc(SAFETY+strlen(tmp_s)+strlen((yyvsp[-2].id_deci).id)+strlen((yyvsp[-1].id_ref).id)+strlen((yyvsp[0].id_deci).id)+9);
                strcpy((yyval.id_deci).id, "cond(");
                strcat((yyval.id_deci).id, tmp_s);
                strcat((yyval.id_deci).id, ", ");
                strcat((yyval.id_deci).id, (yyvsp[-2].id_deci).id);
                strcat((yyval.id_deci).id, (yyvsp[-1].id_ref).id);
                strcat((yyval.id_deci).id, (yyvsp[0].id_deci).id);
                strcat((yyval.id_deci).id, ")");
                (yyval.id_deci).is_a_decision = 1;
               }
            }
            free((yyvsp[-2].id_deci).id);
            free((yyvsp[-1].id_ref).id);
            free((yyvsp[0].id_deci).id);
           }
#line 5223 "ada.tab.c"
    break;

  case 248: /* relation: simple_expression membership range_or_name  */
#line 2173 "ada.y"
           {int line_int = atoi((yyvsp[-1].id_ref).line);
            int column_int = atoi((yyvsp[-1].id_ref).column);
            itoa(condition_nb++, tmp_s, 10);
            (yyval.id_deci).id = malloc(SAFETY+strlen(tmp_s)+strlen((yyvsp[-1].id_ref).id)+strlen((yyvsp[-2].id_deci).id)+strlen((yyvsp[0].id))+23);
            print_coverage_details(COND, tmp_s, current_unit, line_int, column_int);    //perfect : column indicated is just on the membership operator
            strcpy((yyval.id_deci).id, "cond(");
            strcat((yyval.id_deci).id, tmp_s);
            strcat((yyval.id_deci).id, ", ");
            strcat((yyval.id_deci).id, (yyvsp[-1].id_ref).id);
            strcat((yyval.id_deci).id, "(");
            strcat((yyval.id_deci).id, (yyvsp[-2].id_deci).id);
            strcat((yyval.id_deci).id, ", ");
            strcat((yyval.id_deci).id, (yyvsp[0].id));
            strcat((yyval.id_deci).id, ")");
            strcat((yyval.id_deci).id, ")");
            free((yyvsp[-1].id_ref).id);
            free((yyvsp[-2].id_deci).id);
            free((yyvsp[0].id));
            (yyval.id_deci).is_a_decision = 1;
           }
#line 5248 "ada.tab.c"
    break;

  case 249: /* range_or_name: range  */
#line 2196 "ada.y"
                        {(yyval.id) = (yyvsp[0].id);}
#line 5254 "ada.tab.c"
    break;

  case 250: /* range_or_name: name  */
#line 2197 "ada.y"
                        {(yyval.id) = (yyvsp[0].id);}
#line 5260 "ada.tab.c"
    break;

  case 251: /* membership: IN  */
#line 2200 "ada.y"
                                {(yyval.id_ref).id = malloc(6);
                                 strcpy((yyval.id_ref).id, "is_in");
                                 strcpy((yyval.id_ref).line, (yyvsp[0].id_ref).line);
                                 strcpy((yyval.id_ref).column, (yyvsp[0].id_ref).column);
                                }
#line 5270 "ada.tab.c"
    break;

  case 252: /* membership: NOT IN  */
#line 2205 "ada.y"
                                {(yyval.id_ref).id = malloc(10);
                                 strcpy((yyval.id_ref).id, "is_not_in");
                                 strcpy((yyval.id_ref).line, (yyvsp[-1].id_ref).line);
                                 strcpy((yyval.id_ref).column, (yyvsp[-1].id_ref).column);
                                }
#line 5280 "ada.tab.c"
    break;

  case 253: /* simple_expression: unary_adding_operator term  */
#line 2214 "ada.y"
                    {if (!strncmp((yyvsp[-1].id), "String_", 7))        //it is a user defined operator
                       {(yyval.id_deci).id = malloc(SAFETY+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id_deci).id)+13);
                        strcpy((yyval.id_deci).id, "indexed(");
                        strcat((yyval.id_deci).id, (yyvsp[-1].id));
                        strcat((yyval.id_deci).id, ",[");
                        strcat((yyval.id_deci).id, (yyvsp[0].id_deci).id);
                        strcat((yyval.id_deci).id, "])");
                       }
                     else
                       {(yyval.id_deci).id = malloc(SAFETY+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id_deci).id)+1);
                        strcpy((yyval.id_deci).id, (yyvsp[-1].id));
                        strcat((yyval.id_deci).id, (yyvsp[0].id_deci).id);
                       }
                     free((yyvsp[-1].id));
                     free((yyvsp[0].id_deci).id);
                     (yyval.id_deci).is_a_decision = 0;
                    }
#line 5302 "ada.tab.c"
    break;

  case 254: /* simple_expression: term  */
#line 2231 "ada.y"
                                {(yyval.id_deci) = (yyvsp[0].id_deci);}
#line 5308 "ada.tab.c"
    break;

  case 255: /* simple_expression: simple_expression binary_adding_operator term  */
#line 2233 "ada.y"
                    {if (!strncmp((yyvsp[-1].id), "String_", 7))        //it is a user defined operator
                       {(yyval.id_deci).id = malloc(SAFETY+strlen((yyvsp[-2].id_deci).id)+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id_deci).id)+15);
                        strcpy((yyval.id_deci).id, "indexed(");
                        strcat((yyval.id_deci).id, (yyvsp[-1].id));
                        strcat((yyval.id_deci).id, ",[");
                        strcat((yyval.id_deci).id, (yyvsp[-2].id_deci).id);
                        strcat((yyval.id_deci).id, ", ");
                        strcat((yyval.id_deci).id, (yyvsp[0].id_deci).id);
                        strcat((yyval.id_deci).id, "])");
                       }
                     else
                       {(yyval.id_deci).id = malloc(SAFETY+strlen((yyvsp[-2].id_deci).id)+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id_deci).id)+1);
                        strcpy((yyval.id_deci).id, (yyvsp[-2].id_deci).id);
                        strcat((yyval.id_deci).id, (yyvsp[-1].id));
                        strcat((yyval.id_deci).id, (yyvsp[0].id_deci).id);
                       }
                     free((yyvsp[-2].id_deci).id);
                     free((yyvsp[-1].id));
                     free((yyvsp[0].id_deci).id);
                     (yyval.id_deci).is_a_decision = 0;
                    }
#line 5334 "ada.tab.c"
    break;

  case 256: /* unary_adding_operator: PLUS  */
#line 2256 "ada.y"
                                {(yyval.id) = handle_operator_calls((yyvsp[0].id_ref));
                                 if (!(yyval.id))
                                   {(yyval.id) = malloc(4);
                                    strcpy((yyval.id), " + ");
                                   }
                                }
#line 5345 "ada.tab.c"
    break;

  case 257: /* unary_adding_operator: MINUS  */
#line 2262 "ada.y"
                                {(yyval.id) = handle_operator_calls((yyvsp[0].id_ref));
                                 if (!(yyval.id))
                                   {(yyval.id) = malloc(4);
                                    strcpy((yyval.id), " - ");
                                   }
                                }
#line 5356 "ada.tab.c"
    break;

  case 258: /* binary_adding_operator: PLUS  */
#line 2270 "ada.y"
                                {(yyval.id) = handle_operator_calls((yyvsp[0].id_ref));
                                 if (!(yyval.id))
                                   {(yyval.id) = malloc(4);
                                    strcpy((yyval.id), " + ");
                                   }
                                }
#line 5367 "ada.tab.c"
    break;

  case 259: /* binary_adding_operator: MINUS  */
#line 2276 "ada.y"
                                {(yyval.id) = handle_operator_calls((yyvsp[0].id_ref));
                                 if (!(yyval.id))
                                   {(yyval.id) = malloc(4);
                                    strcpy((yyval.id), " - ");
                                   }
                                }
#line 5378 "ada.tab.c"
    break;

  case 260: /* binary_adding_operator: CONC  */
#line 2282 "ada.y"
                                {(yyval.id) = handle_operator_calls((yyvsp[0].id_ref));
                                 if (!(yyval.id))
                                   {(yyval.id) = malloc(4);
                                    strcpy((yyval.id), " & ");
                                   }
                                }
#line 5389 "ada.tab.c"
    break;

  case 261: /* term: factor  */
#line 2291 "ada.y"
                                {(yyval.id_deci) = (yyvsp[0].id_deci);}
#line 5395 "ada.tab.c"
    break;

  case 262: /* term: term multiplying_operator factor  */
#line 2293 "ada.y"
                                {if (!strncmp((yyvsp[-1].id), "String_", 7))        //it is a user defined operator
                                   {(yyval.id_deci).id = malloc(SAFETY+strlen((yyvsp[-2].id_deci).id)+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id_deci).id)+15);
                                    strcpy((yyval.id_deci).id, "indexed(");
                                    strcat((yyval.id_deci).id, (yyvsp[-1].id));
                                    strcat((yyval.id_deci).id, ",[");
                                    strcat((yyval.id_deci).id, (yyvsp[-2].id_deci).id);
                                    strcat((yyval.id_deci).id, ", ");
                                    strcat((yyval.id_deci).id, (yyvsp[0].id_deci).id);
                                    strcat((yyval.id_deci).id, "])");
                                   }
                                  else if(!strncmp((yyvsp[-1].id), " / ", 3))
                                  {
                                    (yyval.id_deci).id = malloc(SAFETY+strlen(tmp_s)+strlen((yyvsp[-2].id_deci).id)+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id_deci).id)+strlen((yyvsp[0].id_deci).id)+14);
                                    itoa(runtime_nb++, tmp_s, 10);
                                    print_coverage_details(RUNE, tmp_s, current_unit, yylineno, column+1); 
                                    strcpy((yyval.id_deci).id, (yyvsp[-2].id_deci).id);
                                    strcat((yyval.id_deci).id, (yyvsp[-1].id));
                                    strcat((yyval.id_deci).id, "rune(");
                                    strcat((yyval.id_deci).id, tmp_s);
                                    strcat((yyval.id_deci).id, ", ");
                                    strcat((yyval.id_deci).id, (yyvsp[0].id_deci).id);
                                    strcat((yyval.id_deci).id, " = 0, ");
                                    strcat((yyval.id_deci).id, (yyvsp[0].id_deci).id);
                                    strcat((yyval.id_deci).id, ")");
                                  }
                                 else
                                   {(yyval.id_deci).id = malloc(SAFETY+strlen((yyvsp[-2].id_deci).id)+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id_deci).id)+1);
                                    strcpy((yyval.id_deci).id, (yyvsp[-2].id_deci).id);
                                    strcat((yyval.id_deci).id, (yyvsp[-1].id));
                                    strcat((yyval.id_deci).id, (yyvsp[0].id_deci).id);
                                   }
                                 free((yyvsp[-2].id_deci).id);
                                 free((yyvsp[-1].id));
                                 free((yyvsp[0].id_deci).id);
                                 (yyval.id_deci).is_a_decision = 0;
                                }
#line 5436 "ada.tab.c"
    break;

  case 263: /* multiplying_operator: MULT  */
#line 2331 "ada.y"
                                {(yyval.id) = handle_operator_calls((yyvsp[0].id_ref));
                                 if (!(yyval.id))
                                   {(yyval.id) = malloc(4);
                                    strcpy((yyval.id), " * ");
                                   }
                                }
#line 5447 "ada.tab.c"
    break;

  case 264: /* multiplying_operator: DIV  */
#line 2337 "ada.y"
                                {(yyval.id) = handle_operator_calls((yyvsp[0].id_ref));
                                 
                                 if (!(yyval.id))
                                   {
                                    (yyval.id) = malloc(4);
                                    strcpy((yyval.id), " / ");
                                   }
                                }
#line 5460 "ada.tab.c"
    break;

  case 265: /* multiplying_operator: MOD  */
#line 2345 "ada.y"
                                {(yyval.id) = handle_operator_calls((yyvsp[0].id_ref));
                                 if (!(yyval.id))
                                   {(yyval.id) = malloc(6);
                                    strcpy((yyval.id), " mod ");
                                   }
                                }
#line 5471 "ada.tab.c"
    break;

  case 266: /* multiplying_operator: REM  */
#line 2351 "ada.y"
                                {(yyval.id) = handle_operator_calls((yyvsp[0].id_ref));
                                 if (!(yyval.id))
                                   {(yyval.id) = malloc(6);
                                    strcpy((yyval.id), " rem ");
                                   }
                                }
#line 5482 "ada.tab.c"
    break;

  case 267: /* factor: primary  */
#line 2360 "ada.y"
                                {(yyval.id_deci) = (yyvsp[0].id_deci);}
#line 5488 "ada.tab.c"
    break;

  case 268: /* factor: NOT primary  */
#line 2362 "ada.y"
         {char *op;
          op = handle_operator_calls((yyvsp[-1].id_ref));
          if (!op) {    //it is not a user defined operator
            op = malloc(4);
            strcpy(op, "not");
          }
          if (!strncmp(op, "String_", 7)) {        //it is a user defined operator
            (yyval.id_deci).id = malloc(SAFETY+strlen(op)+strlen((yyvsp[0].id_deci).id)+13);
            strcpy((yyval.id_deci).id, "indexed(");
            strcat((yyval.id_deci).id, op);
            strcat((yyval.id_deci).id, ",[");
            strcat((yyval.id_deci).id, (yyvsp[0].id_deci).id);
            strcat((yyval.id_deci).id, "])");
            (yyval.id_deci).is_a_decision = 0;
          }
          else {
            char gate_nb_s[10];
            char *le_arg = NULL;
            int line_int = atoi((yyvsp[-1].id_ref).line);
            int column_int = atoi((yyvsp[-1].id_ref).column);
            itoa(gate_nb++, gate_nb_s, 10);
            print_coverage_details(GATE, gate_nb_s, current_unit, line_int, column_int);    //perfect : column indicated is just on the boolean operator
            build_condition((yyvsp[0].id_deci), &le_arg, current_unit, line_int, column_int);
            (yyval.id_deci).id = malloc(SAFETY+strlen(op)+strlen(gate_nb_s)+strlen(le_arg)+5);
            strcpy((yyval.id_deci).id, op);
            strcat((yyval.id_deci).id, "(");
            strcat((yyval.id_deci).id, gate_nb_s),
            strcat((yyval.id_deci).id, ", ");
            strcat((yyval.id_deci).id, le_arg);
            strcat((yyval.id_deci).id, ")");
            free(le_arg);
            (yyval.id_deci).is_a_decision = 1;
          }
          free((yyvsp[-1].id_ref).id);
          free((yyvsp[0].id_deci).id);
          free(op);
         }
#line 5530 "ada.tab.c"
    break;

  case 269: /* factor: ABS primary  */
#line 2400 "ada.y"
          {char *op;
           op = handle_operator_calls((yyvsp[-1].id_ref));
           if (!op)
           {op = malloc(5);
            strcpy(op, "abs ");
           }
           if (!strncmp(op, "String_", 7))        //it is a user defined operator
             {(yyval.id_deci).id = malloc(SAFETY+strlen((yyvsp[0].id_deci).id)+strlen(op)+13);
              strcpy((yyval.id_deci).id, "indexed(");
              strcat((yyval.id_deci).id, op);
              strcat((yyval.id_deci).id, ",[");
              strcat((yyval.id_deci).id, (yyvsp[0].id_deci).id);
              strcat((yyval.id_deci).id, "])");
             }
           else
             {(yyval.id_deci).id = malloc(SAFETY+strlen((yyvsp[0].id_deci).id)+strlen(op)+5);
              strcpy((yyval.id_deci).id, op);
              strcat((yyval.id_deci).id, (yyvsp[0].id_deci).id);
             }
           free((yyvsp[0].id_deci).id);
           (yyval.id_deci).is_a_decision = 0;
          }
#line 5557 "ada.tab.c"
    break;

  case 270: /* factor: primary EXPON primary  */
#line 2423 "ada.y"
          {char *op;
           op = handle_operator_calls((yyvsp[-1].id_ref));
           if (!op)
             {op = malloc(5);
              strcpy(op, " ** ");
             }
           if (!strncmp(op, "String_", 7))        //it is a user defined operator
             {(yyval.id_deci).id = malloc(SAFETY+strlen((yyvsp[-2].id_deci).id)+strlen(op)+strlen((yyvsp[0].id_deci).id)+15);
              strcpy((yyval.id_deci).id, "indexed(");
              strcat((yyval.id_deci).id, op);
              strcat((yyval.id_deci).id, ",[");
              strcat((yyval.id_deci).id, (yyvsp[-2].id_deci).id);
              strcat((yyval.id_deci).id, ", ");
              strcat((yyval.id_deci).id, (yyvsp[0].id_deci).id);
              strcat((yyval.id_deci).id, "])");
             }
           else
             {(yyval.id_deci).id = malloc(SAFETY+strlen((yyvsp[-2].id_deci).id)+strlen(op)+strlen((yyvsp[0].id_deci).id)+1);
              strcpy((yyval.id_deci).id, (yyvsp[-2].id_deci).id);
              strcat((yyval.id_deci).id, op);
              strcat((yyval.id_deci).id, (yyvsp[0].id_deci).id);
             }
           free((yyvsp[-2].id_deci).id);
           free((yyvsp[0].id_deci).id);
           (yyval.id_deci).is_a_decision = 0;
          }
#line 5588 "ada.tab.c"
    break;

  case 271: /* primary: literal  */
#line 2453 "ada.y"
                                {(yyval.id_deci).id = (yyvsp[0].id); (yyval.id_deci).is_a_decision = 0;}
#line 5594 "ada.tab.c"
    break;

  case 272: /* primary: name  */
#line 2454 "ada.y"
                                {(yyval.id_deci).id = (yyvsp[0].id); (yyval.id_deci).is_a_decision = 0;}
#line 5600 "ada.tab.c"
    break;

  case 273: /* primary: allocator  */
#line 2455 "ada.y"
                                {(yyval.id_deci).id = (yyvsp[0].id); (yyval.id_deci).is_a_decision = 0;}
#line 5606 "ada.tab.c"
    break;

  case 274: /* primary: parenthesized_primary  */
#line 2456 "ada.y"
                                {(yyval.id_deci) = (yyvsp[0].id_deci);}
#line 5612 "ada.tab.c"
    break;

  case 275: /* literal: numeric_literal  */
#line 2459 "ada.y"
                                {(yyval.id) = (yyvsp[0].id);}
#line 5618 "ada.tab.c"
    break;

  case 276: /* literal: character_literal  */
#line 2460 "ada.y"
                                {(yyval.id) = handle_identifiers((yyvsp[0].id_ref), 0);}
#line 5624 "ada.tab.c"
    break;

  case 277: /* literal: NuLL  */
#line 2461 "ada.y"
                                {(yyval.id) = malloc(5 ); strcpy((yyval.id), "null");}
#line 5630 "ada.tab.c"
    break;

  case 278: /* relational_operator: EQUAL  */
#line 2464 "ada.y"
                                {(yyval.id_ref).id = handle_operator_calls((yyvsp[0].id_ref));
                                 if (!(yyval.id_ref).id)
                                   {(yyval.id_ref).id = malloc(4);
                                    strcpy((yyval.id_ref).id, " = ");
                                   }
                                 strcpy((yyval.id_ref).line, (yyvsp[0].id_ref).line);
                                 strcpy((yyval.id_ref).column, (yyvsp[0].id_ref).column);
                                }
#line 5643 "ada.tab.c"
    break;

  case 279: /* relational_operator: NE  */
#line 2472 "ada.y"
                                {(yyval.id_ref).id = handle_operator_calls((yyvsp[0].id_ref));
                                 if (!(yyval.id_ref).id)
                                   {(yyval.id_ref).id = malloc(5);
                                    strcpy((yyval.id_ref).id, " <> ");
                                   }
                                 strcpy((yyval.id_ref).line, (yyvsp[0].id_ref).line);
                                 strcpy((yyval.id_ref).column, (yyvsp[0].id_ref).column);
                                }
#line 5656 "ada.tab.c"
    break;

  case 280: /* relational_operator: LT  */
#line 2480 "ada.y"
                                {(yyval.id_ref).id = handle_operator_calls((yyvsp[0].id_ref));
                                 if (!(yyval.id_ref).id)
                                   {(yyval.id_ref).id = malloc(4);
                                    strcpy((yyval.id_ref).id, " < ");
                                   }
                                 strcpy((yyval.id_ref).line, (yyvsp[0].id_ref).line);
                                 strcpy((yyval.id_ref).column, (yyvsp[0].id_ref).column);
                                }
#line 5669 "ada.tab.c"
    break;

  case 281: /* relational_operator: LT_EQ  */
#line 2488 "ada.y"
                                {(yyval.id_ref).id = handle_operator_calls((yyvsp[0].id_ref));
                                 if (!(yyval.id_ref).id)
                                   {(yyval.id_ref).id = malloc(5);
                                    strcpy((yyval.id_ref).id, " <= ");
                                   }
                                 strcpy((yyval.id_ref).line, (yyvsp[0].id_ref).line);
                                 strcpy((yyval.id_ref).column, (yyvsp[0].id_ref).column);
                                }
#line 5682 "ada.tab.c"
    break;

  case 282: /* relational_operator: GT  */
#line 2496 "ada.y"
                                {(yyval.id_ref).id = handle_operator_calls((yyvsp[0].id_ref));
                                 if (!(yyval.id_ref).id)
                                   {(yyval.id_ref).id = malloc(4);
                                    strcpy((yyval.id_ref).id, " > ");
                                   }
                                 strcpy((yyval.id_ref).line, (yyvsp[0].id_ref).line);
                                 strcpy((yyval.id_ref).column, (yyvsp[0].id_ref).column);
                                }
#line 5695 "ada.tab.c"
    break;

  case 283: /* relational_operator: GE  */
#line 2504 "ada.y"
                                {(yyval.id_ref).id = handle_operator_calls((yyvsp[0].id_ref));
                                 if (!(yyval.id_ref).id)
                                   {(yyval.id_ref).id = malloc(5);
                                    strcpy((yyval.id_ref).id, " >= ");
                                   }
                                 strcpy((yyval.id_ref).line, (yyvsp[0].id_ref).line);
                                 strcpy((yyval.id_ref).column, (yyvsp[0].id_ref).column);
                                }
#line 5708 "ada.tab.c"
    break;

  case 284: /* qualified_expression: name TIC parenthesized_primary  */
#line 2515 "ada.y"
                       {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id_deci).id)+8) );
                        strcpy((yyval.id), "tic(");
                        strcat((yyval.id), (yyvsp[-2].id));
                        strcat((yyval.id), ", ");
                        strcat((yyval.id), (yyvsp[0].id_deci).id);
                        strcat((yyval.id), ")");
                        free((yyvsp[-2].id));
                        free((yyvsp[0].id_deci).id);
                       }
#line 5722 "ada.tab.c"
    break;

  case 285: /* parenthesized_primary: aggregate  */
#line 2528 "ada.y"
                        {(yyval.id_deci).id = malloc(SAFETY+strlen((yyvsp[0].id))+6);
                         strcpy((yyval.id_deci).id, "agg(");
                         strcat((yyval.id_deci).id, (yyvsp[0].id));
                         strcat((yyval.id_deci).id, ")");
                         free((yyvsp[0].id));
                         (yyval.id_deci).is_a_decision = 0;
                        }
#line 5734 "ada.tab.c"
    break;

  case 286: /* parenthesized_primary: '(' expression_2 ')'  */
#line 2536 "ada.y"
                        {(yyval.id_deci).id = malloc(SAFETY+strlen((yyvsp[-1].id_deci).id)+3);
                         strcpy((yyval.id_deci).id, "(");
                         strcat((yyval.id_deci).id, (yyvsp[-1].id_deci).id);
                         strcat((yyval.id_deci).id, ")");
                         (yyval.id_deci).is_a_decision = (yyvsp[-1].id_deci).is_a_decision;
                         free((yyvsp[-1].id_deci).id);
                        }
#line 5746 "ada.tab.c"
    break;

  case 287: /* parenthesized_primary: '(' IF cond_expression_list else_expression_opt ')'  */
#line 2544 "ada.y"
                         {(yyval.id_deci).id = malloc(SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[-1].id))+14);
                          strcpy((yyval.id_deci).id, "if_expr([");
                          strcat((yyval.id_deci).id, (yyvsp[-2].id));
                          strcat((yyval.id_deci).id, "], ");
                          strcat((yyval.id_deci).id, (yyvsp[-1].id));
                          strcat((yyval.id_deci).id, ")");
                          (yyval.id_deci).is_a_decision = 0; //unsure about this one
                          free((yyvsp[-2].id));
                          free((yyvsp[-1].id));
                         }
#line 5761 "ada.tab.c"
    break;

  case 288: /* cond_expression_list: cond_expression  */
#line 2556 "ada.y"
                                                {(yyval.id) = (yyvsp[0].id);}
#line 5767 "ada.tab.c"
    break;

  case 289: /* cond_expression_list: cond_expression_list ELSIF cond_expression  */
#line 2558 "ada.y"
                       {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id))+3);
                        strcpy((yyval.id), (yyvsp[-2].id));
                        strcat((yyval.id), ", ");
                        strcat((yyval.id), (yyvsp[0].id));
                        free((yyvsp[-2].id));
                        free((yyvsp[0].id));
                       }
#line 5779 "ada.tab.c"
    break;

  case 290: /* @12: %empty  */
#line 2567 "ada.y"
                                               {(yyval.true_line_column).line = yylineno; (yyval.true_line_column).column = column+1;}
#line 5785 "ada.tab.c"
    break;

  case 291: /* cond_expression: cond_part @12 expression_2  */
#line 2568 "ada.y"
                  {char *expression;
                   build_expression((yyvsp[0].id_deci), &expression, current_unit, (yyvsp[-1].true_line_column).line, (yyvsp[-1].true_line_column).column);
                   (yyval.id) = malloc(SAFETY+strlen((yyvsp[-2].id))+strlen(expression)+19);
                   strcpy((yyval.id), "if_expr_clause(");
                   strcat((yyval.id), (yyvsp[-2].id));
                   strcat((yyval.id), ", ");
                   strcat((yyval.id), expression);
                   strcat((yyval.id), ")");
                   free((yyvsp[-2].id));
                   free((yyvsp[0].id_deci).id);
                  }
#line 5801 "ada.tab.c"
    break;

  case 292: /* else_expression_opt: %empty  */
#line 2582 "ada.y"
                      {(yyval.id) = malloc(SAFETY+22);
                       strcpy((yyval.id), "else_expression(true)"); //should evaluate to true
                      }
#line 5809 "ada.tab.c"
    break;

  case 293: /* @13: %empty  */
#line 2585 "ada.y"
                                              {(yyval.true_line_column).line = yylineno; (yyval.true_line_column).column = column+1;}
#line 5815 "ada.tab.c"
    break;

  case 294: /* else_expression_opt: ELSE @13 expression_2  */
#line 2586 "ada.y"
                      {char *expression;
                       build_expression((yyvsp[0].id_deci), &expression, current_unit, (yyvsp[-1].true_line_column).line, (yyvsp[-1].true_line_column).column);
                       (yyval.id) = malloc(SAFETY+strlen(expression)+18);
                       strcpy((yyval.id), "else_expression(");
                       strcat((yyval.id), expression);
                       strcat((yyval.id), ")");
                       free((yyvsp[0].id_deci).id);
                      }
#line 5828 "ada.tab.c"
    break;

  case 295: /* allocator: NEW name  */
#line 2600 "ada.y"
                        {(yyval.id) = malloc(SAFETY+strlen((yyvsp[0].id))+12);
                         strcpy((yyval.id), "allocator(");
                         strcat((yyval.id), (yyvsp[0].id));
                         strcat((yyval.id), ")");
                         free((yyvsp[0].id));
                        }
#line 5839 "ada.tab.c"
    break;

  case 296: /* @14: %empty  */
#line 2608 "ada.y"
                                                                                 {(yyval.true_line_column).line = yylineno; (yyval.true_line_column).column = column+1;}
#line 5845 "ada.tab.c"
    break;

  case 297: /* number_declaration: identifier_list ':' CONSTANT IS_ASSIGNED @14 expression_2 ';'  */
#line 2609 "ada.y"
                     {char *expression;
                      build_expression((yyvsp[-1].id_deci), &expression, current_unit, (yyvsp[-2].true_line_column).line, (yyvsp[-2].true_line_column).column);
                      (yyval.id) = malloc(SAFETY+strlen((yyvsp[-6].id))+strlen(expression)+28);
                      strcpy((yyval.id), "\n             number([");
                      strcat((yyval.id), (yyvsp[-6].id));
                      strcat((yyval.id), "], ");
                      strcat((yyval.id), expression);
                      strcat((yyval.id), ")");
                      free((yyvsp[-6].id));
                      free((yyvsp[-1].id_deci).id);
                     }
#line 5861 "ada.tab.c"
    break;

  case 298: /* sequence_of_statements: statement  */
#line 2624 "ada.y"
                         {(yyval.id) = malloc((SAFETY+strlen((yyvsp[0].id))+17) );
                          strcpy((yyval.id), "\n              ");
                          strcat((yyval.id), (yyvsp[0].id));
                          free((yyvsp[0].id));
                         }
#line 5871 "ada.tab.c"
    break;

  case 299: /* sequence_of_statements: sequence_of_statements statement  */
#line 2630 "ada.y"
                         {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id))+18) );
                          strcpy((yyval.id), (yyvsp[-1].id));
                          strcat((yyval.id), ",\n              ");
                          strcat((yyval.id), (yyvsp[0].id));
                          free((yyvsp[-1].id));
                          free((yyvsp[0].id));
                         }
#line 5883 "ada.tab.c"
    break;

  case 300: /* statement: unlabeled_statement  */
#line 2639 "ada.y"
                                {(yyval.id) = (yyvsp[0].id);}
#line 5889 "ada.tab.c"
    break;

  case 301: /* statement: label statement  */
#line 2641 "ada.y"
            {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id))+22);
             strcpy((yyval.id), "labeled_statement(");
             strcat((yyval.id), (yyvsp[-1].id));
             strcat((yyval.id), ", ");
             strcat((yyval.id), (yyvsp[0].id));
             strcat((yyval.id), ")");
             free((yyvsp[-1].id));
             free((yyvsp[0].id));
            }
#line 5903 "ada.tab.c"
    break;

  case 302: /* unlabeled_statement: simple_statement  */
#line 2652 "ada.y"
                                                {(yyval.id) = (yyvsp[0].id);}
#line 5909 "ada.tab.c"
    break;

  case 303: /* unlabeled_statement: compound_statement  */
#line 2653 "ada.y"
                                                {(yyval.id) = (yyvsp[0].id);}
#line 5915 "ada.tab.c"
    break;

  case 304: /* unlabeled_statement: pragma  */
#line 2654 "ada.y"
                                                {(yyval.id) = (yyvsp[0].id);}
#line 5921 "ada.tab.c"
    break;

  case 305: /* simple_statement: null_statement  */
#line 2657 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 5927 "ada.tab.c"
    break;

  case 306: /* simple_statement: assignement_statement  */
#line 2658 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 5933 "ada.tab.c"
    break;

  case 307: /* simple_statement: exit_statement  */
#line 2659 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 5939 "ada.tab.c"
    break;

  case 308: /* simple_statement: simple_return_statement  */
#line 2660 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 5945 "ada.tab.c"
    break;

  case 309: /* simple_statement: goto_statement  */
#line 2661 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 5951 "ada.tab.c"
    break;

  case 310: /* simple_statement: procedure_call_statement  */
#line 2662 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 5957 "ada.tab.c"
    break;

  case 311: /* simple_statement: delay_statement  */
#line 2663 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 5963 "ada.tab.c"
    break;

  case 312: /* simple_statement: abort_statement  */
#line 2664 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 5969 "ada.tab.c"
    break;

  case 313: /* simple_statement: raise_statement  */
#line 2665 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 5975 "ada.tab.c"
    break;

  case 314: /* simple_statement: requeue_statement  */
#line 2666 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 5981 "ada.tab.c"
    break;

  case 315: /* compound_statement: if_statement  */
#line 2669 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 5987 "ada.tab.c"
    break;

  case 316: /* compound_statement: case_statement  */
#line 2670 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 5993 "ada.tab.c"
    break;

  case 317: /* compound_statement: loop_statement  */
#line 2671 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 5999 "ada.tab.c"
    break;

  case 318: /* compound_statement: block  */
#line 2672 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 6005 "ada.tab.c"
    break;

  case 319: /* compound_statement: accept_statement  */
#line 2673 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 6011 "ada.tab.c"
    break;

  case 320: /* compound_statement: select_statement  */
#line 2674 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 6017 "ada.tab.c"
    break;

  case 321: /* compound_statement: extended_return_statement  */
#line 2675 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 6023 "ada.tab.c"
    break;

  case 322: /* label: LT_LT identifier_rule GT_GT  */
#line 2678 "ada.y"
                                        {(yyval.id) = (yyvsp[-1].id);}
#line 6029 "ada.tab.c"
    break;

  case 323: /* null_statement: NuLL ';'  */
#line 2681 "ada.y"
                                {(yyval.id) = malloc(5 ); strcpy((yyval.id), "null");}
#line 6035 "ada.tab.c"
    break;

  case 324: /* @15: %empty  */
#line 2684 "ada.y"
                                                            {(yyval.true_line_column).line = yylineno; (yyval.true_line_column).column = column+1;}
#line 6041 "ada.tab.c"
    break;

  case 325: /* assignement_statement: name IS_ASSIGNED @15 expression_2 ';'  */
#line 2685 "ada.y"
                        {char *expression;
                         build_expression((yyvsp[-1].id_deci), &expression, current_unit, (yyvsp[-2].true_line_column).line, (yyvsp[-2].true_line_column).column);
                         (yyval.id) = malloc(SAFETY+strlen((yyvsp[-4].id))+strlen(expression)+11);
                         strcpy((yyval.id), "assign(");
                         strcat((yyval.id), (yyvsp[-4].id));
                         strcat((yyval.id), ", ");
                         strcat((yyval.id), expression);
                         strcat((yyval.id), ")");
                         free((yyvsp[-4].id));
                         free((yyvsp[-1].id_deci).id);
                        }
#line 6057 "ada.tab.c"
    break;

  case 326: /* if_statement: IF cond_clause_list else_opt END IF ';'  */
#line 2699 "ada.y"
               {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-4].id))+strlen((yyvsp[-3].id))+14) );
                strcpy((yyval.id), "if_stmt([");
                strcat((yyval.id), (yyvsp[-4].id));
                strcat((yyval.id), "], ");
                strcat((yyval.id), (yyvsp[-3].id));
                strcat((yyval.id), ")");
                free((yyvsp[-4].id));
                free((yyvsp[-3].id));
               }
#line 6071 "ada.tab.c"
    break;

  case 327: /* cond_clause_list: cond_clause  */
#line 2710 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 6077 "ada.tab.c"
    break;

  case 328: /* cond_clause_list: cond_clause_list ELSIF cond_clause  */
#line 2712 "ada.y"
                   {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id))+3) );
                    strcpy((yyval.id), (yyvsp[-2].id));
                    strcat((yyval.id), ", ");
                    strcat((yyval.id), (yyvsp[0].id));
                    free((yyvsp[-2].id));
                    free((yyvsp[0].id));
                   }
#line 6089 "ada.tab.c"
    break;

  case 329: /* cond_clause: cond_part sequence_of_statements  */
#line 2722 "ada.y"
              {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id))+69) );
               strcpy((yyval.id), "if_clause(");
               strcat((yyval.id), (yyvsp[-1].id));
               strcat((yyval.id), ",\n              stmts([");
               strcat((yyval.id), (yyvsp[0].id));
               strcat((yyval.id), "\n                   ]))");
               free((yyvsp[-1].id));
               free((yyvsp[0].id));
              }
#line 6103 "ada.tab.c"
    break;

  case 330: /* cond_part: decision THEN  */
#line 2733 "ada.y"
                                {(yyval.id) = (yyvsp[-1].id);}
#line 6109 "ada.tab.c"
    break;

  case 331: /* @16: %empty  */
#line 2736 "ada.y"
                              {(yyval.true_line_column).line = yylineno; (yyval.true_line_column).column = column+1;}
#line 6115 "ada.tab.c"
    break;

  case 332: /* decision: @16 expression_2  */
#line 2738 "ada.y"
           {itoa(branch_nb++, tmp_s, 10);
            itoa(decision_nb++, tmp_s2, 10);
            print_coverage_details(BRAN, tmp_s, current_unit, (yyvsp[-1].true_line_column).line, (yyvsp[-1].true_line_column).column);  //ok : column indicated is just before the expression
            print_coverage_details(DECI, tmp_s2, current_unit, (yyvsp[-1].true_line_column).line, (yyvsp[-1].true_line_column).column); //ok : column indicated is just before the expression
            if ((yyvsp[0].id_deci).is_a_decision)
              {(yyval.id) = malloc(SAFETY+strlen(tmp_s)+strlen(tmp_s2)+strlen((yyvsp[0].id_deci).id)+30);
               strcpy((yyval.id), "bran(");
               strcat((yyval.id), tmp_s);
               strcat((yyval.id), ", deci(");
               strcat((yyval.id), tmp_s2);
               strcat((yyval.id), ", ");
               strcat((yyval.id), (yyvsp[0].id_deci).id);
               strcat((yyval.id), "))");
              }
            else
              {itoa(condition_nb++, tmp_s3, 10);
               (yyval.id) = malloc(SAFETY+strlen(tmp_s)+strlen(tmp_s2)+strlen(tmp_s3)+strlen((yyvsp[0].id_deci).id)+45);
               print_coverage_details(COND, tmp_s3, current_unit, (yyvsp[-1].true_line_column).line, (yyvsp[-1].true_line_column).column);  //ok : column indicated is just before the expression
               strcpy((yyval.id), "bran(");
               strcat((yyval.id), tmp_s);
               strcat((yyval.id), ", deci(");
               strcat((yyval.id), tmp_s2);
               strcat((yyval.id), ", cond(");
               strcat((yyval.id), tmp_s3);
               strcat((yyval.id), ", ");
               strcat((yyval.id), (yyvsp[0].id_deci).id);
               strcat((yyval.id), ")))");
              }
            free((yyvsp[0].id_deci).id);
           }
#line 6150 "ada.tab.c"
    break;

  case 333: /* else_opt: %empty  */
#line 2770 "ada.y"
                                        {(yyval.id) = malloc((SAFETY+32) ); strcpy((yyval.id), "\n              else(stmts([]))");}
#line 6156 "ada.tab.c"
    break;

  case 334: /* else_opt: ELSE sequence_of_statements  */
#line 2772 "ada.y"
           {(yyval.id) = malloc((SAFETY+strlen((yyvsp[0].id))+32) );
            strcpy((yyval.id), "\n              else(stmts([");
            strcat((yyval.id), (yyvsp[0].id));
            strcat((yyval.id), "]))");
            free((yyvsp[0].id));
           }
#line 6167 "ada.tab.c"
    break;

  case 335: /* case_statement: case_hdr pragma_s alternative_list END CASE ';'  */
#line 2781 "ada.y"
                 {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-5].id))+strlen((yyvsp[-3].id))+6) );
                  strcpy((yyval.id), (yyvsp[-5].id));
                  strcat((yyval.id), "[");
                  strcat((yyval.id), (yyvsp[-3].id));
                  strcat((yyval.id), "])");
                  free((yyvsp[-5].id));
                  free((yyvsp[-4].id));
                  free((yyvsp[-3].id));
                 }
#line 6181 "ada.tab.c"
    break;

  case 336: /* @17: %empty  */
#line 2792 "ada.y"
                                   {(yyval.true_line_column).line = yylineno; (yyval.true_line_column).column = column+1;}
#line 6187 "ada.tab.c"
    break;

  case 337: /* case_hdr: CASE @17 expression_2 IS  */
#line 2793 "ada.y"
           {char *expression;
            build_expression((yyvsp[-1].id_deci), &expression, current_unit, (yyvsp[-2].true_line_column).line, (yyvsp[-2].true_line_column).column);
            (yyval.id) = malloc(SAFETY+strlen(expression)+13);
            strcpy((yyval.id), "case_stmt(");
            strcat((yyval.id), expression);
            strcat((yyval.id), ", ");
            free((yyvsp[-1].id_deci).id);
           }
#line 6200 "ada.tab.c"
    break;

  case 338: /* alternative_list: alternative  */
#line 2803 "ada.y"
                               {(yyval.id) = (yyvsp[0].id);}
#line 6206 "ada.tab.c"
    break;

  case 339: /* alternative_list: alternative_list alternative  */
#line 2805 "ada.y"
                   {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id))+4);
                    strcpy((yyval.id), (yyvsp[-1].id));
                    strcat((yyval.id), ",\n");
                    strcat((yyval.id), (yyvsp[0].id));
                    free((yyvsp[-1].id));
                    free((yyvsp[0].id));
                   }
#line 6218 "ada.tab.c"
    break;

  case 340: /* @18: %empty  */
#line 2818 "ada.y"
                                      {(yyval.true_line_column).line = yylineno; (yyval.true_line_column).column = column+1;}
#line 6224 "ada.tab.c"
    break;

  case 341: /* alternative: WHEN @18 choice_s2 RIGHT_SHAFT sequence_of_statements  */
#line 2819 "ada.y"
              {if (!strcmp((yyvsp[-2].id), "others")) {
                 (yyval.id) = malloc(SAFETY+strlen((yyvsp[0].id))+31);
                 strcpy((yyval.id), "alternative(others, stmts([");
                 strcat((yyval.id), (yyvsp[0].id));
                 strcat((yyval.id), "]))");
               }
               else {
                 itoa(branch_nb++, tmp_s, 10);
                 itoa(decision_nb++, tmp_s2, 10);
                 print_coverage_details(BRAN, tmp_s, current_unit, (yyvsp[-3].true_line_column).line, (yyvsp[-3].true_line_column).column);
                 print_coverage_details(DECI, tmp_s2, current_unit, (yyvsp[-3].true_line_column).line, (yyvsp[-3].true_line_column).column);
                 (yyval.id) = malloc((SAFETY+strlen(tmp_s)+strlen(tmp_s2)+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id))+44) );
                 strcpy((yyval.id), "alternative(bran(");
                 strcat((yyval.id), tmp_s);
                 strcat((yyval.id), ", ");
                 strcat((yyval.id), "deci(");
                 strcat((yyval.id), tmp_s2);
                 strcat((yyval.id), ", [");
                 strcat((yyval.id), (yyvsp[-2].id));
                 strcat((yyval.id), "])), stmts([");
                 strcat((yyval.id), (yyvsp[0].id));
                 strcat((yyval.id), "]))");
               }
               free((yyvsp[-2].id));
               free((yyvsp[0].id));
              }
#line 6255 "ada.tab.c"
    break;

  case 342: /* choice_s2: choice2  */
#line 2851 "ada.y"
            {(yyval.id) = (yyvsp[0].id);}
#line 6261 "ada.tab.c"
    break;

  case 343: /* choice_s2: choice_s2 '|' choice2  */
#line 2853 "ada.y"
            {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id))+3) );
             strcpy((yyval.id), (yyvsp[-2].id));
             strcat((yyval.id), ", ");
             strcat((yyval.id), (yyvsp[0].id));
             free((yyvsp[-2].id));
             free((yyvsp[0].id));
            }
#line 6273 "ada.tab.c"
    break;

  case 344: /* choice2: expression_2  */
#line 2865 "ada.y"
          {if ((yyvsp[0].id_deci).is_a_decision)    //it would already contain cond(...)
             (yyval.id) = (yyvsp[0].id_deci).id;
           else
             {itoa(condition_nb++, tmp_s, 10);
              (yyval.id) = malloc(SAFETY+strlen(tmp_s)+strlen((yyvsp[0].id_deci).id)+9);
              print_coverage_details(COND, tmp_s, current_unit, yylineno, column);
              strcpy((yyval.id), "cond(");
              strcat((yyval.id), tmp_s);
              strcat((yyval.id), ", ");
              strcat((yyval.id), (yyvsp[0].id_deci).id);
              strcat((yyval.id), ")");
              free((yyvsp[0].id_deci).id);
             }
          }
#line 6292 "ada.tab.c"
    break;

  case 345: /* choice2: discrete_with_range  */
#line 2880 "ada.y"
          {itoa(condition_nb++, tmp_s, 10);
           (yyval.id) = malloc((SAFETY+strlen(tmp_s)+strlen((yyvsp[0].id))+31) );
           print_coverage_details(COND, tmp_s, current_unit, yylineno, column);
           strcpy((yyval.id), "cond(");
           strcat((yyval.id), tmp_s);
           strcat((yyval.id), ", discrete_with_range(");
           strcat((yyval.id), (yyvsp[0].id));
           strcat((yyval.id), "))");
           free((yyvsp[0].id));
          }
#line 6307 "ada.tab.c"
    break;

  case 346: /* choice2: OTHERS  */
#line 2891 "ada.y"
          {(yyval.id) = malloc(7);
           strcpy((yyval.id), "others");
          }
#line 6315 "ada.tab.c"
    break;

  case 347: /* loop_statement: label_opt iteration_opt basic_loop id_opt ';'  */
#line 2897 "ada.y"
                 {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-4].id))+strlen((yyvsp[-3].id))+strlen((yyvsp[-2].id))+16);
                  strcpy((yyval.id), "loop_stmt(");
                  strcat((yyval.id), (yyvsp[-4].id));
                  strcat((yyval.id), ", ");
                  strcat((yyval.id), (yyvsp[-3].id));
                  strcat((yyval.id), ", ");
                  strcat((yyval.id), (yyvsp[-2].id));
                  strcat((yyval.id), ")");
                  free((yyvsp[-4].id));
                  free((yyvsp[-3].id));
                  free((yyvsp[-2].id));
                 }
#line 6332 "ada.tab.c"
    break;

  case 348: /* label_opt: %empty  */
#line 2911 "ada.y"
                                        {(yyval.id) = malloc((9) ); strcpy((yyval.id), "no_label");}
#line 6338 "ada.tab.c"
    break;

  case 349: /* label_opt: statement_identifier ':'  */
#line 2912 "ada.y"
                                        {(yyval.id) = (yyvsp[-1].id);}
#line 6344 "ada.tab.c"
    break;

  case 350: /* statement_identifier: direct_name  */
#line 2915 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 6350 "ada.tab.c"
    break;

  case 351: /* iteration_opt: %empty  */
#line 2919 "ada.y"
                {(yyval.id) = malloc((SAFETY+6) );
                 strcpy((yyval.id), "loop");
                }
#line 6358 "ada.tab.c"
    break;

  case 352: /* iteration_opt: WHILE decision  */
#line 2924 "ada.y"
                {(yyval.id) = malloc((SAFETY+strlen((yyvsp[0].id))+10) );
                 strcpy((yyval.id), "while, ");
                 strcat((yyval.id), (yyvsp[0].id));
                 free((yyvsp[0].id));
                }
#line 6368 "ada.tab.c"
    break;

  case 353: /* iteration_opt: iter_part reverse_opt discrete_range  */
#line 2931 "ada.y"
                {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id))+1);
                 strcpy((yyval.id), (yyvsp[-2].id));
                 strcat((yyval.id), (yyvsp[-1].id));
                 strcat((yyval.id), (yyvsp[0].id));
                 free((yyvsp[-2].id));
                 free((yyvsp[-1].id));
                 free((yyvsp[0].id));
                }
#line 6381 "ada.tab.c"
    break;

  case 354: /* @19: %empty  */
#line 2941 "ada.y"
                                   {(yyval.true_line_column).line = yylineno; (yyval.true_line_column).column = column+1;}
#line 6387 "ada.tab.c"
    break;

  case 355: /* iter_part: FOR @19 identifier_rule IN  */
#line 2942 "ada.y"
            {itoa(branch_nb++, tmp_s, 10);
             itoa(decision_nb++, tmp_s2, 10);
             itoa(condition_nb++, tmp_s3, 10);
             print_coverage_details(BRAN, tmp_s, current_unit, (yyvsp[-2].true_line_column).line, (yyvsp[-2].true_line_column).column);     //ok : column indicated is just after the FOR keyword
             print_coverage_details(DECI, tmp_s2, current_unit, (yyvsp[-2].true_line_column).line, (yyvsp[-2].true_line_column).column);    //ok : column indicated is just after the FOR keyword
             print_coverage_details(COND, tmp_s3, current_unit, (yyvsp[-2].true_line_column).line, (yyvsp[-2].true_line_column).column);    //ok : column indicated is just after the FOR keyword
             (yyval.id) = malloc(SAFETY+strlen(tmp_s)+strlen(tmp_s2)+strlen(tmp_s3)+strlen((yyvsp[-1].id))+34);
             strcpy((yyval.id), "for, bran(");
             strcat((yyval.id), tmp_s);
             strcat((yyval.id), ", deci(");
             strcat((yyval.id), tmp_s2);
             strcat((yyval.id), ", cond(");
             strcat((yyval.id), tmp_s3);
             strcat((yyval.id), ", ");
             strcat((yyval.id), (yyvsp[-1].id));
             strcat((yyval.id), "))), ");
             free((yyvsp[-1].id));
            }
#line 6410 "ada.tab.c"
    break;

  case 356: /* reverse_opt: %empty  */
#line 2962 "ada.y"
                                {(yyval.id) = malloc(9); strcpy((yyval.id), "normal, ");}
#line 6416 "ada.tab.c"
    break;

  case 357: /* reverse_opt: REVERSE  */
#line 2963 "ada.y"
                                {(yyval.id) = malloc(10); strcpy((yyval.id), "reverse, ");}
#line 6422 "ada.tab.c"
    break;

  case 358: /* basic_loop: LOOP sequence_of_statements END LOOP  */
#line 2967 "ada.y"
             {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-2].id))+10) );
              strcpy((yyval.id), "stmts([");
              strcat((yyval.id), (yyvsp[-2].id));
              strcat((yyval.id), "])");
              free((yyvsp[-2].id));
             }
#line 6433 "ada.tab.c"
    break;

  case 359: /* id_opt: %empty  */
#line 2976 "ada.y"
                     {}
#line 6439 "ada.tab.c"
    break;

  case 360: /* id_opt: designator  */
#line 2977 "ada.y"
                    {}
#line 6445 "ada.tab.c"
    break;

  case 361: /* block_body: BEGiN handled_statement_s  */
#line 2982 "ada.y"
             {(yyval.id) = malloc(SAFETY+strlen((yyvsp[0].id))+9);
              strcpy((yyval.id), "body(");
              strcat((yyval.id), (yyvsp[0].id));
              strcat((yyval.id), ")\n");
              free((yyvsp[0].id));
             }
#line 6456 "ada.tab.c"
    break;

  case 362: /* block: label_opt block_declaration block_body END id_opt ';'  */
#line 2991 "ada.y"
        {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-5].id))+strlen((yyvsp[-4].id))+strlen((yyvsp[-3].id))+12);
         strcpy((yyval.id), "block(");
         strcat((yyval.id), (yyvsp[-5].id));
         strcat((yyval.id), ", ");
         strcat((yyval.id), (yyvsp[-4].id));        //the declare part
         strcat((yyval.id), ", ");
         strcat((yyval.id), (yyvsp[-3].id));
         strcat((yyval.id), ")");
         free((yyvsp[-5].id));
         free((yyvsp[-4].id));
         free((yyvsp[-3].id));
        }
#line 6473 "ada.tab.c"
    break;

  case 363: /* block_declaration: %empty  */
#line 3006 "ada.y"
                    {(yyval.id) = malloc(15);
                     strcpy((yyval.id), "local_decl([])");
                    }
#line 6481 "ada.tab.c"
    break;

  case 364: /* block_declaration: DECLARE declarative_part  */
#line 3010 "ada.y"
                    {(yyval.id) = malloc(SAFETY+strlen((yyvsp[0].id))+15);
                     strcpy((yyval.id), "local_decl([");
                     strcat((yyval.id), (yyvsp[0].id));
                     strcat((yyval.id), "])");
                     free((yyvsp[0].id));
                    }
#line 6492 "ada.tab.c"
    break;

  case 365: /* handled_statement_s: sequence_of_statements exception_handler_part_opt  */
#line 3020 "ada.y"
                      {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id))+25);
                       strcpy((yyval.id), "\nstmts([");
                       strcat((yyval.id), (yyvsp[-1].id));
                       strcat((yyval.id), "\n      ]),\n  ");
                       strcat((yyval.id), (yyvsp[0].id));
                       free((yyvsp[-1].id));
                       free((yyvsp[0].id));
                      }
#line 6505 "ada.tab.c"
    break;

  case 366: /* exception_handler_part_opt: %empty  */
#line 3031 "ada.y"
                             {(yyval.id) = malloc(14 );
                              strcpy((yyval.id), "no_exceptions");
                             }
#line 6513 "ada.tab.c"
    break;

  case 367: /* exception_handler_part_opt: exception_handler_part  */
#line 3035 "ada.y"
                             {(yyval.id) = malloc((SAFETY+strlen((yyvsp[0].id))+22) );
                              strcpy((yyval.id), "exception_handler([");
                              strcat((yyval.id), (yyvsp[0].id));
                              strcat((yyval.id), "])");
                              free((yyvsp[0].id));
                             }
#line 6524 "ada.tab.c"
    break;

  case 368: /* exit_statement: EXIT name_opt when_opt ';'  */
#line 3044 "ada.y"
                 {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[-1].id))+14);
                  strcpy((yyval.id), "exit_when(");
                  strcat((yyval.id), (yyvsp[-2].id));
                  strcat((yyval.id), ", ");
                  strcat((yyval.id), (yyvsp[-1].id));
                  strcat((yyval.id), ")");
                  free((yyvsp[-2].id));
                  free((yyvsp[-1].id));
                 }
#line 6538 "ada.tab.c"
    break;

  case 369: /* name_opt: %empty  */
#line 3055 "ada.y"
                        {(yyval.id) = malloc(8); strcpy((yyval.id), "no_name");}
#line 6544 "ada.tab.c"
    break;

  case 370: /* name_opt: name  */
#line 3056 "ada.y"
                        {(yyval.id) = (yyvsp[0].id);}
#line 6550 "ada.tab.c"
    break;

  case 371: /* when_opt: %empty  */
#line 3059 "ada.y"
                        {(yyval.id) = malloc(8 ); strcpy((yyval.id), "no_when");}
#line 6556 "ada.tab.c"
    break;

  case 372: /* when_opt: WHEN decision  */
#line 3060 "ada.y"
                         {(yyval.id) = malloc((SAFETY+strlen((yyvsp[0].id))+1) );
                          strcpy((yyval.id), (yyvsp[0].id));
                          free((yyvsp[0].id));
                         }
#line 6565 "ada.tab.c"
    break;

  case 373: /* simple_return_statement: RETURN ';'  */
#line 3067 "ada.y"
                   {(yyval.id) = malloc(SAFETY+strlen(current_subprogram_name)+16);
                    strcpy((yyval.id), "return(");
                    strcat((yyval.id), "Return_");
                    strcat((yyval.id), current_subprogram_name);
                    strcat((yyval.id), ")");
                   }
#line 6576 "ada.tab.c"
    break;

  case 374: /* simple_return_statement: RETURN expression_2 ';'  */
#line 3075 "ada.y"
                   {char *expression;
                    int line_int = atoi((yyvsp[-2].id_ref).line);
                    int column_int = atoi((yyvsp[-2].id_ref).column);
                    build_expression((yyvsp[-1].id_deci), &expression, current_unit, line_int, column_int);
                    (yyval.id) = malloc(SAFETY+strlen(current_subprogram_name)+strlen(expression)+18);
                    strcpy((yyval.id), "return(");
                    strcat((yyval.id), "Return_");
                    strcat((yyval.id), current_subprogram_name);
                    strcat((yyval.id), ", ");
                    strcat((yyval.id), expression);
                    strcat((yyval.id), ")");
                    free((yyvsp[-1].id_deci).id);
                   }
#line 6594 "ada.tab.c"
    break;

  case 375: /* extended_return_statement: RETURN identifier_rule ':' constant_opt access_or_subtype_disc init_opt opt_handled_statement_s ';'  */
#line 3092 "ada.y"
                            {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-6].id))+strlen((yyvsp[-4].id))+strlen((yyvsp[-3].id))+strlen((yyvsp[-2].id))+strlen((yyvsp[-1].id))+26);
                             strcpy((yyval.id), "extended_return(");
                             strcat((yyval.id), (yyvsp[-6].id));
                             strcat((yyval.id), ", ");
                             strcat((yyval.id), (yyvsp[-4].id));
                             strcat((yyval.id), ", ");
                             strcat((yyval.id), (yyvsp[-3].id));
                             strcat((yyval.id), ", ");
                             strcat((yyval.id), (yyvsp[-2].id));
                             strcat((yyval.id), ", ");
                             strcat((yyval.id), (yyvsp[-1].id));
                             strcat((yyval.id), ")");
                             free((yyvsp[-6].id));
                             free((yyvsp[-4].id));
                             free((yyvsp[-3].id));
                             free((yyvsp[-2].id));
                             free((yyvsp[-1].id));
                            }
#line 6617 "ada.tab.c"
    break;

  case 376: /* constant_opt: %empty  */
#line 3112 "ada.y"
                                {(yyval.id) = malloc(SAFETY+14); strcpy((yyval.id), "not_constant");}
#line 6623 "ada.tab.c"
    break;

  case 377: /* constant_opt: CONSTANT  */
#line 3113 "ada.y"
                                {(yyval.id) = malloc(SAFETY+9); strcpy((yyval.id), "constant");}
#line 6629 "ada.tab.c"
    break;

  case 378: /* opt_handled_statement_s: %empty  */
#line 3116 "ada.y"
                                      {(yyval.id) = malloc((SAFETY+13) ); strcpy((yyval.id), "nothing");}
#line 6635 "ada.tab.c"
    break;

  case 379: /* opt_handled_statement_s: DO handled_statement_s END RETURN  */
#line 3118 "ada.y"
                          {(yyval.id) = (yyvsp[-2].id);}
#line 6641 "ada.tab.c"
    break;

  case 380: /* goto_statement: GOTO name ';'  */
#line 3122 "ada.y"
                 {(yyval.id) = malloc(strlen((yyvsp[-1].id))+7);
                  strcpy((yyval.id), "goto(");
                  strcat((yyval.id), (yyvsp[-1].id));
                  strcat((yyval.id), ")");
                  free((yyvsp[-1].id));
                 }
#line 6652 "ada.tab.c"
    break;

  case 381: /* procedure_call_statement: name ';'  */
#line 3133 "ada.y"
                           {(yyval.id) = malloc(strlen((yyvsp[-1].id))+17);
                            strcpy((yyval.id), "procedure_call(");
                            strcat((yyval.id), (yyvsp[-1].id));
                            strcat((yyval.id), ")");
                            free((yyvsp[-1].id));
                           }
#line 6663 "ada.tab.c"
    break;

  case 382: /* subprogram_declaration: overriding_indicator_opt subprogram_specification ';'  */
#line 3143 "ada.y"
                         {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[-1].id_subprogram).id)+28);
                          strcpy((yyval.id), "subprogram_declaration(");
                          strcat((yyval.id), (yyvsp[-2].id));
                          strcat((yyval.id), ", ");
                          strcat((yyval.id), (yyvsp[-1].id_subprogram).id);
                          strcat((yyval.id), "))");     //one ')' to clause the subprogram specification)
                          free((yyvsp[-2].id));
                          free((yyvsp[-1].id_subprogram).id);
                          free((yyvsp[-1].id_subprogram).xref_subprogram_name);
                         }
#line 6678 "ada.tab.c"
    break;

  case 383: /* subprogram_declaration: overriding_indicator_opt generic_subp_inst ';'  */
#line 3154 "ada.y"
                         {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[-1].id))+37);
                          strcpy((yyval.id), "generic_subprogram_instantiation(");
                          strcat((yyval.id), (yyvsp[-2].id));
                          strcat((yyval.id), ", ");
                          strcat((yyval.id), (yyvsp[-1].id));
                          strcat((yyval.id), ")");
                          free((yyvsp[-2].id));
                          free((yyvsp[-1].id));
                         }
#line 6692 "ada.tab.c"
    break;

  case 384: /* subprogram_declaration: overriding_indicator_opt subprogram_specification_is_push ABSTRACT ';'  */
#line 3164 "ada.y"
                         {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-3].id))+strlen((yyvsp[-2].id_subprogram).id)+38);
                          strcpy((yyval.id), "abstract_subprogram_declaration(");
                          strcat((yyval.id), (yyvsp[-3].id));
                          strcat((yyval.id), ", ");
                          strcat((yyval.id), (yyvsp[-2].id_subprogram).id);
                          strcat((yyval.id), "))");     //one ')' to clause the subprogram specification)
                          free((yyvsp[-3].id));
                          free((yyvsp[-2].id_subprogram).id);
                          free((yyvsp[-2].id_subprogram).xref_subprogram_name);
                         }
#line 6707 "ada.tab.c"
    break;

  case 385: /* subprogram_declaration: overriding_indicator_opt subprogram_specification_is_push NuLL ';'  */
#line 3175 "ada.y"
                         {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-3].id))+strlen((yyvsp[-2].id_subprogram).id)+29);
                          strcpy((yyval.id), "null_prodecure_declaration(");
                          strcat((yyval.id), (yyvsp[-3].id));
                          strcat((yyval.id), ", ");
                          strcat((yyval.id), (yyvsp[-2].id_subprogram).id);
                          strcat((yyval.id), "))");     //one ')' to clause the subprogram specification)
                          free((yyvsp[-3].id));
                          free((yyvsp[-2].id_subprogram).id);
                          free((yyvsp[-2].id_subprogram).xref_subprogram_name);
                         }
#line 6722 "ada.tab.c"
    break;

  case 386: /* subprogram_specification: PROCEDURE compound_name formal_part_opt  */
#line 3189 "ada.y"
                                {(yyval.id_subprogram).xref_subprogram_name = malloc(strlen((yyvsp[-1].id)) + 1);
                                 strcpy((yyval.id_subprogram).xref_subprogram_name, (yyvsp[-1].id));
                                 (yyval.id_subprogram).id = malloc(SAFETY+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id))+100);
                                 strcpy((yyval.id_subprogram).id, "\nprocedure_body(\n  ");
                                 strcat((yyval.id_subprogram).id, (yyvsp[-1].id));
                                 strcat((yyval.id_subprogram).id, ", no_return");
                                 strcat((yyval.id_subprogram).id, ",\n  parameters([");
                                 strcat((yyval.id_subprogram).id, (yyvsp[0].id));
                                 strcat((yyval.id_subprogram).id, "])\n");
                                 free((yyvsp[-1].id));
                                 free((yyvsp[0].id));
                                 //printf("hi from %s\n", $2);
                                }
#line 6740 "ada.tab.c"
    break;

  case 387: /* subprogram_specification: FUNCTION designator formal_part_opt RETURN access_or_subtype_disc  */
#line 3203 "ada.y"
                                {(yyval.id_subprogram).xref_subprogram_name = malloc(strlen((yyvsp[-3].id)) + 1);
                                 strcpy((yyval.id_subprogram).xref_subprogram_name, (yyvsp[-3].id));
                                 (yyval.id_subprogram).id = malloc(SAFETY+strlen((yyvsp[-3].id))+strlen((yyvsp[-3].id))+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id))+100);
                                 strcpy((yyval.id_subprogram).id, "\nfunction_body(\n  ");
                                 strcat((yyval.id_subprogram).id, (yyvsp[-3].id));
                                 strcat((yyval.id_subprogram).id, ", Return_");
                                 strcat((yyval.id_subprogram).id, (yyvsp[-3].id));
                                 strcat((yyval.id_subprogram).id, ", ");
                                 strcat((yyval.id_subprogram).id, (yyvsp[0].id));
                                 strcat((yyval.id_subprogram).id, ",\n  parameters([");
                                 strcat((yyval.id_subprogram).id, (yyvsp[-2].id));
                                 strcat((yyval.id_subprogram).id, "])\n");
                                 free((yyvsp[-3].id));
                                 free((yyvsp[-2].id));
                                 free((yyvsp[0].id));
                                }
#line 6761 "ada.tab.c"
    break;

  case 388: /* designator: compound_name  */
#line 3221 "ada.y"
                                {(yyval.id) = (yyvsp[0].id);}
#line 6767 "ada.tab.c"
    break;

  case 389: /* designator: string_literal  */
#line 3223 "ada.y"
             {(yyval.id) = handle_identifiers((yyvsp[0].id_ref), 0);//0 because although it is a string, it should always be crossreferenced by gnatxref (since it is designator), so it is really an error if it is not found
             }
#line 6774 "ada.tab.c"
    break;

  case 390: /* formal_part_opt: %empty  */
#line 3228 "ada.y"
                                      {(yyval.id) = malloc(1); strcpy((yyval.id), "");}
#line 6780 "ada.tab.c"
    break;

  case 391: /* formal_part_opt: formal_part  */
#line 3229 "ada.y"
                                      {(yyval.id) = (yyvsp[0].id);}
#line 6786 "ada.tab.c"
    break;

  case 392: /* formal_part: '(' parameter_specification_list ')'  */
#line 3233 "ada.y"
                {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-1].id))+1) );
                 strcpy((yyval.id), (yyvsp[-1].id));
                 free((yyvsp[-1].id));
                }
#line 6795 "ada.tab.c"
    break;

  case 393: /* parameter_specification_list: parameter_specification  */
#line 3239 "ada.y"
                                                                        {(yyval.id) = (yyvsp[0].id);}
#line 6801 "ada.tab.c"
    break;

  case 394: /* parameter_specification_list: parameter_specification_list ';' parameter_specification  */
#line 3240 "ada.y"
                                                                        {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id))+3) );
                                                                         strcpy((yyval.id), (yyvsp[-2].id));
                                                                         strcat((yyval.id), ", \n");
                                                                         strcat((yyval.id), (yyvsp[0].id));
                                                                         free((yyvsp[-2].id));
                                                                         free((yyvsp[0].id));
                                                                        }
#line 6813 "ada.tab.c"
    break;

  case 395: /* parameter_specification: identifier_list ':' mode access_or_subtype_disc init_opt  */
#line 3250 "ada.y"
        {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-4].id))+strlen((yyvsp[-2].id))+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id))+16) );
         strcpy((yyval.id), "param([");
         strcat((yyval.id), (yyvsp[-4].id));
         strcat((yyval.id), "], ");
         strcat((yyval.id), (yyvsp[-2].id));
         strcat((yyval.id), ", ");
         strcat((yyval.id), (yyvsp[-1].id));
         strcat((yyval.id), ", ");
         strcat((yyval.id), (yyvsp[0].id));
         strcat((yyval.id), ")");
         free((yyvsp[-4].id));
         free((yyvsp[-2].id));
         free((yyvsp[-1].id));
         free((yyvsp[0].id));
        }
#line 6833 "ada.tab.c"
    break;

  case 396: /* mode: %empty  */
#line 3267 "ada.y"
                        {(yyval.id) = malloc(3 ); strcpy((yyval.id), "in");}
#line 6839 "ada.tab.c"
    break;

  case 397: /* mode: IN  */
#line 3268 "ada.y"
                        {(yyval.id) = malloc(3 ); strcpy((yyval.id), "in");}
#line 6845 "ada.tab.c"
    break;

  case 398: /* mode: OUT  */
#line 3269 "ada.y"
                        {(yyval.id) = malloc(4 ); strcpy((yyval.id), "out");}
#line 6851 "ada.tab.c"
    break;

  case 399: /* mode: IN OUT  */
#line 3270 "ada.y"
                        {(yyval.id) = malloc(7 ); strcpy((yyval.id), "in_out");}
#line 6857 "ada.tab.c"
    break;

  case 400: /* subprogram_specification_is_push: subprogram_specification IS  */
#line 3274 "ada.y"
                                                               {(yyval.id_subprogram) = (yyvsp[-1].id_subprogram);}
#line 6863 "ada.tab.c"
    break;

  case 401: /* $@20: %empty  */
#line 3277 "ada.y"
                                                                                             {strcpy(current_subprogram_name, (yyvsp[-1].id_subprogram).xref_subprogram_name);}
#line 6869 "ada.tab.c"
    break;

  case 402: /* subprogram_body: overriding_indicator_opt subprogram_specification_is_push declarative_part $@20 block_body END id_opt ';'  */
#line 3278 "ada.y"
                  {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-7].id))+strlen((yyvsp[-6].id_subprogram).id)+strlen((yyvsp[-5].id))+strlen((yyvsp[-3].id))+72);
                   strcpy((yyval.id), "subprogram_body(");
                   strcat((yyval.id), (yyvsp[-7].id));
                   strcat((yyval.id), ", ");
                   strcat((yyval.id), (yyvsp[-6].id_subprogram).id);
                   strcat((yyval.id), ",\n  local_decl([");
                   strcat((yyval.id), (yyvsp[-5].id));
                   strcat((yyval.id), "\n            ]),");
                   strcat((yyval.id), (yyvsp[-3].id));
                   strcat((yyval.id), "\n             ))\n");    //one ')' to clause the subprogram specification)
                   free((yyvsp[-7].id));
                   free((yyvsp[-6].id_subprogram).id);
                   free((yyvsp[-6].id_subprogram).xref_subprogram_name);
                   free((yyvsp[-5].id));
                   free((yyvsp[-3].id));
                  }
#line 6890 "ada.tab.c"
    break;

  case 403: /* declarative_part: %empty  */
#line 3299 "ada.y"
                                        {(yyval.id) = malloc((SAFETY+23) ); strcpy((yyval.id), "empty_declarative_part");}
#line 6896 "ada.tab.c"
    break;

  case 404: /* declarative_part: decl_item_or_body_sl  */
#line 3300 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 6902 "ada.tab.c"
    break;

  case 405: /* decl_item_or_body_sl: decl_item_or_body  */
#line 3303 "ada.y"
                                                {(yyval.id) = (yyvsp[0].id);}
#line 6908 "ada.tab.c"
    break;

  case 406: /* decl_item_or_body_sl: decl_item_or_body_sl decl_item_or_body  */
#line 3305 "ada.y"
                       {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id))+5) );
                        strcpy((yyval.id), (yyvsp[-1].id));
                        strcat((yyval.id), ",\n ");
                        strcat((yyval.id), (yyvsp[0].id));
                        free((yyvsp[-1].id));
                        free((yyvsp[0].id));
                       }
#line 6920 "ada.tab.c"
    break;

  case 407: /* decl_item_or_body: body  */
#line 3314 "ada.y"
                                {(yyval.id) = (yyvsp[0].id);}
#line 6926 "ada.tab.c"
    break;

  case 408: /* decl_item_or_body: decl_item  */
#line 3315 "ada.y"
                                {(yyval.id) = (yyvsp[0].id);}
#line 6932 "ada.tab.c"
    break;

  case 409: /* decl_item_s: %empty  */
#line 3318 "ada.y"
                            {(yyval.id) = malloc(1); strcpy((yyval.id), "");}
#line 6938 "ada.tab.c"
    break;

  case 410: /* decl_item_s: decl_item_s1  */
#line 3319 "ada.y"
                            {(yyval.id) = (yyvsp[0].id);}
#line 6944 "ada.tab.c"
    break;

  case 411: /* decl_item_s1: decl_item  */
#line 3322 "ada.y"
                            {(yyval.id) = (yyvsp[0].id);}
#line 6950 "ada.tab.c"
    break;

  case 412: /* decl_item_s1: decl_item_s1 decl_item  */
#line 3323 "ada.y"
                                        {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id))+5) );
                                         strcpy((yyval.id), (yyvsp[-1].id));
                                         strcat((yyval.id), ",\n");
                                         strcat((yyval.id), (yyvsp[0].id));
                                         free((yyvsp[-1].id));
                                         free((yyvsp[0].id));
                                        }
#line 6962 "ada.tab.c"
    break;

  case 413: /* decl_item: decl  */
#line 3332 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 6968 "ada.tab.c"
    break;

  case 414: /* decl_item: use_clause  */
#line 3333 "ada.y"
                                        {(yyval.id) = malloc(11 ); strcpy((yyval.id), "use_clause");}
#line 6974 "ada.tab.c"
    break;

  case 415: /* decl_item: aspect_clause  */
#line 3334 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 6980 "ada.tab.c"
    break;

  case 416: /* decl_item: pragma  */
#line 3335 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 6986 "ada.tab.c"
    break;

  case 417: /* decl: object_declaration  */
#line 3338 "ada.y"
                                   {(yyval.id) = (yyvsp[0].id);}
#line 6992 "ada.tab.c"
    break;

  case 418: /* decl: number_declaration  */
#line 3339 "ada.y"
                                   {(yyval.id) = (yyvsp[0].id);}
#line 6998 "ada.tab.c"
    break;

  case 419: /* decl: type_declaration  */
#line 3340 "ada.y"
                                   {(yyval.id) = (yyvsp[0].id);}
#line 7004 "ada.tab.c"
    break;

  case 420: /* decl: subtype_declaration  */
#line 3341 "ada.y"
                                   {(yyval.id) = (yyvsp[0].id);}
#line 7010 "ada.tab.c"
    break;

  case 421: /* decl: subprogram_declaration  */
#line 3342 "ada.y"
                                   {(yyval.id) = (yyvsp[0].id);}
#line 7016 "ada.tab.c"
    break;

  case 422: /* decl: package_declaration  */
#line 3343 "ada.y"
                                   {(yyval.id) = (yyvsp[0].id);}
#line 7022 "ada.tab.c"
    break;

  case 423: /* decl: task_declaration  */
#line 3344 "ada.y"
                                   {(yyval.id) = (yyvsp[0].id);}
#line 7028 "ada.tab.c"
    break;

  case 424: /* decl: protected_declaration  */
#line 3345 "ada.y"
                                   {(yyval.id) = (yyvsp[0].id);}
#line 7034 "ada.tab.c"
    break;

  case 425: /* decl: exception_declaration  */
#line 3346 "ada.y"
                                   {(yyval.id) = (yyvsp[0].id);}
#line 7040 "ada.tab.c"
    break;

  case 426: /* decl: rename_declaration  */
#line 3347 "ada.y"
                                   {(yyval.id) = (yyvsp[0].id);}
#line 7046 "ada.tab.c"
    break;

  case 427: /* decl: generic_declaration  */
#line 3348 "ada.y"
                                   {(yyval.id) = (yyvsp[0].id);}
#line 7052 "ada.tab.c"
    break;

  case 428: /* decl: body_stub  */
#line 3350 "ada.y"
       {char *name;
        char *path = NULL;
        char *filename = NULL;
        int copy_length;
        FILE *tmp_stream;
        (yyval.id) = malloc(strlen((yyvsp[0].id)) + 17);
        strcpy((yyval.id), "body_stub(");
        strcat((yyval.id), (yyvsp[0].id));         //the full xref name of the body_stub e.g. Speed_42
        strcat((yyval.id), "_stub");
        strcat((yyval.id), ")");
        name = malloc(strlen(current_unit->name)+strlen((yyvsp[0].id))+2);
        strcpy(name, current_unit->name);
        strcat(name, "-");
        //transforms Speed_32 into 'speed' see electronic diary  08/23/09
        copy_length = strlen((yyvsp[0].id)) - strlen(strrchr ((yyvsp[0].id), '_'));
        strncpy(tmp_s, (yyvsp[0].id), copy_length); //strrchr : last occurence of character
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
#line 7111 "ada.tab.c"
    break;

  case 429: /* body: subprogram_body  */
#line 3406 "ada.y"
                                {(yyval.id) = (yyvsp[0].id);}
#line 7117 "ada.tab.c"
    break;

  case 430: /* body: package_body  */
#line 3407 "ada.y"
                                {(yyval.id) = (yyvsp[0].id);}
#line 7123 "ada.tab.c"
    break;

  case 431: /* body: task_body  */
#line 3408 "ada.y"
                                {(yyval.id) = malloc(10); strcpy((yyval.id), "task_body");}
#line 7129 "ada.tab.c"
    break;

  case 432: /* body: protected_body  */
#line 3409 "ada.y"
                                {(yyval.id) = malloc(15); strcpy((yyval.id), "protected_body");}
#line 7135 "ada.tab.c"
    break;

  case 433: /* package_declaration: package_specification ';'  */
#line 3412 "ada.y"
                                                {(yyval.id) = (yyvsp[-1].id);}
#line 7141 "ada.tab.c"
    break;

  case 434: /* package_declaration: generic_pkg_inst ';'  */
#line 3413 "ada.y"
                                                {(yyval.id) = (yyvsp[-1].id);}
#line 7147 "ada.tab.c"
    break;

  case 435: /* $@21: %empty  */
#line 3418 "ada.y"
                        {if (!is_standard || debugMode) {
                            if (debugMode) {
                                fprintf(stdout, "Parsing package specification %s\n", (yyvsp[-1].id));
                            }
                            else {
                                fprintf(stdout, ".");
                                //printout_shortened($2);  //we do not want to print the unique identifier
                                //fprintf(stdout, "\n");
                            }
                            fflush(stdout);
                         }
                        }
#line 7164 "ada.tab.c"
    break;

  case 436: /* package_specification: PACKAGE compound_name IS $@21 decl_item_s private_part_opt END c_id_opt  */
#line 3431 "ada.y"
                        {(yyval.id) = malloc((strlen((yyvsp[-6].id))+strlen((yyvsp[-3].id))+strlen((yyvsp[-2].id))+73) );
                         strcpy((yyval.id), "\npackage_specification(\n ");
                         strcat((yyval.id), (yyvsp[-6].id));
                         strcat((yyval.id), ",\n local_decl([");
                         strcat((yyval.id), (yyvsp[-3].id));
                         strcat((yyval.id), "])\n");
                         strcat((yyval.id), ", ");
                         strcat((yyval.id), (yyvsp[-2].id));
                         strcat((yyval.id), "\n                     )\n");
                         free((yyvsp[-6].id));
                         free((yyvsp[-3].id));
                         free((yyvsp[-2].id));
                         }
#line 7182 "ada.tab.c"
    break;

  case 437: /* private_part_opt: %empty  */
#line 3446 "ada.y"
                                        {(yyval.id) = malloc(11 ); strcpy((yyval.id), "no_private");}
#line 7188 "ada.tab.c"
    break;

  case 438: /* private_part_opt: PRIVATE decl_item_s  */
#line 3448 "ada.y"
                   {(yyval.id) = malloc((strlen((yyvsp[0].id))+12) );
                    strcpy((yyval.id), "private([");
                    strcat((yyval.id), (yyvsp[0].id));
                    strcat((yyval.id), "])");
                    free((yyvsp[0].id));
                   }
#line 7199 "ada.tab.c"
    break;

  case 440: /* c_id_opt: compound_name  */
#line 3457 "ada.y"
                                {free((yyvsp[0].id));}
#line 7205 "ada.tab.c"
    break;

  case 441: /* $@22: %empty  */
#line 3461 "ada.y"
               {if (debugMode) fprintf(stdout, "Parsing package body %s\n", (yyvsp[-1].id));
                else fprintf(stdout, ".");
                fflush(stdout);
               }
#line 7214 "ada.tab.c"
    break;

  case 442: /* package_body: PACKAGE BODY compound_name IS $@22 declarative_part body_opt END c_id_opt ';'  */
#line 3466 "ada.y"
               {strcpy(current_package_name, (yyvsp[-7].id));
                (yyval.id) = malloc((SAFETY+strlen((yyvsp[-7].id))+strlen((yyvsp[-4].id))+strlen((yyvsp[-3].id))+83) );
                strcpy((yyval.id), "\npackage_body(\n  ");
                strcat((yyval.id), (yyvsp[-7].id));
                strcat((yyval.id), ", local_decl([");
                strcat((yyval.id), (yyvsp[-4].id));
                strcat((yyval.id), "]),\n");
                strcat((yyval.id), (yyvsp[-3].id));
                strcat((yyval.id), ")\n");
                free((yyvsp[-7].id));
                free((yyvsp[-4].id));
                free((yyvsp[-3].id));
               }
#line 7232 "ada.tab.c"
    break;

  case 443: /* body_opt: %empty  */
#line 3481 "ada.y"
                        {(yyval.id) = malloc(11 ); strcpy((yyval.id), "empty_body");}
#line 7238 "ada.tab.c"
    break;

  case 444: /* body_opt: block_body  */
#line 3482 "ada.y"
                        {(yyval.id) = (yyvsp[0].id);}
#line 7244 "ada.tab.c"
    break;

  case 445: /* limited_opt: %empty  */
#line 3485 "ada.y"
                                {(yyval.id) = malloc(12 ); strcpy((yyval.id), "not_limited");}
#line 7250 "ada.tab.c"
    break;

  case 446: /* limited_opt: LIMITED  */
#line 3486 "ada.y"
                                {(yyval.id) = malloc(8 ); strcpy((yyval.id), "limited");}
#line 7256 "ada.tab.c"
    break;

  case 447: /* rename_declaration: identifier_list ':' object_qualifier_opt object_subtype_definition renames ';'  */
#line 3495 "ada.y"
                     {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-5].id))+strlen((yyvsp[-2].id))+strlen((yyvsp[-1].id))+16) );
                      strcpy((yyval.id), " rename(");
                      strcat((yyval.id), (yyvsp[-5].id));       //always a single defining_identifier, see !!!! above
                      strcat((yyval.id), ", ");
                                            //the object_qualifier_opt is always ignored as it will always be 'not_qualified', see !!!! above
                      strcat((yyval.id), (yyvsp[-2].id));       //always a subtype_indication with no constraint or an access_type, see !!!! above
                      strcat((yyval.id), ", ");
                      strcat((yyval.id), (yyvsp[-1].id));
                      strcat((yyval.id), ")");
                      free((yyvsp[-5].id));
                      free((yyvsp[-3].id));
                      free((yyvsp[-2].id));
                      free((yyvsp[-1].id));
                     }
#line 7275 "ada.tab.c"
    break;

  case 448: /* rename_declaration: identifier_list ':' EXCEPTION renames ';'  */
#line 3510 "ada.y"
                     {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-4].id))+strlen((yyvsp[-1].id))+22) );
                      strcpy((yyval.id), " rename_exception(");
                      strcat((yyval.id), (yyvsp[-4].id));       //always a single defining_identifier, see !!!! above
                      strcat((yyval.id), ", ");
                      strcat((yyval.id), (yyvsp[-1].id));
                      strcat((yyval.id), ")");
                      free((yyvsp[-4].id));
                      free((yyvsp[-1].id));
                     }
#line 7289 "ada.tab.c"
    break;

  case 449: /* rename_declaration: rename_unit  */
#line 3520 "ada.y"
                     {(yyval.id) = (yyvsp[0].id);}
#line 7295 "ada.tab.c"
    break;

  case 450: /* rename_unit: PACKAGE compound_name renames ';'  */
#line 3525 "ada.y"
              {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[-1].id))+19) );
               strcpy((yyval.id), " package_rename(");
               strcat((yyval.id), (yyvsp[-2].id));
               strcat((yyval.id), ", ");
               strcat((yyval.id), (yyvsp[-1].id));
               strcat((yyval.id), ")");
               free((yyvsp[-2].id));
               free((yyvsp[-1].id));
              }
#line 7309 "ada.tab.c"
    break;

  case 451: /* $@23: %empty  */
#line 3534 "ada.y"
                                                                {strcpy(not_cross_referenced_operator, "");}
#line 7315 "ada.tab.c"
    break;

  case 452: /* rename_unit: overriding_indicator_opt subprogram_specification $@23 renames ';'  */
#line 3535 "ada.y"
              {if (strcmp(not_cross_referenced_operator, "")) {//$2.id is not a user defined operator but is predefined, so instead of printing $2.id on reference we should print "op" not indexed()
                 add_operator((yyvsp[-3].id_subprogram).xref_subprogram_name); //will be checked in handle_operator_calls
                 if (debugMode) fprintf(stdout, "ADDED operator: %s\n", (yyvsp[-3].id_subprogram).xref_subprogram_name);
                 (yyval.id) = malloc(8);
                 strcpy((yyval.id), "nothing"); //because, in this case, the subprogram_rename() will never be needed
               }
               else {
                 (yyval.id) = malloc(SAFETY+strlen((yyvsp[-4].id))+strlen((yyvsp[-3].id_subprogram).id)+strlen((yyvsp[-1].id))+26);
                 strcpy((yyval.id), " subprogram_rename(");
                 strcat((yyval.id), (yyvsp[-4].id));
                 strcat((yyval.id), ", ");
                 strcat((yyval.id), (yyvsp[-3].id_subprogram).id);
                 strcat((yyval.id), "),  ");      //one ')' to clause the subprogram specification
                 strcat((yyval.id), (yyvsp[-1].id));
                 strcat((yyval.id), ")");
               }
               free((yyvsp[-4].id));
               free((yyvsp[-3].id_subprogram).id);
               free((yyvsp[-3].id_subprogram).xref_subprogram_name);
               free((yyvsp[-1].id));
              }
#line 7341 "ada.tab.c"
    break;

  case 453: /* rename_unit: generic_formal_part PACKAGE compound_name renames ';'  */
#line 3557 "ada.y"
              {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[-1].id))+32);
               strcpy((yyval.id), " generic_package_rename(");
               strcat((yyval.id), (yyvsp[-2].id));
               strcat((yyval.id), ", ");
               strcat((yyval.id), (yyvsp[-1].id));
               strcat((yyval.id), ")");
               free((yyvsp[-4].id));
               free((yyvsp[-2].id));
               free((yyvsp[-1].id));
              }
#line 7356 "ada.tab.c"
    break;

  case 454: /* rename_unit: generic_formal_part FUNCTION designator renames ';'  */
#line 3568 "ada.y"
              {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[-1].id))+38);
               strcpy((yyval.id), " generic_subprogram_rename(");
               strcat((yyval.id), (yyvsp[-2].id));
               strcat((yyval.id), ", ");
               strcat((yyval.id), (yyvsp[-1].id));
               strcat((yyval.id), ")");
               free((yyvsp[-4].id));
               free((yyvsp[-2].id));
               free((yyvsp[-1].id));
              }
#line 7371 "ada.tab.c"
    break;

  case 455: /* rename_unit: generic_formal_part PROCEDURE designator renames ';'  */
#line 3579 "ada.y"
              {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-4].id))+strlen((yyvsp[-2].id))+strlen((yyvsp[-1].id))+38);
               strcpy((yyval.id), " generic_subprogram_rename(");
               strcat((yyval.id), (yyvsp[-2].id));
               strcat((yyval.id), ", ");
               strcat((yyval.id), (yyvsp[-1].id));
               strcat((yyval.id), ")");
               free((yyvsp[-4].id));
               free((yyvsp[-2].id));
               free((yyvsp[-1].id));
              }
#line 7386 "ada.tab.c"
    break;

  case 456: /* renames: RENAMES name_or_character_literal  */
#line 3596 "ada.y"
                                            {(yyval.id) = (yyvsp[0].id);}
#line 7392 "ada.tab.c"
    break;

  case 457: /* name_or_character_literal: name  */
#line 3600 "ada.y"
                                                {(yyval.id) = (yyvsp[0].id);}
#line 7398 "ada.tab.c"
    break;

  case 458: /* name_or_character_literal: character_literal  */
#line 3601 "ada.y"
                                                {(yyval.id) = handle_identifiers((yyvsp[0].id_ref), 0);}
#line 7404 "ada.tab.c"
    break;

  case 459: /* task_declaration: task_spec ';'  */
#line 3607 "ada.y"
                   {(yyval.id) = malloc((SAFETY+17) );
                    strcpy((yyval.id), "task_declaration");
                   }
#line 7412 "ada.tab.c"
    break;

  case 463: /* task_def: IS interface_list_opt entry_decl_s rep_spec_s task_private_opt END id_opt  */
#line 3618 "ada.y"
           {free((yyvsp[-5].id));}
#line 7418 "ada.tab.c"
    break;

  case 464: /* interface_list_opt: %empty  */
#line 3623 "ada.y"
                     {(yyval.id) = malloc(3);
                      strcpy((yyval.id), "[]");
                     }
#line 7426 "ada.tab.c"
    break;

  case 465: /* interface_list_opt: NEW interface_list_item_sl WITH  */
#line 3627 "ada.y"
                     {(yyval.id) = malloc(strlen((yyvsp[-1].id))+3);
                      strcpy((yyval.id), "[");
                      strcat((yyval.id), (yyvsp[-1].id));
                      strcat((yyval.id), "]");
                      free((yyvsp[-1].id));
                     }
#line 7437 "ada.tab.c"
    break;

  case 469: /* protected_declaration: protected_spec ';'  */
#line 3644 "ada.y"
                        {(yyval.id) = malloc((SAFETY+22) );
                         strcpy((yyval.id), "protected_declaration");
                        }
#line 7445 "ada.tab.c"
    break;

  case 472: /* protected_def: IS interface_list_opt protected_op_decl_s protected_private_opt END id_opt  */
#line 3655 "ada.y"
                {free((yyvsp[-4].id));}
#line 7451 "ada.tab.c"
    break;

  case 478: /* protected_op_decl: subprogram_specification ';'  */
#line 3667 "ada.y"
                                                        {;}
#line 7457 "ada.tab.c"
    break;

  case 479: /* protected_op_decl: aspect_clause  */
#line 3668 "ada.y"
                                        {;}
#line 7463 "ada.tab.c"
    break;

  case 480: /* protected_op_decl: pragma  */
#line 3669 "ada.y"
                           {free((yyvsp[0].id));}
#line 7469 "ada.tab.c"
    break;

  case 484: /* protected_elem_decl: component_declaration  */
#line 3677 "ada.y"
                                                {;}
#line 7475 "ada.tab.c"
    break;

  case 486: /* protected_op_item_s: pragma_s  */
#line 3683 "ada.y"
                                {free((yyvsp[0].id));}
#line 7481 "ada.tab.c"
    break;

  case 487: /* protected_op_item_s: protected_op_item_s protected_op_item pragma_s  */
#line 3685 "ada.y"
                      {free((yyvsp[0].id));
                      }
#line 7488 "ada.tab.c"
    break;

  case 489: /* protected_op_item: subprogram_body  */
#line 3690 "ada.y"
                                        {free((yyvsp[0].id));}
#line 7494 "ada.tab.c"
    break;

  case 490: /* protected_op_item: overriding_indicator_opt subprogram_specification ';'  */
#line 3691 "ada.y"
                                                                                 {free((yyvsp[-2].id)); free((yyvsp[-1].id_subprogram).id); free((yyvsp[-1].id_subprogram).xref_subprogram_name);}
#line 7500 "ada.tab.c"
    break;

  case 491: /* protected_op_item: aspect_clause  */
#line 3692 "ada.y"
                                        {free((yyvsp[0].id));}
#line 7506 "ada.tab.c"
    break;

  case 492: /* entry_decl_s: pragma_s  */
#line 3695 "ada.y"
                        {free((yyvsp[0].id));}
#line 7512 "ada.tab.c"
    break;

  case 493: /* entry_decl_s: entry_decl_s entry_decl pragma_s  */
#line 3697 "ada.y"
               {free((yyvsp[0].id));
               }
#line 7519 "ada.tab.c"
    break;

  case 494: /* entry_decl: overriding_indicator_opt ENTRY identifier formal_part_opt ';'  */
#line 3702 "ada.y"
             {free((yyvsp[-4].id));}
#line 7525 "ada.tab.c"
    break;

  case 495: /* entry_decl: overriding_indicator_opt ENTRY identifier '(' discrete_range ')' formal_part_opt ';'  */
#line 3704 "ada.y"
             {free((yyvsp[-7].id));}
#line 7531 "ada.tab.c"
    break;

  case 501: /* rep_spec_s: rep_spec_s aspect_clause pragma_s  */
#line 3716 "ada.y"
                                               {free((yyvsp[0].id));}
#line 7537 "ada.tab.c"
    break;

  case 502: /* entry_call: procedure_call_statement  */
#line 3719 "ada.y"
                                        {free((yyvsp[0].id));}
#line 7543 "ada.tab.c"
    break;

  case 503: /* accept_statement: accept_hdr ';'  */
#line 3723 "ada.y"
                   {(yyval.id) = malloc((17) );
                    strcpy((yyval.id), "accept_statement");
                   }
#line 7551 "ada.tab.c"
    break;

  case 504: /* accept_statement: accept_hdr DO handled_statement_s END id_opt ';'  */
#line 3727 "ada.y"
                   {(yyval.id) = malloc((20) );
                    strcpy((yyval.id), "accept_do_statement");
                   }
#line 7559 "ada.tab.c"
    break;

  case 506: /* entry_name: direct_name  */
#line 3735 "ada.y"
                                {free((yyvsp[0].id));}
#line 7565 "ada.tab.c"
    break;

  case 508: /* delay_statement: DELAY expression ';'  */
#line 3740 "ada.y"
                  {(yyval.id) = malloc((16) );
                   strcpy((yyval.id), "delay_statement");
                  }
#line 7573 "ada.tab.c"
    break;

  case 509: /* delay_statement: DELAY UNTIL expression ';'  */
#line 3744 "ada.y"
                  {(yyval.id) = malloc((22) );
                   strcpy((yyval.id), "delay_until_statement");
                  }
#line 7581 "ada.tab.c"
    break;

  case 510: /* select_statement: select_wait  */
#line 3750 "ada.y"
                   {(yyval.id) = malloc((17) );
                   strcpy((yyval.id), "select_statement");
                   }
#line 7589 "ada.tab.c"
    break;

  case 511: /* select_statement: async_select  */
#line 3754 "ada.y"
                   {(yyval.id) = malloc((17) );
                   strcpy((yyval.id), "select_statement");
                   }
#line 7597 "ada.tab.c"
    break;

  case 512: /* select_statement: timed_entry_call  */
#line 3758 "ada.y"
                   {(yyval.id) = malloc((17) );
                   strcpy((yyval.id), "select_statement");
                   }
#line 7605 "ada.tab.c"
    break;

  case 513: /* select_statement: cond_entry_call  */
#line 3762 "ada.y"
                   {(yyval.id) = malloc((17) );
                   strcpy((yyval.id), "select_statement");
                   }
#line 7613 "ada.tab.c"
    break;

  case 519: /* select_alt: accept_statement stmts_opt  */
#line 3778 "ada.y"
                                        {;}
#line 7619 "ada.tab.c"
    break;

  case 520: /* select_alt: delay_statement stmts_opt  */
#line 3779 "ada.y"
                                        {;}
#line 7625 "ada.tab.c"
    break;

  case 522: /* delay_or_entry_alt: delay_statement stmts_opt  */
#line 3783 "ada.y"
                                               {free((yyvsp[-1].id));}
#line 7631 "ada.tab.c"
    break;

  case 528: /* stmts_opt: sequence_of_statements  */
#line 3799 "ada.y"
                                        {free((yyvsp[0].id));}
#line 7637 "ada.tab.c"
    break;

  case 529: /* abort_statement: ABORT name_list ';'  */
#line 3803 "ada.y"
                  {(yyval.id) = malloc((16) );
                   strcpy((yyval.id), "abort_statement");
                  }
#line 7645 "ada.tab.c"
    break;

  case 530: /* compilation: %empty  */
#line 3809 "ada.y"
                          {fprintf(parsed, " nothing\n");}
#line 7651 "ada.tab.c"
    break;

  case 531: /* $@24: %empty  */
#line 3810 "ada.y"
                          {fprintf(parsed, ",");}
#line 7657 "ada.tab.c"
    break;

  case 533: /* compilation: pragma pragma_s  */
#line 3812 "ada.y"
              {fprintf(parsed, "%s", (yyvsp[-1].id));
               if (strcmp((yyvsp[0].id), "")) fprintf(parsed, ", %s", (yyvsp[0].id));
               free((yyvsp[-1].id));
               free((yyvsp[0].id));
              }
#line 7667 "ada.tab.c"
    break;

  case 534: /* compilation_unit: context_specification private_opt unit pragma_s  */
#line 3820 "ada.y"
                   {if (strcmp((yyvsp[0].id), "")) fprintf(parsed, ", %s", (yyvsp[0].id));
                    free((yyvsp[0].id));
                    total_line_no = total_line_no + old_lineno;
                    if (debugMode) {
                      fprintf(stdout, "DEBUG: %i new lines with %i lines in total analised\n", old_lineno, total_line_no);
                      fflush(stdout);
                    }
                   }
#line 7680 "ada.tab.c"
    break;

  case 535: /* compilation_unit: private_opt unit pragma_s  */
#line 3829 "ada.y"
                   {if (strcmp((yyvsp[0].id), "")) fprintf(parsed, ",%s", (yyvsp[0].id));
                    free((yyvsp[0].id));
                    total_line_no = total_line_no + old_lineno;
                    if (debugMode) {
                      fprintf(stdout, "DEBUG: %i new lines with %i lines in total analised\n", old_lineno, total_line_no);
                      fflush(stdout);
                    }
                   }
#line 7693 "ada.tab.c"
    break;

  case 540: /* context_specification: context_specification pragma  */
#line 3845 "ada.y"
                                                     {free((yyvsp[0].id));}
#line 7699 "ada.tab.c"
    break;

  case 547: /* file_name: compound_name  */
#line 3862 "ada.y"
            {free((yyvsp[0].id));}
#line 7705 "ada.tab.c"
    break;

  case 550: /* unit: package_declaration  */
#line 3869 "ada.y"
                                {fprintf(parsed, "%s", (yyvsp[0].id));
                                 fflush(parsed);
                                 free((yyvsp[0].id));
                                }
#line 7714 "ada.tab.c"
    break;

  case 551: /* unit: package_body  */
#line 3873 "ada.y"
                                {fprintf(parsed, "%s", (yyvsp[0].id));
                                 fflush(parsed);
                                 free((yyvsp[0].id));
                                }
#line 7723 "ada.tab.c"
    break;

  case 552: /* unit: subprogram_declaration  */
#line 3877 "ada.y"
                                {fprintf(parsed, "%s", (yyvsp[0].id));
                                 fflush(parsed);
                                 free((yyvsp[0].id));
                                }
#line 7732 "ada.tab.c"
    break;

  case 553: /* unit: subprogram_body  */
#line 3881 "ada.y"
                                {fprintf(parsed, "%s", (yyvsp[0].id));
                                 fflush(parsed);
                                 free((yyvsp[0].id));
                                }
#line 7741 "ada.tab.c"
    break;

  case 554: /* unit: subunit  */
#line 3885 "ada.y"
                                {}
#line 7747 "ada.tab.c"
    break;

  case 555: /* unit: generic_declaration  */
#line 3886 "ada.y"
                                {fprintf(parsed, "%s", (yyvsp[0].id));
                                 fflush(parsed);
                                 free((yyvsp[0].id));
                                }
#line 7756 "ada.tab.c"
    break;

  case 556: /* unit: rename_unit  */
#line 3890 "ada.y"
                                {fprintf(parsed, "%s", (yyvsp[0].id));
                                 fflush(parsed);
                                 free((yyvsp[0].id));
                                }
#line 7765 "ada.tab.c"
    break;

  case 557: /* private_type_definition: limited_opt PRIVATE  */
#line 3898 "ada.y"
                           {(yyval.id) = (yyvsp[-1].id);}
#line 7771 "ada.tab.c"
    break;

  case 558: /* overriding_indicator_opt: %empty  */
#line 3902 "ada.y"
                           {(yyval.id) = malloc(8);
                            strcpy((yyval.id), "nothing");
                           }
#line 7779 "ada.tab.c"
    break;

  case 559: /* overriding_indicator_opt: overriding_indicator  */
#line 3906 "ada.y"
                           {(yyval.id) = (yyvsp[0].id);}
#line 7785 "ada.tab.c"
    break;

  case 560: /* overriding_indicator: NOT OVERRIDING  */
#line 3910 "ada.y"
                       {(yyval.id) = malloc(15);
                        strcpy((yyval.id), "not_overriding");
                       }
#line 7793 "ada.tab.c"
    break;

  case 561: /* overriding_indicator: OVERRIDING  */
#line 3914 "ada.y"
                       {(yyval.id) = malloc(11);
                        strcpy((yyval.id), "overriding");
                       }
#line 7801 "ada.tab.c"
    break;

  case 564: /* name_list: name  */
#line 3924 "ada.y"
                                {free((yyvsp[0].id));}
#line 7807 "ada.tab.c"
    break;

  case 565: /* name_list: name_list ',' name  */
#line 3925 "ada.y"
                                {free((yyvsp[0].id));}
#line 7813 "ada.tab.c"
    break;

  case 566: /* body_stub: TASK BODY direct_name IS SEPARATE ';'  */
#line 3929 "ada.y"
                                                            {(yyval.id) = (yyvsp[-3].id);}
#line 7819 "ada.tab.c"
    break;

  case 567: /* body_stub: PACKAGE BODY compound_name IS SEPARATE ';'  */
#line 3930 "ada.y"
                                                            {(yyval.id) = (yyvsp[-3].id);}
#line 7825 "ada.tab.c"
    break;

  case 568: /* body_stub: overriding_indicator_opt subprogram_specification IS SEPARATE ';'  */
#line 3931 "ada.y"
                                                                                        {(yyval.id) = (yyvsp[-3].id_subprogram).xref_subprogram_name; free((yyvsp[-4].id)); free((yyvsp[-3].id_subprogram).id);}
#line 7831 "ada.tab.c"
    break;

  case 569: /* body_stub: PROTECTED BODY direct_name IS SEPARATE ';'  */
#line 3932 "ada.y"
                                                            {(yyval.id) = (yyvsp[-3].id);}
#line 7837 "ada.tab.c"
    break;

  case 570: /* $@25: %empty  */
#line 3936 "ada.y"
          {if (debugMode) {
             fprintf(stdout, "Parsing subunit %s%s\n", current_unit->filename, current_unit->suffix);
             fflush(stdout);
           }
           else fprintf(stdout, ".");   //for progress feedback
          }
#line 7848 "ada.tab.c"
    break;

  case 572: /* proper_body2: subprogram_body  */
#line 3946 "ada.y"
                                        {fprintf(parsed, "match_body(%s_stub, %s)", current_subprogram_name, (yyvsp[0].id));
                                         fflush(parsed);
                                         free((yyvsp[0].id));
                                        }
#line 7857 "ada.tab.c"
    break;

  case 573: /* proper_body2: package_body  */
#line 3950 "ada.y"
                                        {fprintf(parsed, "match_body(%s_stub, %s)", current_package_name, (yyvsp[0].id));
                                         fflush(parsed);
                                         free((yyvsp[0].id));
                                        }
#line 7866 "ada.tab.c"
    break;

  case 576: /* exception_declaration: identifier_list ':' EXCEPTION ';'  */
#line 3961 "ada.y"
                        {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-3].id))+28) );
                         strcpy((yyval.id), "exception_declaration([");
                         strcat((yyval.id), (yyvsp[-3].id));
                         strcat((yyval.id), "])");
                         free((yyvsp[-3].id));
                        }
#line 7877 "ada.tab.c"
    break;

  case 577: /* exception_handler_part: EXCEPTION exception_handler  */
#line 3970 "ada.y"
                         {(yyval.id) = (yyvsp[0].id);}
#line 7883 "ada.tab.c"
    break;

  case 578: /* exception_handler_part: exception_handler_part exception_handler  */
#line 3972 "ada.y"
                         {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id))+3) );
                          strcpy((yyval.id), (yyvsp[-1].id));
                          strcat((yyval.id), ", ");
                          strcat((yyval.id), (yyvsp[0].id));
                          free((yyvsp[-1].id));
                          free((yyvsp[0].id));
                         }
#line 7895 "ada.tab.c"
    break;

  case 579: /* exception_handler: WHEN exception_choice_s RIGHT_SHAFT sequence_of_statements  */
#line 3982 "ada.y"
                    {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id))+33) );
                         strcpy((yyval.id), "exception_handler([");
                         strcat((yyval.id), (yyvsp[-2].id));
                         strcat((yyval.id), "], stmts([");
                         strcat((yyval.id), (yyvsp[0].id));
                         strcat((yyval.id), "]))");
                         free((yyvsp[-2].id));
                         free((yyvsp[0].id));
                    }
#line 7909 "ada.tab.c"
    break;

  case 580: /* exception_handler: WHEN identifier_rule ':' exception_choice_s RIGHT_SHAFT sequence_of_statements  */
#line 3992 "ada.y"
                    {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-4].id))+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id))+34) );
                     strcpy((yyval.id), "exception_handler(");
                     strcat((yyval.id), (yyvsp[-4].id));
                     strcat((yyval.id), ", [");
                         strcat((yyval.id), (yyvsp[-2].id));
                         strcat((yyval.id), "], stmts([");
                         strcat((yyval.id), (yyvsp[0].id));
                         strcat((yyval.id), "]))");
                         free((yyvsp[-4].id));
                         free((yyvsp[-2].id));
                     free((yyvsp[0].id));
                    }
#line 7926 "ada.tab.c"
    break;

  case 581: /* exception_choice_s: exception_choice  */
#line 4006 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 7932 "ada.tab.c"
    break;

  case 582: /* exception_choice_s: exception_choice_s '|' exception_choice  */
#line 4008 "ada.y"
                     {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id))+2) );
                      strcpy((yyval.id), (yyvsp[-2].id));
                      strcat((yyval.id), ", ");
                      strcat((yyval.id), (yyvsp[0].id));
                      free((yyvsp[-2].id));
                      free((yyvsp[0].id));
                     }
#line 7944 "ada.tab.c"
    break;

  case 583: /* exception_choice: name  */
#line 4017 "ada.y"
                                {(yyval.id) = (yyvsp[0].id);}
#line 7950 "ada.tab.c"
    break;

  case 584: /* exception_choice: OTHERS  */
#line 4019 "ada.y"
                   {(yyval.id) = malloc((SAFETY+7) );
                    strcpy((yyval.id), "others");
                   }
#line 7958 "ada.tab.c"
    break;

  case 585: /* raise_statement: RAISE ';'  */
#line 4026 "ada.y"
                  {(yyval.id) = malloc(36);
                   strcpy((yyval.id), "raise_statement(no_name, no_string)");
                  }
#line 7966 "ada.tab.c"
    break;

  case 586: /* raise_statement: RAISE name ';'  */
#line 4030 "ada.y"
                  {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-1].id))+29);
                   strcpy((yyval.id), "raise_statement(");
                   strcat((yyval.id), (yyvsp[-1].id));
                   strcat((yyval.id), ", no_string)");
                   free((yyvsp[-1].id));
                  }
#line 7977 "ada.tab.c"
    break;

  case 587: /* @26: %empty  */
#line 4036 "ada.y"
                                                     {(yyval.true_line_column).line = yylineno; (yyval.true_line_column).column = column+1;}
#line 7983 "ada.tab.c"
    break;

  case 588: /* raise_statement: RAISE name WITH @26 expression_2 ';'  */
#line 4037 "ada.y"
                  {char *expression;
                   build_expression((yyvsp[-1].id_deci), &expression, current_unit, (yyvsp[-2].true_line_column).line, (yyvsp[-2].true_line_column).column);
                   (yyval.id) = malloc(SAFETY+strlen((yyvsp[-4].id))+strlen(expression)+20);
                   strcpy((yyval.id), "raise_statement(");
                   strcat((yyval.id), (yyvsp[-4].id));
                   strcat((yyval.id), ", ");
                   strcat((yyval.id), expression);
                   strcat((yyval.id), ")");
                   free((yyvsp[-4].id));
                   free((yyvsp[-1].id_deci).id);
                  }
#line 7999 "ada.tab.c"
    break;

  case 589: /* requeue_statement: REQUEUE name ';'  */
#line 4051 "ada.y"
                    {(yyval.id) = malloc((SAFETY+18) );
                     strcpy((yyval.id), "requeue_statement");
                    }
#line 8007 "ada.tab.c"
    break;

  case 590: /* requeue_statement: REQUEUE name WITH ABORT ';'  */
#line 4055 "ada.y"
                    {(yyval.id) = malloc((SAFETY+24) );
                     strcpy((yyval.id), "requeue_abort_statement");
                    }
#line 8015 "ada.tab.c"
    break;

  case 591: /* generic_declaration: generic_formal_part subprogram_specification ';'  */
#line 4061 "ada.y"
                      {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[-1].id_subprogram).id)+40) );
                       strcpy((yyval.id), "generic_subprogram_specification([");
                       strcat((yyval.id), (yyvsp[-2].id)),
                       strcat((yyval.id), "], ");
                       strcat((yyval.id), (yyvsp[-1].id_subprogram).id);
                       strcat((yyval.id), "))");        //one ')' to clause the subprogram specification
                       free((yyvsp[-2].id));
                       free((yyvsp[-1].id_subprogram).id);
                       free((yyvsp[-1].id_subprogram).xref_subprogram_name);
                      }
#line 8030 "ada.tab.c"
    break;

  case 592: /* generic_declaration: generic_formal_part package_specification ';'  */
#line 4072 "ada.y"
                      {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[-1].id))+36) );
                       strcpy((yyval.id), "generic_package_specification([");
                       strcat((yyval.id), (yyvsp[-2].id)),
                       strcat((yyval.id), "], ");
                       strcat((yyval.id), (yyvsp[-1].id));
                       strcat((yyval.id), ")");
                       free((yyvsp[-2].id));
                       free((yyvsp[-1].id));
                      }
#line 8044 "ada.tab.c"
    break;

  case 593: /* generic_formal_part: GENERIC  */
#line 4084 "ada.y"
                      {(yyval.id) = malloc(8);
                       strcpy((yyval.id), "generic");
                      }
#line 8052 "ada.tab.c"
    break;

  case 594: /* generic_formal_part: generic_formal_part generic_formal  */
#line 4088 "ada.y"
                      {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id))+3);
                       strcpy((yyval.id), (yyvsp[-1].id));
                       strcat((yyval.id), ", ");
                       strcat((yyval.id), (yyvsp[0].id));
                       free((yyvsp[-1].id));
                       free((yyvsp[0].id));
                      }
#line 8064 "ada.tab.c"
    break;

  case 595: /* generic_formal: pragma  */
#line 4097 "ada.y"
                        {(yyval.id) = (yyvsp[0].id);}
#line 8070 "ada.tab.c"
    break;

  case 596: /* generic_formal: parameter_specification ';'  */
#line 4098 "ada.y"
                                             {(yyval.id) = (yyvsp[-1].id);}
#line 8076 "ada.tab.c"
    break;

  case 597: /* generic_formal: TYPE direct_name generic_discriminant_part_opt IS generic_type_definition ';'  */
#line 4100 "ada.y"
                 {(yyval.id) = malloc(strlen((yyvsp[-4].id))+strlen((yyvsp[-3].id))+strlen((yyvsp[-1].id))+11);
                  strcpy((yyval.id), "type(");
                  strcat((yyval.id), (yyvsp[-4].id));
                  strcat((yyval.id), ", ");
                  strcat((yyval.id), (yyvsp[-3].id));
                  strcat((yyval.id), ", ");
                  strcat((yyval.id), (yyvsp[-1].id));
                  strcat((yyval.id), ")");
                  free((yyvsp[-4].id));
                  free((yyvsp[-3].id));
                  free((yyvsp[-1].id));
                 }
#line 8093 "ada.tab.c"
    break;

  case 598: /* generic_formal: WITH subprogram_specification subp_default ';'  */
#line 4113 "ada.y"
                 {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-2].id_subprogram).id)+strlen((yyvsp[-1].id))+21);
                  strcpy((yyval.id), "with_subprogram(");
                  strcat((yyval.id), (yyvsp[-2].id_subprogram).id);
                  strcat((yyval.id), ")");     //one ')' to clause the subprogram specification)
                  strcat((yyval.id), ", ");
                  strcat((yyval.id), (yyvsp[-1].id));
                  strcat((yyval.id), ")");
                  free((yyvsp[-2].id_subprogram).id);
                  free((yyvsp[-2].id_subprogram).xref_subprogram_name);
                  free((yyvsp[-1].id));
                 }
#line 8109 "ada.tab.c"
    break;

  case 599: /* generic_formal: WITH PACKAGE direct_name IS NEW name '(' BOX ')' ';'  */
#line 4125 "ada.y"
                 {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-7].id))+strlen((yyvsp[-4].id))+22) );
                  strcpy((yyval.id), "with_package(");
                  strcat((yyval.id), (yyvsp[-7].id));
                  strcat((yyval.id), ", ");
                  strcat((yyval.id), (yyvsp[-4].id));
                  strcat((yyval.id), ", box)");
                  free((yyvsp[-7].id));
                  free((yyvsp[-4].id));
                 }
#line 8123 "ada.tab.c"
    break;

  case 600: /* generic_formal: WITH PACKAGE direct_name IS generic_inst ';'  */
#line 4135 "ada.y"
                 {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-3].id))+strlen((yyvsp[-1].id))+17) );
                  strcpy((yyval.id), "with_package(");
                  strcat((yyval.id), (yyvsp[-3].id));
                  strcat((yyval.id), ", ");
                  strcat((yyval.id), (yyvsp[-1].id));
                  strcat((yyval.id), ")");
                  free((yyvsp[-3].id));
                  free((yyvsp[-1].id));
                 }
#line 8137 "ada.tab.c"
    break;

  case 601: /* generic_formal: use_clause  */
#line 4145 "ada.y"
                 {(yyval.id) = malloc(11 );
                  strcpy((yyval.id), "use_clause");
                 }
#line 8145 "ada.tab.c"
    break;

  case 602: /* generic_discriminant_part_opt: %empty  */
#line 4150 "ada.y"
                                                        {(yyval.id) = malloc((SAFETY+16) ); strcpy((yyval.id), "no_discriminant");}
#line 8151 "ada.tab.c"
    break;

  case 603: /* generic_discriminant_part_opt: discriminant_part  */
#line 4151 "ada.y"
                                                        {(yyval.id) = (yyvsp[0].id);}
#line 8157 "ada.tab.c"
    break;

  case 604: /* generic_discriminant_part_opt: '(' BOX ')'  */
#line 4152 "ada.y"
                                                        {(yyval.id) = malloc(4); strcpy((yyval.id), "box");}
#line 8163 "ada.tab.c"
    break;

  case 605: /* subp_default: %empty  */
#line 4155 "ada.y"
                                {(yyval.id) = malloc(11); strcpy((yyval.id), "empty_subp");}
#line 8169 "ada.tab.c"
    break;

  case 606: /* subp_default: IS name  */
#line 4156 "ada.y"
                                {(yyval.id) = (yyvsp[0].id);}
#line 8175 "ada.tab.c"
    break;

  case 607: /* subp_default: IS BOX  */
#line 4157 "ada.y"
                                {(yyval.id) = malloc(4); strcpy((yyval.id), "box");}
#line 8181 "ada.tab.c"
    break;

  case 608: /* subp_default: IS NuLL  */
#line 4158 "ada.y"
                                {(yyval.id) = malloc(5); strcpy((yyval.id), "null");}
#line 8187 "ada.tab.c"
    break;

  case 609: /* subp_default: IS ABSTRACT name  */
#line 4160 "ada.y"
               {(yyval.id) = malloc(SAFETY+strlen((yyvsp[0].id))+11);
                strcpy((yyval.id), "abstract, ");
                strcat((yyval.id), (yyvsp[0].id));
                free((yyvsp[0].id));
               }
#line 8197 "ada.tab.c"
    break;

  case 610: /* subp_default: IS ABSTRACT BOX  */
#line 4166 "ada.y"
               {(yyval.id) = malloc(SAFETY+14);
                strcpy((yyval.id), "abstract, box");
               }
#line 8205 "ada.tab.c"
    break;

  case 611: /* subp_default: IS ABSTRACT NuLL  */
#line 4170 "ada.y"
               {(yyval.id) = malloc(SAFETY+15);
                strcpy((yyval.id), "abstract, null");
               }
#line 8213 "ada.tab.c"
    break;

  case 612: /* generic_type_definition: '(' BOX ')'  */
#line 4175 "ada.y"
                                                {(yyval.id) = malloc((4) ); strcpy((yyval.id), "box");}
#line 8219 "ada.tab.c"
    break;

  case 613: /* generic_type_definition: RANGE BOX  */
#line 4176 "ada.y"
                                                {(yyval.id) = malloc((10) ); strcpy((yyval.id), "range_box");}
#line 8225 "ada.tab.c"
    break;

  case 614: /* generic_type_definition: MOD BOX  */
#line 4177 "ada.y"
                                                {(yyval.id) = malloc(8); strcpy((yyval.id), "mod_box");}
#line 8231 "ada.tab.c"
    break;

  case 615: /* generic_type_definition: DELTA BOX  */
#line 4178 "ada.y"
                                                {(yyval.id) = malloc((10) ); strcpy((yyval.id), "delta_box");}
#line 8237 "ada.tab.c"
    break;

  case 616: /* generic_type_definition: DELTA BOX DIGITS BOX  */
#line 4179 "ada.y"
                                                {(yyval.id) = malloc((17) ); strcpy((yyval.id), "delta_digits_box");}
#line 8243 "ada.tab.c"
    break;

  case 617: /* generic_type_definition: DIGITS BOX  */
#line 4180 "ada.y"
                                                {(yyval.id) = malloc((11) ); strcpy((yyval.id), "digits_box");}
#line 8249 "ada.tab.c"
    break;

  case 618: /* generic_type_definition: array_type_definition  */
#line 4181 "ada.y"
                                                        {(yyval.id) = (yyvsp[0].id);}
#line 8255 "ada.tab.c"
    break;

  case 619: /* generic_type_definition: access_type_definition  */
#line 4182 "ada.y"
                                                        {(yyval.id) = (yyvsp[0].id);}
#line 8261 "ada.tab.c"
    break;

  case 620: /* generic_type_definition: ABSTRACT TAGGED private_type_definition  */
#line 4184 "ada.y"
                          {(yyval.id) = malloc(SAFETY+strlen((yyvsp[0].id))+27);
                           strcpy((yyval.id), "private, abstract_tagged, ");
                           strcat((yyval.id), (yyvsp[0].id));
                           free((yyvsp[0].id));
                          }
#line 8271 "ada.tab.c"
    break;

  case 621: /* generic_type_definition: TAGGED private_type_definition  */
#line 4190 "ada.y"
                          {(yyval.id) = malloc(SAFETY+strlen((yyvsp[0].id))+27);
                           strcpy((yyval.id), "private, tagged, ");
                           strcat((yyval.id), (yyvsp[0].id));
                           free((yyvsp[0].id));
                          }
#line 8281 "ada.tab.c"
    break;

  case 622: /* generic_type_definition: private_type_definition  */
#line 4196 "ada.y"
                          {(yyval.id) = malloc(SAFETY+strlen((yyvsp[0].id))+27);
                           strcpy((yyval.id), "private, not_tagged, ");
                           strcat((yyval.id), (yyvsp[0].id));
                           free((yyvsp[0].id));
                          }
#line 8291 "ada.tab.c"
    break;

  case 623: /* generic_type_definition: derived_type_definition  */
#line 4202 "ada.y"
                          {(yyval.id) = malloc(SAFETY+strlen((yyvsp[0].id))+6);
                           strcpy((yyval.id), "new(");
                           strcat((yyval.id), (yyvsp[0].id));
                           strcat((yyval.id), ")");
                           free((yyvsp[0].id));
                          }
#line 8302 "ada.tab.c"
    break;

  case 624: /* generic_type_definition: ABSTRACT derived_type_definition  */
#line 4209 "ada.y"
                          {(yyval.id) = malloc(SAFETY+strlen((yyvsp[0].id))+14);
                           strcpy((yyval.id), "abstract_new(");
                           strcat((yyval.id), (yyvsp[0].id));
                           strcat((yyval.id), ")");
                           free((yyvsp[0].id));
                          }
#line 8313 "ada.tab.c"
    break;

  case 625: /* generic_type_definition: interface_type_definition  */
#line 4215 "ada.y"
                                                    {(yyval.id) = (yyvsp[0].id);}
#line 8319 "ada.tab.c"
    break;

  case 626: /* generic_subp_inst: subprogram_specification IS generic_inst  */
#line 4222 "ada.y"
                    {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-2].id_subprogram).id)+strlen((yyvsp[0].id))+4);
                     strcpy((yyval.id), (yyvsp[-2].id_subprogram).id);
                     strcat((yyval.id), "), "); //one ')' to clause the subprogram specification
                     strcat((yyval.id), (yyvsp[0].id));
                     free((yyvsp[-2].id_subprogram).id);
                     free((yyvsp[-2].id_subprogram).xref_subprogram_name);
                     free((yyvsp[0].id));
                    }
#line 8332 "ada.tab.c"
    break;

  case 627: /* generic_subp_inst: FUNCTION designator IS generic_inst  */
#line 4232 "ada.y"
                    {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id))+3);
                     strcpy((yyval.id), (yyvsp[-2].id));
                     strcat((yyval.id), ", ");
                     strcat((yyval.id), (yyvsp[0].id));
                     free((yyvsp[-2].id));
                     free((yyvsp[0].id));
                    }
#line 8344 "ada.tab.c"
    break;

  case 628: /* generic_pkg_inst: PACKAGE compound_name IS generic_inst  */
#line 4242 "ada.y"
                   {(yyval.id) = malloc((SAFETY+strlen((yyvsp[-2].id))+strlen((yyvsp[0].id))+34) );
                    strcpy((yyval.id), "generic_package_instantiation(");
                    strcat((yyval.id), (yyvsp[-2].id));
                    strcat((yyval.id), ", ");
                    strcat((yyval.id), (yyvsp[0].id));
                    strcat((yyval.id), ")");
                    free((yyvsp[-2].id));
                    free((yyvsp[0].id));
                   }
#line 8358 "ada.tab.c"
    break;

  case 629: /* generic_inst: NEW name  */
#line 4253 "ada.y"
                        {(yyval.id) = (yyvsp[0].id);}
#line 8364 "ada.tab.c"
    break;

  case 630: /* aspect_clause: enumeration_or_attribute_definition_clause  */
#line 4258 "ada.y"
                                                               {(yyval.id) = (yyvsp[0].id);}
#line 8370 "ada.tab.c"
    break;

  case 631: /* aspect_clause: record_representation_clause  */
#line 4259 "ada.y"
                                                {(yyval.id) = (yyvsp[0].id);}
#line 8376 "ada.tab.c"
    break;

  case 632: /* aspect_clause: at_clause  */
#line 4260 "ada.y"
                                                {(yyval.id) = (yyvsp[0].id);}
#line 8382 "ada.tab.c"
    break;

  case 633: /* @27: %empty  */
#line 4264 "ada.y"
                                                                             {(yyval.true_line_column).line = yylineno; (yyval.true_line_column).column = column+1;}
#line 8388 "ada.tab.c"
    break;

  case 634: /* enumeration_or_attribute_definition_clause: FOR name USE @27 expression_2 ';'  */
#line 4265 "ada.y"
                              {char *expression;
                               build_expression((yyvsp[-1].id_deci), &expression, current_unit, (yyvsp[-2].true_line_column).line, (yyvsp[-2].true_line_column).column);
                               (yyval.id) = malloc(SAFETY+strlen((yyvsp[-4].id))+strlen(expression)+26);
                               strcpy((yyval.id), "representation_clause(");
                               strcat((yyval.id), (yyvsp[-4].id));
                               strcat((yyval.id), ", ");
                               strcat((yyval.id), expression);
                               strcat((yyval.id), ")");
                               free((yyvsp[-4].id));
                               free((yyvsp[-1].id_deci).id);
                              }
#line 8404 "ada.tab.c"
    break;

  case 635: /* record_representation_clause: FOR name USE RECORD mod_clause_opt component_clause END RECORD ';'  */
#line 4279 "ada.y"
                               {(yyval.id) = malloc(SAFETY+strlen((yyvsp[-7].id))+strlen((yyvsp[-4].id))+strlen((yyvsp[-3].id))+37);
                                strcpy((yyval.id), "record_representation_clause(");
                                strcat((yyval.id), (yyvsp[-7].id));
                                strcat((yyval.id), ", ");
                                strcat((yyval.id), (yyvsp[-4].id));
                                strcat((yyval.id), ", [");
                                strcat((yyval.id), (yyvsp[-3].id));
                                strcat((yyval.id), "])");
                                free((yyvsp[-7].id));
                                free((yyvsp[-4].id));
                                free((yyvsp[-3].id));
                              }
#line 8421 "ada.tab.c"
    break;

  case 636: /* mod_clause_opt: %empty  */
#line 4294 "ada.y"
                 {(yyval.id) = malloc(10);
                  strcpy((yyval.id), "empty_mod");
                 }
#line 8429 "ada.tab.c"
    break;

  case 637: /* @28: %empty  */
#line 4297 "ada.y"
                                           {(yyval.true_line_column).line = yylineno; (yyval.true_line_column).column = column+1;}
#line 8435 "ada.tab.c"
    break;

  case 638: /* mod_clause_opt: AT MOD @28 expression_2 ';'  */
#line 4298 "ada.y"
                 {char *expression;
                  build_expression((yyvsp[-1].id_deci), &expression, current_unit, (yyvsp[-2].true_line_column).line, (yyvsp[-2].true_line_column).column);
                  (yyval.id) = malloc(SAFETY+strlen(expression)+1);
                  strcpy((yyval.id), expression);
                  free((yyvsp[-1].id_deci).id);
                 }
#line 8446 "ada.tab.c"
    break;

  case 639: /* component_clause: %empty  */
#line 4306 "ada.y"
                                        {(yyval.id) = malloc(1); strcpy((yyval.id), "");}
#line 8452 "ada.tab.c"
    break;

  case 640: /* component_clause: component_clause_sl  */
#line 4307 "ada.y"
                                        {(yyval.id) = (yyvsp[0].id);}
#line 8458 "ada.tab.c"
    break;

  case 641: /* component_clause_sl: component_clause_item  */
#line 4310 "ada.y"
                                                {(yyval.id) = (yyvsp[0].id);}
#line 8464 "ada.tab.c"
    break;

  case 642: /* component_clause_sl: component_clause_sl component_clause_item  */
#line 4312 "ada.y"
                      {(yyval.id) = malloc(3+strlen((yyvsp[-1].id))+strlen((yyvsp[0].id)));
                       strcpy((yyval.id), (yyvsp[-1].id));
                       strcat((yyval.id), ", ");
                       strcat((yyval.id), (yyvsp[0].id));
                       free((yyvsp[-1].id));
                       free((yyvsp[0].id));
                      }
#line 8476 "ada.tab.c"
    break;

  case 643: /* @29: %empty  */
#line 4321 "ada.y"
                                                   {(yyval.true_line_column).line = yylineno; (yyval.true_line_column).column = column+1;}
#line 8482 "ada.tab.c"
    break;

  case 644: /* component_clause_item: name AT @29 expression_2 RANGE range ';'  */
#line 4322 "ada.y"
                        {char *expression;
                         build_expression((yyvsp[-3].id_deci), &expression, current_unit, (yyvsp[-4].true_line_column).line, (yyvsp[-4].true_line_column).column);
                         (yyval.id) = malloc(13+strlen((yyvsp[-6].id))+strlen(expression)+strlen((yyvsp[-1].id)));
                         strcpy((yyval.id), "clause(");
                         strcat((yyval.id), (yyvsp[-6].id));
                         strcat((yyval.id), ", ");
                         strcat((yyval.id), expression);
                         strcat((yyval.id), ", ");
                         strcat((yyval.id), (yyvsp[-1].id));
                         strcat((yyval.id), ")");
                         free((yyvsp[-6].id));
                         free((yyvsp[-3].id_deci).id);
                         free((yyvsp[-1].id));
                        }
#line 8501 "ada.tab.c"
    break;

  case 645: /* @30: %empty  */
#line 4338 "ada.y"
                                               {(yyval.true_line_column).line = yylineno; (yyval.true_line_column).column = column+1;}
#line 8507 "ada.tab.c"
    break;

  case 646: /* at_clause: FOR name USE AT @30 expression_2 ';'  */
#line 4339 "ada.y"
            {char *expression;
             build_expression((yyvsp[-1].id_deci), &expression, current_unit, (yyvsp[-2].true_line_column).line, (yyvsp[-2].true_line_column).column);
             (yyval.id) = malloc(SAFETY+strlen((yyvsp[-5].id))+strlen(expression)+29);
             strcpy((yyval.id), "at_representation_clause(");
             strcat((yyval.id), (yyvsp[-5].id));
             strcat((yyval.id), ", ");
             strcat((yyval.id), expression);
             strcat((yyval.id), ")");
             free((yyvsp[-5].id));
             free((yyvsp[-1].id_deci).id);
            }
#line 8523 "ada.tab.c"
    break;


#line 8527 "ada.tab.c"

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

#line 4353 "ada.y"

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
