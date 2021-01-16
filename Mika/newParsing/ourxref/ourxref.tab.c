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
#line 18 "ourxref.y"

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


#line 115 "ourxref.tab.c"

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

#include "ourxref.tab.h"
/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_RUBBISH = 3,                    /* RUBBISH  */
  YYSYMBOL_DLETTER = 4,                    /* DLETTER  */
  YYSYMBOL_XLETTER = 5,                    /* XLETTER  */
  YYSYMBOL_ASTERISK = 6,                   /* ASTERISK  */
  YYSYMBOL_RENAME_REF = 7,                 /* RENAME_REF  */
  YYSYMBOL_SQUARE_BRACKETED = 8,           /* SQUARE_BRACKETED  */
  YYSYMBOL_BRACKETED = 9,                  /* BRACKETED  */
  YYSYMBOL_CURLY_BRACKETED = 10,           /* CURLY_BRACKETED  */
  YYSYMBOL_ANGLE_BRACKETED = 11,           /* ANGLE_BRACKETED  */
  YYSYMBOL_SOURCE_NAME = 12,               /* SOURCE_NAME  */
  YYSYMBOL_FILENO = 13,                    /* FILENO  */
  YYSYMBOL_START_LINE_CHAR_COL = 14,       /* START_LINE_CHAR_COL  */
  YYSYMBOL_LINE_CHAR_COL = 15,             /* LINE_CHAR_COL  */
  YYSYMBOL_SUBUNIT = 16,                   /* SUBUNIT  */
  YYSYMBOL_ENTITY = 17,                    /* ENTITY  */
  YYSYMBOL_INTEGER = 18,                   /* INTEGER  */
  YYSYMBOL_YYACCEPT = 19,                  /* $accept  */
  YYSYMBOL_goal_symbol = 20,               /* goal_symbol  */
  YYSYMBOL_ali_file_list = 21,             /* ali_file_list  */
  YYSYMBOL_22_1 = 22,                      /* $@1  */
  YYSYMBOL_ali_file = 23,                  /* ali_file  */
  YYSYMBOL_24_2 = 24,                      /* $@2  */
  YYSYMBOL_dependency_list = 25,           /* dependency_list  */
  YYSYMBOL_dependency = 26,                /* dependency  */
  YYSYMBOL_hexa = 27,                      /* hexa  */
  YYSYMBOL_subunit_opt = 28,               /* subunit_opt  */
  YYSYMBOL_cross_reference_list = 29,      /* cross_reference_list  */
  YYSYMBOL_cross_reference = 30,           /* cross_reference  */
  YYSYMBOL_31_3 = 31,                      /* $@3  */
  YYSYMBOL_entity_reference_list = 32,     /* entity_reference_list  */
  YYSYMBOL_33_4 = 33,                      /* $@4  */
  YYSYMBOL_entity_reference = 34,          /* entity_reference  */
  YYSYMBOL_35_5 = 35,                      /* $@5  */
  YYSYMBOL_reference_list = 36,            /* reference_list  */
  YYSYMBOL_reference = 37,                 /* reference  */
  YYSYMBOL_level_opt = 38,                 /* level_opt  */
  YYSYMBOL_rename_opt = 39,                /* rename_opt  */
  YYSYMBOL_instance_opt = 40,              /* instance_opt  */
  YYSYMBOL_brackets_list_opt = 41,         /* brackets_list_opt  */
  YYSYMBOL_bracket = 42                    /* bracket  */
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
typedef yytype_int8 yy_state_t;

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
#define YYLAST   31

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  19
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  24
/* YYNRULES -- Number of rules.  */
#define YYNRULES  40
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  53

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   273


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
      15,    16,    17,    18
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,    94,    94,    97,    99,    98,   105,   104,   117,   118,
     121,   134,   135,   136,   137,   141,   142,   147,   148,   152,
     151,   163,   164,   164,   168,   167,   181,   182,   185,   197,
     209,   210,   213,   214,   217,   218,   220,   221,   224,   225,
     226
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
  "\"end of file\"", "error", "\"invalid token\"", "RUBBISH", "DLETTER",
  "XLETTER", "ASTERISK", "RENAME_REF", "SQUARE_BRACKETED", "BRACKETED",
  "CURLY_BRACKETED", "ANGLE_BRACKETED", "SOURCE_NAME", "FILENO",
  "START_LINE_CHAR_COL", "LINE_CHAR_COL", "SUBUNIT", "ENTITY", "INTEGER",
  "$accept", "goal_symbol", "ali_file_list", "$@1", "ali_file", "$@2",
  "dependency_list", "dependency", "hexa", "subunit_opt",
  "cross_reference_list", "cross_reference", "$@3",
  "entity_reference_list", "$@4", "entity_reference", "$@5",
  "reference_list", "reference", "level_opt", "rename_opt", "instance_opt",
  "brackets_list_opt", "bracket", YY_NULLPTR
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
     265,   266,   267,   268,   269,   270,   271,   272,   273
};
#endif

#define YYPACT_NINF (-44)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-23)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int8 yypact[] =
{
       4,    -2,     9,     7,   -44,     4,   -44,    -5,   -44,     4,
      11,   -44,    -3,   -44,    -1,    11,   -44,   -44,   -44,   -44,
     -44,     2,     8,   -44,   -44,   -44,   -44,     5,    15,    10,
     -44,   -44,     6,     5,   -44,   -44,    18,   -44,    14,   -44,
     -44,    -8,   -44,   -44,   -44,    -9,   -44,    12,    14,   -44,
      14,   -44,   -44
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int8 yydefact[] =
{
       0,     0,     0,     2,     3,     6,     8,     0,     1,     0,
       0,     9,     0,     5,     0,     7,    17,    14,    12,    13,
      11,    15,     0,    18,    16,    10,    19,     0,    30,    20,
      21,    31,     0,     0,    24,    23,    32,    33,    34,    35,
      36,    26,    38,    39,    40,    25,    37,     0,    34,    27,
      34,    29,    28
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int8 yypgoto[] =
{
     -44,   -44,   -44,   -44,    17,   -44,   -44,    23,   -44,   -44,
     -44,    16,   -44,   -44,   -44,    -4,   -44,   -44,   -44,   -44,
     -44,   -43,   -44,   -44
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int8 yydefgoto[] =
{
      -1,     2,     3,     9,     4,    10,     5,     6,    21,    25,
      15,    16,    27,    29,    33,    30,    36,    45,    49,    32,
      38,    40,    41,    46
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int8 yytable[] =
{
      17,    42,    43,    44,    47,    51,    48,    52,     1,     8,
       7,    -4,    18,    12,    19,    20,    14,    22,    24,    28,
      26,    31,    39,    34,   -22,    37,    13,    50,    11,    35,
       0,    23
};

static const yytype_int8 yycheck[] =
{
       3,     9,    10,    11,    13,    48,    15,    50,     4,     0,
      12,     4,    15,    18,    17,    18,     5,    18,    16,    14,
      12,     6,     8,    17,    14,     7,     9,    15,     5,    33,
      -1,    15
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int8 yystos[] =
{
       0,     4,    20,    21,    23,    25,    26,    12,     0,    22,
      24,    26,    18,    23,     5,    29,    30,     3,    15,    17,
      18,    27,    18,    30,    16,    28,    12,    31,    14,    32,
      34,     6,    38,    33,    17,    34,    35,     7,    39,     8,
      40,    41,     9,    10,    11,    36,    42,    13,    15,    37,
      15,    40,    40
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int8 yyr1[] =
{
       0,    19,    20,    21,    22,    21,    24,    23,    25,    25,
      26,    27,    27,    27,    27,    28,    28,    29,    29,    31,
      30,    32,    33,    32,    35,    34,    36,    36,    37,    37,
      38,    38,    39,    39,    40,    40,    41,    41,    42,    42,
      42
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     1,     1,     0,     3,     0,     3,     1,     2,
       5,     1,     1,     1,     1,     0,     1,     1,     2,     0,
       5,     1,     0,     3,     0,     8,     0,     2,     3,     2,
       0,     1,     0,     1,     0,     1,     0,     2,     1,     1,
       1
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
  case 4: /* $@1: %empty  */
#line 99 "ourxref.y"
                {init_list_dependencies();  //data structure to keep track of declared dependencies in foo.ali
                }
#line 1174 "ourxref.tab.c"
    break;

  case 6: /* $@2: %empty  */
#line 105 "ourxref.y"
           { if (debugMode) {
               print_queue();
               //fgetc(stdin);
             }
           }
#line 1184 "ourxref.tab.c"
    break;

  case 7: /* ali_file: dependency_list $@2 cross_reference_list  */
#line 111 "ourxref.y"
           { if (debugMode) {
               print_ali_queue();
             }
           }
#line 1193 "ourxref.tab.c"
    break;

  case 10: /* dependency: DLETTER SOURCE_NAME INTEGER hexa subunit_opt  */
#line 122 "ourxref.y"
             {if ((yyvsp[0].id) == NULL) {
                 if (debugMode) fprintf(stderr, "\nRead Dependency to file: %s", (yyvsp[-3].id));
                 add_dependency((yyvsp[-3].id), 0);
               }
               else {
                 if (debugMode) fprintf(stderr, "\n%s is sub-unit\n", (yyvsp[-3].id));
                 add_dependency((yyvsp[-3].id), 1);
               }
             }
#line 1207 "ourxref.tab.c"
    break;

  case 15: /* subunit_opt: %empty  */
#line 141 "ourxref.y"
              {(yyval.id) = NULL;}
#line 1213 "ourxref.tab.c"
    break;

  case 16: /* subunit_opt: SUBUNIT  */
#line 143 "ourxref.y"
              {(yyval.id) = (yyvsp[0].id);
              }
#line 1220 "ourxref.tab.c"
    break;

  case 19: /* $@3: %empty  */
#line 152 "ourxref.y"
                  {dependency_node_ptr x;
                   if (debugMode) fprintf(stderr, "\nAnalysis number : %s for %s\n", (yyvsp[-1].id), (yyvsp[0].id));
                   strcpy(current_referenced_file, (yyvsp[0].id));
                   strcpy(original_referenced_file, (yyvsp[0].id));
                   x = retrieve_dependency_file_name(atoi((yyvsp[-1].id)));
                   if (debugMode) fprintf(stderr, "Retrieved reference to file %s subunit: %i\n", x->file_name, x->is_a_subunit);
                   if (!x->is_a_subunit) add_ali(get_ali(x->file_name));
                  }
#line 1233 "ourxref.tab.c"
    break;

  case 22: /* $@4: %empty  */
#line 164 "ourxref.y"
                                              {strcpy(current_referenced_file, original_referenced_file);}
#line 1239 "ourxref.tab.c"
    break;

  case 24: /* $@5: %empty  */
#line 168 "ourxref.y"
                   {fprintf(stdout, "\n%s ", (yyvsp[0].id));
                    if ((yyvsp[-2].line_col).letter == 'U') fprintf(stdout, "procedure\n");
                    else if ((yyvsp[-2].line_col).letter == 'V') fprintf(stdout, "function\n");
                    else fprintf(stdout, " \n");
                    fprintf(stdout, "  Decl:  %s    %s:%s", current_referenced_file, (yyvsp[-2].line_col).line, (yyvsp[-2].line_col).column);
                    fflush(stdout);
                    first_ref = 1;
                    new_file = 1;
                   }
#line 1253 "ourxref.tab.c"
    break;

  case 25: /* entity_reference: START_LINE_CHAR_COL level_opt ENTITY $@5 rename_opt instance_opt brackets_list_opt reference_list  */
#line 178 "ourxref.y"
                   {;}
#line 1259 "ourxref.tab.c"
    break;

  case 28: /* reference: FILENO LINE_CHAR_COL instance_opt  */
#line 186 "ourxref.y"
            {if (first_ref)
               fprintf(stdout, "\n  Ref: ");
             else
               fprintf(stdout, "\n        ");
             fflush(stdout);
             first_ref = 0;
             new_file = 0;
             strcpy(current_referenced_file, retrieve_dependency_file_name(atoi((yyvsp[-2].id)))->file_name);
             fprintf(stdout, "  %s    %s:%s", current_referenced_file, (yyvsp[-1].line_col).line, (yyvsp[-1].line_col).column);
             fflush(stdout);
            }
#line 1275 "ourxref.tab.c"
    break;

  case 29: /* reference: LINE_CHAR_COL instance_opt  */
#line 198 "ourxref.y"
            {if (first_ref)
               fprintf(stdout, "\n  Ref:  %s", current_referenced_file);
             else
               fprintf(stdout, "  ");
             fflush(stdout);
             first_ref = 0;
             fprintf(stdout, "  %s:%s", (yyvsp[-1].line_col).line, (yyvsp[-1].line_col).column);
             fflush(stdout);
            }
#line 1289 "ourxref.tab.c"
    break;

  case 38: /* bracket: BRACKETED  */
#line 224 "ourxref.y"
                            {free((yyvsp[0].id));}
#line 1295 "ourxref.tab.c"
    break;

  case 39: /* bracket: CURLY_BRACKETED  */
#line 225 "ourxref.y"
                            {free((yyvsp[0].id));}
#line 1301 "ourxref.tab.c"
    break;

  case 40: /* bracket: ANGLE_BRACKETED  */
#line 226 "ourxref.y"
                            {free((yyvsp[0].id));}
#line 1307 "ourxref.tab.c"
    break;


#line 1311 "ourxref.tab.c"

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

#line 228 "ourxref.y"

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
