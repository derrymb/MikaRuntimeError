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
//09/03/2010
//used to keep track of the operators that are renamed but for which no user defined operator is defined and hence will use the predefined operator instead
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct operator_node {
  char *operator_reference;
  struct operator_node *next;
};

typedef struct operator_node *operator_node_ptr;
static operator_node_ptr head_list_operators;
static operator_node_ptr current_list_operators;

void init_list_operators()
{
  head_list_operators = NULL;
  current_list_operators = NULL;
}

operator_node_ptr create_operator_node(char *operator_reference)
{
  operator_node_ptr x = malloc (sizeof *x);
  if (!x)
    {fprintf(stderr, "Parser ERROR: out of memory.\n");
     exit(616);
    }
  x->operator_reference = malloc((strlen(operator_reference)+1)*sizeof(char));
  if (!(x->operator_reference))
    {fprintf(stderr, "Parser ERROR: out of memory.\n");
     exit(64);
    }
  strcpy(x->operator_reference, operator_reference);
  x->next = NULL;
  return x;
}

int absent_operator(char *operator_reference)
{
  operator_node_ptr x = head_list_operators;
  while (x) {
    if (!strcmp(x->operator_reference, operator_reference))
      return 0;     // is present
    x = x->next;
  }
  return 1;     // is absent
}

void add_operator(char *operator_reference) //add to the tail
{
  operator_node_ptr x;
  if (absent_operator(operator_reference)) {
    x = create_operator_node(operator_reference);
    if (!head_list_operators) {   /* the first element */
	  head_list_operators = x;
      current_list_operators = x;
    }
    else {
      if (!current_list_operators) current_list_operators = x;
      current_list_operators->next = x;
      current_list_operators = x;
    }
  }
}

void print_operators_queue()
{
  operator_node_ptr x;
  x = head_list_operators;
  fprintf(stdout, "OPERATOR LIST FOLLOWS\n");
  while (x) {
    fprintf(stdout, "operator: %s\n", x->operator_reference);
    x = x->next;
  }
}