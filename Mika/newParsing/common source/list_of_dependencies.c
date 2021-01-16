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
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct dependency_node {
  char *file_name;
  int is_a_subunit;
  struct dependency_node *next;
};

typedef struct dependency_node *dependency_node_ptr;
dependency_node_ptr head_list_dependencies;
dependency_node_ptr tail_list_dependencies;

void init_list_dependencies()
{
  //fprintf(stdout, "init_list_dependencies is called\n");
  //fgetc(stdin);
  head_list_dependencies = NULL;
  tail_list_dependencies = NULL;
}

dependency_node_ptr create_dependency_node(char *file_name)
{
  dependency_node_ptr x = malloc (sizeof *x);
  if (!x)
    {fprintf(stderr, "Ourxref ERROR: out of memory.\n");
     exit(61);
    }
  x->file_name = malloc((strlen(file_name)+1)*sizeof(char));
  if (!(x->file_name))
    {fprintf(stderr, "Ourxref ERROR: out of memory.\n");
     exit(64);
    }
  strcpy(x->file_name, file_name);
  x->next = NULL;
  return x;
}

void add_dependency(char *file_name, int is_a_subunit)
{
  dependency_node_ptr x;
  x = create_dependency_node(file_name);
  x->is_a_subunit = is_a_subunit;
  if (!x) {
      fprintf(stderr, "Ourxref ERROR in add_dependency: returned dependency node is NULL for file %s\n", file_name);
      exit(61);
  }
  if (!head_list_dependencies) {   /* the first element */
	head_list_dependencies = x;
    tail_list_dependencies = x;
  }
  else {
    tail_list_dependencies->next = x;
    tail_list_dependencies = x;
  }
}

dependency_node_ptr retrieve_dependency_file_name(int index)
{
  int i;
  dependency_node_ptr x;
  if (!head_list_dependencies) {
      fprintf(stderr, "Ourxref ERROR in retrieve_dependency_file_name: head_list_dependencies is NULL for index %i\n", index);
      fflush(stderr);
      exit(61);
  }
  x = head_list_dependencies;
  for (i = 1; i < index; i++) {
    x = x->next;
    if (!x) {
      fprintf(stderr, "Ourxref ERROR: index number %i not found in list of dependencies.\n", index);
      exit(61);
    }
  }
  return x;
}

void print_queue()
{
  int index = 1;
  dependency_node_ptr x;
  x = head_list_dependencies;
  fprintf(stderr, "DEPENDENCIES LIST FOLLOWS\n");
  while (x) {
    fprintf(stderr, "dependendency no %i is file %s \n", index, x->file_name);
    x = x->next;
    index++;
  }
  fflush(stderr);
}
