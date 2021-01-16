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

struct ali_node {
  char *ali_path;
  struct ali_node *next;
};

typedef struct ali_node *ali_node_ptr;
static ali_node_ptr head_list_alies;
static ali_node_ptr tail_list_alies;
static ali_node_ptr current_list_alies;

void init_list_alies()
{
  head_list_alies = NULL;
  tail_list_alies = NULL;
  current_list_alies = NULL;
}

ali_node_ptr create_ali_node(char *path)
{
  ali_node_ptr x = malloc (sizeof *x);
  if (!x)
    {fprintf(stderr, "Ourxref ERROR: out of memory.\n");
     exit(616);
    }
  x->ali_path = malloc((strlen(path)+1)*sizeof(char));
  if (!(x->ali_path))
    {fprintf(stderr, "Ourxref ERROR: out of memory.\n");
     exit(64);
    }
  strcpy(x->ali_path, path);
  x->next = NULL;
  return x;
}

int absent(char *path)
{
  ali_node_ptr x = head_list_alies;
  while (x) {
    if (!strcmp(x->ali_path, path))
      return 0;     // is present
    x = x->next;
  }
  return 1;     // is absent
}

void add_ali(char *path) //add to the tail
{
  ali_node_ptr x;
  if (absent(path)) {
    x = create_ali_node(path);
    if (!head_list_alies) {   /* the first element */
	  head_list_alies = x;
      tail_list_alies = x;
      current_list_alies = x;
    }
    else {
      if (!current_list_alies) current_list_alies = x;
      tail_list_alies->next = x;
      tail_list_alies = x;
    }
  }
}

char *get_next_ali()
{
  ali_node_ptr x = current_list_alies;
  if (!current_list_alies) return NULL;
  else {
    current_list_alies = current_list_alies->next;
    return (x->ali_path);
  }
}

void print_ali_queue()
{
  ali_node_ptr x;
  x = head_list_alies;
  fprintf(stderr, "\nALI LIST FOLLOWS\n");
  while (x) {
    if (x==current_list_alies) fprintf(stderr, "CURRENT ->:");
    fprintf(stderr, "ali file: %s\n", x->ali_path);
    x = x->next;
  }
}
