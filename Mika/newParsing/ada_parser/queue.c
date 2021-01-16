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
/* queue of strings : add at the tail, remove from the head */
/* doubly linked */
/* possibility to add at the head also */
// used to store the filenames to be processed: we add during binding info processing (foo.bind) and also while parsing separate body declarations (body_stub non-terminal)
// we retrieve during yywrap
// we only add filenames mentionned in foo.xref (otherwise it is definetly dead code)
//   and we add the filename as mentionned in foo.xref (via double_tst DT) as they are the real filenames (could be krunched or not)
// there should be no duplicates
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "queue.h"

#pragma warning( disable : 4996 )  //ignore warnings such as : warning C4996: 'strcpy': This function or variable may be unsafe. Consider using strcpy_s instead. To disable deprecation, use _CRT_SECURE_NO_WARNINGS. See online help for details.
extern void my_exit(int);

struct name_node {
  struct unit_type *unit;
  struct name_node *next, *prev;
};

typedef struct name_node *node_ptr;
static node_ptr head_queue, tail_queue;

void init_queue(){
  head_queue = NULL;
  tail_queue = NULL;
} /* end init_queue */

int queue_isEmpty() {
    if (head_queue == NULL) return 1;
    else return 0;
}

node_ptr create_node(char *name, char *filename, char *suffix, char *path)
{
  node_ptr x = malloc (sizeof *x);
  if (!x) {
      printf("Mika ERROR: out of memory.\n");
      my_exit(61);
  }
  x->unit = malloc (sizeof *(x->unit));
  x->unit->name = malloc((strlen(name)+1)*sizeof(char));
  if (!(x->unit->name))
    {printf("Mika ERROR: out of memory.\n");
     my_exit(64);
    }
  strcpy(x->unit->name, name);
  x->unit->filename = malloc((strlen(filename)+1)*sizeof(char));
  if (!(x->unit->filename))
    {printf("Mika ERROR: out of memory.\n");
     my_exit(64);
    }
  strcpy(x->unit->filename, filename);
  x->unit->suffix = malloc((strlen(suffix)+1)*sizeof(char));
  if (!(x->unit->suffix))
    {printf("Mika ERROR: out of memory.\n");
     my_exit(64);
    }
  strcpy(x->unit->suffix, suffix);
  x->unit->path = malloc((strlen(path)+1)*sizeof(char));
  if (!(x->unit->path))
    {printf("Mika ERROR: out of memory.\n");
     my_exit(64);
    }
  strcpy(x->unit->path, path);
  x->next = NULL;
  x->prev = NULL;
  return x;
} /* end create_node */


void add_tail(char *name, char *filename, char *suffix, char *path)
{
  node_ptr x;
  x = create_node(name, filename, suffix, path);
  if (!tail_queue) {   /* the first element */
    tail_queue = x;
	head_queue = x;
  }
  else {
    tail_queue->next = x;
    x->prev = tail_queue;
    x->next = NULL;
    tail_queue = x;
  }
} /* end add_tail */


void add_head(char *name, char *filename, char *suffix, char *path)
{
  node_ptr x;
  x = create_node(name, filename, suffix, path);
  if (!head_queue) {	/* the first element */
    tail_queue = x;
	head_queue = x;
  }
  else {
    head_queue->prev = x;
    x->next = head_queue;
    x->prev = NULL;
    head_queue = x;
  }
} /* end add_head */

int query_is_in_queue(char *filename, char *suffix)
{
   node_ptr x;
   x = head_queue;
   while (x)
   {if (!strcmp(x->unit->filename, filename) && !strcmp(x->unit->suffix, suffix))
      return(1);
    else x = x->next;
   }
   return(0);  //not found
}

struct unit_type *get_queue()
{
  struct unit_type *tmp;
  node_ptr x;
  if (head_queue) {
    x = head_queue;
    tmp = x->unit;
    head_queue = x->next;
    free(x);
    return(tmp);
  }
  else {
    printf("Mika ERROR: major problem in Q\n");
    my_exit(60);
    return NULL;  //just to return something to keep the compiler happy
  }
} /* end get_queue */


void print_queue()
{
  node_ptr x;
  x = head_queue;
  fprintf(stdout, "\n");
  while (x) {
    fprintf(stdout, "name:%s filename:%s suffix:%s path:%s \n", x->unit->name, x->unit->filename, x->unit->suffix, x->unit->path);
    x = x->next;
  }
} /* end print_queue */