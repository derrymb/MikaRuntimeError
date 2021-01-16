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
/* binary tree of filenames */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

struct binary_filename_node {
  char name[_MAX_PATH];           	/* key : filename name */
  int parsed;				//has been parsed
  struct binary_filename_node *left;
  struct binary_filename_node *right;
};

struct binary_filename_node *create_filename_node(char *name) {
    struct binary_filename_node *r;

  r = (struct binary_filename_node *) malloc (sizeof(struct binary_filename_node));
  if (!r)
    {fprintf(stdout, "Mika ERROR: out of memory.\n");
     my_exit(100);
    }
  strcpy(r->name, name);
  r->parsed = 0;
  r->left = NULL;
  r->right = NULL;
  return r;
}   /* end create_filename_node */

struct binary_filename_node *add_filename(struct binary_filename_node *root, struct binary_filename_node *r, struct binary_filename_node *c){
    int cmp;

  if (!r) {
    r = c;
    if (!root) return r;       /* first entry */
    cmp = strcmp(c->name, root->name);
    if (cmp < 0) 
      root->left = r;
    else if (cmp > 0)
      root->right = r;
    else
      ;  	// nothing : A duplicate
    return r;
  }
  cmp = strcmp(c->name, r->name);
  if (cmp < 0)
    add_filename(r, r->left, c);
  else if (cmp > 0)
    add_filename(r, r->right, c);
  else
    ; /*nothing : A duplicate what should be the returned value?*/

} /* end add_filename */

int search_filename(struct binary_filename_node *root, char *key)
{ int cmp;

  if (!root)
    return 0;
  else {
    cmp = strcmp(key, root->name);
    if (cmp < 0)
      return search_filename(root->left, key);
    else if (cmp > 0)
      return search_filename(root->right, key);
    else
      return 1;
    }
} /* search_filename */


int filename_has_been_parsed(struct binary_filename_node *root, char *key)
{ int cmp;

  if (!root)
    return 0;	//not found
  else {
    cmp = strcmp(key, root->name);
    if (cmp < 0)
      return filename_has_been_parsed(root->left, key);
    else if (cmp > 0)
      return filename_has_been_parsed(root->right, key);
    else {
	  root->parsed = 1;		//found: indicate it has been parsed
	  return 1;
    }
  }
} /* filename_has_been_parsed */


int query_filename_has_been_parsed(struct binary_filename_node *root, char *key)
{ int cmp;

  if (!root)
    return(0);	//not found
  else {
    cmp = strcmp(key, root->name);
    if (cmp < 0)
      return query_filename_has_been_parsed(root->left, key);
    else if (cmp > 0)
      return query_filename_has_been_parsed(root->right, key);
    else {
	  return(root->parsed == 1);		//found: check whether it has been parsed
    }
  }
} /* filename_has_been_parsed */

//to be called once all files in the current queue have been parsed (see ada.l)
/*void add_non_parsed(struct binary_filename_node *root)
{
  if (root) {
    if (root->parsed == 0) {//file not yet parsed : added to the queue of files to be parsed
      char *krunched_filename = malloc(strlen(root->name)+1-4);
      char *extension = malloc(5);  //'.adb' or '.ads'
      int i = 0;
      int j = 0;

      while (root->name[i] != '.') {
        krunched_filename[i] = root->name[i];
        i++;
      }
      krunched_filename[i] = '\0';
      while (root->name[i] != '\0') {
        extension[j] = root->name[i];
        i++;
        j++;
      }
      extension[j] = '\0';
      add_head(krunched_filename, extension, "unkrunched name does not exist");   //add package name at the back of the queue (see queue.c)
      fprintf(stdout, "%s %s\n", krunched_filename, extension);
    }
    add_non_parsed(root->left);
    add_non_parsed(root->right);
  }
  return;
}*/


void print_filename(struct binary_filename_node *root)
{
 if (root) {
        fprintf(stdout, "%s\n", root->name);
        fflush(stdout);
        print_filename(root->left);
        print_filename(root->right);
 }
 return;
} /* print_filename */

/*
void main()
{
struct binary_filename_node *binary_filename_root = NULL;

binary_filename_root = add_filename(binary_filename_root, binary_filename_root, create_filename_node("hello"));
add_filename(binary_filename_root, binary_filename_root, create_filename_node("king"));
 add_filename(binary_filename_root, binary_filename_root, create_filename_node("george"));
 add_filename(binary_filename_root, binary_filename_root, create_filename_node("hurfuwuife"));

if (search_filename(binary_filename_root, "king")) printf("king present\n");
if (search_filename(binary_filename_root, "george")) printf("george present\n");
if (!search_filename(binary_filename_root, "queen")) printf("queen absent\n");
print_filename(binary_filename_root);
}
*/
