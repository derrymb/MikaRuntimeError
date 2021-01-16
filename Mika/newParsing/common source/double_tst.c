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
//See "Algorithms in C 3rd ed - parts 1-4" by robert Sedgewick pp.639
//Implements a Double Ternary Search Trie
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

extern int is_standard; //from ada.y
extern int debugMode;

int first = 1;
int unique = 0;             //used to generate unique numbering
struct decl_item{
  char *xref_id;            // key : file_name_line_column_identifier e.g. a.ads:10:12:speed
  char *unique_id;          // e.g. Speed_12
};

struct ref_item{
  struct decl_item *the_decl;	/* the unique declaration */
};

typedef struct decl_node* decl_link;
struct decl_node{
  struct decl_item* item;
  int d;		// character
  decl_link l, m, r;	// left, middle, right
};

typedef struct ref_node* ref_link;
struct ref_node{
  struct ref_item* item;
  int d;		// character
  ref_link l, m, r;	// left, middle, right
};

typedef struct match* next_match;
struct match{
  char *unique_id;
  next_match next;
};

//list of unxrefed entities and their matching unique_ids : initially filled in during unxrefed_insert_start
//matching unique_ids completed in decl_print
typedef struct matches* next_matches;
struct matches{
  char *unxrefed_entity;    //the unxrefed entity
  next_match matching;      //the list of matching unique_ids
  next_matches next;
};

static next_matches matches_head = NULL;

static decl_link decl_head; //double TST for declarations
static ref_link ref_head;   //double TST for everything else
static decl_link unxrefed_head; //double TST for un-crossreferenced variables (should not exist but does for dead code and some gnat run time libraries variables)

decl_link decl_inserted;
decl_link unxrefed_inserted;
ref_link ref_inserted;

void decl_init_start()
{
  decl_head = NULL;
}

void unxrefed_init_start()
{
  unxrefed_head = NULL;
}

void ref_init_start()
{
  ref_head = NULL;
}

decl_link decl_create(int d)
{
  decl_link x = malloc (sizeof *x);
  if (!x)
    {printf("Mika ERROR: out of memory.\n");
     my_exit(54);
    }
  x->item = NULL;
  x->d = d;
  x->l = NULL;
  x->m = NULL;
  x->r = NULL;
  return x;
} /* end decl_create */

decl_link unxrefed_create(int d)
{
  decl_link x = malloc (sizeof *x);
  if (!x)
    {printf("Mika ERROR: out of memory.\n");
     my_exit(55);
    }
  x->item = NULL;
  x->d = d;
  x->l = NULL;
  x->m = NULL;
  x->r = NULL;
  return x;
} /* end unxrefed_create */

ref_link ref_create(int d)
{
  ref_link x = malloc (sizeof *x);
  if (!x)
    {printf("Mika ERROR: out of memory.\n");
     my_exit(56);
    }
  x->item = NULL;
  x->d = d;
  x->l = NULL;
  x->m = NULL;
  x->r = NULL;
  return x;
} /* end ref_create */

struct decl_item *decl_search(decl_link h, char *v, int w)
{
  int i = v[w];

  if ( h == NULL ) return NULL;
  if ( i == '\0') return h->item;
  if (i < h->d) return decl_search(h->l, v, w);
  if (i == h->d) return decl_search(h->m, v, w+1);
  if (i > h->d) return decl_search(h->r, v, w);
  fprintf(stdout, "Mika ERROR: Major problem in DTST\n");
  my_exit(50);
  return NULL;  //just to return something to keep the compiler happy
}
struct decl_item *unxrefed_search(decl_link h, char *v, int w)
{
  int i = v[w];

  if ( h == NULL ) return NULL;
  if ( i == '\0') return h->item;
  if (i < h->d) return unxrefed_search(h->l, v, w);
  if (i == h->d) return unxrefed_search(h->m, v, w+1);
  if (i > h->d) return unxrefed_search(h->r, v, w);
  fprintf(stdout, "Mika ERROR: Major problem in DTST\n");
  my_exit(51);
  return NULL;  //just to return something to keep the compiler happy
}
struct decl_item *ref_search(ref_link h, char *v, int w)
{
  int i = v[w];

  if ( h == NULL ) return NULL;
  if ( i == '\0') {
    if (h->item) return h->item->the_decl;
    else return NULL;
  }
  if (i < h->d) return ref_search(h->l, v, w);
  if (i == h->d) return ref_search(h->m, v, w+1);
  if (i > h->d) return ref_search(h->r, v, w);
  fprintf(stdout, "Mika ERROR: Major problem in DTST\n");
  my_exit(52);
  return NULL;  //just to return something to keep the compiler happy
}

struct decl_item *decl_search_start(char *key)
{
  return decl_search(decl_head, key, 0);
}

struct decl_item *unxrefed_search_start(char *key)
{
  return unxrefed_search(unxrefed_head, key, 0);
}

struct decl_item *ref_search_start(char *key)
{
  return ref_search(ref_head, key, 0);
}

//v is the key e.g. "a.ads:10:12:speed"
//double declaration allowed in gnat version gcc 3.4.1
decl_link decl_insert(decl_link h, char *v, int w)
{
  int i = v[w];

  if (h == NULL) h = decl_create(i);
  if (i == '\0') {
	  if (h->item) {    //i.e. has already been declared: that's fine
        decl_inserted = h;
        return h;       //the link returns contains non null
	  }
	  else {
		  decl_inserted = h;
		  return h;
	  }
  }
  if (i < h->d) h->l = decl_insert(h->l, v, w);
  if (i == h->d) h->m = decl_insert(h->m, v, w+1);
  if (i > h->d) h->r = decl_insert(h->r, v, w);
  return h;
}

decl_link unxrefed_insert(decl_link h, char *v, int w)
{
  int i = v[w];

  if (h == NULL) h = unxrefed_create(i);
  if (i == '\0') {
	  if (h->item) {
            unxrefed_inserted = h;
            return h;     //the link returns contains non null
	  }
	  else {
		  unxrefed_inserted = h;
		  return h;
	  }
  }
  if (i < h->d) h->l = unxrefed_insert(h->l, v, w);
  if (i == h->d) h->m = unxrefed_insert(h->m, v, w+1);
  if (i > h->d) h->r = unxrefed_insert(h->r, v, w);
  return h;
}

ref_link ref_insert(ref_link h, char *v, int w)
{
  int i = v[w];

  if (h == NULL) h = ref_create(i);
  if (i == '\0') {
	ref_inserted = h;
	return h;
  }
  if (i < h->d) h->l = ref_insert(h->l, v, w);
  if (i == h->d) h->m = ref_insert(h->m, v, w+1);
  if (i > h->d) h->r = ref_insert(h->r, v, w);
  return h;
}

//e.g. decl_insert_start("a.ads:10:12:speed", "speed")
//with gnat from gcc 3.4.1 the same xref_id can be entered twice (see folder problem double body)
struct decl_item *decl_insert_start(char *xref_id, char *identifier)
{
  char tmp_str[32];
  struct decl_item *ptr;
  int len_identifier;

  decl_head = decl_insert(decl_head, xref_id, 0);  //decl_inserted is now the place holder for the item
  if (!decl_inserted->item) { //NULL: i.e. 1st declaration
    ptr = malloc (sizeof *(ptr));
    decl_inserted->item = ptr;    //the place holder in the decl TST is updated to hold decl_item
    if (!ptr) {
      printf("Mika ERROR: out of memory.\n");
      my_exit(57);
    }
    // filling in the decl_item(xref_id, unique_id)
    ptr->xref_id = xref_id;

    itoa(unique++, tmp_str, 10);          //10 is the radix
    len_identifier = strlen(identifier);
    //because strings (see 17/09/2004) are converted to a list of ASCII codes the identifiers can be very long
    // to prevent them getting longer to what Prolog can sustain we limit the length of the unique_ids to 33 characters or so
    if (len_identifier>32 && !strncmp(identifier, "string_", 7)) { //we are probably (could be an identifier starting with 'string_') dealing with a string
        len_identifier = 33;    //only 33 characters will be kept for the unique_id: longer identifiers will be truncated
    }
	ptr->unique_id = malloc((len_identifier+strlen(tmp_str)+2)*sizeof(char));
    if (!(ptr->unique_id)) {
      printf("Mika ERROR: out of memory.\n");
      my_exit(58);
    }
    strncpy(ptr->unique_id, identifier, len_identifier);
    ptr->unique_id[len_identifier] = '\0';  //necessary because not automatic
    strcat(ptr->unique_id, "_");
    strcat(ptr->unique_id, tmp_str);
    ptr->unique_id[0] -= ('a' - 'A');     //put first letter into uppercasereturn ptr;
    return ptr;
  }
  else return decl_inserted->item;       //the declaration was already made: we return the original declaration (with the same unique_id)
} //decl_insert_start function


struct decl_item *unxrefed_insert_start(char *xref_id, char *identifier)
{
  char tmp_str[32];
  struct decl_item *ptr;
  int len_identifier;
  next_matches match_entry;

  unxrefed_head = unxrefed_insert(unxrefed_head, xref_id, 0);  //unxrefed_inserted is now the place holder for the item
  if (!unxrefed_inserted->item) { //NULL: i.e. 1st declaration
    ptr = malloc (sizeof *(ptr));
    unxrefed_inserted->item = ptr;    //the place holder in the decl TST is updated to hold decl_item
    if (!ptr) {
      printf("Mika ERROR: out of memory.\n");
      my_exit(59);
    }

    // filling in the decl_item(xref_id, unique_id)
    ptr->xref_id = xref_id;
    //fprintf(stdout, "INSET %s\n", xref_id);
    itoa(unique++, tmp_str, 10);          //10 is the radix
    //because strings (see 17/09//2004) are converted to a list of ASCII codes the identifiers can be very long
    // to prevent them getting longer to what Prolog can sustain we limit the length of the unique_ids to 33 characters or so
    len_identifier = strlen(identifier);
    if (len_identifier>32) {
        len_identifier = 33;    //only 33 characters will be kept for the unique_id: longer identifiers will be truncated
    }
    ptr->unique_id = malloc((len_identifier+strlen(tmp_str)+2)*sizeof(char));
    if (!(ptr->unique_id)) {
      printf("Mika ERROR: out of memory.\n");
      my_exit(62);
    }
    if (len_identifier>32) {
        strncpy(ptr->unique_id, identifier, 33);
        ptr->unique_id[33] = '\0';  //necessary because not automatic since 33 is less or equal then strlen(identifier)
    }
    else {
        strcpy(ptr->unique_id, identifier); //the full identifier will be used
    }
    strcat(ptr->unique_id, "_");
    strcat(ptr->unique_id, tmp_str);
    ptr->unique_id[0] -= ('a' - 'A');     //put first letter into uppercasereturn ptr;

    //adding to the list of matches in preparation for collecting potential matches during decl_print
    match_entry = malloc(sizeof *(match_entry));
    match_entry->unxrefed_entity = malloc(strlen(ptr->unique_id)+1);
    match_entry->matching = NULL;
    strcpy(match_entry->unxrefed_entity, ptr->unique_id);
    match_entry->next = matches_head;
    matches_head = match_entry;

    return ptr;
    }
  else return unxrefed_inserted->item;       //the declaration was already made: we return the original declaration (with the same unique_id)
} //unxrefed_insert_start function

void ref_insert_start(char *xref_id, struct decl_item *the_decl)
{
  struct ref_item *ptr;

  ref_head = ref_insert(ref_head, xref_id, 0);
  if (ref_inserted->item) { //i.e. has already been inserted
    ;
  }
  else {
    ptr = malloc (sizeof *(ptr));
    ref_inserted->item = ptr;
    if (!ptr) {
      printf("Mika ERROR: out of memory.\n");
      my_exit(63);
    }
    ptr->the_decl = the_decl;   // filling in the item
  }
}

void match(next_matches start_match, char *unique_id)
{
  int i = 0;
  int len1, len2;
  if (start_match == NULL) return;
  len1 = strlen(start_match->unxrefed_entity) - strlen(strrchr(start_match->unxrefed_entity, '_'));
  len2 = strlen(unique_id) - strlen(strrchr(unique_id, '_'));
  if (len1 == len2) {
    while ((start_match->unxrefed_entity[i] == unique_id[i]) && i < len1)
      i++;
    if (i == len1) { //we have a match
      next_match new_match_entry;
      new_match_entry = malloc(sizeof(*new_match_entry));
      new_match_entry->unique_id = malloc(strlen(unique_id)+1);
      strcpy(new_match_entry->unique_id, unique_id);
      new_match_entry->next = start_match->matching;
      start_match->matching = new_match_entry;
    }
  }
  match(start_match->next, unique_id);   //we check other unxrefed entities
}

void decl_print(decl_link h)
{
 next_matches start_match = matches_head;
 if (h == NULL) return;
 if (h->item) {
  if (first) {
    fprintf(parsed, "a(%s, '%s')", h->item->unique_id, h->item->xref_id);
    first = 0;
  }
  else fprintf(parsed, ",\na(%s, '%s')", h->item->unique_id, h->item->xref_id);
  match(start_match, h->item->unique_id);
 }
 decl_print(h->l);
 decl_print(h->m);
 decl_print(h->r);
}


//05-03-08
//printing all unproperly xrefed entities last
//format : unxrefed(unique_is, xref_id, [list of unique_id that matches just the entities names])
//very ineficient because the TST data structure is optimised for first character search ...
void unxrefed_print(decl_link h)
{
 if (h == NULL) return;
 if (h->item) {
   next_matches start_match = matches_head;
   next_match start_list;
   fprintf(parsed, ",\nunxrefed(%s, '%s', [", h->item->unique_id, h->item->xref_id);
   while (start_match && strcmp(start_match->unxrefed_entity, h->item->unique_id)) {
     start_match = start_match->next;
   }
   if (start_match) {
     start_list = start_match->matching;
     if (start_list) {
       fprintf(parsed, "%s", start_list->unique_id);
       start_list = start_list->next;
       while (start_list) {
         fprintf(parsed, ", %s", start_list->unique_id);
         start_list = start_list->next;
       }
     }
     else ; //the list is empty : no match found for this id will be a problem if used during execution
   }
   else {
     fprintf(stdout, "Mika ERROR: Major problem in DTST\n");
     my_exit(53);
   }
   fprintf(parsed, "])");     //to close the list
 }
 unxrefed_print(h->l);
 unxrefed_print(h->m);
 unxrefed_print(h->r);
}

void ref_print(ref_link h)
{
 if (h == NULL) return;
 if (h->item) printf("ref %s is declared as %s\n", h->item->the_decl->xref_id, h->item->the_decl->unique_id);
 ref_print(h->l);
 ref_print(h->m);
 ref_print(h->r);
}

void decl_print_start()
{
  decl_print(decl_head);
}

void unxrefed_print_start()
{
  unxrefed_print(unxrefed_head);
}

void ref_print_start()
{
  ref_print(ref_head);
}

void test_driver(void) {
	struct decl_item *current;
    #define bufferSize 80
	char s1[bufferSize+1], s2[bufferSize + 1];
	parsed = stdout;
	decl_init_start();
	do {
		printf("Enter a unique id: ");
		gets_s(s1, bufferSize);
		printf("Enter its suffix: ");
		gets_s(s2, bufferSize);
		if (s1[0] != '\0') current = decl_insert_start(s1, s2);
	} while(*s1);
	decl_print_start();
}

/*
void main(void)
{
  struct decl_item *current;
  char s1[80];
  char s2[80];

  unxrefed_init_start();
  do {
   printf("Enter a unique id: ");
   gets(s1);
   printf("Enter its suffix: ");
   gets(s2);
   if (s1[0] != '\0') current = unxrefed_insert_start(s1, s2);
  } while(*s1);
  do {
   printf("Search ");
   gets(s1);
   current = unxrefed_search_start(s1);
   if (!current) printf("not found\n");
   else printf("%s with unique %s\n", current->xref_id, current->unique_id);
  } while(*s1);
  unxrefed_print_start();
}

*/