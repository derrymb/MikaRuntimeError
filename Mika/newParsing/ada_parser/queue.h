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
#ifndef QUEUE_HEADER
#define QUEUE_HEADER
struct unit_type {
    char* name;         //e.g. system-address_operations
    char* filename;     //e.g. a-excpol
    char* suffix;       //e.g. ".ads" or ".adb"
    char* path;         //e.g. F:/GNAT/2009/lib/gcc/i686-pc-mingw32/4.3.4/adalib
};
typedef struct name_node* node_ptr;
void init_queue();
int queue_isEmpty();
void add_tail(char* name, char* filename, char* suffix, char* path);
void add_head(char* name, char* filename, char* suffix, char* path);
struct unit_type* get_queue();
void print_queue();
#endif