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
#pragma warning( disable : 4996 ) // ignore warnings such as : warning C4996: 'strcpy': This function or variable may be unsafe.
#include <stdio.h>
#include <string.h>
char path[_MAX_PATH];

char *find_path(char *name, char *extension, char *kind, char *gnatlsExe, char *user_gnatproject_call_str, char *user_gnatls_call_str, int debugMode)    {
  FILE *tmp_stream;
  char tmp_s[_MAX_PATH*10];        /* temporary string holder */
  char read_tmp[_MAX_PATH];
  char *tmp_char;

    strcpy_s(tmp_s, _MAX_PATH, gnatlsExe);
    strcat(tmp_s, " ");
    strcat(tmp_s, user_gnatproject_call_str);
    strcat(tmp_s, " ");
    strcat(tmp_s, user_gnatls_call_str);
    strcat(tmp_s, kind);
    strcat(tmp_s, " ");
    strcat(tmp_s, name);
    strcat(tmp_s, " > mika_tmp.txt");
    if (debugMode) {
      fprintf(stderr, "Mika DEBUG: call to gnat ls: %s\n", tmp_s);
      fflush(stderr);
    }
    if (system(tmp_s) != 0 ) {            //gnat ls called
      fprintf(stderr, "Mika ERROR: call to gnat ls (to see if unit exists) failed: %s\n", tmp_s);
      fflush(stderr);
      my_exit(10);
    }
    tmp_stream = fopen("mika_tmp.txt", "r");
    fgets(read_tmp, 256, tmp_stream);
    fclose(tmp_stream);     //we should delete mika_tmp.txt
    read_tmp[strlen(read_tmp)-1] = '\0';
    if (strstr(read_tmp, "Can't find ")) {
      return NULL;
    }
    while (tmp_char = strchr(read_tmp, '/')) {
      *tmp_char = '\\';
    }
    if (!strstr(read_tmp, ":")) {
      _getcwd(path, 256);   //we add the cwd in front if the full path is not given
      strcat(path, "\\");
      strcat(path, read_tmp);
    }
    else
      strcpy(path, read_tmp);
    if (tmp_char = strrchr(path, '\\')) //last occurence
        *tmp_char = '\0';
    else {
      fprintf(stderr, "Mika ERROR: Something is wrong in the path (%s) found for the unit (%s) \n", path, name);
      fflush(stderr);
      my_exit(10);
    }
    if (debugMode) {
      fprintf(stderr, "Mika DEBUG: full path to unit %s is : %s\n", name, path);
      fflush(stderr);
    }
    strcpy(tmp_s, path);
    strcat(tmp_s, "\\");
    strcat(tmp_s, name);
    strcat(tmp_s, extension);
    tmp_stream = fopen(tmp_s, "r");
    if (!tmp_stream) {
      return NULL;
    }
    fclose(tmp_stream);
    if (debugMode) {
      fprintf(stderr, "Mika DEBUG: full path to %s%s is : %s\n", name, extension, path);
      fflush(stderr);
    }
    return path;
}