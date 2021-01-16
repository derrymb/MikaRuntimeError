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
#include "malloc.h"
#include "string.h"
//e.g. from F:\bck_Mika\examples\ch9 to F:/bck_Mika/examples/ch9/
void transform_path_to_prolog(char *path, char **result)
{
  char ch = path[0];
  int i = 0;
  *result = (char *)malloc(strlen(path) + 2);
  while (ch != '\0') {
    if (ch == '\\') (*result)[i] = '/';
    else (*result)[i] = path[i];
    i++;
	ch = path[i];
  }
  (*result)[i] = '/';
  (*result)[i+1] = '\0';
}//transform_path_to_prolog