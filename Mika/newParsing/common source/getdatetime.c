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
#include <time.h>                   //for time and date retrieval

//generate a pointer to a string formating the current date and time in format yy_mm_dd_hh_mm_ss
char *getdatetime()
{
    struct tm *newtime;
    time_t long_time;
    char *datetime_str = malloc(50*sizeof(char));
    char tmp_str[10];

    time( &long_time );                /* Get time as long integer. */
    newtime = localtime( &long_time ); /* Convert to local time. */
        itoa((newtime->tm_year)+1900, tmp_str, 10);
        strcpy(datetime_str, tmp_str);
        strcat(datetime_str, "_");
        itoa((newtime->tm_mon)+1, tmp_str, 10);
        if ((newtime->tm_mon)+1 < 10) strcat(datetime_str, "0");
        strcat(datetime_str, tmp_str);
        strcat(datetime_str, "_");
        itoa(newtime->tm_mday, tmp_str, 10);
        if (newtime->tm_mday < 10) strcat(datetime_str, "0");
        strcat(datetime_str, tmp_str);
        strcat(datetime_str, "_");
        itoa(newtime->tm_hour, tmp_str, 10);
        if (newtime->tm_hour < 10) strcat(datetime_str, "0");
        strcat(datetime_str, tmp_str);
        strcat(datetime_str, "_");
        itoa(newtime->tm_min, tmp_str, 10);
        if (newtime->tm_min < 10) strcat(datetime_str, "0");
        strcat(datetime_str, tmp_str);
        strcat(datetime_str, "_");
        itoa(newtime->tm_sec, tmp_str, 10);
        if (newtime->tm_sec < 10) strcat(datetime_str, "0");
        strcat(datetime_str, tmp_str);
        return datetime_str;
} //getdatetime function