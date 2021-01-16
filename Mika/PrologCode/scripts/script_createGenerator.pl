%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                 Copyright 2020 Dr Christophe Meudec 
%%                                     <http://www.echancrure.eu/>
%% This file is part of Mika.
%% Mika is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by
%%   the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
%% Mika is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
%%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
%% You should have received a copy of the GNU General Public License along with Mika.  If not, see <https://www.gnu.org/licenses/>.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%creates generator.exe : our Ada test inputs generator
:- compile(['../mika_main']).   %consult is for debugging

%runtime_entry(start) is called when the Sicstus application starts
user:runtime_entry(start) :-
   %prolog_flag(argv, Flags), format("flags:~q\n", [Flags]),  %for debugging purposes
   prolog_flag(argv, [Install_dir, Parsed_dir, Target_source_file_name, Target_raw_subprogram_name, Received_line_no, Driver_source_file_name, Driver_subprogram_name, Strategy, Check_coverage, Elaboration, CreateTimeStampedDirectory, Debug_mode, Mika_dg, Coverage_thoroughness]),
   %calls our Ada test inputs generator (from mika_main.pl)
   from_command_line(Install_dir, Parsed_dir, Target_source_file_name, Target_raw_subprogram_name, Received_line_no, Driver_source_file_name, Driver_subprogram_name, Strategy, Check_coverage, Elaboration, CreateTimeStampedDirectory, Debug_mode, Mika_dg, Coverage_thoroughness).

:- consult(['script_createPrologExecutable_util']).
:- createPrologExecutable('generator', Exit_code),       %sets up and calls spld sicstus' "all in one executable builder"
   halt(Exit_code).