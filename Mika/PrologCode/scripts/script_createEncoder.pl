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
%creates encoder.exe : our Prolog encoder which transforms the source.pl input file containing our generated Ada Prolog terms
% into a compiled Prolog Object file
:- compile(['../midoan_solver_dosyntax']).      %our specific syntax for Source.pl containing our generated Ada Prolog terms

%runtime_entry(start) is called when the Sicstus application starts
user:runtime_entry(start) :-
        prolog_flag(argv, [Source, Target]),    %get the command line arguments
        compile([Source]),
        save_files(Source, Target).             %that's what encoder.exe does : it compiles its input file into .po file of Prolog objects

:- consult(['script_createPrologExecutable_util']).
:- createPrologExecutable('encoder', Exit_code),           %sets up and calls spld sicstus' "all in one executable builder"
   halt(Exit_code).