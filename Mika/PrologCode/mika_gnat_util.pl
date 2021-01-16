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
% mika_gnat_util.pl
% some utilities to deal with gnat, in particular to manipulate source file names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%package names in Ada are identifiers (no minus characters) and can be compound (i.e. identifiers separated by '.')
% these are mapped to file names (all lower case and replacing '.' in package names with '-' in file names) except for packages 'a.something', 'g.something', 'i.something' and 's.something' which are mapped to 'a~something', 'g~something', 'i~something' and 's~something'
%thus files names starting with 'a-', 'g-', 'i-', 's-' are part of the RTL
% file name <-> package name
%! but the above only works if the file names are not krunched in which case we cannot retrieve the package name from the file name
% if krunched we only have file name <- package name

%%%
%the file name denotes an RTL compilation unit if file name starts with 'a-', 'g-', 'i-', 's-'
file_name_denotes_a_RTL_compilation_unit(File_name) :-
        ((sub_atom(File_name, 0, _, _, 'a-') ; sub_atom(File_name, 0, _, _, 'g-') ; sub_atom(File_name, 0, _, _, 'i-') ; sub_atom(File_name, 0, _, _, 's-')) ->
                true
        ;
                fail
        ).
%%%
package_name_to_file_name_prefix(Package_name, File_prefix) :-
        ((Package_name == 'a' ; Package_name == 'g' ; Package_name == 'i' ; Package_name == 's') ->
                atom_concat(Package_name, '~', Package_with_prefix)	%to respect gnat's file naming rules
        ;
	        atom_concat(Package_name, '-', Package_with_prefix)
        ),
        atom_codes(Package_with_prefix, Package_with_prefix_codes),
        substitute(46, Package_with_prefix_codes, 45, File_prefix_codes),      %substitute 46 ('.') with 45 ('-')
        atom_codes(File_prefix, File_prefix_codes).
%%%
%convert an a file name to an Ada identifier (e.g. used to create new arguments variables that contain the original file name of the subprogram under test)
file_name_etc_to_ada_var_name(File_name, File_extension, Line, Column, Id, Ada_identifier) :-
        file_name_to_ada_var_name(File_name, New_file_name),
        atom_concat(New_file_name, '_', D1),
	atom_concat(D1, File_extension, D2),
	atom_concat(D2, '_', D3),
	number_codes(Line, L1), atom_codes(Line_atom, L1),
	atom_concat(D3, Line_atom, D4),
	atom_concat(D4, '_', D5),
	number_codes(Column, L2), atom_codes(Column_atom, L2),
	atom_concat(D5, Column_atom, D6),
	atom_concat(D6, '_', D7),
	atom_concat(D7, Id, Ada_identifier).

%%%
        file_name_to_ada_var_name(File_name, Ada_identifier) :-
                atom_codes(File_name, File_name_codes),
                substitute(45, File_name_codes, 95, New_file_name_codes),      %45 is '-' , 95 is '_' (Ada identifiers cannot contain '-'
                atom_codes(Ada_identifier, New_file_name_codes).
%%%
%see issue "Does not work with krunched file names"
file_name_without_extension_to_package_name(File_name, Package_name) :-
        atom_codes(File_name, File_name_codes),
        substitute(45, File_name_codes, 46, New_file_name_codes),      %45 is '-' , 46 is '.'
        atom_codes(Package_name, New_file_name_codes).