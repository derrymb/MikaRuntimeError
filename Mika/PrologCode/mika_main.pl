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
% mika_main.pl
% front end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%useful during debugging: the determinacy checker can be used to detect unwanted non-determinism
%:- load_files(library(detcheck),
%              [when(compile_time), if(changed)]).
:- set_prolog_flag('agc_margin', 1000000).
:- use_module(['mika_symbolic', 'common_util', 'mika_globals']).

:- compile('mika_main_portray').        %display of variable debug information; useful during tracing
:- compile('mika_gnat_util').           %some utilities to deal with gnat, in particular to manipulate source file names

:- use_module([	library('lists'),       %provides memberchk/2 predicate
		library('system'),	%provides now/1, datime/1 and file_exists/2 predicate
		library('random')	%provides setrand/1 predicate
	     ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
initialise_globals(Install_dir, Strategy, Debug_mode) :-
        mika_globals:mika_globals__set_NBT('errorMessageNb', 0),	        %number of error messages generated: used to set trace_points in debug mode only
        mika_globals:mika_globals__set_NBT('debug_info', 'pre_elaboration'),    %for debug only
        mika_globals:mika_globals__set_NBT('phase', 'elaboration'),             %initially we are in the elaboration phase: used during cfg building and elaboration control
        mika_globals:mika_globals__init_BT_path('current_path_bran', []),
	mika_globals:mika_globals__init_BT_path('current_path_deci', []),
        mika_globals:mika_globals__init_BT_path('current_path_mcdc_gate', []),
	mika_globals:mika_globals__init_BT_path('current_path_cond', []),
	mika_globals:mika_globals__set_BT('call_stack_bran', []),
	mika_globals:mika_globals__set_BT('call_stack_deci', []),
	mika_globals:mika_globals__set_BT('call_stack_cond', []),
        mika_globals:mika_globals__set_NBT('covered_bran', []),
        mika_globals:mika_globals__set_NBT('covered_deci', []),
	mika_globals:mika_globals__set_NBT('covered_cond', []),
        mika_globals:mika_globals__set_NBT('to_cover', []),
        mika_globals:mika_globals__set_NBT('path_nb', 0),
        mika_globals:mika_globals__set_NBT('test_driver_test_nb', 0),
	mika_globals:mika_globals__set_NBT('abandoned_path_nb', 0),	        %counts the number of timed out and unsuccessful labeling tests
        mika_globals:mika_globals__set_NBT('strategy', Strategy),              %one of branch|decision|condition|mcdc
        mika_globals:mika_globals__set_NBT('install_dir', Install_dir),        %the install dir of the generator executable
        mika_globals:mika_globals__set_NBT('debug_mode', Debug_mode),          %debug or release
        mika_globals:mika_globals__set_NBT('message_mode', Debug_mode),        %debug or release
        mika_globals:mika_globals__set_NBT('coverage_thoroughness', 'subprogram_only'),      %default is subprogram only
        mika_globals:mika_globals__set_NBT('driver', 'no_driver'),      %used to indicate that we are in a driver mode and also to retrive the target subprogram xrefed name
        mika_globals:mika_globals__set_NBT('test_points', []),          %the list of Test Points
        !.
%%%
%only to be used during debuging : not to be used for stand alone executable because of the trace
go_trace(Install_dir, Parsed_dir, Target_source_file_name, Target_raw_subprogram_name, Received_line_no, Driver, Strategy, Check_coverage,
   Elaboration, CreateTimeStampedDirectory, Debug_mode, Mika_dg, Coverage_thoroughness) :-
        leash(['call', 'fail', 'exception', 'redo']),                           %skips the exit port during debugging
        set_prolog_flag('debugger_print_options', [portrayed(true)]),     %change print_depth in mika_main_portray.pl if you need to
        set_prolog_flag('toplevel_print_options', [portrayed(true), numbervars(true)]),    %not sure if this useful
        trace,  %within go_trace only
        go(Install_dir, Parsed_dir, Target_source_file_name, Target_raw_subprogram_name, Received_line_no, Driver, Strategy, Check_coverage,
   Elaboration, CreateTimeStampedDirectory, Debug_mode, Mika_dg, Coverage_thoroughness).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%e.g.go_trace('F:/bck_mika/bin/', 'F:/bck_mika/working directory/bck_Mika/examples/stvr/stvr_mika/', stvr, calculate_date_difference, '_', branch, no, ignored, no, debug, mika_dg_m, 'R').
from_command_line(Install_dir, Parsed_dir, Target_source_file_name, Target_raw_subprogram_name, Received_line_no, 'no_driver', 'no_driver', Strategy, Check_coverage,
   Elaboration, CreateTimeStampedDirectory, Debug_mode, Mika_dg, Coverage_thoroughness) :-
   !,
   go(Install_dir, Parsed_dir, Target_source_file_name, Target_raw_subprogram_name, Received_line_no, 'no_driver', Strategy, Check_coverage,
   Elaboration, CreateTimeStampedDirectory, Debug_mode, Mika_dg, Coverage_thoroughness).
from_command_line(Install_dir, Parsed_dir, Target_source_file_name, Target_raw_subprogram_name, Received_line_no, Driver_source_file_name, Driver_subprogram_name, Strategy, Check_coverage,
   Elaboration, CreateTimeStampedDirectory, Debug_mode, Mika_dg, Coverage_thoroughness) :-
   !,
   go(Install_dir, Parsed_dir, Target_source_file_name, Target_raw_subprogram_name, Received_line_no, driver(Driver_source_file_name, Driver_subprogram_name), Strategy, Check_coverage,
   Elaboration, CreateTimeStampedDirectory, Debug_mode, Mika_dg, Coverage_thoroughness).
%%%
%e.g. go_trace('E:\\Google Drive\\Mika\\bin\\', 'E:\\Google Drive\\Mika\\examples\\ch7\\ch7_1_mika\\', 'ch7_1', 'if_check', '_', no_driver, branch, no, ignored, no, debug, no, 'R').
%e.g. go_trace('F:/bck_mika/bin/', 'F:/bck_mika/working directory/bck_Mika/examples/search/searchRU_mika/', 'searchru', 'search', '_', no_driver, decision, no, ignored, no, debug, no, 'S').
go(Install_dir, Parsed_dir, Target_source_file_name, Target_raw_subprogram_name, Received_line_no, Driver, Strategy, Check_coverage,
   Elaboration, CreateTimeStampedDirectory, Debug_mode, Mika_dg, Coverage_thoroughness) :-
        initialise_globals(Install_dir, Strategy, Debug_mode),		%all globals are initialised
        (Driver == 'no_driver' ->       %normal test inputs generation : nothing to do
                Source_file_name_to_load = Target_source_file_name
        ;
         Driver = driver(Driver_source_file_name, _Driver_subprogram_name) ->
                Source_file_name_to_load = Driver_source_file_name
        ;
                common_util:common_util__error(10, "generator's Driver parameter has a wrong syntax: it should be 'no_driver' or Driver(_, _)", 'no_error_consequences', [('driver', Driver)], 101111, 'mika_main', 'go', 'no_localisation', 'no_extra_info')
        ),
        user:file_name_without_extension_to_package_name(Target_source_file_name, Target_package_name),       %the package name or unit name comes from the source file name : see issue "Does not work with krunched file names". Package names are only used for test points
        set_coverage_thoroughness(Coverage_thoroughness),               %set the NBT global Coverage_thoroughness (e.g. generate tests to cover the current subprogram only, or everything except the RTL etc.)
        (Mika_dg == 'no' ->
                true
        ;
         atom_concat('mika_dg_', Debug_atom, Mika_dg) ->        %debugging backdoor: we can only pass atoms on the command line because of the use of prolog_flag(argv, [...]) in script_creat_generator.pl : if more complex information is necessary (that requires actual compound terms) then a separate file that is read could be used
                (atom_codes(Debug_atom, Debug_codes),           %g for no gc, m for debug message mode (in front end put mika_dg_m in 'additional gnatmake test driver switches' textbox)
                 parse_mika_dg(Debug_codes)
                )
        ;
                common_util:common_util__error(1, "mika_dg's syntax is wrong and ignored: contact Midoan", 'no_error_consequences', [('mika_dg', Mika_dg)], 17924, 'mika_main', 'go', 'no_localisation', 'no_extra_info')
        ),
        transform_line_no_to_number(Received_line_no, Line_no),
        common_util:common_util__error(1, "MIKA : Consulting the source file", 'no_error_consequences', 'no_arguments', 1214103, 'mika_main', 'go', 'no_localisation', 'no_extra_info'),
        %consulting the large input file
        %if there are syntax errors in the input file (i.e. non Prolog format), Sicstus is likely to run out of memory without indicating the origin of the problem.
        % you can use ECLiPSe to debug the input file (do not forget to compile midoan_solver_dosyntax.pl first)
        % ECLiPse will highlight the origin of the error but is likely to be very slow and even crash when the file is actually syntax compliant (even on line-command mode)
        atom_concat(Parsed_dir, Source_file_name_to_load, Parsed_dir_and_file_name),
        (Debug_mode == 'debug' ->
                (atom_concat(Parsed_dir_and_file_name, '.pl', Input_file),
                 (file_exists(Input_file) ->
                        consult(Input_file)
                 ;
                        common_util:common_util__error(10, "Input parsed file does not exist", 'no_error_consequences', [('input_file', Input_file)], 10101140, 'mika_main', 'go', 'no_localisation', "Ensure Parsed_dir and Source_file_name are correct")
                 )
                )
        ;
                (atom_concat(Parsed_dir_and_file_name, '.po', Input_file),
                 (file_exists(Input_file) ->
                        load_files(Input_file)
                 ;
                        common_util:common_util__error(10, "Input object parsed file does not exist", 'no_error_consequences', [('input_file', Input_file)], 10109140, 'mika_main', 'go', 'no_localisation', "Ensure Parsed_dir and Source_file_name are correct")
                 )
                )
        ),
        user:orig_dir(Orig_dir),                %the directory where the original Ada code was (it is embedded in the parsed file): use for various purposes
        mika_globals:mika_globals__set_NBT('orig_dir', Orig_dir),              %the golbal is used to retrieve the original test points
        set_is_subprogram_only_unit(Orig_dir, Target_source_file_name),
        set_main_test_point_name(Target_package_name),

        seed,   %seed the random generator

        atom_concat(Install_dir, 'attr.mika', Attr_mika_file),
        (file_exists(Attr_mika_file) ->
                consult([Attr_mika_file])                         %contains gnat's implementation defined attributes (such as address_size)
        ;
                common_util:common_util__error(10, "'attr.mika' file does not exist", 'no_error_consequences', [('attr_mika_file', Attr_mika_file)], 1012951, 'mika_main', 'go', 'no_localisation', "Invalid Install_dir ?; Incorrect installation?")
        ),
        mika_globals:mika_globals__set_NBT('context', Elaboration),                %not_ignored or ignored
        mika_symbolic:mika_symbolic__initialise_context(Elaboration),             %used during symbolic execution: at the moment Elaboration is only 'ignored' or 'not_ignored': could become more finely grained in the future see 'context' issue in 'doc_issues.odt'
        normalise_subprogram_name(Target_raw_subprogram_name, Target_subprogram_name),        %e.g. from '*' to op_42
        datime(datime(Year, Month, Day, Hour, Min, Sec)),	                %from library(system)
        set_directory(CreateTimeStampedDirectory, datime(Year, Month, Day, Hour, Min, Sec), Target_subprogram_name, Line_no, Parsed_dir, New_dir),
        atom_concat('mika_', Target_subprogram_name, D1),
        atom_concat(D1, '_tests.txt', Result_file),
        atom_concat(D1, '.debug', Debug_file),
        atom_concat(D1, '_driver', Subprogram_driver_name),
	atom_concat(Subprogram_driver_name, '.adb', Driver_file),
	(stream_property(_, alias('answer_output')) ->	%to detect if our answer_output stream is already open (useful during debugging)
		close('answer_output')
   	;
        	true
   	),
	(stream_property(_, alias('debug_output')) ->	%to detect if our debug_output stream is already open (useful during debugging)
		close('debug_output')
   	;
        	true
   	),
        open(Result_file, 'write', _, [alias('answer_output')]),	%mika_<Target_subprogram_name>_tests.txt  stream
	(Debug_mode == 'debug' ->
                (open(Debug_file, 'write', _, [alias('debug_output')]),	%foo.debug stream
                 print_preamble('debug_output', datime(Year, Month, Day, Hour, Min, Sec), Orig_dir, Target_package_name, Target_subprogram_name, Strategy, Elaboration, New_dir, Driver)
                )
        ;
                true
        ),
        print_preamble('answer_output', datime(Year, Month, Day, Hour, Min, Sec), Orig_dir, Target_package_name, Target_subprogram_name, Strategy, Elaboration, New_dir, Driver),

        %where it all happens
	mika_symbolic:mika_symbolic__analyse(Input_file, Target_source_file_name, Target_package_name, Target_subprogram_name, Line_no, Driver, Strategy, Actual_coverage),
        (stream_property(_, alias('dummy_mika_test_point')) ->	%to detect if our dummy_mika_test_point stream was created
		close('dummy_mika_test_point')
   	;
        	true
   	),
        (Debug_mode == 'debug' ->
                (mika_coverage:util_print_debug,		%debugging print the cfgs and the overall branches, decisions and conditions
                 mika_globals:mika_globals__get_NBT('to_cover', To_cover),
                 format('debug_output', "to_cover :~w\n", [To_cover]),
                 (Strategy == 'mcdc' ->
                        (mika_globals:mika_globals__get_NBT('overall_mcdc_deci', All_mcdc_deci),
                         format('debug_output', "Overall MCDC Decisions:~w\n", [All_mcdc_deci])
                        )
                 ;
                        true
                 ),
                 close('debug_output')
                )
        ;
                true
        ),
        mika_globals:mika_globals__get_NBT('path_nb', Nb_tests),
        (Nb_tests == 0 ->
                true
        ;
                (print_test_driver(Driver_file, datime(Year, Month, Day, Hour, Min, Sec), Subprogram_driver_name, Orig_dir, Target_package_name, Target_subprogram_name, Strategy, Elaboration, New_dir, Driver),
                 print_init_driver_ads(Nb_tests),       %we have to wait to have the number of tests generated before writing in this file
                 print_init_driver_adb                  %we have to wait to know all the necessary minor test points (accessible via : mika_globals:mika_globals__get_NBT(test_points, _)) before writing in this file
                )
        ),
	%checking coverage information if it exists
	(Check_coverage == 'yes' ->
		(atom_concat(Orig_dir, Source_file_name_to_load, Coverage_file_without_ext),
		 atom_concat(Coverage_file_without_ext, '.cov', Coverage_file),
		 (file_exists(Coverage_file) ->
			(%coverage information needs to be checked
			 open(Coverage_file, 'read', _, [alias('coverage_input')]),
			 read('coverage_input', cov(CovL)),
                         close('coverage_input'),
			 Look_for =.. [Strategy, Nb_tests_expected, Expected_coverage],
			 (search_coverage_info(CovL, Target_subprogram_name, Line_no, Look_for) ->	%look for coverage information in file foo.cov
				((Nb_tests == Nb_tests_expected ->
					common_util:common_util__error(1, "Number of tests generated identical : OK", 'no_error_consequences', 'no_arguments', 1165142, 'main', 'go', 'no_localisation', 'no_extra_info')
				 ;
					common_util:common_util__error(10, "Actual number of tests generated is different than expected", "The coverage file might be wrong or Mika is wrong", [('actual_nb_tests', Nb_tests), ('expected_nb_test', Nb_tests_expected)], 1014819, 'mika_main', 'go', 'no_localisation', 'no_extra_info')
                                 ),
				 (Actual_coverage == Expected_coverage ->
					common_util:common_util__error(1, "Percentage covered identical : OK", 'no_error_consequences', 'no_arguments', 1170135, 'main', 'go', 'no_localisation', 'no_extra_info')    %we check both the number of tests generated and the percentage coverage : better chance of detecting that the file has changed
				 ;
					common_util:common_util__error(10, "Actual coverage generated is different than expected", "The coverage file might be wrong or Mika is wrong", [('actual_coverage', Actual_coverage), ('expected_coverage', Expected_coverage)], 10173256, 'mika_main', 'go', 'no_localisation', 'no_extra_info')
				 )
				)
			 ;
				common_util:common_util__error(10, "Necessary coverage information not found", "Cannot perform coverage check: coverage files needs to be updated", [('strategy', Strategy), ('target_subprogram_name', Target_subprogram_name), ('line_nb', Line_no)], 10177218, 'mika_main', 'go', 'no_localisation', 'no_extra_info')
			 )
			)
		 ;
			common_util:common_util__error(10, "Coverage check wanted but coverage file does not exist", "Cannot perform coverage check: create coverage file", [('coverage_file', Coverage_file)], 10181200, 'mika_main', 'go', 'no_localisation', 'no_extra_info')
		 )
		)
	;
		true
	),
        common_util:common_util__error(1, "Mika Generator : test inputs generation successful", 'no_error_consequences', 'no_arguments', 1194444, 'mika_main', 'go', 'no_localisation', 'no_extra_info').
go(_Install_dir, _Parsed_dir, _Target_source_file_name, _Target_raw_subprogram_name, _Received_line_no, _Driver, _Strategy, _Check_coverage,
   _Elaboration, _CreateTimeStampedDirectory, _Debug_mode, _Mika_dg, _Coverage_thoroughness) :-
        common_util:common_util__error(10, "General error in generator", 'no_error_consequences', 'no_arguments', 109999, 'generator', 'runtime_entry', 'no_localisation', 'no_extra_info').
%%%
set_coverage_thoroughness(Coverage_thoroughness) :-
        (Coverage_thoroughness == 'S' ->
                mika_globals:mika_globals__set_NBT('coverage_thoroughness', 'subprogram_only')
        ;
         Coverage_thoroughness == 'F' ->
                mika_globals:mika_globals__set_NBT('coverage_thoroughness', 'package_call_tree')
        ;
         Coverage_thoroughness == 'A' ->
                mika_globals:mika_globals__set_NBT('coverage_thoroughness', 'entire_call_tree')
        ;
         Coverage_thoroughness == 'R' ->
                mika_globals:mika_globals__set_NBT('coverage_thoroughness', 'everything_including_rtl')
        ;
                common_util:common_util__error(1, "Coverage thoroughness syntax is wrong and ignored: contact Midoan", 'no_error_consequences', [('coverage_thoroughness', Coverage_thoroughness)], 122553, 'mika_main', 'go', 'no_localisation', 'no_extra_info')
        ).
%%%
parse_mika_dg([]).
parse_mika_dg([Next|Rest]) :-
        atom_codes(Atom, [Next]),
        (Atom == 'g' ->
                (set_prolog_flag('gc', 'off'),      %to use in case of segmentation fault due to Sicstus bug
                 common_util:common_util__error(1, "mika_dg: nogc effective", 'no_error_consequences', 'no_arguments', 17925, 'mika_main', 'parse_mika_dg', 'no_localisation', 'no_extra_info')
                )
        ;
         Atom == 'm' ->
                (mika_globals:mika_globals__set_NBT('message_mode', 'debug'),
                 common_util:common_util__error(1, "mika_dg: message mode is debug", 'no_error_consequences', 'no_arguments', 17925, 'mika_main', 'parse_mika_dg', 'no_localisation', 'no_extra_info')
                )
        ;
                common_util:common_util__error(1, "mika_dg: unknown switch is ignored", 'no_error_consequences', [('switch', Atom)], 17925, 'mika_main', 'parse_mika_dg', 'no_localisation', 'no_extra_info')
        ),
        parse_mika_dg(Rest).
%%%
transform_line_no_to_number(Received_line_no, Line_no) :-
        (number(Received_line_no) ->
                Line_no = Received_line_no
        ;
                (Received_line_no == '_' ->
                        Line_no = _
                ;
                        (atom_codes(Received_line_no, Codes),
                         number_codes(Line_no, Codes)
                        )
                )
        ).
%%%
create_dummy_mika_test_point :-
        open('dummy_mika_test_point.txt', 'write', _, [alias('dummy_mika_test_point')]),	%mika_packagefoo_test_point.adb
        print_copyright('dummy_mika_test_point'),
        format('dummy_mika_test_point', "--fake test point for RTL variables: necessary because we cannot set test points for RTL libraries\n", []),
        format('dummy_mika_test_point', "--this should be empty if not, ignoring the elaboration of the subprogram under test should help\n", []),
        format('dummy_mika_test_point', "--if not, send its contents to Midoan.\n", []).
%%%
%seeding the random generator to ensure non deterministic behaviour : good for release mode
%in debug mode : harder to see time efficiency improvements due to variations; but then again some bugs may be dependent on path followed : non-deterministic would cover more possibilities
%26/04/2010 : at the moment seeding is not random but deterministic : good for regression;
%             in release mode we should offer the choice as a switch because for debugging : it would be good if we could reproduce the error and also good just give the choice.
%             This entry comes from 'doc_issues.odt' priority 4
seed :-
        setrand(rand(1, 9, 70)).     %fixed run-time : useful for measuring runtime improvements during regreession testing or simple debugging
        /*now(N),		%unifies with the date and time
        S is N mod 30000,
        random(R1),
        X is integer(S*R1),
        random(R2),
        Y is integer(S*R2),
        random(R3),
        Z is integer(S*R3),
        setrand(rand(X,Y,Z)), %must be in the range [1,30269), [1,30307), and [1,30323) respectively
        */
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%e.g. from '*' to op_42
normalise_subprogram_name(Raw_subprogram_name, Subprogram_name) :-
        (atom(Raw_subprogram_name) ->
                true
        ;
                common_util:common_util__error(10, "The passed subprogram name is not in the correct format (e.g. contain spaces)", "Cause : it is not an atom", [('raw_subprogram_name', Raw_subprogram_name)], 1024605, 'mika_main', 'normalise_subprogram_name', 'no_localisation', 'no_extra_info')
        ),
        nsn_transform_for_operators(Raw_subprogram_name, Subprogram_name, _).
%%%
%used for operator subprograms : transforms the input subprogram's name into its internal representation
nsn_transform_for_operators('+', string_43, '"+"') :- !.
nsn_transform_for_operators('-', string_45, '"-"') :- !.
nsn_transform_for_operators('*', string_42, '"*"') :- !.
nsn_transform_for_operators('/', string_47, '"/"') :- !.
nsn_transform_for_operators('**', string_42_42, '"**"') :- !.
nsn_transform_for_operators('&', string_38, '"&"') :- !.
nsn_transform_for_operators('=', string_61, '"="') :- !.
nsn_transform_for_operators('/=', string_60_62, '"/="') :- !.
nsn_transform_for_operators('<', string_60, '"<"') :- !.
nsn_transform_for_operators('<=', string_60_61, '"<="') :- !.
nsn_transform_for_operators('>', string_62, '">"') :- !.
nsn_transform_for_operators('>=', string_62_61, '">="') :- !.
nsn_transform_for_operators('and', string_97_110_100, '"and"') :- !.
nsn_transform_for_operators('mod', string_109_111_100, '"mod"') :- !.
nsn_transform_for_operators('or', string_111_114, '"or"') :- !.
nsn_transform_for_operators('rem', string_114_101_109, '"rem"') :- !.
nsn_transform_for_operators('xor', string_120_111_114, '"xor"') :- !.
nsn_transform_for_operators('abs', string_97_98_115, '"abs"') :- !.
nsn_transform_for_operators('not', string_110_111_116, '"not"') :- !.
nsn_transform_for_operators(Subprogram_name, Subprogram_name, Subprogram_name) :- !.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%retrieve coverage information from foo.cov or fails
search_coverage_info(SubprogramL, Subprogram_name, Line_no, Look_for) :-
        (number(Line_no) ->        %line number specified [an overloaded subprogram]
                concat_numbers_with_underline(Line_no, Subprogram_name, Search, 'nozero')
        ;
                Search = Subprogram_name
        ),
	Subprogram_look_for =.. [Search, Look_forL],
	(memberchk(Subprogram_look_for, SubprogramL) ->
		(memberchk(Look_for, Look_forL) ->
			true
		;
			fail
		)
	;
         number(Line_no) ->     %a real hack : if the suplied line no is wrong we try the first subprogram name that matches
                (Subprogram_look_for3 =.. [Subprogram_name, Look_forL3],
                 (memberchk(Subprogram_look_for3, SubprogramL) ->
                        (memberchk(Look_for, Look_forL3) ->
			        true
		        ;
			        fail
		        )
                 ;
                        (mika_globals:mika_globals__get_NBT('context', Elaboration),       %11/04/08 the coverage level can vary with the chosen context quite a hack at the moment
                         atom_concat(Search, Elaboration, Search2),
                         Subprogram_look_for2 =.. [Search2, Look_forL2],
	                 (memberchk(Subprogram_look_for2, SubprogramL) ->
		                (memberchk(Look_for, Look_forL2) ->
			                true
		                ;
			                fail
		                )
	                 ;
                                fail
                         )
                        )
                 )
                )
         ;
                (mika_globals:mika_globals__get_NBT('context', Elaboration),       %11/04/08 the coverage level can vary with the chosen elaboration setting quite a hack at the moment
                         atom_concat(Search, Elaboration, Search2),
                         Subprogram_look_for2 =.. [Search2, Look_forL2],
	                 (memberchk(Subprogram_look_for2, SubprogramL) ->
		                (memberchk(Look_for, Look_forL2) ->
			                true
		                ;
			                fail
		                )
	                 ;
                                fail
                         )
                        )
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%creates a time stamped new working directory
set_directory(CreateTimeStampedDirectory, datime(Year, Month, Day, Hour, Min, Sec), Subprogram_name, Line_no, Dir, New_dir_for_printing) :-
        working_directory(_, Dir),      %set the working directory to the working directory passed from the command line
        (number(Line_no) ->                     %line number is specified [an overloaded subprogram]
                concat_numbers_with_underline(Line_no, Subprogram_name, Atom_dir0, 'nozero')
        ;
                Atom_dir0 = Subprogram_name
        ),
        (CreateTimeStampedDirectory == 'yes' ->
                (concat_numbers_with_underline(Year, Atom_dir0, Atom_dir1, 'zero'),
                 concat_numbers_with_underline(Month, Atom_dir1, Atom_dir2, 'zero'),
                 concat_numbers_with_underline(Day, Atom_dir2, Atom_dir3, 'zero'),
                 concat_numbers_with_underline(Hour, Atom_dir3, Atom_dir4, 'zero'),
                 concat_numbers_with_underline(Min, Atom_dir4, Atom_dir5, 'zero'),
                 concat_numbers_with_underline(Sec, Atom_dir5, Atom_dir6, 'zero'),
                 (file_exists(Atom_dir6) ->      %may already exist if the same subprogram is tested differently (e.g. decision vs branch, or different elaboration setting) and the test generation is very quick i.e. within the same second
                        (Sec_1 is Sec + 1,
                         concat_numbers_with_underline(Sec_1, Atom_dir5, New_dir, 'zero')       %we just set the time to the next second
                        )
                 ;
                        New_dir = Atom_dir6
                 ),
                 make_directory(New_dir)
                )
        ;
                (New_dir = Atom_dir0,
                 (file_exists(New_dir) ->
                        true %changed 25/06/09  will be overwritten instead of trying to delete it (because impossible to delete if the directory is open in explorer for example) was : delete_file(New_dir, [directory, recursive])    %we delete the directory and its contents!
                 ;
                        make_directory(New_dir)
                 )
                )
        ),
        working_directory(_, New_dir),
        atom_concat(Dir, New_dir, New_dir_for_printing0),
        atom_concat(New_dir_for_printing0, '/', New_dir_for_printing).

concat_numbers_with_underline(Number, Atom_in, Atom_out, Zero) :-
        number_codes(Number, Number_codes0),
        (Zero == 'zero' ->
                (Number < 10 ->
                        Number_codes = [48|Number_codes0]       %we add a '0'
                ;
                        Number_codes = Number_codes0
                )
        ;
                Number_codes = Number_codes0
        ),
        atom_codes(Number_atom, Number_codes),
        atom_concat(Atom_in, '_', Atom_in2),
        atom_concat(Atom_in2, Number_atom, Atom_out).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%test driver (mika_subprogramfoo_driver.adb) written in full
print_test_driver(Driver_file, datime(Year, Month, Day, Hour, Min, Sec), Subprogram_driver_name, Orig_dir, Target_package_name, Subprogram_name, Strategy, Elaboration, New_dir, Driver) :-
        mika_globals:mika_globals__get_NBT('is_subprogram_only_unit', Is_subprogram_only_unit),
        open(Driver_file, 'write', _, [alias('driver_output')]),	%mika_subprogramfoo_driver.adb
        print_preamble('driver_output', datime(Year, Month, Day, Hour, Min, Sec), Orig_dir, Target_package_name, Subprogram_name, Strategy, Elaboration, New_dir, Driver),
        format('driver_output', "Pragma Ada_95;           -- the entire test driver and test points are in Ada 95\n", []),
        format('driver_output', "with mika_TP_util;       -- utilities, in particular the Overall success variable\n", []),
        format('driver_output', "with init_driver;        -- bridge to all the necessary minor test points: call them in turn\n", []),
        (Is_subprogram_only_unit == 'yes' ->
                (mika_globals:mika_globals__get_NBT('main_test_point_name', a(_Test_point_file, Test_point_name)),
                 format('driver_output', "with ~w;\n", [Test_point_name])
                )
        ;
                format('driver_output', "with ~w;                 -- where the subprogram under test is located\n", [Target_package_name])
        ),
        format('driver_output', "with Ada.Command_Line;   -- to set the command line exit status\n", []),
	format('driver_output', "with Text_IO; use Text_IO;\n", []),
	format('driver_output', "procedure ~w is\n", [Subprogram_driver_name]),
        format('driver_output', "begin\n", []),
        (Subprogram_name == 'elaboration' ->
                format('driver_output', "  Put_line(""Below are the test run outcomes for elaboration only of package ~w in file ~w for strategy ~w"");\n", [Target_package_name, Orig_dir, Strategy])
        ;
                format('driver_output', "  Put_line(""Below are the test run outcomes for subprogram ~w of package ~w in file ~w for strategy ~w"");\n", [Subprogram_name, Target_package_name, Orig_dir, Strategy])
        ),
        format('driver_output', "  if init_driver.Nb_Test = 0 then \n", []),
        format('driver_output', "    Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure); -- no test inputs to execute\n", []),
        format('driver_output', "    put(""Error: there are no test inputs to execute"");\n", []),
        format('driver_output', "  else \n", []),
        format('driver_output', "    for I in 1..init_driver.Nb_Test loop    --number of tests generated\n", []),
        format('driver_output', "      mika_TP_util.Overall := True;  -- Overall test run success is initially true, may be set to false within any of the test points if the expected outcome is different to the actual outcome\n", []),
        format('driver_output', "      put(""TEST NUMBER"" & Integer'image(I) & "":"");\n", []),
        format('driver_output', "      init_driver.call_TPs(-I);    -- calls all the minor test points to initialise the local test inputs for this test\n", []),
        (Is_subprogram_only_unit == 'yes' ->
                format('driver_output', "      ~w(I);       -- calls the main test point : declares the parameters, initialise them, initialise the local test inputs, call the subprogram under test, compare local outputs, the parameters and return value\n", [Test_point_name])
        ;
                format('driver_output', "      ~w.Mika_Test_Point(I);       -- calls the main test point : declares the parameters, initialise them, initialise the local test inputs, call the subprogram under test, compare local outputs, the parameters and return value\n", [Target_package_name])
        ),
        format('driver_output', "      init_driver.call_TPs(I);     -- calls all the minor test points to compare the local actual outputs against the expected outputs for this test\n", []),
        format('driver_output', "      if mika_TP_util.Overall then\n", []),
        format('driver_output', "        put_line("" OK"");\n", []),
        format('driver_output', "      else\n", []),
        format('driver_output', "        new_line;\n", []),
        format('driver_output', "        Ada.Command_Line.Set_Exit_Status(Ada.Command_Line.Failure); -- it will end in overall failure\n", []),
        format('driver_output', "      end if;\n", []),
        format('driver_output', "    end loop;\n", []),
        format('driver_output', "  end if;\n", []),
        format('driver_output', "end ~w;", [Subprogram_driver_name]),
        close('driver_output').
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%preamble for foo_mika_tests.txt and foo.debug
print_preamble(Stream, datime(Year, Month, Day, Hour, Min, Sec), Orig_dir, Target_package_name, Subprogram_name, Strategy, Elaboration, New_dir, Driver) :-
        print_copyright(Stream),
        format(Stream, "-- Original Directory:\t~w\n", Orig_dir),
        format(Stream, "-- Target Directory:\t~w\n", New_dir),
        format(Stream, "-- Package:\t\t~w\n", Target_package_name),
        format(Stream, "-- Subprogram:\t\t~w\n", Subprogram_name),
        format(Stream, "-- Strategy:\t\t~w\n", Strategy),
        format(Stream, "-- Elaboration:\t\t", []),
        (Elaboration == 'ignored' ->
                format(Stream, "Ignored\n", [])
        ;
                format(Stream, "Not Ignored\n", [])
        ),
        format(Stream, "-- Coverage Depth:\t",[]),
        mika_globals:mika_globals__get_NBT('coverage_thoroughness', Coverage_thoroughness),
        (Coverage_thoroughness == 'subprogram_only' ->
                format(Stream, "Subprogram Only\n", [])
        ;
         Coverage_thoroughness == 'package_call_tree' ->
                format(Stream, "Package Call Tree\n", [])
        ;
         Coverage_thoroughness == 'entire_call_tree' ->
                format(Stream, "Entire Call Tree\n", [])
        ;
         Coverage_thoroughness == 'everything_including_rtl' ->
                format(Stream, "Everything Including the RTL\n", [])
        ;
                common_util:common_util__error(10, "Unexpected coverage thoroughness: contact Midoan", 'no_error_consequences', [('coverage_thoroughness', Coverage_thoroughness)], 1050642, 'mika_main', 'print_preamble', 'no_localisation', 'no_extra_info')
        ),
        format(Stream, "-- Time Stamp:\t\t", []),
        print_double(Stream, Day),
        format(Stream, ":", []),
        print_double(Stream, Month),
        format(Stream, ":~d ", [Year]),
        print_double(Stream, Hour),
        format(Stream, ":", []),
        print_double(Stream, Min),
        format(Stream, ":", []),
        print_double(Stream, Sec),
        format(Stream, "\n", []),
        (Driver == 'no_driver' ->
                true
        ;
         Driver = driver(Driver_source_file_name, Driver_subprogram_name) ->
                format(Stream, "-- ANALYSIS and COMPLETION of TEST DRIVER CALLED ~w in FILE ~w \n", [Driver_subprogram_name, Driver_source_file_name])
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
print_copyright(Stream) :-
        format(Stream, "----------------------------------------------------------\n", []),
        format(Stream, "--              MIKA TEST INPUTS GENERATOR              --\n", []),
        format(Stream, "-- Copyright Midoan Software Engineering Solutions Ltd. --\n", []),
        format(Stream, "--                http://www.midoan.com/                --\n", []),
        format(Stream, "----------------------------------------------------------\n", []).
%%%
        print_init_driver_ads(Nb_tests) :-
                open('init_driver.ads', 'write', _, [alias('init_driver_ads')]),
                print_copyright('init_driver_ads'),
                format('init_driver_ads', "package Init_driver is\n", []),
                format('init_driver_ads', "  Nb_Test : Integer := ~w;  --number of tests generated for this run\n", Nb_tests),
                format('init_driver_ads', "  procedure call_TPs(Test_number : in Integer);  --will call all the necessary minor test points\n", []),
                format('init_driver_ads', "end Init_driver;\n", []),
                close('init_driver_ads').
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
print_init_driver_adb :-
        open('init_driver.adb', 'write', _, [alias('init_driver_adb')]),
        print_copyright('init_driver_adb'),
        mika_globals:mika_globals__get_NBT('test_points', WithL),
        pida_print_withed_packages(WithL),
        format('init_driver_adb', "package body Init_driver is\n", []),
        format('init_driver_adb', "  procedure call_TPs(Test_number : in Integer) is\n", []),
        format('init_driver_adb', "  begin\n", []),
        format('init_driver_adb', "    null;\n", []),     %needed in case there are no test points
        format('init_driver_adb', "    -- calls all the minor Test Points;\n", []),
        pida_print_TP_calls(WithL),
        format('init_driver_adb', "  end;\n", []),
        format('init_driver_adb', "end Init_driver;\n", []),
        close('init_driver_adb').
%%%
        pida_print_withed_packages([_]) :-   %the last one the main_TP is not needed : already withed from the main driver
                !.
        pida_print_withed_packages([Target_package_name|R]) :-
                format('init_driver_adb', "with ~w;\n", Target_package_name),
                pida_print_withed_packages(R).
%%%
        pida_print_TP_calls([_]) :-          %the last one the main_TP is not needed : will be called separately from the main test driver
                !.
        pida_print_TP_calls([Target_package_name|R]) :-
                format('init_driver_adb', "    ~w.Mika_Test_Point(Test_number);\n", Target_package_name),
                pida_print_TP_calls(R).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%e.g. prints 6 as 06
print_double(Stream, Int) :-
        (Int >= 10 ->
                format(Stream, "~d", Int)
        ;
                format(Stream, "0~d", Int)
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set_is_subprogram_only_unit(Orig_dir, Target_source_file_name) :-
        atom_concat(Orig_dir, Target_source_file_name, F1),
        atom_concat(F1, '.ads', Specification_file),
        ((\+ file_exists(Specification_file), \+ sub_atom(Target_source_file_name, _Before, _Length, _after, '-')) ->
                mika_globals:mika_globals__set_NBT('is_subprogram_only_unit', 'yes')     %i.e. we are dealing with with a file that contains only a single subprogram unit : affects the printing out of testpoints etc.
        ;
                mika_globals:mika_globals__set_NBT('is_subprogram_only_unit', 'no')      %i.e. we are dealing with a package
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
set_main_test_point_name(Target_package_name) :-
        mika_globals:mika_globals__get_NBT('is_subprogram_only_unit', Is_subprogram_only_unit),
        (Is_subprogram_only_unit == 'yes' ->
                (atom_concat(Target_package_name, '_mika_test_point.adb', Test_point_file),
                 atom_concat(Target_package_name, '_mika_test_point', Test_point_name)
                )
        ;
                (package_name_to_file_name_prefix(Target_package_name, File_prefix),
                 atom_concat(File_prefix, 'mika_test_point.adb', Test_point_file),
                 atom_concat(Target_package_name, '.mika_test_point', Test_point_name)
                )
        ),
        mika_globals:mika_globals__set_NBT('main_test_point_name', a(Test_point_file, Test_point_name)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     END   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%