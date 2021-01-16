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
% mika_symbolic.pl
% defines module mika_symbolic
% symbolic interpretation of intermediate Ada Code
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(mika_symbolic, [      mika_symbolic__dynamic_procedure/6,
                                mika_symbolic__dynamic_function/7,
                                mika_symbolic__procedure/6,
                                mika_symbolic__function/7,
                                mika_symbolic__analyse/7,
                                handle_selected_name/2,		%needed in mika_coverage
				mika_symbolic__parse/6,
                                mika_symbolic__check_context/2,
                                mika_symbolic__initialise_context/1
                         ]
         ).
%%%
:- compile([midoan_solver_undosyntax]).
:- compile([mika_symbolic__execute]).	        %actual symbolic execution of Ada statements
:- compile([mika_symbolic__execute_choose_truth]).      %handle control flow choices
:- compile([mika_symbolic__execute_subprogram_call]).   %handle subprogram calls
:- compile([mika_symbolic__execute_util]).      %utilitarian predicates used during the symbolic execution of intermediate Ada statements
:- compile([mika_symbolic__interpret]).	        %actual symbolic interpretation of Ada expressions
:- compile([mika_symbolic__interpret_util]).	%utilitarian predicates used during the symbolic interpretation of Ada expressions
:- compile([mika_symbolic__util]).	        %utilitarian predicates
:- compile([mika_symbolic__denote]).            %syntactic denotation : denoting without executing

:- use_module(common_util, [        common_util__create_dummy_name/1,
                                    common_util__error/9
                           ]
         ).

:- use_module([mika_globals, mika_coverage, mika_name_atts, mika_package_atts, mika_print, mika_sub_atts, mika_seav_atts, mika_generic_atts, mika_unhandled_atts]).

:- use_module(midoan_solver_main, [	midoan_solver__interpret/5,
                                        midoan_solver__label_reals/1,
                                        midoan_solver__sdl/1,
                                        midoan_solver__real_max/3,
                                        midoan_solver__real_min/3,
                                        midoan_solver__get_type_from_value/2,
                                        midoan_solver__controlled_unification/2
                                  ]
             ).

:- use_module(library(lists)).	%Sicstus list library
:- use_module(library(terms)).	%Sicstus terms library (e.g. term_variables_bag/2)
:- use_module(library(random)).	%Sicstus random library
:- use_module(library(timeout)).%Sicstus timeout library (for timing ou the labeling of real variables)
:- use_module(library(system)). %Sicstus system library : for file_exists/2

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dynamic context/2.                   %for the test data generation context of the form: context(filename, ignored|not_ignored)
:- asserta((context(_, ignored) :- !)).        %the catch all default. : use asserta to overide

mika_symbolic__initialise_context(ignored) :-
        asserta((context(_, ignored) :- !)).
mika_symbolic__initialise_context(not_ignored) :-
        asserta((context(_File_name, not_ignored) :- !)).        %asserta is important: add at the top

%outcome is 'ignored' or 'not_ignored', set by mika_symbolic__initialise_context
mika_symbolic__check_context(_Filename, Outcome) :-
        context(_Filename, Outcome).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Constants
maxForIterationReached(501).            %max number of iterations of a for loop before forced failure: used in for_body
maxLoopIterationReached(101).           %max number of iterations of a loop before forced failure: used in loop_body
maxWhileIterationReached(11).           %max number of iterations of a while loop before forced failure: used in while_body
realLabelingTimeoutBasic(10000).        %basic allocation in milliseconds for real labeling
realLabelingTimeoutPerVar(1000).        %allocation per variable in milliseconds for real labeling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mika_symbolic__analyse(Input_file, Target_source_file_name, Target_package_name, Target_subprogram_name, Line_no, Driver, Strategy, Coverage_achieved) :-
        %trace,
        user:mika_body(All_contents),                   %retrieve all contents from the consulted source file
        last(All_contents, mika_ref(Source_namesL)),    %retrieve the source Ada names of variables
        mika_name_atts:mika_name_atts__initL(Source_namesL),            %add the ada source name attribute to all variables: includes guessing of unxrefed variables
        %trace,
        mika_coverage:mika_coverage__build_cfg(All_contents, Strategy),	%CFG building for the entire parsed file
        %trace,
        fix_separates(All_contents),                    %matches stubs with their body
        common_util__error(1, "MIKA : Elaborating", no_error_consequences, no_arguments, 122689, mika_symbolic, mika_symbolic__analyse, no_localisation, no_extra_info),
        %elaborating : all top level declarations and the main 'main', if elaboration contains branches they are followed if feasible; elaboration never backtracks : it is assumed to be deterministic.
        %depending on the context of a package assignments may ('not_ignored' context) or may not ('ignored' context) be taken into account
        %as all declarations within package specifications and bodies are executed, all global variables become attributed variables (as Types, SEAVs, Packages or Subprograms)
        %all mains of package bodies are executed, hence, remember, that some code statements are actually executed (not just syntactic transformation)
        %trace,
        (exec(All_contents, _) ->       %ELABORATION
		true
        ;
                common_util__error(10, "Elaboration failed", no_error_consequences, [(Input_file, Input_file)], 1024030, mika_symbolic, mika_symbolic_analyse, no_localisation, "this should never happen")
        ),
        mika_globals:mika_globals__set_NBT('phase', 'post_elaboration'),
        common_util__error(1, "MIKA : END Elaborating", 'no_error_consequences', 'no_arguments', 1236100, 'mika_symbolic', mika_symbolic__analyse, no_localisation, no_extra_info),
        %garbage_collect,       %memory monitoring
        term_variables_bag(All_contents, All_vars),
        (Target_subprogram_name == 'elaboration' ->
                Target_subprogram_name_xref = 'elaboration'
        ;
                (find_sub_var(All_vars, Target_source_file_name, Target_subprogram_name, Line_no, Sub_var),      %find the appropriate subprogram attributed variable holding the declarations and the body of the subprogram to execute
                 mika_sub_atts:mika_sub_atts__get('name', Sub_var, Target_subprogram_name_xref),
                 (Driver == 'no_driver' ->
                        Driver_subprogram_var = 'no_driver'    %there is no driver context : normal test inputs generation
                 ;
                  Driver = driver(Driver_source_file_name, Driver_subprogram_name) ->
                        (find_sub_var(All_vars, Driver_source_file_name, Driver_subprogram_name, '_', Driver_subprogram_var),
                         mika_globals:mika_globals__set_NBT('driver', driver(Target_subprogram_name_xref))
                        )
                 )
                )
        ),
        mika_coverage:mika_coverage__post_elaboration(Strategy, Target_subprogram_name_xref),  %calculate the list of successors for the subprogram under test and set 'to_cover' and, if necessary 'overall_mcdc_deci'
        !,	%never backtrack any of the above: cfg building and the elaboration phase
        %trace,
        post_elaboration(Target_package_name, Target_subprogram_name, Sub_var, All_contents, All_vars, Strategy, Driver, Driver_subprogram_var, Coverage_achieved, Driver_achieves_full_coverage),  %where it all happens after elaboration (including reporting)
        close_all_TPs,                  %all test point streams are closed
        (Driver_achieves_full_coverage = 'no' ->
                (mika_globals:mika_globals__get_NBT('path_nb', Nb),
                 mika_globals:mika_globals__get_NBT('abandoned_path_nb', Nb_abandoned),
                 Nb1 is Nb - Nb_abandoned,
                 format('answer_output', "~w test inputs generated\n", Nb1),
                 format('answer_output', "~w paths fully attempted but for which test inputs could not be generated\n", Nb_abandoned)
                )
        ;
                true
        ),
        format('answer_output', "----------------------END OF REPORT-----------------------", []),
        close('answer_output').
%%%
%05/07/09 deals with stub and body, pre-processes the parsed input to match the stubs with the bodies, will be processed normally later during execute body_stub(Stub_name)
        fix_separates([]).
        fix_separates([Item|Rest]) :-
                (Item = match_body(Stub_name, Body) ->
                        Stub_name = Body
                ;
                        true
                ),
                fix_separates(Rest).
%%%
post_elaboration(Target_package_name, Target_subprogram_name, Sub_var, All_contents, All_vars, Strategy, Driver, Driver_subprogram_var, Coverage_achieved, Driver_achieves_full_coverage) :-
        (Target_subprogram_name == 'elaboration' ->
                (context_checking(All_vars),            %25/04/08 may modify the mode of some of the seav variables depending on context desired (e.g introduces init_elab mode)
                 common_util__error(1, "MIKA : No subprogram to process : elaboration only", 'no_error_consequences', 'no_arguments', 1236100, 'mika_symbolic', post_elaboration, no_localisation, no_extra_info),    %it has already been done during our 'elaboration'
                 keep_only_seavs(All_vars, SEAV_list, Unhandled_list),	%used for reporting
                 ParamL_driver = [],
                 Return_c = 'no_return',
                 Driver_achieves_full_coverage = 'no'
                )
        ;
                (mika_globals:mika_globals__get_NBT('context', Context),
                 (Context == 'not_ignored' ->
                        true
                 ;
                        (% 02/01/09 as the context is ignored the current path is reset back to []
                         mika_globals:mika_globals__clean_up,   %erase all backtractable globals and current path
                         %and now reset them : the path so far, which includes the elaboration, is erased
                         mika_globals:mika_globals__init_BT_path('current_path_bran', []),
	                 mika_globals:mika_globals__init_BT_path('current_path_deci', []),
                         mika_globals:mika_globals__init_BT_path('current_path_mcdc_gate', []),
	                 mika_globals:mika_globals__init_BT_path('current_path_cond', []),
                         mika_globals:mika_globals__set_BT('call_stack_bran', []),
	                 mika_globals:mika_globals__set_BT('call_stack_deci', []),
	                 mika_globals:mika_globals__set_BT('call_stack_cond', [])
                        )
                 ),
                 (Driver == 'no_driver' ->
                        Driver_achieves_full_coverage = 'no'                                            %there is no driver context : normal test inputs generation
                 ;
                        (format('answer_output', "\nTESTS IN TEST DRIVER ARE ANALYSED\n", []),
                         exec(procedure_call(Driver_subprogram_var), _),  %call the test driver which must be a parameterless procedure
                         (mika_coverage:mika_coverage__finished(Strategy) ->
                                (Driver_achieves_full_coverage = 'yes',
                                 format('answer_output', "----------------------------------------------------------\n", []),
                                 format('answer_output', "FINAL REPORT\n", []),
                                 format('answer_output', "   NO NEW TEST INPUTS NECESSARY\n", []),
			         Coverage_achieved = 100
                                )
                         ;
                                Driver_achieves_full_coverage = 'no'
                         ),
                         !     %never backtrack above this point: driver execution
                        )
                 ),
                 (Driver_achieves_full_coverage == 'no' ->
                        (mika_globals:mika_globals__set_NBT('driver', 'no_driver'),     %to indicate that the driver has been executed
                         context_checking(All_vars),            %25/04/08 may modify the mode of some of the seav variables depending on context desired (e.g introduces init_elab mode)
                         %trace,
                         setup_and_run(Target_subprogram_name, Sub_var, All_contents, SEAV_list, Unhandled_list, ParamL_driver, return(Return_c), Flow),        %where it all happens prior to reporting : may fail if we unsuccessfully backtrack : that is normal
                         (common_util:common_util__is_an_exception(Flow) ->
                                true			%it is worth reporting the path followed
                         ;
                                (mika_coverage:mika_coverage__current_path_contain_uncovered(Strategy, Current_path_contain_uncovered, Newly_covered),
                                 %added 09-02-07, check if the path executed actually contains something new
                                 %this may happens with the last iteration of a loop see 27/28-11-06
                                 (Current_path_contain_uncovered == 'no' ->
                                        (mika_globals:mika_globals__get_NBT('to_cover', To_cover),
                                         (To_cover == [] ->     %12/11/10 to ensure that at least one test is generated even if, for example, there is no branches but banch coverage is required
                                                true
                                         ;
                                                (common_util__error(2, "Final path does not contain anything new, ...", 'no_error_consequences', 'no_arguments', 1288145, 'mika_symbolic', 'post_elaboration', 'no_localisation', 'no_extra_info'),
                                                 fail			%chronic failure : continue repeat testing criteria (backtracking)
                                                )
                                         )
                                        )
                                 ;
                                        true			%it is worth reporting the path followed
                                 )
                                )
                         )
                        )
                 ;
                        true
                 )
                )
        ),
        (Driver_achieves_full_coverage == 'no' ->
                (%an interesting path has been followed, worth labeling attempt and reporting
                 label_and_report(Strategy, SEAV_list, Unhandled_list, Target_package_name, Target_subprogram_name, Sub_var, ParamL_driver, return(Return_c), Newly_covered),		%report generation, test cases are generated etc. [may fail because of labeling]
                 %checking coverage criterion
                 (common_util:common_util__is_an_exception(Flow) -> %an uncaught exception has reached the top level
                        (Flow =.. ['exception'|Exception_arguments],
                         format('answer_output', "----------------------------------------------------------\n", []),
                         format('answer_output', "Uncaught exception detected : ~w\n", [Exception_arguments]),
                         Coverage_achieved = -1,
                         !
                        )
                 ;
                  Target_subprogram_name == 'elaboration' ->
		        (print_coverage(Strategy, Coverage_achieved),
                         !
		        )
	         ;
                        (mika_coverage:mika_coverage__finished(Strategy) ->
                                (%success : testing criterion achieved
                                 format('answer_output', "----------------------------------------------------------\n", []),
                                 format('answer_output', "FINAL REPORT\n", []),
                                 format('answer_output', "   100% ~w OVERALL COVERAGE PREDICTED.\n", [Strategy]),
                                 Coverage_achieved = 100,
                                 !
                                )
                        ;
                                fail			%chronic failure : continue repeat testing criterion
                        )
                 )
                )
        ;
                !
	).
%This is the end : subprogram coverage could not be finished at all
post_elaboration(_Target_package_name, _Target_subprogram_name, _Sub_var, _All_contents, _All_vars, Strategy, _Driver, _Driver_subprogram_var, Coverage_achieved, _Driver_achieves_full_coverage) :-
        print_coverage(Strategy, Coverage_achieved).
%%%
%e.g. if the context is ignored all SEAVs' mode is reset to 'unused'
        context_checking([]).
        context_checking([Var|Rest]) :-
                (mika_seav_atts:mika_seav_atts__is_seav(Var) ->
                        mika_seav_atts:mika_seav_atts__context_checking(Var)
                ;
                        true
                ),
                context_checking(Rest).
%%%
        print_coverage(Strategy, Percentage_achieved) :-
                mika_coverage:mika_coverage__not_finished(Strategy, Not_covered, Percentage_achieved, Already_covered),
                format(answer_output, "----------------------------------------------------------\n", []),
                format(answer_output, "FINAL REPORT\n", []),
                format(answer_output, "~w% ~w OVERALL COVERAGE PREDICTED.\n", [Percentage_achieved, Strategy]),
                length(Already_covered, Nac),
                (Strategy == 'branch' ->
                        format(answer_output, "~w BRANCHES PREDICTED COVERED:\n", [Nac])
                ;
                 (Strategy == 'decision' ; Strategy == 'mcdc') ->
                        format(answer_output, "~w DECISIONS PREDICTED COVERED:\n", [Nac])
                ;
                        common_util:common_util__error(10, "Testing strategy is unknown", no_error_consequences, [(strategy, Strategy)], 1025944, mika_symbolic, print_coverage, no_localisation, "Strategy can only be one of branch|decision|condition|mcdc")
                ),
                print_individuals_overall_coverage_details(Already_covered, Strategy),
                length(Not_covered, Nnc),
                (Strategy == 'branch' ->
                        format(answer_output, "~w BRANCHES PREDICTED REMAINING TO BE COVERED:\n", [Nnc])
                ;
                 (Strategy == 'decision' ; Strategy == 'mcdc') ->
                        format(answer_output, "~w DECISIONS PREDICTED REMAINING TO BE COVERED:\n", [Nnc])
                ;
                        common_util:common_util__error(10, "Testing strategy is unknown", no_error_consequences, [(strategy, Strategy)], 1025944, mika_symbolic, print_coverage, no_localisation, "Strategy can only be one of branch|decision|condition|mcdc")
                ),
                print_individuals_overall_not_covered_details(Not_covered, Strategy).
%%%
                print_individuals_overall_coverage_details([], _Strategy).
                print_individuals_overall_coverage_details([Next|Rest], Strategy) :-
                        (Strategy == 'branch' ->
                                (Next = (Number, Truth_value),
                                 user:bran(Number, Name, Filename, Suffix, Path, Line, Column),
                                 format(answer_output, "Number ~w, outcome ~w, in file ~w~w, on line ~w and column ~w\n", [Number, Truth_value, Filename, Suffix, Line, Column])
                                )
                        ;
                         Strategy == 'decision' ->
                                (Next = (Number, Truth_value),
                                 user:deci(Number, Name, Filename, Suffix, Path, Line, Column),
                                 format(answer_output, "Number ~w, outcome ~w, in file ~w~w, on line ~w and column ~w\n", [Number, Truth_value, Filename, Suffix, Line, Column])
                                )
                        ;
                         Strategy == 'mcdc' ->
                                (Next = Number,
                                 user:deci(Number, Name, Filename, Suffix, Path, Line, Column),
                                 format(answer_output, "Number ~w, in file ~w~w, on line ~w and column ~w\n", [Number, Filename, Suffix, Line, Column])
                                )
                        ;
                                common_util:common_util__error(10, "Testing strategy is unknown", no_error_consequences, [(strategy, Strategy)], 1025944, mika_symbolic, print_individuals_overall_coverage_details, no_localisation, "Strategy can only be one of branch|decision|condition|mcdc")
                        ),
                        print_individuals_overall_coverage_details(Rest, Strategy).
%%%
                print_individuals_overall_not_covered_details([], _Strategy).
                print_individuals_overall_not_covered_details([Next|Rest], Strategy) :-
                        (Strategy == 'branch' ->
                                (Next = (Number, Truth_value),
                                 user:bran(Number, Name, Filename, Suffix, Path, Line, Column),
                                 format(answer_output, "Number ~w, outcome ~w, in file ~w~w, on line ~w and column ~w\n", [Number, Truth_value, Filename, Suffix, Line, Column])
                                )
                        ;
                         Strategy == 'decision' ->
                                (Next = (Number, Truth_value),
                                 user:deci(Number, Name, Filename, Suffix, Path, Line, Column),
                                 format(answer_output, "Number ~w, outcome ~w, in file ~w~w, on line ~w and column ~w\n", [Number, Truth_value, Filename, Suffix, Line, Column])
                                )
                        ;
                         Strategy == 'mcdc' ->
                                (Next = (Id_deci, Gatelist, Outcome),
                                 user:deci(Id_deci, Name, Filename, Suffix, Path, Line, Column),
                                 format(answer_output, "Number ~w, in file ~w~w, on line ~w and column ~w. ", [Id_deci, Filename, Suffix, Line, Column]),
                                 (Outcome == [] ->
                                        (format(answer_output, "All decision outcomes covered, missing gates:\n", []),
                                         print_overall_gates_not_covered_details(Gatelist)
                                        )
                                 ;
                                  Gatelist == [] ->             %i.e. a decision with a missing outcome but without gates
                                        format(answer_output, "Missing outcomes ~w\n", [Outcome])
                                 ;
                                        (format(answer_output, "Missing outcomes ~w, missing gates:\n", [Outcome]),
                                         print_overall_gates_not_covered_details(Gatelist)
                                        )
                                 )
                                )
                        ;
                                common_util:common_util__error(10, "Testing strategy is unknown", no_error_consequences, [(strategy, Strategy)], 1025944, mika_symbolic, print_individuals_overall_coverage_details, no_localisation, "Strategy can only be one of branch|decision|condition|mcdc")
                        ),
                        print_individuals_overall_not_covered_details(Rest, Strategy).
%%%
print_covered_in_path(Strategy) :-
        mika_coverage:util_get_globals(Strategy, _Global_covered, Global_current_path, _Global_overall),
        mika_globals:mika_globals__get_BT_path(Global_current_path, Current_path),
        remove_duplicates(Current_path, Reduced_current_path),		%from Sicstus list library
        format('answer_output', "PREDICTED COVERED ", []),
        (Strategy == 'branch' ->
                format('answer_output', "BRANCHES :\n", [])
        ;
         Strategy == 'decision' ->
                format('answer_output', "DECISIONS :\n", [])
        ;
         Strategy == 'mcdc' ->
                format('answer_output', "GATES OR ATOMIC DECISIONS:\n", [])
        ;
                common_util:common_util__error(10, "Testing strategy is unknown", no_error_consequences, [(strategy, Strategy)], 1025944, mika_symbolic, print_covered_in_path, no_localisation, "Strategy can only be one of branch|decision|condition|mcdc")
        ),
        (Strategy == 'mcdc' ->  %we also want to print out decisions without gates in the path
                (mika_globals:mika_globals__get_BT_path('current_path_deci', Decisions),
                 mika_globals:mika_globals__get_NBT('overall_mcdc_deci', Overall_mcdc_deci),
                 mika_coverage:remove_decisions_with_gates(Decisions, Overall_mcdc_deci, Decisions_without_gates),
                 append(Reduced_current_path, Decisions_without_gates, Path)
                )
        ;
                Path = Reduced_current_path
        ),
        print_individuals_coverage_details(Path, Strategy).
        %%%
        print_individuals_coverage_details([], _Strategy).
        print_individuals_coverage_details([(start(_Target_subprogram_name), 'true')|Rest], Strategy) :-
                print_individuals_coverage_details(Rest, Strategy).
        print_individuals_coverage_details([Next|Rest], Strategy) :-
                (Strategy == 'branch' ->
                        (Next = (Number, Truth_value),
                         user:bran(Number, _Name, Filename, Suffix, _Path, Line, Column),
                         format('answer_output', "Number ~w, outcome ~w, in file ~w~w, on line ~w and column ~w\n", [Number, Truth_value, Filename, Suffix, Line, Column])
                        )
                ;
                 Strategy == 'decision' ->
                        (Next = (Number, Truth_value),
                         user:deci(Number, _Name, Filename, Suffix, _Path, Line, Column),
                         format('answer_output', "Number ~w, outcome ~w, in file ~w~w, on line ~w and column ~w\n", [Number, Truth_value, Filename, Suffix, Line, Column])
                        )
                ;
                 Strategy == 'mcdc' ->
                        (Next = gate(_Id_deci, Id_gate, Op, Mcdc_coverage) ->
                                (user:gate(Id_gate, _Name, Filename, Suffix, _Path, Line, Column),
                                 format('answer_output', "Gate Number ~w, Operator ~w, ~w, in file ~w~w, on line ~w and column ~w\n", [Id_gate, Op, Mcdc_coverage, Filename, Suffix, Line, Column])
                                )
                        ;
                         Next = (Number, Truth_value) ->        %a decision without gates
                                (user:deci(Number, _Name, Filename, Suffix, _Path, Line, Column),
                                 format('answer_output', "Decision Number ~w, outcome ~w, in file ~w~w, on line ~w and column ~w\n", [Number, Truth_value, Filename, Suffix, Line, Column])
                                )
                        ;
                                common_util:common_util__error(10, "Unexpected coverage for mcdc", no_error_consequences, [(next, Next)], 1035122, mika_symbolic, print_covered_in_path, no_localisation, 'no_extra_info')
                        )
                ;
                        common_util:common_util__error(10, "Testing strategy is unknown", no_error_consequences, [(strategy, Strategy)], 1025944, mika_symbolic, print_covered_in_path, no_localisation, "Strategy can only be one of branch|decision|condition|mcdc")
                ),
                print_individuals_coverage_details(Rest, Strategy).
        %%%
        print_overall_gates_not_covered_details([]).
        print_overall_gates_not_covered_details([gate(Id_gate, Op, Remaining_coverage)|Rest]) :-
                user:gate(Id_gate, _Name, Filename, Suffix, _Path, Line, Column),
                format(answer_output, "    Gate number ~w, Operator ~w, in file ~w~w, on line ~w and column ~w, for outcomes: ", [Id_gate, Op, Filename, Suffix, Line, Column]),
                print_gate_remaining_coverage(Remaining_coverage),
                print_overall_gates_not_covered_details(Rest).

                %%%
                print_gate_remaining_coverage([Next|Rest]) :-
                        (Rest == [] ->
                                format(answer_output, "(~w)\n", [Next])
                        ;
                                (format(answer_output, "(~w), ", [Next]),
                                 print_gate_remaining_coverage(Rest)
                                )
                        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%symbolically execute the subprogram under test post elaboration by calling it; hence does not apply to elaboration only case
%subprogram call needs to be specific to setup_and_run because the arguments need to be declared here and now unlike subsequent internal subprogram calls
setup_and_run(Target_subprogram_name, Sub_var, All_contents, SEAV_list, Unhandled_list, ParamL_driver, return(Return_c), Flow) :-
        !,
        common_util__error(1, "MIKA : Processing subprogram", no_error_consequences, [(target_subprogram_name, Target_subprogram_name)], 1344122, mika_symbolic, setup_and_run, no_localisation, no_extra_info),
        mika_sub_atts:mika_sub_atts__get('params', Sub_var, Params),
        check_for_unhandled(Params, Has_unhandled),
        (Has_unhandled == 'no' ->
                (mika_sub_atts:mika_sub_atts__get('return', Sub_var, Return),
                 my_copy_term('locals', a(Params, Return), a(Params_c, Return_c)),
                 !,
                 declare_setup_params(Params_c, Param_vars, ParamL_driver),	%from mika_symbolic__util.pl : declared as unused (for 'out' vars) or in (for 'in', 'in_out' vars) SEAVs;
                 %there is no need for view conversion at the top level nor is there a need to post match 'out' and 'in_out' parameters, Initialised parameters are ignored
                 %ParamL_driver contains all the parameters in the form driver(Xref_name, Ada_name, Type_name)
                 %calling exec ...
	         %trace,

                 (Return == 'no_return' ->        %it is a procedure
                        ((Param_vars == [] ->
                                exec(procedure_call(Sub_var), Flow)                                %where it all happens : procedure without arguments
                         ;
                                exec(procedure_call(indexed(Sub_var, Param_vars)), Flow)           %where it all happens : procedure with arguments
                         ),
                         term_variables_bag(a(All_contents, Param_vars), All_global_and_params_vars)
                        )
                 ;
                        (%it is a function also need to declare the Return variable
		         mika_sub_atts:mika_sub_atts__get('return_type', Sub_var, Subtype_indication_or_access_definition),
		         exec(object('not_qualified', [Return_c], Subtype_indication_or_access_definition, 'no_init'), _),
		         (Param_vars == [] ->
                                exec(assign(Return_c, Sub_var), Flow)                              %where it all happens : function without arguments
                         ;
                                exec(assign(Return_c, indexed(Sub_var, Param_vars)), Flow)         %where it all happens : function with arguments
                         ),
                         term_variables_bag(a(All_contents, Param_vars, Return_c), All_global_and_params_vars)
                        )
                 ),
                 keep_only_seavs(All_global_and_params_vars, SEAV_list, Unhandled_list)  %used for reporting
                )
        ;
                common_util__error(10, "Subprogram under test parameter specification contains unhandled entities", "Test inputs generation cannot proceed", [(target_subprogram_name, Target_subprogram_name)], 1039823, mika_symbolic, setup_and_run, no_localisation, no_extra_info)
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%report generation for a path that according to the midoan_solver is so far valid (might still prove impossible to label)
label_and_report(Strategy, SEAV_list, Unhandled_list, Target_package_name, Target_subprogram_name, Sub_var, ParamL_driver, return(Return_c), Newly_covered) :-
        (prepare_mika_report(Strategy, SEAV_list, Unhandled_list, IL, RL, EL, Inputs, Outputs, Contexts, Nb, Newly_covered) ->       %done prior to labeling to keep ranges intact : could do it after if a copy is taken
	 %prepare_mika_report should logically never fail, but since it does too often because of bugs and is hard to debug we check for failure
                true    %all is well in printing out the start of report
        ;
                common_util__error(10, "Could not print report", "Tests results files cannot be viewed", no_arguments, 10396125, mika_symbolic, label_and_report, no_localisation, "Should never happen")
        ),
	%trace,
        format('answer_output', "\nCONSTRUCTED TEST INPUT\n", []),
        common_util__error(1, "MIKA : START Generating Values", no_error_consequences, no_arguments, 138991, mika_symbolic, label_and_report, no_localisation, no_extra_info),
        ((midoan_labeling:midoan_labeling__enums(EL), midoan_labeling:midoan_labeling__integers(IL)) ->
                (length(RL, Nb_reals),
                 realLabelingTimeoutBasic(Basic_allocation),
                 realLabelingTimeoutPerVar(Per_var_allocation),
                 Time_allowed is Basic_allocation + Per_var_allocation*Nb_reals,
                 (time_out(midoan_solver__label_reals(RL), Time_allowed, Timed_out) -> %time in milliseconds
			(Timed_out == 'success' ->
				((mika_globals:mika_globals__get_NBT('test_points', []) ->	%to detect if the main test point has not already been created
		                        (create_test_point('main', Target_package_name),        %only done the first time we have a successful labeling
                                         mika_globals:mika_globals__set_NBT('test_points', [Target_package_name])             %the list of stream test points is initialised, the main test point is always the last
                                        )
                                 ;
                                        true    %no need to create it
                                 ),
                                 append(Inputs, Outputs, Vars_needed),
                                 length(Vars_needed, Nb_vars),
                                 set_demo_treshold(Nb_vars),   %sets the threshold for printing variables' values in demo mode
                                 mika_globals:mika_globals__get_NBT('test_points', Current_TPs),
                                 add_TPs(Vars_needed, Current_TPs, All_TPs),
                                 mika_globals:mika_globals__set_NBT('test_points', All_TPs),
                                 for_all_TP_init(All_TPs, Nb),
                                 last(All_TPs, Main_TP),          %because it is always added first
                                 tp_print_return_declaration(Main_TP, Return_c, Sub_var),            %to the main_TP only
                                 tp_print_parameters_declaration(ParamL_driver, Main_TP),            %declare the parameters to the main_TP only
                                 for_all_TP_begin(All_TPs),
                                 common_util__error(1, "MIKA : END Generating Values", no_error_consequences, no_arguments, 1406115, mika_symbolic, label_and_report, no_localisation, no_extra_info),
				 common_util__error(1, "MIKA : START Print Solution Input Variables", no_error_consequences, no_arguments, 1407138, mika_symbolic, label_and_report, no_localisation, no_extra_info),
				 mika_print:mika_print__solutions(Inputs, input_value, Target_subprogram_name, ParamL_driver),	%also prints input solutions to the test point files
                                 mika_print:mika_print__solutions(Contexts, input_value, Target_subprogram_name, []),  %should only print to test point, but does not
                                 common_util__error(1, "MIKA : END Print Solution Input Variables", no_error_consequences, no_arguments, 1410136, mika_symbolic, label_and_report, no_localisation, no_extra_info),
				 tp_print_call(Main_TP, Return_c, ParamL_driver, Target_subprogram_name),              %prints the subprogram under call in the main test point
                                 for_all_TP_end_1(All_TPs, Nb),
                                 common_util__error(1, "MIKA : START Print Solution Output Variables", 'no_error_consequences', 'no_arguments', 1413139, 'mika_symbolic', 'label_and_report', 'no_localisation', 'no_extra_info'),
				 format('answer_output', "\nCODE BEHAVIOUR\n", []),
				 mika_print:mika_print__solutions(Outputs, 'constraint', Target_subprogram_name, ParamL_driver),%also prints output solutions to the test driver file
				 common_util__error(1, "MIKA : END Print Solution Output Variables", no_error_consequences, no_arguments, 1417137, mika_symbolic, label_and_report, no_localisation, no_extra_info),
                                 for_all_TP_message(All_TPs, "    end;\n"),
				 flush_output(answer_output)
				)
			;
				(common_util__error(2, "Timed out real numbers generating values", no_error_consequences, no_arguments, 1422126, mika_symbolic, label_and_report, no_localisation, no_extra_info),
                                 labeling_failure("Consistency unsure (due to timed out floating point numbers sampling) path is abandonned\n"),
                                 fail %i.e. we did not manage to find a test
				)
			)
		;
			(labeling_failure("Consistency unsure (due to unsuccessful floating point numbers sampling) path is abandonned\n"),
			 fail %i.e. we did not manage to find a test
			)
		 )
                )
	;
		(labeling_failure("Inconsistent (detected by unsuccessful enumeration or integer sampling)\n"),
		 fail %i.e. we did not manage to find a test : backtrack symbolic execution and tries to find another path taht fulfills the testing criterion
		)
	),
	mika_coverage:mika_coverage__add_to_covered,	%the coverage achieved is updated
	!.

%only SEAV_list is an input, the rest are outputs necessary for labeling
%only for the foo.mika file
prepare_mika_report(Strategy, SEAV_list, Unhandled_list, IL, RL, EL, Inputs, Outputs, Contexts, Nb1, Newly_covered) :-
        mika_globals:mika_globals__get_NBT('debug_mode', DebugMode),
        common_util__error(1, "MIKA : Report Generation", no_error_consequences, no_arguments, 144494, mika_symbolic, prepare_mika_report, no_localisation, no_extra_info),
        pmr_partition_seavs(SEAV_list, Unused, Inputs, Outputs, Contexts),
        partition_atomic(Inputs, IL, RL, EL),   %partition the variables used in this path condition into the lists of integer, real, enumeration variables respectively ready for labelling
	format('answer_output', "----------------------------------------------------------\n", []),
        mika_globals:mika_globals__get_NBT('path_nb', Nb),
        Nb1 is Nb + 1,
        mika_globals:mika_globals__set_NBT('path_nb', Nb1),
        format('answer_output', "TEST NUMBER ~d\n", [Nb1]),
        print_covered_in_path(Strategy),
        print_newly_covered(Newly_covered, Strategy),
	(DebugMode == 'debug' ->
                (format(answer_output, "\nUNHANDLED VARIABLES:\n", []),
                 mika_print:mika_print__unhandled(Unhandled_list),
                 format(answer_output, "\nUNUSED TOP LEVEL SOURCE CODE VARIABLES:\n", []),
                 mika_print:mika_print__list_of_vars(Unused),
                 common_util__error(1, "MIKA : START Print Range Input Variables", no_error_consequences, no_arguments, 1457110, mika_symbolic, prepare_mika_report, no_localisation, no_extra_info),
	         format(answer_output, "\nRANGES of INPUT VARIABLES (according to the path traversal condition only)\n", []),
	         mika_print:mika_print__ranges(Inputs, input_value),
                 common_util__error(1, "MIKA : END Print Range Input Variables", no_error_consequences, no_arguments, 1460108, mika_symbolic, prepare_mika_report, no_localisation, no_extra_info),

                 common_util__error(1, "MIKA : START Print Effects", no_error_consequences, no_arguments, 1462105, mika_symbolic, prepare_mika_report, no_localisation, no_extra_info),
	         format(answer_output, "\nSYMBOLIC EFFECT of TRAVERSAL (ACTIONS) (of the path on the output variables)\n", []),
	         mika_print:mika_print__effects(Outputs, symbolic),
                 common_util__error(1, "MIKA : END Print Effects", no_error_consequences, no_arguments, 1465103, mika_symbolic, prepare_mika_report, no_localisation, no_extra_info),

	         common_util__error(1, "MIKA : START Print Value Effects", no_error_consequences, no_arguments, 1467108, mika_symbolic, prepare_mika_report, no_localisation, no_extra_info),
	         format(answer_output, "\nCONSTRAINED EFFECT of TRAVERSAL (ACTIONS) (of the path on the output variables)\n", []),
	         mika_print:mika_print__effects(Outputs, constraint),
                 common_util__error(1, "MIKA : END Print Value Effects", no_error_consequences, no_arguments, 1470106, mika_symbolic, prepare_mika_report, no_localisation, no_extra_info),

	         common_util__error(1, "MIKA : START Print Range Output Variables", no_error_consequences, no_arguments, 1472111, mika_symbolic, prepare_mika_report, no_localisation, no_extra_info),
	         format(answer_output, "\nRANGES of OUTPUT VARIABLES (according to the path traversal condition)\n", []),
	         mika_print:mika_print__ranges(Outputs, constraint),
                 common_util__error(1, "MIKA : END Print Range Output Variables", no_error_consequences, no_arguments, 1475109, mika_symbolic, prepare_mika_report, no_localisation, no_extra_info)
                )
        ;
                true
        ).
%%%
        print_newly_covered(Newly_covered, Strategy) :-
                (Strategy == 'branch' ->
                        format('answer_output', "\nBRANCHES COVERAGE INCREASED BY BRANCHES:\n", [])
                ;
                 Strategy == 'decision' ->
                        format('answer_output', "\nDECISION COVERAGE INCREASED BY DECISIONS:\n", [])
                ;
                 Strategy == 'mcdc' ->
                        format('answer_output', "\nMC/DC COVERAGE INCREASED BY GATES OR ATOMIC DECISIONS:\n", [])
                ;
                        common_util:common_util__error(10, "Testing strategy is unknown", no_error_consequences, [(strategy, Strategy)], 1025944, mika_symbolic, print_newly_covered, no_localisation, "Strategy can only be one of branch|decision|condition|mcdc")
                ),
                print_individuals_coverage_details(Newly_covered, Strategy).
%%%
labeling_failure(Message) :-
        format(answer_output, Message, []),
        flush_output(answer_output),
        mika_globals:mika_globals__get_NBT(abandoned_path_nb, Abandoned_Nb),
	Abandoned_Nb1 is Abandoned_Nb + 1,
	mika_globals:mika_globals__set_NBT(abandoned_path_nb, Abandoned_Nb1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
        %called on successful labeling: first argument is the list of all SEAVs needed (inputs and outputs)
        %creates the list of the necessary test points package names
        add_TPs([], All_TPs, All_TPs).
        add_TPs([SEAV|R], Current_TPs, All_TPs) :-
                mika_seav_atts:mika_seav_atts__get('name', SEAV, Xref_name),
                mika_symbolic__parse(Xref_name, Var_file_name, _File_extension, _Line, _Column, _Id),
                (user:file_name_denotes_a_RTL_compilation_unit(Var_file_name) ->
                        add_TPs(R, Current_TPs, All_TPs)    %29/05/08: ignored : belongs to an RTL
                ;
                        (user:file_name_without_extension_to_package_name(Var_file_name, Var_package_name),     %see issue "Does not work with krunched file names"
                         (memberchk(Var_package_name, Current_TPs) ->
                                add_TPs(R, Current_TPs, All_TPs)    %ignored : the test point already exist
                         ;
                                (create_test_point('minor', Var_package_name),
                                 add_TPs(R, [Var_package_name|Current_TPs], All_TPs)
                                )
                         )
                        )
                ).
%%%
create_test_point(Kind, Package_name) :-
        ((Kind == 'main', mika_globals:mika_globals__get_NBT('is_subprogram_only_unit', 'yes')) ->
                (First_main_TP = 'yes',
                 mika_globals:mika_globals__get_NBT('main_test_point_name', a(Test_point_file, Test_point_name))
                )
        ;
                (First_main_TP = 'no',
                 user:package_name_to_file_name_prefix(Package_name, Prefix),
                 atom_concat(Prefix, 'mika_test_point.adb', Test_point_file),
                 Test_point_name = 'Mika_Test_Point'
                )
        ),
        open(Test_point_file, 'write', _, [alias(Package_name)]),	%mika_package-mika_test_point.adb
        user:print_copyright(Package_name),
        format(Package_name, "pragma Ada_95; --because child packages are used\n", []),
        %22/01/09
        %add user created contents :here context_clause and/or pragma : used to help the compilation process of the automatically generated code
        % we do check for the existence of test drivers : if they does not exist we should fail; test execution will not be reliable
        % if it does exist read and insert all the first lines starting with "--MIKA "
        %only works if the original test points are in the original directory i.e. the directory where the original Ada file was
        %trace,
        mika_globals:mika_globals__get_NBT('orig_dir', Orig_dir),
        atom_concat(Orig_dir, Test_point_file, Original_TP),
	(file_exists(Original_TP) ->
                (open(Original_TP, 'read', _, [alias(orig_TP)]),
                 read_write_user_context('orig_TP', Package_name),
                 close('orig_TP')
                )
        ;
                true
        ),
        format(Package_name, "with mika_TP_util; use mika_TP_util;\n", []),
        format(Package_name, "with Text_IO; use Text_IO;\n", []),
        format(Package_name, "with Ada.Characters.Latin_1; use Ada.Characters.Latin_1; --for non printable characters recognition\n", []),
        (First_main_TP == 'yes' ->
                format(Package_name, "with ~w;\n", [Package_name])
        ;
                format(Package_name, "separate (~w)\n", [Package_name])
        ),
        format(Package_name, "procedure ~w(Test_number : in Integer) is\n", [Test_point_name]),
        format(Package_name, "begin\n", []),
        format(Package_name, "  case Test_Number is\n", []).
%%%
%user context for TPs. Reading what follows '--MIKA " from the original test point and writing to the new test point
read_write_user_context('orig_TP', Stream) :-
        read_line('orig_TP', Line),
        Line = [45, 45, 77, 73, 75, 65, 32|Rest], %the codes for "--MIKA "
        !,
        put_codes(Rest, Stream),                %write Rest to the target stream
        nl(Stream),                       %go to the next line
        read_write_user_context('orig_TP', Stream).
read_write_user_context(_, _).  %failure nothing valid to read
%%%
%writes a list of codes one by one to a stream
        put_codes([], _).
        put_codes([Code|Rest], Stream) :-
                put_code(Stream, Code),
                put_codes(Rest, Stream).
%%%
for_all_TP_init([With|Rest], Nb) :-
        (Rest == [] ->          %for the main TP (which is the last)
                (format(With, "  when ~w =>\n", [Nb]),
	         format(With, "    declare\n", [])
                )
        ;
                (format(With, "  when -~w =>\n", [Nb]), %notice the -
                 for_all_TP_init(Rest, Nb)
                )
        ).
%%%
for_all_TP_begin([]).
for_all_TP_begin([With|R]) :-
        format(With, "    begin\n", []),
        format(With, "      null;\n", []),
        format(With, "      -- necessary test inputs are initialised below\n", []),
        for_all_TP_begin(R).
%%%
tp_print_call(Main_TP, Return_c, ParamL_driver, Target_subprogram_name) :-
        format(Main_TP, "      --next the subprogram under test is called\n", []),
        (Return_c == 'no_return' ->       %a procedure
	        true
	;                               %a function
		(mika_seav_atts:mika_seav_atts__get('name', Return_c, Return_c_name),
		 mika_symbolic__parse(Return_c_name, _File_name, _File_extension, _Line, _Column, Id),
		 format(Main_TP, "      ~w :=", [Id])
		)
	),
        user:nsn_transform_for_operators(_, Target_subprogram_name, Call),       %transforms e.g. string_42 in to "*"
        (ParamL_driver == [] ->         %parameterless procedure or function
	        (Call == 'elaboration' ->
		        format(Main_TP, "      --no subprogram to call : elaboration only!\n", [])
		;
			format(Main_TP, "      ~w;\n", Call)
		)
	;                               %with parameters procedure or function
		(format(Main_TP, "      ~w(", Call),
		 tppc_print_arguments(ParamL_driver, Main_TP),
		 format(Main_TP, ");\n", [])
		)
	).
%%%
        %prints arguments for the call to the subprogram under test
        tppc_print_arguments([driver(_Xref_name, Ada_name, _Type_name)|Rest], Main_TP) :-
                format(Main_TP, "~w", [Ada_name]),
                (Rest == [] ->
                        true
                ;
                        (mika_globals:mika_globals__get_NBT('current_TP_stream', Current_TP_stream),
                         mika_print:break_lines(Current_TP_stream),
                         format(Main_TP, ", ", []),
                         tppc_print_arguments(Rest, Main_TP)
                        )
                ).
%%%
for_all_TP_end_1([With|Rest], Nb) :-
        (Rest == [] ->
                format(With, "      --test outputs are compared below\n", [])    %for the main TP (which is the last)
        ;
                (format(With, "    end;\n", []),
                 format(With, "  when ~w =>\n", [Nb]),
                 format(With, "    begin\n", []),
                 format(With, "      null;\n", []),
                 format(With, "      --test outputs are compared below\n", []),
                 for_all_TP_end_1(Rest, Nb)
                )
        ).

for_all_TP_message([], _Message).
for_all_TP_message([With|Rest], Message) :-
        format(With, Message, []),
        for_all_TP_message(Rest, Message).
%%%
        close_all_TPs :-
                mika_globals:mika_globals__get_NBT('test_points', All_TPs),
                close_all_TPs(All_TPs).
%%%
                close_all_TPs([]).      %only used if there no TPs (i.e. no successful labeling)
                close_all_TPs([With|R]) :-
                        format(With, "    when others => null;\n", []),
                        format(With, "  end case;\n", []),
                        (R == [] ->     %for the main TP (which is the last)
                                (mika_globals:mika_globals__get_NBT('is_subprogram_only_unit', Is_subprogram_only_unit),
                                 (Is_subprogram_only_unit == 'yes' ->
                                        (mika_globals:mika_globals__get_NBT(main_test_point_name, a(_Test_point_file, Test_point_name)),
                                         format(With, "end ~w;\n", [Test_point_name])
                                        )
                                 ;
                                        format(With, "end Mika_Test_Point;\n", [])
                                 )
                                )
                        ;
                                (format(With, "end Mika_Test_Point;\n", []),
                                 close_all_TPs(R)
                                )
                        ),
                        close(With).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
tp_print_return_declaration(Main_TP, Return_c, Sub_var) :-
        (Return_c == 'no_return' ->
		true	%no need to declare a return variable to the test driver
	;
		(%it is a function also need to declare the Return variable to the test driver
                 mika_sub_atts:mika_sub_atts__get('return_type', Sub_var, Subtype_indication_or_access_definition),
                 handle_intermediate_types(Subtype_indication_or_access_definition, Type_var, Return_type_name),
		 mika_symbolic__parse(Return_type_name, _File_name2, _File_extension2, _Line2, _Column2, Return_type_id),
		 mika_seav_atts:mika_seav_atts__get('name', Return_c, Return_c_name),
		 mika_symbolic__parse(Return_c_name, _File_name, _File_extension, _Line, _Column, Return_c_name_id),
		 (midoan_type:midoan_type__obtain_basetype(Type_var, unconst_array(_)) ->
                        (%an uncontrained array result type: need to declare the actual contsrained array type
                         generate_type_instantion_from_object(Return_type_id, Return_c, Type_instantiation),  %similar to generate_type_instantiation
                         format(Main_TP, "      ~w : ~w;\n", [Return_c_name_id, Type_instantiation])
                        )
                 ;
                        format(Main_TP, "      ~w : ~w;\n", [Return_c_name_id, Return_type_id])
                 )
		)
	).
%%%
tp_print_parameters_declaration([], _Main_TP).
tp_print_parameters_declaration([driver(_Xref_name, Ada_name, Type_name)|Rest], Main_TP) :-
        format(Main_TP, "      ~w : ~w;\n", [Ada_name, Type_name]),
        tp_print_parameters_declaration(Rest, Main_TP).
%%%
%sets the threshold for printing variables' values in demo mode
%based on number of variables and on path
%between 0 and 1 : if random above the threshold the concerned variable will not be ignored
set_demo_treshold(Nb_vars) :-
        random(N),
        (N > 0.5 ->
                (Nb_vars < 6 ->
                        Demo_threshold = 0.1
                ;
                 Nb_vars < 20 ->
                        Demo_threshold = 0.3
                ;
                 Nb_vars < 60 ->
                        Demo_threshold = 0.5
                ;
                        Demo_threshold = 0.75
                )
        ;
                Demo_threshold = 0.1
        ),
        mika_globals:mika_globals__set_NBT(demo_threshold, Demo_threshold).

% defining the operators precedence of the solver so that the input_file can be read properly
:- compile([midoan_solver_dosyntax]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     END    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%