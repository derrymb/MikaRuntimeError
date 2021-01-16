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
% mika_coverage__util.pl
% module mika_coverage
% utilitarian predicates used during coverage analysis of the parsed files
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
util_get_globals(Kind, Global_covered, Global_current_path, Global_overall) :-
	(Kind == 'branch' ->
                (Global_covered = 'covered_bran',
                 Global_current_path = 'current_path_bran'
                )
        ;
         Kind == 'decision' ->
                (Global_covered = 'covered_deci',
                 Global_current_path = 'current_path_deci'
                )
        ;
         Kind == 'mcdc' ->
                (Global_covered = 'overall_mcdc_deci',    %double role in mcdc case
                 Global_current_path = 'current_path_mcdc_gate',
		 Global_overall = 'overall_mcdc_deci'     %double role in mcdc case
                )
	;
	 Kind == 'condition' ->
                (Global_covered = 'covered_cond',
                 Global_current_path = 'current_path_cond'
                )
	;
		common_util:common_util__error(10, "Global could not be retrieved", "cannot proceed", [('kind', Kind)], 103107, 'mika_coverage__util', 'util_get_globals', 'no_localisation', "Is unexpected")
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
util_get_stack_name(Kind, Stack_name) :-
	(Kind == 'branch' ->
		Stack_name = 'call_stack_bran'
	;
	 (Kind == 'decision' ; Kind == 'mcdc')->
		Stack_name = 'call_stack_deci'
	;
	 Kind == 'condition' ->
		Stack_name = 'call_stack_cond'
	;
		common_util:common_util__error(10, "Unknown kind in stack", "cannot proceed", [('kind', Kind)], 104512, 'mika_coverage__util', 'util_get_stack_name', 'no_localisation', "Is unexpected")
	).

util_get_stack(Kind, Current_stack) :-	% a query
	util_get_stack_name(Kind, Stack_name),
	mika_globals:mika_globals__get_BT(Stack_name, Current_stack).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%for debugging purposes only in foo.debug
util_print_debug :-
	arc_bran(From, To, Bool),
	format('debug_output', "arc_bran(~w,\t~w,\t~w).\n", [From, To, Bool]),
	fail.
util_print_debug :-
        call_bran(Start, Subprogram_name, Bool),
        format('debug_output', "call_bran(~w,\t~w,\t~w).\n", [Start, Subprogram_name, Bool]),
	fail.
util_print_debug :-
	arc_deci(From, To, Bool),
	format('debug_output', "arc_deci(~w,\t~w,\t~w).\n", [From, To, Bool]),
	fail.
util_print_debug :-
        call_deci(Start, Subprogram_name, Bool),
        format('debug_output', "call_deci(~w,\t~w,\t~w).\n", [Start, Subprogram_name, Bool]),
	fail.
util_print_debug :-
        is_an_instance(Subprogram_name, Generic_subprogram_name),
        format('debug_output', "is_an_instance(~w,\t~w).\n", [Subprogram_name, Generic_subprogram_name]),
	fail.
util_print_debug :-
        is_a_rename(Subprogram_name, Original_subprogram_name),
        format('debug_output', "is_a_rename(~w,\t~w).\n", [Subprogram_name, Original_subprogram_name]),
        fail.
util_print_debug :-
	successor((From, Truth), L),
	format('debug_output', "successor((~w,\t~w),\t~w).\n", [From, Truth, L]),
	fail.
util_print_debug.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%the calculation for a single arc is fine
% but we re-apply it blindly for all arcs and that is not efficient at all but it works fine
% this is not a simple problem [see Sept. 06] and unless efficiency becomes an issue I would be reluctant to embark on changing it.
calculate_successors(Original_strategy, Original_Subprogram_name) :-
        (Original_strategy == 'mcdc' ->
                Strategy = 'decision'
        ;
                Strategy = Original_strategy
        ),
        (is_a_rename(Original_Subprogram_name, Target_subprogram_name) ->      %to allow the test data generation of renamed subprograms
                (ucss_successor_for_subprogram(Strategy, Target_subprogram_name, Original_Subprogram_name),
                 successor((start(Target_subprogram_name), 'true'), Out_set),     %retrieve the successors calculated
                 assert(successor((start(Original_Subprogram_name), 'true'), Out_set))
                )
        ;
                ucss_successor_for_subprogram(Strategy, Original_Subprogram_name, Original_Subprogram_name)
        ).
%%%
        ucss_successor_for_subprogram(Strategy, Subprogram_name, Original_Subprogram_name) :-
                build_successor([(start(Subprogram_name), 'true')], Strategy, Original_Subprogram_name, [], Out_set),
                assert(successor((start(Subprogram_name), 'true'), Out_set)),
                for_each_node(Out_set, Strategy, Original_Subprogram_name).
%%%
                for_each_node([], _Strategy, _Original_Subprogram_name).
                for_each_node([Id_truth|R], Strategy, Original_Subprogram_name) :-
                        (successor(Id_truth, _Out_set) ->     %25/01/08 only if not already recorded ...
                                (!,
                                 true
                                )
                        ;
                                (build_successor([Id_truth], Strategy, Original_Subprogram_name, [], Out_set),
                                 assert(successor(Id_truth, Out_set))
                                )
                        ),
                        for_each_node(R, Strategy, Original_Subprogram_name).
%%%
                % works for single node
                %based on reachable/3 from C:\sicstus\library\ugraphs.pl
                build_successor([], _Strategy, _Original_Subprogram_name, In_set, Out_set) :-
                        ord_del_element(In_set, 'end', Out_set),          %'end' is deleted
                        !.
                build_successor([First|Rest], Strategy, Original_Subprogram_name, In_set, Out_set) :-
                        (successor(First, Successor_set) ->      %has already been calculated
                                (!,
                                 ord_union(In_set, Successor_set, New_in_set),
                                 Next = Rest
                                )
                        ;
                                (mika_globals:mika_globals__get_NBT('coverage_thoroughness', Coverage_thoroughness),
                                 neighbours(Strategy, First, Coverage_thoroughness, Original_Subprogram_name, Neighbours),
                                 list_to_ord_set(Neighbours, Neighbours_set),
                                 ord_union(In_set, Neighbours_set, New_in_set, New),
                                 append(Rest, New, Next)
                                )
                        ),
                        build_successor(Next, Strategy, Original_Subprogram_name, New_in_set, Out_set).
%%%
                        neighbours('branch', (From, Truth), Coverage_thoroughness, Original_Subprogram_name, Neighbours) :-
                                (arc_bran(From, To1, Truth) ->
                                        ((arc_bran(To1, _, 'true') ->
                                                N1 = [(To1, 'true')]
                                        ;
                                                N1 = []
                                        ),
                                        (arc_bran(To1, _, 'false') ->
                                                N2 = [(To1, 'false')]
                                        ;
                                                N2 = []
                                        ),
                                        (Coverage_thoroughness == 'subprogram_only' ->
                                                Calls = []              %sucessors from calls are ignored
                                        ;
                                                add_all_calls(From, Truth, Coverage_thoroughness, Original_Subprogram_name, Calls)
                                        ),
                                        append(N1, N2, N4),
                                        append(N4, Calls, Neighbours)
                                       )
                                ;
                                        %may be because it is imported
                                        Neighbours = []
                                ),
                                !.
                        neighbours('decision', (From, Truth), Coverage_thoroughness, Original_Subprogram_name, Neighbours) :-
                                (arc_deci(From, To1, Truth) ->
                                        ((arc_deci(To1, _, 'true') ->
                                                N1 = [(To1, 'true')]
                                         ;
                                                N1 = []
                                         ),
                                         (arc_deci(To1, _, 'false') ->
                                                N2 = [(To1, 'false')]
                                         ;
                                                N2 = []
                                         ),
                                         (Coverage_thoroughness == 'subprogram_only' ->
                                                Calls = []              %sucessors from calls are ignored
                                         ;
                                                add_all_calls(From, Truth, Coverage_thoroughness, Original_Subprogram_name, Calls)
                                         ),
                                         append(N1, N2, N4),
                                         append(N4, Calls, Neighbours)
                                        )
                                ;
                                        Neighbours = [] %may be because it is imported
                                ),
                                !.
%%%
                                        %collect all calls in call_tmp which holds a list of (start(xrefed_subprogram_name), true)
                                        %messy because: how can we collect all the matches on backtracking?
                                        add_all_calls(From, Truth, Coverage_thoroughness, Original_Subprogram_name, _Calls) :-
                                                retractall(call_tmp(_)),        %why not use mika_globals:mika_globals__set_NBT(call_tmp, []) instead?
                                                asserta(call_tmp([])),
                                                call_bran(From, Subprogram_name, Truth),
                                                (check_to_be_covered(Subprogram_name, Coverage_thoroughness, Original_Subprogram_name) ->
                                                        ((is_an_instance(Subprogram_name, Recorded_name) ->      %it is actually the Generic_subprogram_name
                                                                true
                                                         ;
                                                          is_a_rename(Subprogram_name, Recorded_name) -> %actually the Original_subprogram_name
                                                                true
                                                         ;
                                                                Recorded_name = Subprogram_name
                                                         ),
                                                         call_tmp(Current),
                                                         retractall(call_tmp(_)),
                                                         asserta(call_tmp([(start(Recorded_name), 'true')|Current]))
                                                        )
                                                ;
                                                        true
                                                ),
                                                fail.
                                        add_all_calls(_From, _Truth, _Coverage_thoroughness, _Original_Subprogram_name, Calls) :-
                                                !,
                                                call_tmp(Calls).

%%%
                                                %e.g. check_to_be_covered(mcdc.adb:22:10:example2, everything_except_rtl, mcdc.adb:114:10:simple)
                                                %e.g. check_to_be_covered(mcdc-something.adb:22:10:example2, everything_except_rtl, mcdc.adb:114:10:simple)
                                                check_to_be_covered(Target_subprogram_name, Coverage_thoroughness, Original_Subprogram_name) :-
                                                        (Coverage_thoroughness == 'package_call_tree' ->
                                                                (mika_symbolic:mika_symbolic__parse(Original_Subprogram_name, Original_Filename, _, _, _, _),
                                                                 mika_symbolic:mika_symbolic__parse(Target_subprogram_name, Target_Filename, _, _, _, _),
                                                                 !,
                                                                 (Original_Filename = Target_Filename ->
                                                                        true    %i.e. the target file name is the same than the target_filename
                                                                 ;
                                                                        (atom_concat(Original_Filename, '-', Prefix),
                                                                         (atom_concat(Prefix, Target_Filename, _Rest) ->
                                                                                true    %i.e. Target_Filename == Original_Filename + '_' + _Rest
                                                                         ;
                                                                                fail
                                                                         )
                                                                        )
                                                                 )
                                                                )
                                                        ;
                                                         Coverage_thoroughness == 'entire_call_tree' ->
                                                                ((atom_concat('a-', _, Target_subprogram_name) ;
                                                                  atom_concat('s-', _, Target_subprogram_name) ;
                                                                  atom_concat('i-', _, Target_subprogram_name) ;
                                                                  atom_concat('g-', _, Target_subprogram_name)) ->
                                                                        fail
                                                                ;
                                                                        true    %i.e. the target file name does not start with something that indicates it belongs to the RTL
                                                                )
                                                        ;
                                                         Coverage_thoroughness == 'everything_including_rtl' ->
                                                                true    %everything so the answer is yes
                                                        ;
                                                                common_util:common_util__error(10, "Unexpected coverage thoroughness: contact Midoan", no_error_consequences, [(coverage_thoroughness, Coverage_thoroughness)], 1025040, mika_coverage, check_to_be_covered, no_localisation, no_extra_info)
                                                        ),
                                                        !.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%used to retrieve a clean list of successors
check_successors(Id, Truth, Successors) :-
        (successor((Id, Truth), Successors_dirty) ->
                (!, true)
        ;
         (Id = start(Subprogram), is_a_rename(Subprogram, Original_subprogram)) ->      %then may be it was not found because it is a rename
                check_successors(start(Original_subprogram), Truth, Successors_dirty)
        ;
         (Id = start(Subprogram), is_an_instance(Subprogram, Generic_subprogram)) ->    %then may be it was not found because it is a generic instantiation
                check_successors(start(Generic_subprogram), Truth, Successors_dirty)
        ;
                Successors_dirty = []
        ),
        mika_list:mika_list__subtract(Successors_dirty, [(start(_), _)], Successors).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%