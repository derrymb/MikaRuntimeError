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
% mika_coverage.pl
% defines module coverage
% implement heuristics, pruning etc. to achieve a given testing criteria : guides the symbolic execution
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% documentation for non-logical variables
% handled by mika_globals module
%path_nb			: non backtrackable	: integer used to count the paths followed
%%% below are globals for coverage tracking
%covered_bran			: non backtrackable	: list of branches covered by previous tests (list of (Id, true|false))
%covered_deci			: non backtrackable	: list of decisions covered by previous tests (list of (Id, true|false))
%covered_cond			: non backtrackable	: list of conditions covered by previous tests (list of (Id, true|false))
%to_cover                       : non backtrackable	: list of branches or decisions that need to be covered according to, the user defined, level of coverage thoroughness desired
%current_path_bran		: backtrackable		: chronological list of branches in the current path (list of (Id, true|false))
%current_path_deci		: backtrackable		: chronological list of decisions in the current path (list of (Id, true|false))
%current_path_cond		: backtrackable		: chronological list of conditions in the current path (list of (Id, true|false))
%current_path_mcdc_gate         : backtrackable		: chronological list of gates coverage (list of gate(Deci_id, Gate_id, op, covers|masked(Truth [,Truth]))
%overall_mcdc_deci              : non backtrackable     : list of (Deci_id, Gate_list, Remaining_list) with Gate_list made of [gate(Gate_id, Op, Remaining_list)] with Remaining_list made of [(Truth_value, Truth_value)] for the gates or the decision
%%% below are cfg globals
%current_node_bran		: backtrackable		: keep the current branch node (an Id) during CFG extraction
%current_node_deci		: backtrackable		: keep the current decision node (an Id) during CFG extraction
%current_arc_bran 		: backtrackable		: keep the current branch arc (true|false) during CFG extraction
%current_arc_deci 		: backtrackable		: keep the current decision arc (true|false) during CFG extraction
%current_check			: non backtrackable	: used within check_well_formedness in mika_coverage_check.pl as a simple non-logical parameter
%%%
%current_mcdc_deci              : backtrackable		: keep the current decision Id, as a simple non-logical parameter, for building overall_mcdc_deci
%overall_mcdc_deci              : non backtrackable	: %list of (Deci_id, Gate_list, Truth_list) with Gate_list made of [gate(Gate_id, Op, Remaining_list)] with Remaining_list made of [(Truth_value, Truth_value)]
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(mika_coverage, []).

%%%%%%%%%%%
:- use_module(library(ordsets), [ list_to_ord_set/2,
                                  ord_del_element/3,
                                  ord_union/4,
                                  ord_union/3
				]
	     ).

:- use_module(common_util, [ common_util__error/9
                           ]
             ).

:- use_module([library(lists),		%Sicstus list library (remove_duplicates/2)
               mika_unhandled_atts, mika_list, mika_globals]).

%cfg dynamic predicates
%arc(Start, End, true|false), Start and End are labels
:- dynamic arc_bran/3.          %cfg of branches
:- dynamic arc_deci/3.          %cfg of decisions
%call(Start, Subprogram_name, true|false)
:- dynamic call_bran/3.         %subprogram call from a branch
:- dynamic call_deci/3.         %subprogram call from a decision
%is_an_instance(Subprogram_name, Generic_subprogram_name) %addedd 25/01/08
:- dynamic is_an_instance/2.            %used to follow calls of instantiated generics
%is_a_rename(Subprogram_name, Original_subprogram_name) %addedd 07/03/08
:- dynamic is_a_rename/2.               %used to follow calls of renamed subprogram
%successor((Id, Truth), [(Id, Truth)*])
:- dynamic successor/2.                 %successor list for branches or decisions
%%%
:- dynamic tmp_call/1.                  %temporary assert used in add_all_calls/3

:- compile([mika_coverage__build_cfg]).     %main predicates for cfg building
:- compile([mika_coverage__util]).	%utilitarian predicates
:- compile([mika_coverage__check]).      %for post cfg building error checking

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%recording the current subpaths considered
%Kind is 'branch', 'decision', 'mcdc' or 'condition'
%Tag is an element of the current path
%for 'branch', 'decision', and 'condition' it is (Id, Truth) where Id is just a unique number assigned to the branch, decision or condition at parsing time
%     and Truth is 'true' or 'false'
%for 'mcdc' it is gate(gate_id, op, covers|masked(Truth [,Truth]))
%ISSUE : could also print the actual location in the file of the branch, decision or condition via consultation of foo.cond_ids file
%          generated at parsing time
mika_coverage__add_to_current_path(Kind, Tag) :-
	(Tag = (Id, true) ->
                common_util__error(0, "Path info: TRUE\t", no_error_consequences, [(kind, Kind), (id, Id)], 012254, mika_coverage, mika_coverage__add_to_current_path, no_localisation, no_extra_info)
        ;
         Tag = (Id, false)->
                common_util__error(0, "Path info: FALSE\t", no_error_consequences, [(kind, Kind), (id, Id)], 012756, mika_coverage, mika_coverage__add_to_current_path, no_localisation, no_extra_info)
        ;
                true
        ),
	util_get_globals(Kind, _Global_covered, Global_current_path, _Global_overall),
        mika_globals:mika_globals__add_BT_path(Global_current_path, Tag).    %leaves a choice point behind
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%call stack for better assessing whether a mika_coverage__path_may_lead_to_uncovered_not_in_current_path
mika_coverage__update_call_stack(pop, Kind) :-	% a subprogram return
	util_get_stack_name(Kind, Stack_name),
	(mika_globals:mika_globals__get_BT(Stack_name, [_|New_stack]) ->
		mika_globals:mika_globals__set_BT(Stack_name, New_stack)
	;
		true %failed because 1st call (within mika_symbolic) : no need for call stack : nothing to pop
	).
mika_coverage__update_call_stack(push, Kind) :-	% a subprogram call
	(mika_coverage__get_latest_traversed(Kind, (Id, Truth)) ->	%obtain the latest arc followed for the current path
		(util_get_stack_name(Kind, Stack_name),
		 mika_globals:mika_globals__get_BT(Stack_name, Current_stack),
		 mika_globals:mika_globals__set_BT(Stack_name, [from(Id, Truth)| Current_stack])
		)
	;	%failed because 1st call (within mika_symbolic) : no need for call stack : nothing to push
		true
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%adding to the list of branches, decisions or conditions that have already been covered for sure (list of tags)
%only called once after successful labeling
%Kind is 'branch', 'decision', mcdc or 'condition'
mika_coverage__add_to_covered :-
	add_to_covered('branch'),
	add_to_covered('decision'),
        add_to_covered('mcdc'),
	add_to_covered('condition').
%%%
        add_to_covered(Kind) :-
                (Kind == 'mcdc' ->
                        update_coverage_to_overall_mcdc_deci
                ;
                        (util_get_globals(Kind, Global_covered, Global_current_path, _Global_overall),
                         mika_globals:mika_globals__get_BT_path(Global_current_path, Current_path),
                         remove_duplicates(Current_path, Reduced_current_path),		%from Sicstus list library
                         mika_globals:mika_globals__get_NBT(Global_covered, Already_covered),
                         mika_list:mika_list__union(Already_covered, Reduced_current_path, Covered),
                         mika_globals:mika_globals__set_NBT(Global_covered, Covered)
                        )
                ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mika_coverage__is_in_to_cover(Tag, Is_in_to_cover) :-
        mika_globals:mika_globals__get_NBT('to_cover', To_cover),
        (memberchk(Tag, To_cover) ->
                Is_in_to_cover = 'yes'
        ;
                Is_in_to_cover = 'no'
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%indicates whether the passed arc out of a branch, decision or condition was successfully covered in the past
%Kind is 'branch', 'decision', 'mcdc' or 'condition'
%Coverage is out variable yes|no
mika_coverage__has_been_covered(Kind, Tag, Has_been_covered) :-
        util_get_globals(Kind, Global_covered, _Global_current_path, _Global_overall),
	mika_globals:mika_globals__get_NBT(Global_covered, Already_covered),
        (Kind == mcdc ->
                (memberchk((Tag, Gate_list, Truth_list), Already_covered) -> %here Tag is Id_deci and Already_covered is Overall_mcdc_deci
        	        (gates_covered(Gate_list, Truth_list) ->
                	        Has_been_covered = yes
        	        ;
                	        Has_been_covered = no
                        )
                 ;
	                Has_been_covered = yes %really here it means that the Id_deci is not to be covered at all (it is of no interest) before we had this error message : common_util__error(10, "MCDC coverage checking failure : should never happen", no_error_consequences, [(id_deci, Tag)], 103939, mika_coverage, check_coverage_in_overall_mcdc_deci, no_localisation, "There is a problem in Mika")
                )
        ;
                (memberchk(Tag, Already_covered) ->
                        Has_been_covered = yes          %was already covered during a previous successfully labeled path
                 ;
                        Has_been_covered = no           %not part of a successfully labeled path
                )
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Kind is 'branch', 'decision' or 'condition'
%Is_in_the_current_path is out variable yes|no
mika_coverage__is_in_the_current_path(Kind, (Id, Truth), Is_in_the_current_path) :-
        util_get_globals(Kind, _Global_covered, Global_current_path, _Global_overall),
        mika_globals:mika_globals__get_BT_path(Global_current_path, Current_path),
        (memberchk((Id, Truth), Current_path) ->
                Is_in_the_current_path = 'yes'
        ;
                Is_in_the_current_path = 'no'
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Gates_about_to_be_covered is a list of gate(Id_deci, Id, Op, Mcdc_coverage)
mika_coverage__choice_will_increase_current_gate_coverage(Gates_about_to_be_covered, Gates_that_will_increase_coverage, Will_increase_current_coverage) :-
        filter_mcdc_coverage(Gates_about_to_be_covered, Gates_that_may_increase_coverage),
        mika_globals:mika_globals__get_NBT(overall_mcdc_deci, Overall_mcdc_deci),            %list of (Deci_id, Gate_list, Truth_list) with Gate_list made of [gate(Gate_id, Op, Remaining_list)] with Remaining_list made of [(Truth_value, Truth_value)]
        intersect_mcdc_coverage(Gates_that_may_increase_coverage, Overall_mcdc_deci, Gates_that_will_increase_coverage),
        (Gates_that_will_increase_coverage == [] ->
                Will_increase_current_coverage = no
        ;
                Will_increase_current_coverage = yes
        ).

        %some gate coverage never increase mcdc coverage (e.g. FF for and gate and all the masked gates)
        filter_mcdc_coverage([], []).
        filter_mcdc_coverage([gate(Id_deci, Id, Op, Mcdc_coverage)|Rest], Gates_that_may_increase_coverage) :-
                ((Mcdc_coverage = masked(_) ; Mcdc_coverage = masked(_, _) ; (Op == and , Mcdc_coverage = covers(false, false)) ; (Op == or , Mcdc_coverage = covers(true, true))) ->
                        Gates_that_may_increase_coverage = Gates_that_may_increase_coverage_rest
                ;
                        Gates_that_may_increase_coverage = [gate(Id_deci, Id, Op, Mcdc_coverage)|Gates_that_may_increase_coverage_rest]
                ),
                filter_mcdc_coverage(Rest, Gates_that_may_increase_coverage_rest).

        intersect_mcdc_coverage([], _Overall_mcdc_deci, []).
        intersect_mcdc_coverage([gate(Id_deci, Id_gate, Op, Mcdc_coverage)|Rest], Overall_mcdc_deci, Gates_that_will_increase_coverage) :-
                (memberchk((Id_deci, Gate_list, _Truth_list), Overall_mcdc_deci) ->
                        (memberchk(gate(Id_gate, Op, Remaining_list), Gate_list),                %should never fail
                         (Mcdc_coverage = covers(Truth) ->
                                Covers = (Truth)
                         ;
                          Mcdc_coverage = covers(Truth_le, Truth_ri) ->
                                Covers = (Truth_le, Truth_ri)
                         ),
                         (memberchk(Covers, Remaining_list) ->
                                Gates_that_will_increase_coverage = [gate(Id_deci, Id_gate, Op, Mcdc_coverage)|Gates_that_will_increase_coverage_rest]
                         ;
                                Gates_that_will_increase_coverage = Gates_that_will_increase_coverage_rest
                         )
                        )
                ;
                        Gates_that_will_increase_coverage = Gates_that_will_increase_coverage_rest
                ),
                intersect_mcdc_coverage(Rest, Overall_mcdc_deci, Gates_that_will_increase_coverage_rest).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mika_coverage__current_path_contain_uncovered(Kind, Current_path_contain_uncovered, Newly_covered) :-
        (Kind == 'mcdc' ->      %need to check for new gates and also new decisions without gates that may be covered
                (mika_globals:mika_globals__get_BT_path('current_path_mcdc_gate', Gates_about_to_be_covered),        %only holds a list of gates
                 mika_coverage__choice_will_increase_current_gate_coverage(Gates_about_to_be_covered, Newly_covered_gates, Current_path_contain_uncovered_gates),
                 mika_coverage__current_path_contain_uncovered('decision', _Current_path_contain_uncovered_decisions, Newly_covered_decisions), %checking all decisions too
                 mika_globals:mika_globals__get_NBT('overall_mcdc_deci', Overall_mcdc_deci),
                 remove_decisions_with_gates(Newly_covered_decisions, Overall_mcdc_deci, Newly_covered_decisions_without_gates),        %but only considering decisions without gates : decisions with gates are recorded by the gates that increase the coverage above
                 (Newly_covered_decisions_without_gates == [] ->
                        Current_path_contain_uncovered_decisions_without_gates = 'no'
                 ;
                        Current_path_contain_uncovered_decisions_without_gates = 'yes'
                 ),
                 ((Current_path_contain_uncovered_gates == 'no', Current_path_contain_uncovered_decisions_without_gates == 'no') ->
                        (Current_path_contain_uncovered = 'no',
                         Newly_covered = []
                        )
                 ;
                  (Current_path_contain_uncovered_gates == 'no', Current_path_contain_uncovered_decisions_without_gates == 'yes') ->
                        (Current_path_contain_uncovered = 'yes',
                         Newly_covered = Newly_covered_decisions_without_gates
                        )
                 ;
                  (Current_path_contain_uncovered_gates == 'yes', Current_path_contain_uncovered_decisions_without_gates == 'no') ->
                        (Current_path_contain_uncovered = 'yes',
                         Newly_covered = Newly_covered_gates
                        )
                 ;
                  (Current_path_contain_uncovered_gates == 'yes', Current_path_contain_uncovered_decisions_without_gates == 'yes') ->
                        (Current_path_contain_uncovered = 'yes',
                         append(Newly_covered_gates, Newly_covered_decisions_without_gates, Newly_covered)
                        )
                 )
                )
        ;
                (util_get_globals(Kind, Global_covered, Global_current_path, _Global_overall),
                 mika_globals:mika_globals__get_NBT(Global_covered, Already_covered),
                 mika_globals:mika_globals__get_BT_path(Global_current_path, Current_path),
                 remove_duplicates(Current_path, Reduced_current_path),		%from Sicstus list library
                 mika_list:mika_list__subtract(Reduced_current_path, Already_covered, Newly_covered_including_not_wanted),        %i.e. according to coverage thoroughness wanted  not all arcs are desired
                 mika_globals:mika_globals__get_NBT('to_cover', To_cover),    %list of (Id, true|false)
                 mika_list:mika_list__intersection(Newly_covered_including_not_wanted, To_cover, Newly_covered),
                 (Newly_covered == [] ->
                        Current_path_contain_uncovered = 'no'
                 ;
                        Current_path_contain_uncovered = 'yes'
                 )
                )
        ).
        %%%
        remove_decisions_with_gates([], _Overall_mcdc_deci, []).
        remove_decisions_with_gates([(Id_deci, Truth_value)|Rest], Overall_mcdc_deci, Decisions_without_gates) :-
                (member((Id_deci, [], _), Overall_mcdc_deci) ->
                        (Decisions_without_gates = [(Id_deci, Truth_value)|Decisions_without_gates_rest],
                         remove_decisions_with_gates(Rest, Overall_mcdc_deci, Decisions_without_gates_rest)
                        )
                ;
                        remove_decisions_with_gates(Rest, Overall_mcdc_deci, Decisions_without_gates)
                ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%obtain the latest arc followed for the current path
mika_coverage__get_latest_traversed(Kind, (Id, Truth)) :-
        util_get_globals(Kind, _Global_covered, Global_current_path, _Global_overall),
        mika_globals:mika_globals__get_BT_path(Global_current_path, Current_path),
        Current_path = [(Id, Truth)|_].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%according to the cfg and the Kind, following Id via Truth may lead to uncovered nodes that need to be covered and that is not in the current path
%the current call stack needs to be used
%Kind is 'branch', 'decision', 'mcdc' or 'condition'
mika_coverage__path_may_lead_to_uncovered_not_in_current_path(Kind, (Id, Truth), Path_may_lead_to_uncovered_not_in_current_path) :-
	mika_globals:mika_globals__get_NBT(phase, Phase),
        (Phase == 'elaboration' ->        %the cfg does not exist yet
                Path_may_lead_to_uncovered_not_in_current_path = 'yes'
        ;
                (check_successors(Id, Truth, To_be_covered_Reachable),
		 ord_del_element(To_be_covered_Reachable, (Id, Truth), Clean_L),      %deletes (Id Truth) from To_be_covered_Reachable (because it may be part of a loop and appears as one on its own successor)
		 contain_uncovered_not_in_current_path(Clean_L, Kind, Local_path_may_lead_to_uncovered_not_in_current_path),  %Local_path_may_lead_to_uncovered_not_in_current_path is for the current subprogram and subsequent subprogram calls only
	         (Local_path_may_lead_to_uncovered_not_in_current_path == 'yes' ->
		        Path_may_lead_to_uncovered_not_in_current_path = 'yes'
	         ;
		        (util_get_stack(Kind, Current_stack),   %To be sure, also need to look at the successors of the current subprogram calls
		         mc__pmltn_check_all_previous_calls(Current_stack, Kind, Path_may_lead_to_uncovered_not_in_current_path)
		        )
	         )
                )
        ),
	!.
%%%
        mc__pmltn_check_all_previous_calls([], _Kind, no).
        mc__pmltn_check_all_previous_calls([from(Id, Truth)|Previous], Kind, Path_may_lead_to_uncovered_not_in_current_path) :-
                check_successors(Id, Truth, To_be_covered),
                ord_del_element(To_be_covered, (Id, Truth), To_be_covered_clean),
                contain_uncovered_not_in_current_path(To_be_covered_clean, Kind, Contain_uncovered_not_in_current_path),
                (Contain_uncovered_not_in_current_path == yes ->
                        Path_may_lead_to_uncovered_not_in_current_path = yes
                ;
                        mc__pmltn_check_all_previous_calls(Previous, Kind, Path_may_lead_to_uncovered_not_in_current_path)
                ).
%%%
        contain_uncovered_not_in_current_path([], _Kind, no).
        contain_uncovered_not_in_current_path([(Id, Truth)|Rest], Kind, Contain_uncovered_not_in_current_path) :-
                (Kind == mcdc ->
                        mika_coverage__has_been_covered(mcdc, Id, Coverage)
                ;
                        mika_coverage__has_been_covered(Kind, (Id, Truth), Coverage)
                ),
                (Coverage == no ->
                        (mika_coverage__is_in_the_current_path(Kind, (Id, Truth), Is_in_the_current_path),
                         (Is_in_the_current_path == yes ->
                                contain_uncovered_not_in_current_path(Rest, Kind, Contain_uncovered_not_in_current_path)
                         ;
                                Contain_uncovered_not_in_current_path = yes      %original arc truly can lead to an arc that has not been covered and is not in the current path
                         )
                        )
                ;
                        contain_uncovered_not_in_current_path(Rest, Kind, Contain_uncovered_not_in_current_path)
                ),
                !.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Kind is 'branch', 'decision', 'mcdc' or 'condition'
%might fail
mika_coverage__finished(Kind) :-
	get_remaining_to_be_covered(Kind, _Overall_reachable, _Already_covered, To_be_covered),
	(To_be_covered == [] ->
                true
        ;
                (common_util__error(0, "Path info: now trying to cover", no_error_consequences, [(to_be_covered, To_be_covered)], 030615, mika_coverage, mika_coverage__finished, no_localisation, no_extra_info),
                 fail
                )
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Kind is 'branch', 'decision', 'mcdc' or 'condition'
%called when full coverage cannot be achieved only calculates the percentage coverage achieved
mika_coverage__not_finished(Kind, Remaining_to_be_covered, Percentage_achieved, Already_covered) :-
        %trace,
        get_remaining_to_be_covered(Kind, To_cover, Already_covered, Remaining_to_be_covered),
        length(To_cover, Nb_to_cover),
	(Nb_to_cover = 0 ->
	        Percentage_achieved is 100
        ;
	        (length(Remaining_to_be_covered, Nb_remaining_to_be_covered),
		 Percentage_achieved is integer(100.0*(1 - (Nb_remaining_to_be_covered / Nb_to_cover)))
		)
        ).
%To_cover, Already_covered and To_be_covered are lists of (Id, true|false) for 'branch', 'decision' and 'condition'
%for mcdc To_cover and Already_covered is a list of Ids and To_be_covered is a list of (Id_deci, Gate_list, Truth_list)
get_remaining_to_be_covered(Kind, To_cover, Already_covered, To_be_covered) :-
        mika_globals:mika_globals__get_NBT('to_cover', To_cover_id_truth),    %list of (Id, true|false)
        (Kind == 'mcdc' ->
                (get_deci_ids_only(To_cover_id_truth, Ids_to_cover),
                 To_cover = Ids_to_cover,
                 mika_globals:mika_globals__get_NBT('overall_mcdc_deci', Overall_mcdc_deci),
                 get_to_be_covered_from_overall_mcdc_deci(Ids_to_cover, Overall_mcdc_deci, Already_covered, To_be_covered)
                )
        ;
	        (To_cover = To_cover_id_truth,
                 util_get_globals(Kind, Global_covered, _Global_current_path, _Global_overall),
                 mika_globals:mika_globals__get_NBT(Global_covered, Already_covered_dirty),	%list of (Id, true|false) for 'branch', 'decision' and 'condition'
                 mika_list:mika_list__intersection(To_cover_id_truth, Already_covered_dirty, Already_covered),
                 mika_list:mika_list__subtract(To_cover, Already_covered, To_be_covered)
                )
        ).
%%%
        get_deci_ids_only([], []).
        get_deci_ids_only([(Id_deci, _Truth)|Rest], Ids_only) :-
                get_deci_ids_only(Rest, Tmp_ids_only),
                (memberchk(Id_deci, Tmp_ids_only) ->
                        Ids_only = Tmp_ids_only
                ;
                        Ids_only = [Id_deci|Tmp_ids_only]
                ).

        %get the list of reachable decisions with coverage detail that remains to be mcdc covered
        %Overall_mcdc_deci is a list of (Id_deci, Gate_list, Truth_list)
        %Already_covered is a list of Id_deci
        %To_be_covered is a list of (Id_deci, Gate_list, Truth_list)
        get_to_be_covered_from_overall_mcdc_deci([], _Overall_mcdc_deci, [], []).
        get_to_be_covered_from_overall_mcdc_deci([Id_deci|Reachable_rest], Overall_mcdc_deci, Already_covered, To_be_covered) :-
                memberchk((Id_deci, Gate_list, Truth_list), Overall_mcdc_deci), %should never fail
                (gates_covered(Gate_list, Truth_list) ->
                        (Already_covered = [Id_deci|Already_covered_rest],
                         get_to_be_covered_from_overall_mcdc_deci(Reachable_rest, Overall_mcdc_deci, Already_covered_rest, To_be_covered)
                        )
                ;
                        (To_be_covered = [(Id_deci, Gate_list, Truth_list)|To_be_covered_rest],
                         get_to_be_covered_from_overall_mcdc_deci(Reachable_rest, Overall_mcdc_deci, Already_covered, To_be_covered_rest)
                        )
                ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%builds a control flow graph representation of the structure of the source code
mika_coverage__build_cfg(All_contents, Strategy) :-
        mika_globals:mika_globals__set_NBT('overall_mcdc_deci', []),
	%%%start cfg initialisations
	retractall(arc_bran(_, _, _)),
	retractall(arc_deci(_, _, _)),
        retractall(call_bran(_, _, _)),
	retractall(call_deci(_, _, _)),
        retractall(successor(_, _, _)),
        mika_globals:mika_globals__set_BT('current_node_bran', start(elaboration)),
	mika_globals:mika_globals__set_BT('current_node_deci', start(elaboration)),
        mika_globals:mika_globals__set_BT('current_arc_bran', true),
	mika_globals:mika_globals__set_BT('current_arc_deci', true),
        %trace,
	%%%end cfg initialisations
      %  cover(All_contents, _),
        %trace,
	(\+     (%Where it all happens : covers (but leaves choice points behind) the entire code to extract CFG and overall branches, decisions and conditions
                 %was simply : cover(All_contents, _), but made it very difficult to detect unautorised failure of cover
                 %cover should only fail if there are no more choice points
                 %we use 'if' construct rather than -> to be sure to explore all the solutions of cover
                 %NOTE not fool-proof : cover may fail unexpectedly and yet build a correct cfg (but that may be imcomplete (e.g. some parts of the code have not been tackled at all)
                 if(cover(All_contents, _),
                    true,                       %success
                    common_util__error(10, "CFG building failed", no_error_consequences, no_arguments, 1035455, mika_coverage, mika_coverage__build_cfg, no_localisation, "There is a problem in Mika's cfg building")
                   ),
                 create_arc('branch_decision', 'end', 'end'), %the end of the elaboration
                 fail
                )
	),
        %trace,
        calculate_successors(Strategy, 'elaboration'),   %we do this even for subprogram testing because if the elaboration (which needs to be performed) contains branches etc, they need to be followed and successors may need to be checked
	check_well_formedness,	%debugging check in mika_coverage_check.pl
	!.
%%%
%cfg building
%also sets up the current node
create_arc(Kind, Id_bran, Id_deci) :-
	%Kind is never branch because a branch is always also a decision so Kind == branch_decision
	(Kind == decision ->	%a decision not within a branch
		(mika_globals:mika_globals__get_BT(current_node_deci, N),
		 mika_globals:mika_globals__get_BT(current_arc_deci, Tv),        %true or false
		 (arc_deci(N, Id_deci, Tv) ->		%from N to Id_deci while Tv holds
			(%already exists
                         %we can only fail if The current id_branch exist (see 26/09/07)
                         mika_globals:mika_globals__get_BT(current_node_bran, N_bran),
                         mika_globals:mika_globals__get_BT(current_arc_bran, Tv_bran),	%true or false
                         (arc_bran(N_bran, _, Tv_bran) ->
                                (!,
                                 fail   %we can backtrack safely
                                )
                         ;
                                (%we cannot backtrack before the current branch arc needs to be added
                                 mika_globals:mika_globals__set_BT(current_node_deci, Id_deci)       %still
                                )
                         )
			)
		 ;
			(assert(arc_deci(N, Id_deci, Tv)),
			 mika_globals:mika_globals__set_BT(current_node_deci, Id_deci)
                        )
                 )
		)
	;
	 Kind == branch_decision ->	%a branch (and therefore also a decision)
		(mika_globals:mika_globals__get_BT(current_node_bran, N_bran),
		 mika_globals:mika_globals__get_BT(current_arc_bran, Tv_bran),	%true or false
		 mika_globals:mika_globals__set_BT(current_node_bran, Id_bran),	%needs it here to allow proceeding forward
		 (arc_bran(N_bran, Id_bran, Tv_bran) ->		%from N to Id_bran while Tv holds
                        true	%already exists but we don't fail because we need to check for the deci below
		 ;
			assert(arc_bran(N_bran, Id_bran, Tv_bran))
                 ),
		 mika_globals:mika_globals__get_BT(current_node_deci, N_deci),
		 mika_globals:mika_globals__get_BT(current_arc_deci, Tv_deci),        %true or false
		 (arc_deci(N_deci, Id_deci, Tv_deci) ->		%from N to Id_deci while Tv holds
                         fail   %we also backtrack
		 ;
			(assert(arc_deci(N_deci, Id_deci, Tv_deci)),
			 mika_globals:mika_globals__set_BT(current_node_deci, Id_deci)
                        )
                 )
		)
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%only called during cfg construction
add_decision_to_overall_mcdc_deci(Id_deci) :-
        mika_globals:mika_globals__set_BT(current_mcdc_deci, Id_deci),       %used to construct overall_mcdc_deci
        mika_globals:mika_globals__get_NBT(overall_mcdc_deci, Overall_mcdc_deci),
        (memberchk((Id_deci, _Gate_list, _Truth_list), Overall_mcdc_deci) ->
                true    %already present : nothing to do
        ;
                mika_globals:mika_globals__set_NBT(overall_mcdc_deci, [(Id_deci, [], [true, false])|Overall_mcdc_deci])
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%only called during cfg construction
add_gate_to_overall_mcdc_deci(Gate_nb, Op) :-
        mika_globals:mika_globals__get_BT(current_mcdc_deci, Id_deci),
        mika_globals:mika_globals__get_NBT(overall_mcdc_deci, Overall_mcdc_deci),
        (Op == and ->
                Remaining_list = [(true, true), (true, false), (false, true)]   %minimum mcdc testing for a 'and' gate
        ;
         Op == or ->
                Remaining_list = [(false, false), (true, false), (false, true)]   %minimum mcdc testing for a 'or' gate
        ;
         Op == and_then ->
                Remaining_list = [(false), (true, false), (true, true)]   %minimum mcdc testing for a 'and_then' gate
        ;
         Op == or_else ->
                Remaining_list = [(true), (false, false), (false, true)]   %minimum mcdc testing for a 'or_else' gate
        ;
         Op == xor ->
                Remaining_list = [(true, true), (false, false), (false, true), (true, false)]   %'xor' gate is different the list will become empty whenever any 3 truth have been covered
        ;
         Op == not ->
                Remaining_list = [(true), (false)]        %minimum mcdc testing for a 'not' gate
        ),
        (nth(_Nth, Overall_mcdc_deci, (Id_deci, Gate_list, Truth_list), Overall_mcdc_deci_rest) ->
                (memberchk(gate(Gate_nb, _, _), Gate_list) ->
                        true    %already exist : nothing done because nothing to do
                ;
                        (New_overall_mcdc_deci = [(Id_deci, [gate(Gate_nb, Op, Remaining_list)|Gate_list], Truth_list)|Overall_mcdc_deci_rest],   %the gate list of the decision is updated
                         mika_globals:mika_globals__set_NBT(overall_mcdc_deci, New_overall_mcdc_deci)
                        )
                )
        ;
                common_util__error(10, "MCDC cfg update failure : should never happen", no_error_consequences, [(id_deci, Id_deci)], 1063945, mika_coverage, add_gate_to_overall_mcdc_deci, no_localisation, "There is a problem in Mika")
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%updating the coverage within overall_mcdc_deci only called via mika_coverage__add_to_covered in mcdc case
update_coverage_to_overall_mcdc_deci :-
        %trace,
        mika_globals:mika_globals__get_NBT(overall_mcdc_deci, Overall_mcdc_deci),            %list of (Deci_id, Gate_list, Truth_list) with Gate_list made of [gate(Gate_id, Op, Remaining_list)] with Remaining_list made of [(Truth_value, Truth_value)]
        mika_globals:mika_globals__get_BT_path(current_path_deci, Current_path_deci),                %list of decisions in the current path (list of (Id, true|false))
        remove_duplicates(Current_path_deci, Reduced_current_path_deci),		%from Sicstus list library
        mika_list:mika_list__subtract(Reduced_current_path_deci, [(start(_), _)], Clean_current_path_deci),
        update_decisions_coverage_in_overall_mcdc_deci(Clean_current_path_deci, Overall_mcdc_deci, Overall_mcdc_deci_decision_updated),
        mika_globals:mika_globals__get_BT_path(current_path_mcdc_gate, Current_path_mcdc_gate),      %list of gate(Deci_id, Gate_id, Op, covers|masked(Truth [,Truth])) in the current path
        update_gates_coverage_in_overall_mcdc_deci(Current_path_mcdc_gate, Overall_mcdc_deci_decision_updated, New_overall_mcdc_deci),
        mika_globals:mika_globals__set_NBT(overall_mcdc_deci, New_overall_mcdc_deci).

        %update the coverage of the decisions within Overall_mcdc_deci
        update_decisions_coverage_in_overall_mcdc_deci([], Overall_mcdc_deci, Overall_mcdc_deci).
        update_decisions_coverage_in_overall_mcdc_deci([(Id_deci, Truth)|Rest], Overall_mcdc_deci, New_overall_mcdc_deci) :-
                (nth(_Nth, Overall_mcdc_deci, (Id_deci, Gate_list, Truth_list), Overall_mcdc_deci_rest) ->
                        (delete(Truth_list, Truth, New_truth_list),
                         update_decisions_coverage_in_overall_mcdc_deci(Rest, [(Id_deci, Gate_list, New_truth_list)|Overall_mcdc_deci_rest], New_overall_mcdc_deci)
                        )
                ;
                        update_decisions_coverage_in_overall_mcdc_deci(Rest, Overall_mcdc_deci, New_overall_mcdc_deci) %because the Id_deci followed is of no interest, we had an error message : common_util__error(10, "MCDC coverage update failure : should never happen", no_error_consequences, no_arguments, 103639, mika_coverage, update_decisions_coverage_in_overall_mcdc_deci, no_localisation, "There is a problem in Mika")
                ).

        %update the coverage of the gates within Overall_mcdc_deci
        update_gates_coverage_in_overall_mcdc_deci([], Overall_mcdc_deci, Overall_mcdc_deci).
        update_gates_coverage_in_overall_mcdc_deci([gate(Id_deci, Gate_id, Op, Coverage)|Rest], Overall_mcdc_deci, New_overall_mcdc_deci) :-
                ((Coverage = covers(_) ; Coverage = covers(_, _)) ->
                        ((nth(_Nth, Overall_mcdc_deci, (Id_deci, Gate_list, Truth_list), Overall_mcdc_deci_rest), nth(_Nth2, Gate_list, gate(Gate_id, Op, Remaining), Gate_list_rest)) ->
                                ((Coverage = covers(Truth) ->
                                        delete(Remaining, (Truth), New_remaining)
                                 ;
                                  Coverage = covers(Truth_le, Truth_ri) ->
                                        (delete(Remaining, (Truth_le, Truth_ri), New_remaining_tmp),
                                         ((Op == xor, New_remaining_tmp = [_]) ->       %'xor' gate is different the list will become empty whenever any 3 truth have been covered
                                                New_remaining = []
                                         ;
                                                New_remaining = New_remaining_tmp
                                         )
                                        )
                                 ),
                                 Overall_mcdc_deci_updated = [(Id_deci, [gate(Gate_id, Op, New_remaining)|Gate_list_rest], Truth_list)|Overall_mcdc_deci_rest]
                                )
                        ;
                                Overall_mcdc_deci_updated = Overall_mcdc_deci   %not changed (because ignored) we had : common_util__error(10, "MCDC coverage update failure : should never happen", no_error_consequences, no_arguments, 1065513, mika_coverage, update_gates_coverage_in_overall_mcdc_deci, no_localisation, "There is a problem in Mika")
                        )
                ;
                        Overall_mcdc_deci_updated = Overall_mcdc_deci   %not changed (because masked)
                ),
                update_gates_coverage_in_overall_mcdc_deci(Rest, Overall_mcdc_deci_updated, New_overall_mcdc_deci).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%decides if a decision (made up of a Truth_list remaining to be covered, and a list of gates with remaining coverage information) is fully mcdc covered
gates_covered(Gate_list, Truth_list) :-
        (Truth_list \= [] ->
                fail
        ;
         Gate_list == [] ->
                true
        ;
         memberchk(gate(_Gate_id, _Op, [_|_]), Gate_list) ->   %there is a gate in the gate list that has at least one uncovered test
                fail
        ;
                true
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%calculate the list of successors for the subprogram under test and set 'to_cover' and, if necessary 'overall_mcdc_deci'
mika_coverage__post_elaboration(Strategy, Subprogram_name_xref) :-
        (Subprogram_name_xref == 'elaboration' ->
                true
        ;
                calculate_successors(Strategy, Subprogram_name_xref)    %calculate the necessary successor information
        ),
        check_successors(start(Subprogram_name_xref), true, To_cover),	        %list of (Id, true|false) that we need to cover according to depth of coverage desired by user
        mika_globals:mika_globals__set_NBT('to_cover', To_cover),
        (Strategy == 'mcdc' ->
                update_overall_mcdc_deci_according_to_to_cover(To_cover)
        ;
                true
        ).
%%%
        update_overall_mcdc_deci_according_to_to_cover(To_cover) :-
                mika_globals:mika_globals__get_NBT('overall_mcdc_deci', Overall_mcdc_deci),            %list of (Deci_id, Gate_list, Truth_list) with Gate_list made of [gate(Gate_id, Op, Remaining_list)] with Remaining_list made of [(Truth_value, Truth_value)]
                overall_mcdc_deci_intersect_to_cover(Overall_mcdc_deci, To_cover, To_cover_mcdc_deci),
                mika_globals:mika_globals__set_NBT('overall_mcdc_deci', To_cover_mcdc_deci).
%%%
                overall_mcdc_deci_intersect_to_cover([], _To_cover, []).
                overall_mcdc_deci_intersect_to_cover([(Deci_id, Gate_list, Truth_list)|Rest], To_cover, To_cover_mcdc_deci) :-
                        (memberchk((Deci_id, _), To_cover) ->
                                To_cover_mcdc_deci = [(Deci_id, Gate_list, Truth_list)|Rest_to_cover_mcdc_deci]
                        ;
                                To_cover_mcdc_deci = Rest_to_cover_mcdc_deci
                        ),
                        overall_mcdc_deci_intersect_to_cover(Rest, To_cover, Rest_to_cover_mcdc_deci).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%remove ID_deci from the cfg, from to_cover, from successors, and, if Strategy = 'mcdc', from 'overall_mcdc_deci' too
mika_coverage:mika_coverage__remove_bitwise_decision(Kind, Id_deci) :-
        (Kind == 'branch' ->
                true    %we are only removing decisions : so there's is no need to do anything in the branch coverage case
        ;
                (mika_coverage__is_in_to_cover((Id_deci, _), Is_in_to_cover),
                 (Is_in_to_cover == 'yes' ->
                        (remove_bitwise_cfg(Id_deci, Origin_IdL),               %removing from the cfg means removing the arcs from Id_deci and updating all the arcs to ID_deci (we keep all Origin_Ids to be used below)
                         remove_bitwise_cover(Id_deci),                         %removing from to_cover means updating to_cover by removing (Id_deci, _) from it
                         remove_bitwise_successors(Id_deci, Origin_IdL),        %removing from successor means removing from the successors of all the Origin_Ids AND the predecessors of all the Origin_Ids
                         remove_bitwise_overall_mcdc_deci(Id_deci)              %removing from overall_mcdc_deci means removing (Id_deci, ...) from it
                        )
                 ;
                        true    %has already been remove previously : we are in backtracking mode
                 )
                )
        ).
%%%
        remove_bitwise_cfg(Id_deci, Origin_IdL) :-
                get_predecessors(Id_deci, Origin_IdL),
                arc_deci(Id_deci, Destination, _),      %destination is unique because is a bitwise decision
                !,
                retractall(arc_deci(Id_deci, _, _)),
                modify_origins(Origin_IdL, Destination).
%%%
                get_predecessors(Id_deci, PredecessorsL) :-
                        mika_globals:mika_globals__set_NBT(origin_ids, []),
                        \+      (arc_deci(Origin, Id_deci, Truth),
                                 mika_globals:mika_globals__get_NBT(origin_ids, Predecessors_partialL),
                                 (member((Origin, Truth), Predecessors_partialL) ->
                                        true
                                 ;
                                        mika_globals:mika_globals__set_NBT(origin_ids, [(Origin, Truth)|Predecessors_partialL])
                                 ),
                                 fail
                                ),
                        mika_globals:mika_globals__get_NBT(origin_ids, PredecessorsL).
%%%
                modify_origins([], _Destination).
                modify_origins([(Origin, Truth)|Rest], Destination) :-
                        retract(arc_deci(Origin, _, Truth)),    %can only match one
                        !,
                        assert(arc_deci(Origin, Destination, Truth)),
                        modify_origins(Rest, Destination).
%%%
        remove_bitwise_cover(Id_deci) :-
                mika_globals:mika_globals__get_NBT('to_cover', To_cover),
                mika_list:mika_list__subtract(To_cover, [(Id_deci, 'true'), (Id_deci, 'false')], New_to_cover),
                mika_globals:mika_globals__set_NBT('to_cover', New_to_cover).
%%%
        remove_bitwise_successors(Id_deci, Origin_IdL) :-
                retract(successor((Id_deci, 'true'), _)),    %can only match one
                retract(successor((Id_deci, 'false'), _)),    %can only match one
                !,
                %need to remove [(Id_deci, 'true'), (Id_deci, 'false')] in the successor list of all the predecessors of Id_deci (the first predecessors are in Origin_IdL)
                remove_from_predecessors(Origin_IdL, Id_deci).
%%%
                remove_from_predecessors([], _Id_deci).
                remove_from_predecessors([(Id, Truth)|Rest], Id_deci) :-
                        successor((Id, Truth), Successors),
                        mika_list:mika_list__subtract(Successors, [(Id_deci, 'true'), (Id_deci, 'false')], New_successors),
                        retract(successor((Id, Truth), _)),     %can only match one
                        !,
                        assert(successor((Id, Truth), New_successors)),
                        get_predecessors(Id, Predecessors),
                        append(Predecessors, Rest, New_predecessors),
                        remove_from_predecessors(New_predecessors, Id_deci).
%%%
        remove_bitwise_overall_mcdc_deci(Id_deci) :-
                mika_globals:mika_globals__get_NBT('overall_mcdc_deci', Overall_mcdc_deci),     %list of (Deci_id, Gate_list, Truth_list) with Gate_list made of [gate(Gate_id, Op, Remaining_list)] with Remaining_list made of [(Truth_value, Truth_value)]
                mika_list:mika_list__subtract(Overall_mcdc_deci, [(Id_deci, _, _)], New_overall_mcdc_deci),
                !,
                mika_globals:mika_globals__set_NBT('overall_mcdc_deci', New_overall_mcdc_deci).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%