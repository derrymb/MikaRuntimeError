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
% mika_symbolic__execute_choose_truth.pl
% part of the mika_symbolic module : choose the flow to follow and follow it
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%generate_combinations(bran(1, deci(1, gate(4, or, cond(1, Var_seav(bool.adb:7:18:x, out) > 5), cond(2, Var_seav(bool.adb:7:18:x, out) < 42)))),
%                     [bran(1, deci(1, gate(4, or, cond(1, bool.adb:7:18:x > 5, Var_fd(-100..100) > 5, true), cond(2, bool.adb:7:18:x < 42, Var_fd(-100..100) < 42, true), true), true), true),
%                      bran(1, deci(1, gate(4, or, cond(1, bool.adb:7:18:x > 5, Var_fd(-100..100) > 5, true), cond(2, bool.adb:7:18:x < 42, Var_fd(-100..100) < 42, false), true), true), true),
%                      bran(1, deci(1, gate(4, or, cond(1, bool.adb:7:18:x > 5, Var_fd(-100..100) > 5, false), cond(2, bool.adb:7:18:x < 42, Var_fd(-100..100) < 42, true), true), true), true),
%                      bran(1, deci(1, gate(4, or, cond(1, bool.adb:7:18:x > 5, Var_fd(-100..100) > 5, false), cond(2, bool.adb:7:18:x < 42, Var_fd(-100..100) < 42, false), false), false), false)
%                     ])
%!!!generate_combinations must be able to backtrack if a condition via function calls creates choice points
%i.e. DO NOT try to insert cuts
%The key to our approach especially with respect to generate_combinations is that we symbolically interpret expressions only ONCE
%It is a little messy but it works
%While this could be improved for efficiency by only generating a combination whenever it contains something not yet covered
%  or if the current path contains something new or if it may lead to something new, we feel that the current version allows for easier debugging.
%Until conditions coverage has been tackled we should not do too much simplications on this.
%[09-02-07] Removed coverage information for conditions
%Outcome is one of 'true'|'false'|bitwise_deci(Id_deci, _Id_conds, arg(_Symb, _Cons))|exception(...)
choose_truth(Expression, Outcome) :-
        /*(Expression = deci(5, _) ->
                %trace
        ;
                true
        ),*/
        %trace,
        generate_combinations(Expression, CombinationsL),               %generate a list of combinations in CombinationsL
        mika_globals:mika_globals__get_NBT('strategy', Kind),                          %global branch, decision or condition coverage desired
        (CombinationsL = bitwise_deci(Id_deci, _Id_conds, arg(_Symb, _Cons)) ->
                (Outcome = CombinationsL,                               %because already performed
                 mika_coverage:mika_coverage__remove_bitwise_decision(Kind, Id_deci) %need to update the traversal information : Id_deci has been fully covered (or needs to be removed from list of decisions but may impact cfg too)
                )
        ;
                (mika_globals:mika_globals__get_NBT('driver', Driver),
                 (Driver == 'no_driver' ->                              %normal test inputs generation
                        choose_combination(CombinationsL, Kind, Chosen_combination, Outcome)            %pick the best combination, may have to be redone
                 ;
                  Driver = driver(_Target_subprogram_name_xref) ->      %we are in a driver mode
                        get_next_combination(CombinationsL, Chosen_combination, Outcome)        %pick the next combination, may have to be redone
                 ;
                        common_util:common_util__error(10, "Driver's value is unexpected", "Should never happen", [('driver', Driver)], 10135147, 'mika_symbolic', 'choose_truth', 'no_localisation', 'no_extra_info')
                 ),
                 traverse(Chosen_combination)                            %(Boolean) traverse that combination (may fail)
                )
        ).
%%%
        get_next_combination([], _, _) :-
                common_util:common_util__error(10, "In driver mode, none of the combinations succeeded", "Should never happen", 'no_arguments', 1009311, 'mika_symbolic', 'get_next_combination', 'no_localisation', 'no_extra_info').
        get_next_combination([Next|_Rest], Combination, Outcome) :-
                (Next = combination(Combination, Outcome) ->
                        true
                ;
                 Next = bran(_Id, _Deci, Outcome) ->
                        Combination = Next
                ;
                 Next = deci(_Id_deci, _Deci, Outcome) ->
                        Combination = Next
                ;
                        common_util:common_util__error(10, "Combination is of the wrong format", "Should never happen", '[(next, Next)]', 1009459, 'mika_symbolic', 'get_next_combination', 'no_localisation', 'no_extra_info')
                ).
        get_next_combination([_Next|Rest], Combination, Outcome) :-
                get_next_combination(Rest, Combination, Outcome).
%%
%we leave choice points behind even after a successful labeling of a path
%randomness in the list could be added
%timing could be added : e.g. abort (or better: label what you have so far ...) a subpath after a certain time limit
choose_combination([First|Rest], Kind, Chosen_combination, Outcome) :-                                   %fails if first argument is empty list
        (First = combination(Last_alternative_discrete_choice, Outcome_first) -> %one of the combinations in the last alternative but not the last
                (%we have to analyse a condition but coverage is either decision or branch
                 %Exp can be a single condition cond (..) or an or expression made of ...
                 %identical to below '%Kind == branch but we have to analyse a decision'
                 (common_util:common_util__is_an_exception(Outcome_first) ->
                        (Chosen_combination = First,
                         Outcome = Outcome_first
                        )
                 ;
                        (mika_coverage:mika_coverage__current_path_contain_uncovered(Kind, Current_path_contain_uncovered, _Newly_covered),
                         (mika_coverage:mika_coverage__get_latest_traversed(Kind, (Id, Truth)) ->
                                mika_coverage:mika_coverage__path_may_lead_to_uncovered_not_in_current_path(Kind, (Id, Truth), Path_may_lead_to_uncovered_not_in_current_path)
                         ;
                                %failed because nothing traversed so far
                                Path_may_lead_to_uncovered_not_in_current_path = 'yes'
                         ),
                         ((Current_path_contain_uncovered == 'no' , Path_may_lead_to_uncovered_not_in_current_path == 'no') ->
                                 (mika_globals:mika_globals__get_NBT('to_cover', To_cover),
                                         (To_cover == [] ->     %12/11/10 to ensure that at least one test is generated even if, for example, there is no branches but branch coverage is required
                                                random_branch(0.1, Last_alternative_discrete_choice, Rest, Outcome_first, Kind, Chosen_combination, Outcome)
                                         ;
                                                fail    %see top 06/10/06
                                         )
                                 )
                         ;
                                random_branch(0.1, Last_alternative_discrete_choice, Rest, Outcome_first, Kind, Chosen_combination, Outcome)
                         )
                        )
                 )
                )
        ;
        %the strategy for branches or for decisions is essentially the same
         Kind == 'branch' ->
                (First = bran(Id, _Deci, Outcome_first) ->
                        choose_combination_main(First, Rest, Id, Outcome_first, Kind, Chosen_combination, Outcome)
                ;
                 First = deci(_Id_deci, _Deci, Outcome_first) ->        %tackling a decision during branch coverage
                        (common_util:common_util__is_an_exception(Outcome_first) ->
                                (Chosen_combination = First,
                                 Outcome = Outcome_first
                                )
                        ;
                                (mika_coverage:mika_coverage__current_path_contain_uncovered(Kind, Current_path_contain_uncovered, _Newly_covered),
                                 (Current_path_contain_uncovered == 'yes' ->
                                        random_branch(0.1, First, Rest, Outcome_first, Kind, Chosen_combination, Outcome)
                                 ;
                                        (%we obtain the latest arc followed for the current path
                                         (mika_coverage:mika_coverage__get_latest_traversed(Kind, (Id, Truth)) ->
                                                mika_coverage:mika_coverage__path_may_lead_to_uncovered_not_in_current_path(Kind, (Id, Truth), Path_may_lead_to_uncovered_not_in_current_path)
                                         ;
                                                %failed because nothing traversed so far
                                                Path_may_lead_to_uncovered_not_in_current_path = 'yes'
                                         ),
                                         (Path_may_lead_to_uncovered_not_in_current_path == 'no' ->
                                                (mika_globals:mika_globals__get_NBT('to_cover', To_cover),
                                                 (To_cover == [] ->     %12/11/10 to ensure that at least one test is generated even if, for example, there is no branches but banch coverage is required
                                                        random_branch(0.1, First, Rest, Outcome_first, Kind, Chosen_combination, Outcome)
                                                 ;
                                                        fail    %see top 06/10/06
                                                 )
                                                )
                                         ;
                                                random_branch(0.1, First, Rest, Outcome_first, Kind, Chosen_combination, Outcome)
                                         )
                                        )
                                 )
                                )
                        )
                )
        ;
         (Kind == 'decision' ; Kind == 'mcdc') ->
                (First = bran(_Id, deci(Id, _Exp, Outcome_first), _Outcome) ->
                        choose_combination_main(First, Rest, Id, Outcome_first, Kind, Chosen_combination, Outcome)
                ;
                 First = deci(Id, _Deci, Outcome_first) ->
                        choose_combination_main(First, Rest, Id, Outcome_first, Kind, Chosen_combination, Outcome)
                )
        ;
         Kind == 'condition' ->
                common_util__error(10, "Condition coverage testing not handled", "Cannot proceed", no_arguments, 10154136, mika_symbolic, choose_combination, no_localisation, "Mika limitation")
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Id is a branch id or a decision id depending on Kind
choose_combination_main(First, Rest, Id, Outcome_first, Kind, Chosen_combination, Outcome) :-
        (common_util:common_util__is_an_exception(Outcome_first) ->
                (Chosen_combination = First,
                 Outcome = Outcome_first
                )
        ;
                ((Kind == 'branch' ; Kind == 'decision' ; Kind == 'condition') ->
                        (mika_coverage:mika_coverage__is_in_to_cover((Id, Outcome_first), Is_in_to_cover),
                         (Is_in_to_cover == 'no' ->
                                (Has_been_covered = 'yes',        %it is if
                                 Is_in_the_current_path = 'yes'   %it is if
                                )
                         ;
                                (mika_coverage:mika_coverage__has_been_covered(Kind, (Id, Outcome_first), Has_been_covered),
                                 mika_coverage:mika_coverage__is_in_the_current_path(Kind, (Id, Outcome_first), Is_in_the_current_path)
                                )
                         ),
                         mika_coverage:mika_coverage__current_path_contain_uncovered(Kind, Current_path_contain_uncovered, _Newly_covered),
                         mika_coverage:mika_coverage__path_may_lead_to_uncovered_not_in_current_path(Kind, (Id, Outcome_first), Path_may_lead_to_uncovered_not_in_current_path),
                         (((Has_been_covered, Is_in_the_current_path, Current_path_contain_uncovered, Path_may_lead_to_uncovered_not_in_current_path) == (no, no, no, no)) ->
                                Likelihood_taking_rest = 0.1          % 90% of taking First first and Rest after
                         ;
                         ((Has_been_covered, Is_in_the_current_path, Current_path_contain_uncovered, Path_may_lead_to_uncovered_not_in_current_path) == (no, no, no, yes)) ->
                                Likelihood_taking_rest = 0.1          % 90% of taking First first and Rest after
                         ;
                         ((Has_been_covered, Is_in_the_current_path, Current_path_contain_uncovered, Path_may_lead_to_uncovered_not_in_current_path) == (no, no, yes, no)) ->
                                Likelihood_taking_rest = 0.1          % 90% of taking First first and Rest after
                         ;
                         ((Has_been_covered, Is_in_the_current_path, Current_path_contain_uncovered, Path_may_lead_to_uncovered_not_in_current_path) == (no, no, yes, yes)) ->
                                Likelihood_taking_rest = 0.1          % 90% of taking First first and Rest after
                         ;
                         ((Has_been_covered, Is_in_the_current_path, Current_path_contain_uncovered, Path_may_lead_to_uncovered_not_in_current_path) == (no, yes, no, no)) ->
                                Likelihood_taking_rest = 0.5          % 50/50% of taking First or Rest first
                         ;
                         ((Has_been_covered, Is_in_the_current_path, Current_path_contain_uncovered, Path_may_lead_to_uncovered_not_in_current_path) == (no, yes, no, yes)) ->
                                Likelihood_taking_rest = 0.1          % 90% of taking First first and Rest after
                         ;
                         ((Has_been_covered, Is_in_the_current_path, Current_path_contain_uncovered, Path_may_lead_to_uncovered_not_in_current_path) == (no, yes, yes, no)) ->
                                Likelihood_taking_rest = 0.1          % 90% of taking First first and Rest after
                         ;
                         ((Has_been_covered, Is_in_the_current_path, Current_path_contain_uncovered, Path_may_lead_to_uncovered_not_in_current_path) == (no, yes, yes, yes)) ->
                                Likelihood_taking_rest = 0.1          % 90% of taking First first and Rest after
                         ;
                         ((Has_been_covered, Is_in_the_current_path, Current_path_contain_uncovered, Path_may_lead_to_uncovered_not_in_current_path) == (yes, no, no, no)) ->
                                Likelihood_taking_rest = 1.0          % 100% of taking Rest, First is never offered
                         ;
                         ((Has_been_covered, Is_in_the_current_path, Current_path_contain_uncovered, Path_may_lead_to_uncovered_not_in_current_path) == (yes, no, no, yes)) ->
                                Likelihood_taking_rest = 0.1          % 90% of taking First first and Rest after
                         ;
                         ((Has_been_covered, Is_in_the_current_path, Current_path_contain_uncovered, Path_may_lead_to_uncovered_not_in_current_path) == (yes, no, yes, no)) ->
                                Likelihood_taking_rest = 0.1          % 90% of taking First first and Rest after
                         ;
                         ((Has_been_covered, Is_in_the_current_path, Current_path_contain_uncovered, Path_may_lead_to_uncovered_not_in_current_path) == (yes, no, yes, yes)) ->
                                Likelihood_taking_rest = 0.1          % 90% of taking First first and Rest after
                         ;
                         ((Has_been_covered, Is_in_the_current_path, Current_path_contain_uncovered, Path_may_lead_to_uncovered_not_in_current_path) == (yes, yes, no, no)) ->
                                Likelihood_taking_rest = 1.0          % 100% of taking Rest, First is never offered
                         ;
                         ((Has_been_covered, Is_in_the_current_path, Current_path_contain_uncovered, Path_may_lead_to_uncovered_not_in_current_path) == (yes, yes, no, yes)) ->
                                Likelihood_taking_rest = 0.1          % 90% of taking First first and Rest after
                         ;
                         ((Has_been_covered, Is_in_the_current_path, Current_path_contain_uncovered, Path_may_lead_to_uncovered_not_in_current_path) == (yes, yes, yes, no)) ->
                                Likelihood_taking_rest = 0.1          % 90% of taking First first and Rest after
                         ;
                         ((Has_been_covered, Is_in_the_current_path, Current_path_contain_uncovered, Path_may_lead_to_uncovered_not_in_current_path) == (yes, yes, yes, yes)) ->
                                Likelihood_taking_rest = 0.1          % 90% of taking First first and Rest after
                         ),
                         random_branch(Likelihood_taking_rest, First, Rest, Outcome_first, Kind, Chosen_combination, Outcome)
                        )
                ;
                 Kind == 'mcdc' ->
                        (extract_gates_coverage(First, Gates_about_to_be_covered),
                         mika_coverage:mika_coverage__has_been_covered('mcdc', Id, Has_been_mcdc_covered),   %i.e. all gates, if any, have been covered and overall the decision has been covered
                         (Has_been_mcdc_covered == 'yes' ->
                                calculate_mcdc_likelihood(Id, Outcome_first, Likelihood_taking_rest)
                         ;
                                %(mika_coverage:mika_coverage__has_been_covered('decision', (Id, Outcome_first), Decision_as_been_covered),      %looking at decision coverage first
                                 %(Decision_as_been_covered == 'no' ->
                                 %       Likelihood_taking_rest = 0.1          % 90% of taking First first and Rest after
                                 %;
                                        (mika_coverage:mika_coverage__choice_will_increase_current_gate_coverage(Gates_about_to_be_covered, _Gates_that_increase_coverage, Will_increase_current_mcdc_coverage),
                                         (Will_increase_current_mcdc_coverage == 'yes' ->
                                                Likelihood_taking_rest = 0.1          % 90% of taking First first and Rest after
                                         ;
                                          (mika_coverage:mika_coverage__has_been_covered('decision', (Id, Outcome_first), Decision_has_been_covered),
                                           ((Decision_has_been_covered == 'no', Gates_about_to_be_covered == []) ->      %an atomic uncovered decision
                                                Likelihood_taking_rest = 0.1          % 90% of taking First first and Rest after
                                           ;
                                                calculate_mcdc_likelihood(Id, Outcome_first, Likelihood_taking_rest)
                                           )
                                          )
                                         )
                                        )
                                 %)
                                %)
                         ),
                         random_branch(Likelihood_taking_rest, First, Rest, Outcome_first, Kind, Chosen_combination, Outcome)
                        )
                )
        ).
%%%
        %extracts all the gates coverage information from a combination: purely syntactic
        extract_gates_coverage(bran(_Id_bran, deci(Id_deci, Decision, _Outcome_first), _Outcome), Gate_list) :-
                extract_gates_coverage_exp(Decision, Id_deci, Gate_list).
        extract_gates_coverage(deci(Id_deci, Decision, _Outcome_first), Gate_list) :-
                extract_gates_coverage_exp(Decision, Id_deci, Gate_list).
        extract_gates_coverage_exp(cond(_Id, _Symbolic, _Constraint, _Outcome), _Id_deci, []).
        extract_gates_coverage_exp(gate(Id, Op, Cond_Le, Cond_Ri, _Outcome, Mcdc_coverage), Id_deci, [gate(Id_deci, Id, Op, Mcdc_coverage)|Rest]) :-
                extract_gates_coverage_exp(Cond_Le, Id_deci, R1),
                extract_gates_coverage_exp(Cond_Ri, Id_deci, R2),
                append(R1, R2, Rest).
        extract_gates_coverage_exp(gate(Id, Op, Cond_Le, _Outcome, Mcdc_coverage), Id_deci, [gate(Id_deci, Id, Op, Mcdc_coverage)|Rest]) :-
                extract_gates_coverage_exp(Cond_Le, Id_deci, Rest).
%%%
        calculate_mcdc_likelihood(Id, Outcome_first, Likelihood_taking_rest) :-
                mika_coverage:mika_coverage__current_path_contain_uncovered('mcdc', Path_so_far_increases_mcdc_coverage, _Newly_covered),
                (Path_so_far_increases_mcdc_coverage == 'yes' ->
                        Likelihood_taking_rest = 0.1          % 90% of taking First first and Rest after
                ;
                        (mika_coverage:mika_coverage__path_may_lead_to_uncovered_not_in_current_path('mcdc', (Id, Outcome_first), Path_may_lead_to_uncovered_not_in_current_path),
                         (Path_may_lead_to_uncovered_not_in_current_path == 'yes' ->
                                Likelihood_taking_rest = 0.1          % 90% of taking First first and Rest after
                         ;
                                %has been covered, current choice will not increase coverage, path so far does not increase coverage, but may still need to be covered due to side effects *see is_leap from tomorrow subprogram in array_date where is_leap in mcdc 'subprogram only' does not get covered properly because within 'is_leap' everything is marked as covered since we are not interested in it
                                Likelihood_taking_rest = 0.9          % 90% of taking Rest (was (10/09/10) 1.9 and commented % 90% of taking Rest, First is never offered)
                         )
                        )
                ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%09/11/07 added to add randomness to the control flow graph search
%Bias [0, 1.0]; 0 : always First; 1 : always Rest; x : 100x% for Rest
random_branch(Bias, First, Rest, Outcome_first, Kind, Chosen_combination, Outcome) :-
        ((Rest == [], Bias = 1.0) ->    %added 18/12/09 nothing left but we have to take the rest -> fail and backtrack
                fail
        ;
         Rest == [] ->
                (Chosen_combination = First,
                 Outcome = Outcome_first
                )
        ;
         Bias = 1.0 ->                                          %no choice is given
                choose_combination(Rest, Kind, Chosen_combination, Outcome)
        ;
         Bias = 0.0 ->                                          %no choice is given
                (Chosen_combination = First,
                 Outcome = Outcome_first
                )
        ;
                (random(R),      %float in [0.0 .. 1.0[
                 %may give different path traversal for different runs which may screw your total path number checking for regressions test
                 % if this is a problem see Issue 09/11/07
                 (R > Bias ->
                        ((
                                (Chosen_combination = First,
                                 Outcome = Outcome_first
                                )
                        ;
                                choose_combination(Rest, Kind, Chosen_combination, Outcome)
                        ))
                ;
                        ((
                                choose_combination(Rest, Kind, Chosen_combination, Outcome)
                        ;
                                choose_combination([First], Kind, Chosen_combination, Outcome)   %needed to re-call choose_combination to re-evaluate the current cfg search after the effects of considering Rest
                        ))
                )
               )
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%generate a list of combinations in CombinationsL recursively
%calls symbolically_interpret on leaf expressions
%A list of combinations is generated from a Boolean expression, a combination can be:
%               combination(Last_alternative_discrete_choice, Outcome_first)    for the last alternative of a case statement (which must always be taken)
%                       in that context Last_alternative_discrete_choice is alist of conditions of the form cond(Id_cond, Cond_symb, Cond_cons, Truth)
%               bran(Id, deci(Id, Expression, Outcome), Outcome)    for a branch
%               deci(Id, Expression, Outcome)       for a decision
%where choice is of the form : op(Id, Expression_le, Expression_ri, Truth) where op is 'or', 'and', 'xor', 'or_else' (in which case, Expression_le has a truth of false) or 'and_then' (in which case, Expression_le has a truth of true)
%                              op(Id, Expression_le, Truth) where op is 'not', 'or_else' (in which case, Expression_le has a truth of true) or 'and_then' (in which case, Expression_le has a truth of false)
%                              cond(Id, Cond_symb, Cond_const, Truth)
%                              where the atomic condition has been symbolically interpreted
generate_combinations(bran(Id_bran, Expression), CombinationsL_out) :-
        !,
        generate_combinations(Expression, CombinationsL),
        single_combine(CombinationsL, bran, Id_bran, CombinationsL_out).
generate_combinations(deci(Id_deci, Expression), CombinationsL_out) :-
        !,
        %trace,
        generate_combinations(Expression, CombinationsL),
        (CombinationsL = bitwise_cond(Id_conds, arg(Symb, Cons), _Type_bitwise) ->  % a bitwise expression
                CombinationsL_out = bitwise_deci(Id_deci, Id_conds, arg(Symb, Cons))
        ;
                single_combine(CombinationsL, deci, Id_deci, CombinationsL_out)
        ).
generate_combinations(or_else(Id_gate, Le, Ri), CombinationsL) :-
        !,
        generate_combinations(Le, Combinations_Le),
        generate_combinations(Ri, Combinations_Ri),
        combine(Combinations_Le, Combinations_Ri, or_else, Id_gate, CombinationsL).
generate_combinations(and_then(Id_gate, Le, Ri), CombinationsL) :-
        !,
        generate_combinations(Le, Combinations_Le),
        generate_combinations(Ri, Combinations_Ri),
        combine(Combinations_Le, Combinations_Ri, and_then, Id_gate, CombinationsL).
generate_combinations(not(Id_gate, Le), CombinationsL) :-
        !,
        generate_combinations(Le, Combinations_Le),
        (Combinations_Le = bitwise_cond(Id_cond_Le, arg(Symb_Le, Cons_Le), types(Type)) ->   % a bitwise not operator
                (midoan_solver__interpret(bitwise(not, Cons_Le), types(Type), Const_exp, _Type_bitwise, _Exception_),
                 CombinationsL = bitwise_cond(Id_cond_Le, arg(bitwise(not, Symb_Le), Const_exp), types(Type))
                )
        ;
                combine_unary(Combinations_Le, not, Id_gate, CombinationsL)
        ).
%an alternative in a case statement
generate_combinations(case_alternative_discrete_choices(exp(Exp_symb, Exp_cons), Alternatives), CombinationsL_out) :-
        !,
        Alternatives = bran(Id_bran, deci(Id_deci, Discrete_choicesL)),
        combine_alternative_discrete_choices(exp(Exp_symb, Exp_cons), Discrete_choicesL, CombinationsL2),
        single_combine(CombinationsL2, deci, Id_deci, CombinationsL3),
        single_combine(CombinationsL3, bran, Id_bran, CombinationsL_out).
generate_combinations(cond(Id_cond, Atomic), CombinationsL) :-
        !,
        symbolically_interpret_boolean(Atomic, Symbolic, Constraint, Type, Exception),     %Type could be i, e (for atomic Booleans), modular_integer, array, b
        (common_util:common_util__is_an_exception(Exception) ->
                CombinationsL = [combination(cond(Id_cond, Symbolic, Constraint, Exception), Exception)]  %the exception is propagated
        ;
         Type == 'unhandled_expression' ->      %see note 20/01/10 surely we could do better than generate a fatal error
                common_util__error(10, "Condition contains unhandled entities", 'no_error_consequences', [(id_cond, Id_cond)], 10338125, mika_symbolic, generate_combinations, generate_combinations(cond(_, _), _), no_extra_info)
        ;
         (Type == 'i' ; Type == 'modular_integer' ; Type == 'array')  ->
                 CombinationsL = bitwise_cond([Id_cond], arg(Symbolic, Constraint), types(Type))     %part of a bitwise operator (or integers, modular integers or one dimentional arrays)
        ;
         (Type == 'b' ; Type == 'e')->        %normal Boolean expression
                (%15/01/09 surely we should have only one combination if Constraint is ground
                 CombinationsL = [combination(cond(Id_cond, Symbolic, Constraint, 'true'), 'true'),  combination(cond(Id_cond, Symbolic, Constraint, 'false'), 'false')]
                )
        ;
                common_util__error(10, "Condition's argument is of the wrong type", 'no_error_consequences', [(type, Type), (id_cond, Id_cond)], 10334151, mika_symbolic, generate_combinations, generate_combinations(cond(_, _), _), "Arguments should be an integer, a boolean, a modular integer or a one dimentional array")
        ).
%for binary operators (i.e. and, or, xor)
generate_combinations(Boolean_expression, CombinationsL) :-
        !,
        Boolean_expression =.. [Op, Id_gate, Le, Ri],
        generate_combinations(Le, Combinations_Le),
        generate_combinations(Ri, Combinations_Ri),
        ((Combinations_Le = bitwise_cond(Id_cond_Le, arg(Symb_Le, Cons_Le), types(Type_Le)), Combinations_Ri = bitwise_cond(Id_cond_Ri, arg(Symb_Ri, Cons_Ri), types(Type_Ri))) ->       % a bitwise and operator
                (midoan_solver__interpret(bitwise(Op, Cons_Le, Cons_Ri), types(Type_Le, Type_Ri), Const_exp, Type_bitwise, _Exception_),
                 (Type_bitwise == 'unhandled_expression' ->
                        common_util__error(10, "Condition contains unhandled entities", no_error_consequences, [(operator, Op), (id_gate, Id_gate)], 1027347, mika_symbolic, generate_combinations, generate_combinations(gate, d), no_extra_info)
                 ;
                        (append(Id_cond_Le, Id_cond_Ri, Id_conds),
                         CombinationsL = bitwise_cond(Id_conds, arg(bitwise(Op, Symb_Le, Symb_Ri), Const_exp), types(Type_bitwise))
                        )
                 )
                )
        ;
         (Combinations_Le \= bitwise_cond(_, _, _), Combinations_Ri \= bitwise_cond(_, _, _)) ->        % a boolean operator
                combine(Combinations_Le, Combinations_Ri, Op, Id_gate, CombinationsL)
        ;
                common_util__error(10, "'or' operator's arguments are of the wrong type", no_error_consequences, [(left, Combinations_Le), (right, Combinations_Ri)], 10128829, mika_symbolic, generate_combinations, generate_combinations(or(_, _), _), "both arguments should be exactly bitwise, booleans or arrays")
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%combine takes 2 lists of combination(Exp, Outcome), an operator and returns a list of combinations
combine([], _, _, _Id_gate, []).
combine([First|Rest], L, Op, Id_gate, CombinationsL) :-
        combine(Rest, L, Op, Id_gate, Combinations_rest),
        combine_binary(L, First, Op, Id_gate, Combinations_first),
        append(Combinations_first, Combinations_rest, CombinationsL).

%wrapping the combinations with deci or bran, and the id removing combinations
single_combine([], _, _, []).
single_combine([First|Rest], Kind, Id, Combinations) :-
        single_combine(Rest, Kind, Id, Combinations_rest),
        (Kind == deci ->
                (First = combination(Expression, Outcome),
                 Build =.. [deci, Id, Expression, Outcome]      %tackling deci
                )
        ;
         Kind == bran ->
                (First = deci(_Id, Expression, Outcome),
                 Build =.. [bran, Id, First, Outcome]           %tackling bran
                )
        ),
        append([Build], Combinations_rest, Combinations).

%case conditions
%only used in case_stmt construct
%the various choices are disjuncted i.e. 'OR_else-ed'
%e.g. case X is
%       when 0 | 1 | 2 | 5 => Stmts;
%parsed: alternative(bran(17, deci(19, [cond(28, 0), cond(29, 1), cond(30, 2), cond(31, 5)])), stmts([...]))
%       [combination(cond(28, bool.ads:36:22:x = 0, Var_fd(-100..100) = 0, true), [28 , true], true),
%        combination(or(cond(28, bool.ads:36:22:x = 0, Var_fd(-100..100) = 0, false), cond(29, bool.ads:36:22:x = 1, Var_fd(-100..100) = 1, true), true), [29 , true, 28 , false], true),
%        combination(or(cond(28, bool.ads:36:22:x = 0, Var_fd(-100..100) = 0, false), or(cond(29, bool.ads:36:22:x = 1, Var_fd(-100..100) = 1, false), cond(30, bool.ads:36:22:x = 2, Var_fd(-100..100) = 2, true), true), true), [30 , true, 29 , false, 28 , false], true),
%        combination(or(cond(28, bool.ads:36:22:x = 0, Var_fd(-100..100) = 0, false), or(cond(29, bool.ads:36:22:x = 1, Var_fd(-100..100) = 1, false), or(cond(30, bool.ads:36:22:x = 2, Var_fd(-100..100) = 2, false), cond(31, bool.ads:36:22:x = 5, Var_fd(-100..100) = 5, true), true), true), true), [31 , true, 30 , false, 29 , false, 28 , false], true),
%        combination(or(cond(28, bool.ads:36:22:x = 0, Var_fd(-100..100) = 0, false), or(cond(29, bool.ads:36:22:x = 1, Var_fd(-100..100) = 1, false), or(cond(30, bool.ads:36:22:x = 2, Var_fd(-100..100) = 2, false), cond(31, bool.ads:36:22:x = 5, Var_fd(-100..100) = 5, false), false), false), false), [31 , false, 30 , false, 29 , false, 28 , false], false)
%       ])
%ChoiceL from the parser is of the form [cond(28, 0), cond(29, 1), cond(30, 2), cond(31, [3, 5])] for 0|1|2|3..5
combine_alternative_discrete_choices(exp(Exp_symb, Exp_cons), [cond(Id_cond, Match)], CombinationsL_out) :-
        !,
        coec_check_match(Match, Exp_symb, Exp_cons, Choice_symb, Choice_cons),
        CombinationsL_out = [combination(case_alternative_discrete_choices([cond(Id_cond, Choice_symb, Choice_cons, true)]), true), combination(case_alternative_discrete_choices([cond(Id_cond, Choice_symb, Choice_cons, false)]), false)].
combine_alternative_discrete_choices(exp(Exp_symb, Exp_cons), [cond(Id_cond, Match)|Rest], CombinationsL_out) :-
        combine_alternative_discrete_choices(exp(Exp_symb, Exp_cons), Rest, CombinationsL_rest),
        coec_check_match(Match, Exp_symb, Exp_cons, Choice_symb, Choice_cons),
        combine_binary(CombinationsL_rest, combination(case_alternative_discrete_choices([cond(Id_cond, Choice_symb, Choice_cons, false)]), false), case_alternative_discrete_choices, _, CombinationsL1),
        append([combination(case_alternative_discrete_choices([cond(Id_cond, Choice_symb, Choice_cons, true)]), true)], CombinationsL1, CombinationsL_out).
%%%
%Match can be an expression, others or discrete_with_range (name RANGE range or a range)
%CHECK: but the range could be a function call creating further choice points ...
coec_check_match(Match, Exp_symb, Exp_cons, Choice_symb, Choice_cons) :-
        (Match == 'others' ->
                (Choice_symb = =(Exp_symb, others),
                 midoan_type:standard_type('boolean', Boolean_type_var),
                 midoan_type:midoan_type__get_attribute(Boolean_type_var, last, Choice_cons)
                )
        ;
         Match = discrete_with_range(name, range(_Range)) ->    %similar to subtype_indication see handle_subtype_indication for help: in combine_or_else_case
                common_util__error(10, "Range within the list of choices of an alternative not yet handled", "Cannot proceed", no_arguments, 10436172, mika_symbolic, coec_check_match, coec_check_match(discrete_with_range(name, range(a))), "Mika limitation")
        ;
         Match = discrete_with_range(Match_exp) ->
                (handle_range(Match_exp, range([Min_symb, Max_symb]), range([Min_cons, Max_cons])),
                 Choice_symb = is_in(Exp_symb, [Min_symb, Max_symb]),
                 Choice_cons = is_in(Exp_cons, [Min_cons, Max_cons])
                )
        ;
         midoan_type:midoan_type__is_type(Match) ->
                (handle_range(Match, Type_name, range([Min_cons, Max_cons])),
                 Choice_symb = is_in(Exp_symb, Type_name),
                 Choice_cons = is_in(Exp_cons, [Min_cons, Max_cons])
                )
        ;
         Match = Match_exp ->   %an expression
                (symbolically_interpret(Match_exp, Match_symb, Match_cons, Type, _Exception_),
                 (Type == unhandled_expression ->
                        common_util__error(10, "Choice in case statement contains unhandled entities", no_error_consequences, [(match_exp, Match_exp)], 10451152, mika_symbolic, coec_check_match, no_localisation, no_extra_info)
                 ;
                        (Choice_symb = =(Exp_symb, Match_symb),
                         Choice_cons = =(Exp_cons, Match_cons)
                        )
                 )
                )
        ).

%for binary operators (i.e. and, or, or_else, xor, and_then)
combine_binary([], _, _, _Id_gate, []).
combine_binary([Combination_ri|Rest], Combination_le, Op, Id_gate, Combinations_out) :-
        combine_binary(Rest, Combination_le, Op, Id_gate, Combinations_rest),
        Combination_ri = combination(Expression_ri, Outcome_ri),
        Combination_le = combination(Expression_le, Outcome_le),
        (Op == case_alternative_discrete_choices ->
                (apply_op(or, Outcome_le, Outcome_ri, Outcome),
                 Expression_ri = case_alternative_discrete_choices(Cond_list_ri),
                 Expression_le = case_alternative_discrete_choices(Cond_list_le),
                 append(Cond_list_ri, Cond_list_le, New_cond_list),
                 New_expression =.. [case_alternative_discrete_choices, New_cond_list]
                )
        ;
                (apply_op(Op, Outcome_le, Outcome_ri, Outcome),
                 transform_masked(Op, Outcome_le, Outcome_ri, Expression_le, Expression_ri, New_expression_le, New_expression_ri),
                 (((Op == or_else , Outcome_le == true) ; (Op == and_then , Outcome_le == false)) ->       %for actually short circuited gates
                        New_expression = gate(Id_gate, Op, New_expression_le, Outcome, covers(Outcome_le))
                 ;
                        New_expression = gate(Id_gate, Op, New_expression_le, New_expression_ri, Outcome, covers(Outcome_le, Outcome_ri))
                 )
                )
        ),
        append([combination(New_expression, Outcome)], Combinations_rest, Combinations_out).

%for unary operators (i.e. not, or_else (only in case first argument is true), and_then (only in case first argument is false)
combine_unary([], _, _Id_gate, []).
combine_unary([Combination_le|Rest], Op, Id_gate, Combinations_out) :-
        combine_unary(Rest, Op, Id_gate, Combinations_rest),
        Combination_le = combination(Expression_le, Outcome_le),
        apply_op(Op, Outcome_le, Outcome),
        %no need to check for masking : never applies to any unary boolean operator
        New_expression = gate(Id_gate, Op, Expression_le, Outcome, covers(Outcome_le)),
        append([combination(New_expression, Outcome)], Combinations_rest, Combinations_out).
%%%
%transform the expressions by changing the mcdc coverage if an out operator mask them
transform_masked(Op, Outcome_le, Outcome_ri, Expression_le, Expression_ri, New_expression_le, New_expression_ri) :-
        mask(Op, Outcome_le, Expression_ri, New_expression_ri), %transforms the right operand if it is masked by the left one
        mask(Op, Outcome_ri, Expression_le, New_expression_le). %transforms the left operand if it is masked by the right one

mask('and', Truth, Expression, New_expression) :-
        (Truth == 'true' ->
                New_expression = Expression     %no masking
        ;
                mark_as_masked(Expression, New_expression)
        ).
mask('or', Truth, Expression, New_expression) :-
        (Truth == 'false' ->
                New_expression = Expression     %no masking
        ;
                mark_as_masked(Expression, New_expression)
        ).
mask('xor', _, Expression, Expression).        %no masking
mask('or_else', _, Expression, Expression).    %no masking
mask('and_then', _, Expression, Expression).    %no masking

%03-04-10 : added recursive call so that all inner gates are also marked as masked
mark_as_masked(Expression, New_expression) :-
        (Expression = gate(Id_gate, Op, Expression_le, Outcome, covers(Outcome_le)) ->
                (mark_as_masked(Expression_le, New_expression_le),
                 New_expression = gate(Id_gate, Op, New_expression_le, Outcome, masked(Outcome_le))    %masked
                )
        ;
         Expression = gate(Id_gate, Op, Expression_le, Expression_ri, Outcome, covers(Outcome_le, Outcome_ri)) ->
                (mark_as_masked(Expression_le, New_expression_le),
                 mark_as_masked(Expression_ri, New_expression_ri),
                 New_expression = gate(Id_gate, Op, New_expression_le, New_expression_ri, Outcome, masked(Outcome_le, Outcome_ri))    %masked
                )
        ;
                New_expression = Expression     %no masking because Expression is not a gate (e.g. a cond)
        ).
%%%
%rewritten to remove unwanted non-determinism
apply_op(not, Le, Result) :-
        (Le == true ->
                Result = false
        ;
                Result = true
        ).
apply_op(or, Le, Ri, Result) :-
        (Le == true ->
                Result = true
        ;
         Ri == true ->
                Result = true
        ;
                Result = false
        ).
apply_op(or_else, Le, Ri, Result) :-
        ((Le == true ; Ri == true) ->
                Result = true
        ;
                Result = false
        ).
apply_op(and, Le, Ri, Result) :-
        ((Le == true , Ri == true) ->
                Result = true
        ;
                Result = false
        ).
apply_op(and_then, Le, Ri, Result) :-
        apply_op(and, Le, Ri, Result).
apply_op(xor, Le, Ri, Result) :-
        ((Le == true , Ri == true) ->
                Result = false
        ;
         (Le == false , Ri == false) ->
                Result = false
        ;
                Result = true
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
traverse(bran(Id, Deci, Outcome)) :-
        (common_util:common_util__is_an_exception(Outcome) ->
                true
        ;
                (%03/06/07 here and below
                 % traverse generates another solution : it is the backtracking of mika_coverage__add_to_current_path
                 % which fails but undoes the path ...
                 % so we MUST use if(_, _, _) instead of (_ -> _ ; _)
                 if(traverse(Deci),
                    mika_coverage:mika_coverage__add_to_current_path('branch', (Id, Outcome)),
                   (common_util__error(0, "Path info: FAILED branch\t", 'no_error_consequences', [('id', Id), ('outcome', Outcome)], 052757, mika_symbolic, traverse, no_localisation, no_extra_info),
                    fail
                   )
                 )
                )
        ).
traverse(deci(Id, Exp, Outcome)) :-
        %03/06/07
        %here and above
        % traverse generates another solution : it is the backtracking of mika_coverage__add_to_current_path
        % which fails but undoes the path ...
        % so we MUST use if(_, _, _) instead of (_ -> _ ; _)
        if(traverse_exp(Exp, Id),
           mika_coverage:mika_coverage__add_to_current_path(decision, (Id, Outcome)),
           (common_util__error(0, "Path info: FAILED decision\t", no_error_consequences, [(id, Id), (outcome, Outcome)], 054958, mika_symbolic, traverse, no_localisation, no_extra_info),
            fail
           )
        ).
traverse(case_alternative_discrete_choices(Cond_list)) :-
        (Cond_list = [] ->
                true
        ;
                (Cond_list = [First_cond|Rest_cond],
                 traverse(First_cond),
                 traverse(case_alternative_discrete_choices(Rest_cond))
                )
        ).
traverse(cond(Id, Symbolic, Constraint, Outcome)) :-
        (Outcome == 'true' ->
                (midoan_solver__sdl(Constraint) ->
                        true
                ;
                        (common_util__error(0, "Path info: FAILED true condition\t", 'no_error_consequences', [(id, Id), (symbolic, Symbolic), (constraint, Constraint)], 057459, mika_symbolic, traverse, no_localisation, no_extra_info),
                         fail
                        )
                )
        ;
                (midoan_solver__sdl(not(Constraint)) ->
                        true
                ;
                        (common_util__error(0, "Path info: FAILED false condition\t", 'no_error_consequences', [(id, Id), (symbolic, Symbolic), (constraint, Constraint)], 058201, mika_symbolic, traverse, no_localisation, no_extra_info),
                         fail
                        )
                )
        ),
        mika_coverage:mika_coverage__add_to_current_path('condition', (Id, Outcome)).
%%%

%handles all binary operators
traverse_exp(gate(Id, Op, Cond_Le, Cond_Ri, _Outcome, Mcdc_coverage), Id_deci) :-
        if((traverse_exp(Cond_Le, Id_deci), traverse_exp(Cond_Ri, Id_deci)),
                mika_coverage:mika_coverage__add_to_current_path(mcdc, gate(Id_deci, Id, Op, Mcdc_coverage)),
                fail
        ).
%handles all unary operators
traverse_exp(gate(Id, Op, Cond_Le, _Outcome, Mcdc_coverage), Id_deci) :-
        if(traverse_exp(Cond_Le, Id_deci),
                mika_coverage:mika_coverage__add_to_current_path(mcdc, gate(Id_deci, Id, Op, Mcdc_coverage)),
                fail
        ).
traverse_exp(case_alternative_discrete_choices(Cond_list), _Id_deci) :-
        traverse(case_alternative_discrete_choices(Cond_list)).
traverse_exp(cond(Id, Symbolic, Constraint, Outcome), _Id_deci) :-
        traverse(cond(Id, Symbolic, Constraint, Outcome)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%