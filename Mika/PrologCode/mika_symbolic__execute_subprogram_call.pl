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
% mika_symbolic__execute_subprogram_call.pl
% part of the mika_symbolic module : handle subprogram calls
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%subprogram calls : handles all procedure and function with or without parameters
%The Symbolic, Constraint and Type output variables are needed during interpretation only (i.e. for function calls) :
%  handled with mika_symbolic__interpret.pl
% procedure calls are actual statements: handled within mika_symbolic__execute.pl
handle_subprogram_call(Functor, Operand, Symbolic, Constraint, Type, Exception) :-
        mika_globals:mika_globals__get_NBT('strategy', Strategy),                  %global branch, decision or condition coverage desired
        mika_sub_atts:mika_sub_atts__get('status', Functor, Status),
        mika_sub_atts:mika_sub_atts__get('name', Functor, Subprogram_name),
        (Status == 'bodied' ->    %a normal subprogram whose body is available
                (mika_coverage:mika_coverage__update_call_stack('push', Strategy),
                 mika_coverage:mika_coverage__add_to_current_path('condition', (start(Subprogram_name), true)),
                 mika_coverage:mika_coverage__add_to_current_path('decision', (start(Subprogram_name), true)),
                 mika_coverage:mika_coverage__add_to_current_path('branch', (start(Subprogram_name), true)),
                 mika_sub_atts:mika_sub_atts__get('params', Functor, Params),
                 mika_sub_atts:mika_sub_atts__get('decls', Functor, Decls),
                 mika_sub_atts:mika_sub_atts__get('body', Functor, Body),
                 mika_sub_atts:mika_sub_atts__get('return', Functor, Return),
                 my_copy_term('locals', a(Return, Params, Decls, Body), a(Return_c, Params_c, Decls_c, Body_c)),
                 declare_params(Params_c),      %declare parameters as unused Seav variables (possibly with initialisation)
                 %trace,
                 freeze_arguments(Operand, Operand_frozen),
                 (check_for_unhandled(Operand_frozen, Has_unhandled),
                        (Has_unhandled == 'no' ->
                                (match_params(Operand_frozen, Params_c, pre),        %in and in_out parameters are assigned THE passed operands
                                 (Return == no_return ->        %it is a procedure
                                        true
                                 ;
                                        (mika_sub_atts:mika_sub_atts__get(return_type, Functor, Subtype_indication_or_access_definition),
                                         exec(object(not_qualified, [Return_c], Subtype_indication_or_access_definition, no_init), carry_on) %we declare the return variable
                                        )
                                 ),
                                 exec(Decls_c, carry_on),
                                 %garbage_collect,
                                 exec(Body_c, Flow),
                                 mika_coverage:mika_coverage__update_call_stack('pop', Strategy),
                                 (common_util:common_util__is_an_exception(Flow) ->
                                        Exception = Flow
                                 ;
                                        ((Return == 'no_return' ->        %it is a procedure : Flow could be 'carry_on' or 'return'
                                                match_params(Operand_frozen, Params_c, post)
                                         ;                              %it is a function : Flow must be 'return'
                                                (Flow == 'return' ->
                                                        (symbolically_interpret(Return_c, Symbolic, Constraint, Type, _Exception_),
                                                         (Type == unhandled_expression ->
                                                                (mika_name_atts:mika_name_atts__get(name, Return, Return_name),
                                                                 create_unhandled(Constraint, function_call_no_return, "return contains unhandled entities")
                                                                )
                                                         ;
                                                                true
                                                         )
                                                        )
                                                %;
                                                % Flow = exception_raised(_Exception_name, _String_Expression) ->
                                                %        true
                                                ;
                                                        common_util__error(10, "Control path in a function without a return statement", "Cannot proceed", [(subprogram_name, Subprogram_name)], 1068189, mika_symbolic, handle_subprogram_call, no_localisation, "Should never happen")
                                                )
                                         )
                                        )
                                 )
                                )
                        ;
                                (%some of the arguments are unhandled
                                 Type = 'unhandled_expression',
                                 (Return == 'no_return' ->
                                        mika_unhandled_atts:mika_unhandled_atts__create(Constraint, Subprogram_name, procedure_call)
                                 ;
                                        (mika_name_atts:mika_name_atts__get(name, Return, Return_name),
                                         mika_unhandled_atts:mika_unhandled_atts__create(Constraint, Return_name, function_call_return)
                                        )
                                 )
                                )
                        )
                 )
                )
        ;
         Status = rename_of_default(Operator) ->     %06/05/08
                (freeze_arguments(Operand, Operand_frozen),     %!! we do not check the order of the operands ...
                 check_for_unhandled(Operand_frozen, Has_unhandled),
                 (Has_unhandled == 'no' ->
                        (Call =.. [Operator|Operand_frozen],
                         symbolically_interpret(Call, Symbolic, Constraint, Type, _Exception_),
                         (Type == 'unhandled_expression' ->
                                create_unhandled(Constraint, 'renameof_default_operator', "Call to a renamed operator contains unhandled entities")
                         ;
                                true
                         )
                        )
                 ;
                         create_unhandled(Constraint, 'renameof_default_operator', "Renamed operator operands contains unhandled entities")
                 )
                )
        ;
         Status = rename_of(Orig_sub_var) ->     %a renamed as spec subprogram whose body is not yet set
                (create_subprogram_rename(Functor, Orig_sub_var),
                 handle_subprogram_call(Functor, Operand, Symbolic, Constraint, Type, Exception)   %we actually perform the call on a full bodied subprogram
                )
        ;
         Status == 'imported' ->     %an imported subprogram
                handle_imported_subprogram(Functor, Operand, Symbolic, Constraint, Type)
        ;
                (%12/10/09 trying to handle calling a subprogram that has no body yet (see 'Non standard Ada' issue)
                 mika_globals:mika_globals__get_NBT('phase', Phase),
                 (Phase == 'elaboration' ->
                        (mika_sub_atts:mika_sub_atts__get('delayed_calls', Functor, Delayed_calls),
                         append([call(Operand)], Delayed_calls, New_delayed_calls),
                         mika_sub_atts:mika_sub_atts__update('delayed_calls', Functor, New_delayed_calls),
                         common_util__error(9, "Delayed call to a subprogram made", "May be unsound", [(name, Subprogram_name), (status, Status)], 912619, mika_symbolic, handle_subprogram_call, no_localisation, "non standard Ada?")
                        )
                 ;
                        common_util__error(10, "Trying to call a subprogram that has no body yet, is not a rename of another subprogram nor is it imported", "Cannot proceed", [(name, Subprogram_name), (status, Status)], 10103200, mika_symbolic, handle_subprogram_call, no_localisation, "Should never happen")
                 )
                )
        ),
        ((mika_globals:mika_globals__get_NBT('phase', 'post_elaboration'), mika_globals:mika_globals__get_NBT('driver', driver(Subprogram_name))) ->   %we are after the elaboration phase and executing the driver and the target subprogram matches : we nee to monitor the coverage achieved
                (%trace,
                 mika_coverage:mika_coverage__current_path_contain_uncovered(Strategy, Current_path_contain_uncovered, Newly_covered),
                 mika_globals:mika_globals__get_NBT('test_driver_test_nb', Test_driver_test_nb),
                 Test_driver_test_nb_1 is Test_driver_test_nb + 1,
                 mika_globals:mika_globals__set_NBT('test_driver_test_nb', Test_driver_test_nb_1),
                 format('answer_output', "\nTest Driver Test Number ~w:\n", [Test_driver_test_nb_1]),
                 (Current_path_contain_uncovered == 'no' ->
                        format('answer_output', "\nDoes not increase the coverage: i.e. is redundant\n", [])
                 ;
                        mika_symbolic:print_newly_covered(Newly_covered, Strategy)
                 ),
                 mika_coverage:mika_coverage__add_to_covered	%the coverage achieved is updated
                )
        ;
                true
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
create_subprogram_rename(Sub_rename, Orig_sub_var1) :-
        rename_of_rename(Orig_sub_var1, Orig_sub_var),
        %need to create a proper bodied subprogram with the parameters properly renamed
        %similar to handle_generic_subprogram_instantiation
        %trace,
        mika_sub_atts:mika_sub_atts__get(name, Sub_rename, Name),
        mika_sub_atts:mika_sub_atts__get(return, Sub_rename, Return),
        mika_sub_atts:mika_sub_atts__get(params, Sub_rename, Params),
        mika_sub_atts:mika_sub_atts__get(return, Orig_sub_var, Orig_return),
        mika_sub_atts:mika_sub_atts__get(return_type, Orig_sub_var, Orig_return_type),
        mika_sub_atts:mika_sub_atts__get(params, Orig_sub_var, Orig_params),
        mika_sub_atts:mika_sub_atts__get(decls, Orig_sub_var, Orig_decls),
        mika_sub_atts:mika_sub_atts__get(body, Orig_sub_var, Orig_body),
        my_copy_term('locals', a(Orig_return, Orig_params, Orig_decls, Orig_body), a(Return_c, Params_c, Decls_c, Body_c)),
        (Return == no_return ->
                true
        ;
                ((mika_name_atts:mika_name_atts__is_name_atts(Return_c) ->
                        mika_name_atts:mika_name_atts__unput(Return_c, _) %to allow matching to take place below
                 ;
                        true
                 ),
                 Return_c = Return
                )
        ),
        match_renamed_params(Params_c, Params),
        mika_sub_atts:mika_sub_atts__unput(Sub_rename),
        mika_sub_atts:mika_sub_atts__create(Sub_rename, Name, bodied, Return_c, Orig_return_type, Params, Decls_c, Body_c, []).

rename_of_rename(Orig_sub_var_in, Orig_sub_var_out) :-
        mika_sub_atts:mika_sub_atts__get(status, Orig_sub_var_in, Status),
        (Status = rename_of(Orig_sub_var_tmp) ->
                rename_of_rename(Orig_sub_var_tmp, Orig_sub_var_out)
        ;
                Orig_sub_var_out = Orig_sub_var_in
        ).

%need to match the names of the parameters one by one in order
%also used as a template for check_params_matching in mika_symbolic__execute_util.pl
match_renamed_params([], []) :-
        !.
match_renamed_params([], [param([], _, _, _)]) :-
        !.
match_renamed_params([param([], _, _, _)|Rest_params], Params_renames) :-
        !,
        match_renamed_params(Rest_params, Params_renames).
match_renamed_params([param([Param|Rest_ids], _, _, _)|Rest_params], Params_renames) :-
        !,
        match_renamed_params_single(Params_renames, Param, Params_renames_rest),
        match_renamed_params([param(Rest_ids, _, _, _)|Rest_params], Params_renames_rest).

match_renamed_params_single([param([], _, _, _)|Rest_params_renames], Param, Params_renames_rest) :-
        !,
        match_renamed_params_single(Rest_params_renames, Param, Params_renames_rest).
match_renamed_params_single([param([Param_rename|Rest_ids], _, _, _)|Rest_params_renames], Param, [param(Rest_ids, _, _, _)|Rest_params_renames]) :-
        !,
        (Param_rename == Param ->
                true                            %nothing to do since they are already identical 28/04/09
        ;
                (mika_name_atts:mika_name_atts__unput(Param, _),  %to allow matching to take place below
                 Param = Param_rename
                )
        ).
match_renamed_params_single(_, _, _) :-
        common_util__error(10, "Parameters do not match in subprogram rename", "Cannot proceed", no_arguments, 1066125, mika_symbolic, match_renamed_params_single, no_localisation, no_extra_info).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Arg is a passed argument
% Arg_frozen contains a mixture of record, array seavs or a single seav as well as 'selected' and 'indexed' expressions.
% The rest are solver variables.
%13/11/07 : should only really be done for arguments with 'out' and 'in_out' parameter modes
%see Issue 'Freezing subprogram calls arguments'
freeze_arguments([], []) :-
        !.
freeze_arguments([Arg|Rest], [Arg_frozen|Rest_frozen]) :-
        !,
        ((compound(Arg), Arg = named([Name], Value)) ->
                (% a named parameter
                 freeze_arguments([Value], [Frozen_value]),
                 Arg_frozen = named([Name], Frozen_value)
                )
        ;
                (mika_seav_atts:mika_seav_atts__is_seav(Arg) ->
                        Arg_frozen = Arg         %a seav : left 'as is'
                ;
                 mika_sub_atts:mika_sub_atts__is_sub_atts(Arg) ->
                        (%a parameterless function call [19/03/08] : according to Ada RM 6.4.1 arguments for 'out' and 'in_out' parameters must be a name that denotes a variable; thus subprogram calls are forbidden
                         symbolically_interpret(Arg, _Symb, Const, Type, _Exception_),
                         (Type == unhandled_expression ->
                                common_util__error(6, "Argument contains unhandled entities", no_error_consequences, no_arguments, 619906, mika_symbolic, freeze_arguments, no_localisation, no_extra_info)
                         ;
                                true
                         ),
                         Arg_frozen = frozen(Const)
                        )
                ;
                 Arg = selected(Name, Field) ->
                        (remove_selected_package(Name, Prefix_less_name),
                         freeze_arguments([Prefix_less_name], [Frozen_name]),       %should only be done for record vars (could be a prefix)
                         % we do do 'symbolically_interpret(Field, _Field_symb, Field_const, _Field_type),' here
                         % because it will return the name of the field
                         % Field is a name var as it should be
                         Arg_frozen = selected(Frozen_name, Field)   %Out_frozen_name is a mixture, Field_const is a solver var
                        )
                ;
                 Arg = indexed(Name, Index) ->
                        (remove_selected_package(Name, Prefix_less_name),
                         (midoan_type:midoan_type__is_type(Prefix_less_name) -> %a 'view conversion' conversion no need for freezing argument
                                (freeze_arguments(Index, Frozen_index),
                                 Arg_frozen = indexed(Prefix_less_name, Frozen_index)                %changed [26/11/07]
                                )
                         ;
                          mika_sub_atts:mika_sub_atts__is_sub_atts(Prefix_less_name) -> %a function call [19/03/08] : according to Ada RM 6.4.1 arguments for 'out' and 'in_out' parameters must be a name that denotes a variable; thus subprogram calls are forbidden
                                (symbolically_interpret(indexed(Prefix_less_name, Index), _Symb, Const, Type, _Exception_),
                                 (Type == unhandled_expression ->
                                        common_util__error(6, "Argument contains unhandled entities", no_error_consequences, no_arguments, 6222141, mika_symbolic, freeze_arguments, no_localisation, no_extra_info)
                                 ;
                                        true
                                 ),
                                 Arg_frozen = frozen(Const)
                                )
                         ;
                                (freeze_arguments([Prefix_less_name], [Frozen_name]),       %should only be done if Name is array var (could be a function call)
                                 symbolically_interpret(Index, _Index_symb, Index_const, Index_type, _Exception_),
                                 (is_unhandled_type([Index_type]) ->
                                        common_util__error(6, "Argument contains unhandled entities", no_error_consequences, no_arguments, 6231144, mika_symbolic, freeze_arguments, no_localisation, no_extra_info)
                                 ;
                                        true
                                 ),
                                 Arg_frozen = indexed(Frozen_name, frozen(Index_const))    %Out_frozen_name is a mixture, Index_const is a solver var
                                )
                         )
                        )
                ;
                        (% anything else is an expression
                         symbolically_interpret(Arg, _Symb, Const, Type, _Exception_),
                         (is_unhandled_type([Type]) ->
                                common_util__error(6, "Argument contains unhandled entities", "Cannot proceed", no_arguments, 6241135, mika_symbolic, freeze_arguments, no_localisation, no_extra_info)
                         ;
                                true
                         ),
                         Arg_frozen = frozen(Const)
                        )
                )
        ),
        freeze_arguments(Rest, Rest_frozen).
freeze_arguments(ArgL, Arg_frozenL) :-  %should never be taken : freeze_arguments/2 should never fail
        !,
        common_util__error(10, "Failure detected: that should never fail", "Cannot proceed", [(argl, ArgL), (arg_frozenl, Arg_frozenL)], 10225142, mika_symbolic, freeze_arguments, no_localisation, "Should never happen").
%%%
        %e.g. selected(selected(selected(ada, calendar), arithmetic), day_count)
        remove_selected_package(selected(Selector, Selected), Entity) :-
                !,
                (Selector = selected(Inner_selector, Inner_selected) ->
                        remove_selected_package(selected(Inner_selector, Inner_selected), Inner_entity)
                ;
                        Inner_entity =  Selector
                ),
                (mika_package_atts:mika_package_atts__is_package_atts(Inner_entity) ->
                        Entity = Selected
                ;
                        Entity = selected(Inner_entity, Selected)
                ).
        remove_selected_package(Entity, Entity).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%declare subprogram variables as unused Seav variables
%we do this this way (preserving the param(NameL, Mode, Type, Init)) to keep the Mode and Init value after declaration :
%  they will be used during matching
%changed 15/04/2009: we now use normal object declaraction for declaring unused parameters.
declare_params([]).
declare_params([param(NameL, _Mode, Subtype_indication_or_access_definition, Init)|Rest]) :-
        exec(object(not_qualified, NameL, Subtype_indication_or_access_definition, Init), carry_on),
        declare_params(Rest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%match parameter passed to those declared
%can be named passed or positional
match_params([], _Declared_params, _Matching_mode).
match_params([First|Rest], Declared_params, Matching_mode) :-
        ((compound(First), First = named(_Names, _Value)) ->
                match_params_named([First|Rest], Declared_params, Matching_mode)
        ;
                match_params_positional(Declared_params, [First|Rest], Matching_mode)
        ).
%%%
match_params_named([], _Declared_params,  _Matching_mode).
match_params_named([named([Name], Operand)|Rest_operands], Declared_params, Matching_mode) :-
        mika_name_atts:mika_name_atts__get(name, Name, Operand_name),
        match_params_single_named(Declared_params, Operand_name, Operand, Matching_mode),
        match_params_named(Rest_operands, Declared_params, Matching_mode).

match_params_single_named([], Operand_name, Operand, _Matching_mode) :-
        !,
        common_util__error(10, "Parameter not found during parameter matching named", "Cannot proceed", [(operand_name, Operand_name), (operand, Operand)], 10276162, mika_symbolic, match_params_single_named, no_localisation, "Should never happen").
match_params_single_named([param([], _, _, _)|Rest_decl], Operand_name, Operand, Matching_mode) :-
        !,
        match_params_single_named(Rest_decl, Operand_name, Operand, Matching_mode).
match_params_single_named([param([Param|Rest_ids], Mode, _Type, _Init)|Rest_decl], Operand_name, Operand, Matching_mode) :-
        !,
        (mika_seav_atts:mika_seav_atts__get(name, Param, Operand_name) ->  %matches!
                match_single_param(Matching_mode, Mode, Param, Operand)
        ;
                match_params_single_named([param(Rest_ids, Mode, _Type, _Init)|Rest_decl], Operand_name, Operand, Matching_mode)
        ).

%%%
%call : match_params_positional(Params_c, Operand, Matching_mode),
%where Matching_mode is either 'pre' (before execution of the actual call) or 'post' (after execution of the actual call)
match_params_positional([], Operands, _Matching_mode) :-
        (Operands == [] ->
                true
        ;
                common_util__error(10, "Too many arguments during matching positional", "Cannot proceed", [(operands, Operands)], 10295136, mika_symbolic, match_params_positional, no_localisation, "Should never happen")
        ).
match_params_positional([param([Param|Rest_inner_params], Mode, _Type, _Init)|Rest_params], [Operand|Rest_operands], Matching_mode) :-
        match_single_param(Matching_mode, Mode, Param, Operand),
        (Rest_inner_params == [] ->
                match_params(Rest_operands, Rest_params, Matching_mode)
        ;
                match_params(Rest_operands, [param(Rest_inner_params, Mode, _Type, _Init)|Rest_params], Matching_mode)
        ).

%%%
match_single_param(Matching_mode, Mode, Param, Operand) :-
        (Matching_mode == pre ->
                ((Mode == in ; Mode == in_out) ->
                        %08/03/05
                        %Operand is frozen and is therefore a mixture of array and record variables and solver vars
                        exec(assign(Param, Operand), carry_on)
                ;
                        true
                )
        ;
         Matching_mode == post ->
                ((Mode == out ; Mode == in_out) ->
                        %08/03/05
                        %Operand is frozen and is therefore a mixture of array and record variables and solver vars
                        ((nonvar(Operand), Operand = indexed(Type, Arg), midoan_type:midoan_type__is_type(Type)) -> %a view conversion changed [26/11/07]
                                (Arg = [Var],
                                 exec(assign(Var, Param), carry_on)
                                )
                        ;
                                exec(assign(Operand, Param), carry_on)
                        )
                ;
                        true
                )
        ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%see issues document, in particular we should make access parameters become unhandled ...
handle_imported_subprogram(Imported_subprogram, Operand, Symbolic, Constraint, Type) :-
        mika_sub_atts:mika_sub_atts__get('name', Imported_subprogram, Subprogram_name_xref),
        mika_symbolic__parse(Subprogram_name_xref, _File_name, _File_extension, _Line, _Column, Id),
        mika_sub_atts:mika_sub_atts__get('return', Imported_subprogram, Return_var),
        (Return_var == 'no_return' ->
                (common_util__error(7, "Call to imported procedure is ignored: calls have no effect at all", no_error_consequences, [(subprogram_name_xref, Subprogram_name_xref), (operand, Operand)], 7368207, mika_symbolic, handle_imported_subprogram, no_localisation, "create special case in handle_imported_subprogram/2 if wanted"),
                 true   %i.e. do nothing : no effects at all
                )
        ;
                (mika_sub_atts:mika_sub_atts__get('return_type', Imported_subprogram, Subtype_indication_or_access_definition),
                 handle_intermediate_types(Subtype_indication_or_access_definition, Return_type, _Return_type_name),
                 mika_sub_atts:mika_sub_atts__get('params', Imported_subprogram, _Params),
                 (Id == 'getenv' ->
                        (Operand = [&(string([71,78,65,84,95,77,69,77,79,82,89,95,76,73,77,73,84]), _)] ->      %i.e. "GNAT_MEMORY_LIMIT" & _
                                (common_util__error(2, "Imported subprogram is simulated", "result given may be wrong", [(imported, getenv(GNAT_MEMORY_LIMIT)), (result_given, 1000)], 283203, mika_symbolic, handle_imported_subprogram, no_localisation, no_extra_info),
                                 GNAT_MEMORY_LIMIT = 1000,      %in Kbytes; or read from a configuration file
                                 symbolically_interpret(indexed(Return_type, [GNAT_MEMORY_LIMIT]), Symbolic, Constraint, Type, _Exception_),
                                 (is_unhandled_type([Type]) ->
                                        common_util__error(10, "Unhandled entities", no_error_consequences, no_arguments, 1038029, mika_symbolic, handle_imported_subprogram, no_localisation, no_extra_info)
                                 ;
                                        true
                                 )
                                )
                        ;
                                (common_util__error(5, "Call to imported getenv function is a guessed stub: has no side effects and the return value is a total guess", no_error_consequences, [(subprogram_name_xref, Subprogram_name_xref), (operand, Operand)], 5348377, mika_symbolic, handle_imported_subprogram, no_localisation, "create special case in handle_imported_subprogram/2 if wanted"),
                                 symbolically_interpret(tic(Return_type, first), Symbolic, Constraint, Type, _Exception_),
                                 (Type == unhandled_expression ->
                                        common_util__error(10, "Unhandled entities", no_error_consequences, no_arguments, 1038930, mika_symbolic, handle_imported_subprogram, no_localisation, no_extra_info)
                                 ;
                                        true
                                 )
                                )
                        )
                 ;
                  Id == atoi ->
                        (symbolically_interpret(indexed(Return_type, Operand), Symbolic, Constraint, Type, _Exception_),      %a conversion (from an address to an integer) : roughly properly handled
                         (Type == unhandled_expression ->
                                common_util__error(10, "Unhandled entities", no_error_consequences, no_arguments, 1039931, mika_symbolic, handle_imported_subprogram, no_localisation, no_extra_info)
                         ;
                                true
                         )
                        )
                ;
                 Id == c_malloc ->
                        (common_util__error(5, "Call to imported c_malloc function is a guessed stub: address '10' is alway returned", no_error_consequences, [(subprogram_name_xref, Subprogram_name_xref), (operand, Operand)], 5358226, mika_symbolic, handle_imported_subprogram, no_localisation, no_extra_info),
                         symbolically_interpret(indexed(Return_type, [10]), Symbolic, Constraint, Type, _Exception_),      %static address : 10 is returned
                         (Type == unhandled_expression ->
                                common_util__error(10, "Unhandled entities", no_error_consequences, no_arguments, 10409112, mika_symbolic, handle_imported_subprogram, no_localisation, no_extra_info)
                         ;
                                true
                         )
                        )
                ;
                 Id == msize ->
                        (common_util__error(5, "Call to imported msize function is a guessed stub: '1' is alway returned", no_error_consequences, [(subprogram_name_xref, Subprogram_name_xref), (operand, Operand)], 5364214, mika_symbolic, handle_imported_subprogram, no_localisation, no_extra_info),
                         symbolically_interpret(indexed(Return_type, [1]), Symbolic, Constraint, Type, _Exception_),      %static address : 10 is returned
                         (Type == unhandled_expression ->
                                common_util__error(10, "Unhandled entities", no_error_consequences, no_arguments, 1041932, mika_symbolic, handle_imported_subprogram, no_localisation, no_extra_info)
                         ;
                                true
                         )
                        )
                ;
                        (common_util__error(5, "Call to imported function is a guessed stub: has no side effects and the return value is a total guess", no_error_consequences, [(subprogram_name_xref, Subprogram_name_xref), (operand, Operand)], 5372244, mika_symbolic, handle_imported_subprogram, no_localisation, "create special case in handle_imported_subprogram/2 if wanted"),
                         symbolically_interpret(tic(Return_type, first), Symbolic, Constraint, Type, _Exception_),     %an nothing else
                         (Type == 'unhandled_expression' ->
                                common_util__error(10, "Unhandled entities", no_error_consequences, no_arguments, 1043332, mika_symbolic, handle_imported_subprogram, no_localisation, no_extra_info)
                         ;
                                true
                         )
                        )
                 )
                )
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%