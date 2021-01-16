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
% mika_seav_atts.pl
% defines the module seav for Symbolic Execution Attributed Variables
% implementing Symbolic Execution Attributed Variables (SEAVs) (some old, out of date, documention is in SEAV.odt document)
%   briefly the attribute is of the form : mika_seav(Name, Input_value, Output_value, Type, Mode, Symbolic, Constraint)
%                                                    N     I            O             T     M     S         C
%   Name         : name of the variable
%   Input_value  : the test input data of the variable
%   Output_value : a template role is unclear (see update_var_on_assignement predicate) of the variable
%   Type         : just the type_name of the variable
%   Mode         : unused|init_elab|out_elab|in|out|in_out of the variable
%   Symbolic     : current symbolic expression used for debug of the variable
%   Constraint   : current variable value of the variable
%%%
:- module(mika_seav_atts, []).

:- use_module([library(atts)]).	%Sicstus attribute library

:- attribute mika_seav_atts/7.	%name of the attribute
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%internal predicate
%called prior to unification
%Value is a non-variable or another attributed variable (if Value is a plain variable verify_attributes is not called)
% verify_attributes mostly gives rise to error messages except with an atom in which case it should fail
verify_attributes(Var, Value, Goals) :-
        (get_atts(Var, mika_seav_atts(N, I, O, T, M, S, C)) ->
                (atomic(Value) ->       %unification of a seav variable with an atom, can happen
                        fail            % but it is a failure
                ;
                 compound(Value) ->     %unification of a seav variable with a compound
                        fail
                ;
                 var(Value) ->          %unification of a seav variable with another attributed variable
                        (get_atts(Value, mika_seav_atts(N2, I2, O2, T2, M2, S2, C2)) ->
                                (N = N2,
                                 midoan_solver:midoan_solver__controlled_unification(I, I2),  %needed because could be a solver var or free
                                 midoan_solver:midoan_solver__controlled_unification(O, O2),  %needed because could be a solver var or free
                                 T = T2,
                                 M = M2,
                                 S = S2,
                                 midoan_solver:midoan_solver__controlled_unification(C, C2),  %needed because could be a solver var or free
                                 Goals = []
                                )
                        ;
                         mika_name_atts:mika_name_atts__is_name_atts(Value) ->       %during parameter matching
                                (mika_name_atts:mika_name_atts__unput(Value, _),
                                 put_atts(Value, mika_seav_atts(N, I, O, T, M, S, C)),
                                 Goals = []
                                )
                        ;
                                common_util:common_util__error(10, "Unification error", "Clearly never allowed", [(name, N)], 107733, mika_seav_atts, verify_attributes, no_localisation, "Unification of a SEAV with a non SEAV attributed variable")
                        )
                ;
                        %should never happen: not expected what is Value?
                        common_util:common_util__error(10, "Unification error", "Clearly never allowed", [(name, N)], 108134, mika_seav_atts, verify_attributes, no_localisation, "Unification of a SEAV variable with a non expected term")
                )
        ;
                Goals = []     %no actions as we are not concerned here : not a seav variable
        ).
%%%
%25/04/08 after elaboration we call this on all variables to check the context desired: mode may be changed
mika_seav_atts__context_checking(Seav) :-
        get_atts(Seav, mika_seav_atts(Seav_name, Input_value, Output_value, Type, Mode, Symbolic, Constraint)),
        (Mode == 'constant' ->
                true
        ;
                (mika_symbolic:mika_symbolic__parse(Seav_name, File_name, _File_extension, _Line, _Column, _Id),
                 mika_symbolic:mika_symbolic__check_context(File_name, Context),
                 (Context == 'ignored' ->
                        put_atts(Seav, mika_seav_atts(Seav_name, _, _, Type, 'unused', _, _))
                 ;
                        (Mode == 'unused' ->
                                true
                        ;
                         Mode == 'in' ->
                                local_label(Seav)
                        ;
                         Mode == 'out' ->
                                put_atts(Seav, mika_seav_atts(Seav_name, Constraint, Output_value, Type, 'init_elab', Symbolic, Constraint))
                        ;
                         Mode == 'in_out' ->
                                (local_label(Seav),
                                 put_atts(Seav, mika_seav_atts(Seav_name, Input_value, Output_value, Type, 'init_elab', Symbolic, Constraint))
                                )
                        )
                 )
                )
        ).

%09/01/09
%weird but the idea is to label 'in' and 'in_out' vars after elaboration when the context should be taken into account
%while this should not be allowed in general, the idea of doing this would be to allow us to detect code that depends on default initialisations
% because the ouput comparison will be wrong
        local_label(Seav) :-
                mika_symbolic:partition_atomic([Seav], IL, RL, EL),
                midoan_labeling:midoan_labeling__enums(EL),
                midoan_labeling:midoan_labeling__integers(IL),
                midoan_solver:midoan_solver__label_reals(RL).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mika_seav_atts__is_seav(Var) :-
        var(Var),
        get_atts(Var, mika_seav_atts(_, _, _, _, _, _, _)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mika_seav_atts__create(Seav, Seav_name, object(Qualifier, Type_name, Init)) :-
        ((Qualifier == aliased ; Qualifier == aliased_constant) ->
                common_util:common_util__error(8, "Aliased object is ignored (i.e. 'Access will not work)", "(i.e. 'Access will not work)", [(name, Seav_name)], 89636, mika_seav_atts, mika_seav_atts__create, no_localisation, no_extra_info)
        ;
                true
        ),
        (Init == no_init ->
		put_atts(Seav, mika_seav_atts(Seav_name, _, _, Type_name, 'unused', _, _))
	;
	 Init = init(Symbolic_init, Constraint_init) ->
		((Qualifier == not_qualified ; Qualifier == aliased) ->
			(midoan_type:midoan_type__variable_declaration(Output, Type_name),
                         put_atts(Seav, mika_seav_atts(Seav_name, _, Output, Type_name, out, Symbolic_init, Constraint_init))
                        )
		;
		 (Qualifier == constant ; Qualifier == aliased_constant) ->
			put_atts(Seav, mika_seav_atts(Seav_name, Constraint_init, _, Type_name, constant, Symbolic_init, _))
		;
			common_util:common_util__error(10, "Creation of an SEAV with a non-handled qualifier", "Clearly never allowed", [(qualifier, Qualifier)], 1011039, mika_seav_atts, mika_seav_atts__create, no_localisation, no_extra_info)
		)
	).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mika_seav_atts__get('name', Seav, R) :-
        get_atts(Seav, mika_seav_atts(R, _, _, _, _, _, _)).
mika_seav_atts__get('input_value', Seav, R) :-
        get_atts(Seav, mika_seav_atts(_, R, _, _, _, _, _)).
mika_seav_atts__get('output_value', Seav, R) :-
        get_atts(Seav, mika_seav_atts(_, _, R, _, _, _, _)).
mika_seav_atts__get('type', Seav, R) :-
        get_atts(Seav, mika_seav_atts(_, _, _, R, _, _, _)).
mika_seav_atts__get('mode', Seav, R) :-
        get_atts(Seav, mika_seav_atts(_, _, _, _, R, _, _)).
mika_seav_atts__get('symbolic', Seav, R) :-
        get_atts(Seav, mika_seav_atts(_, _, _, _, _, R, _)).
mika_seav_atts__get('constraint', Seav, R) :-
	get_atts(Seav, mika_seav_atts(_, _, _, _, _, _, R)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%needed during symbolic interpretation
mika_seav_atts__update('input_value', Seav, I) :-
        get_atts(Seav, mika_seav_atts(N, _, O, T, M, S, C)),
        put_atts(Seav, mika_seav_atts(N, I, O, T, M, S, C)).
mika_seav_atts__update('output_value', Seav, O) :-
        get_atts(Seav, mika_seav_atts(N, I, _, T, M, S, C)),
        put_atts(Seav, mika_seav_atts(N, I, O, T, M, S, C)).
mika_seav_atts__update('type', Seav, T) :-
        get_atts(Seav, mika_seav_atts(N, I, O, _, M, S, C)),
        put_atts(Seav, mika_seav_atts(N, I, O, T, M, S, C)).
mika_seav_atts__update('mode', Seav, M) :-
        get_atts(Seav, mika_seav_atts(N, I, O, T, _, S, C)),
        put_atts(Seav, mika_seav_atts(N, I, O, T, M, S, C)).
mika_seav_atts__update('symbolic', Seav, S) :-
        get_atts(Seav, mika_seav_atts(N, I, O, T, M, _, C)),
        put_atts(Seav, mika_seav_atts(N, I, O, T, M, S, C)).
mika_seav_atts__update('constraint', Seav, C) :-
        get_atts(Seav, mika_seav_atts(N, I, O, T, M, S, _)),
	put_atts(Seav, mika_seav_atts(N, I, O, T, M, S, C)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%remove the attribute: only done during re-declaration of a private or incomplete object in objects/2 from mika_symbolic__init.pl
mika_seav_atts__unput(Seav, Name) :-
        mika_seav_atts__get(name, Seav, Name),
        put_atts(Seav, -mika_seav_atts(_, _, _, _, _, _, _)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mika_seav_atts__copy(Seav, Copy) :-
        get_atts(Seav, mika_seav_atts(N, I, O, T, M, S, C)),
        copy_term((I, O, M, S, C), (Ic, Oc, Mc, Sc, Cc)),
        put_atts(Copy, mika_seav_atts(N, Ic, Oc, T, Mc, Sc, Cc)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%