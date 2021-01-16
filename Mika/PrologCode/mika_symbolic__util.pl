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
% mika_symbolic__util.pl
% part of the mika_symbolic module : utilitarian predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%partition SEAVs into unused, in, out, context lists
pmr_partition_seavs([], [], [], [], []).
pmr_partition_seavs([SEAV|Rest], UL, IL, OL, CL) :-
        mika_seav_atts:mika_seav_atts__get(mode, SEAV, Mode),
        (Mode == unused ->	%unused variables initialised or not
                (UL, IL, OL, CL) = ([SEAV|UL1], IL1, OL1, CL1)
        ;
         Mode == constant  ->				%we ignore it
                (UL, IL, OL, CL) = (UL1, IL1, OL1, CL1)
        ;
         Mode == in ->
                (UL, IL, OL, CL) = (UL1, [SEAV|IL1], OL1, CL1)
        ;
         Mode == in_out ->
                (UL, IL, OL, CL) = (UL1, [SEAV|IL1], [SEAV|OL1], CL1)
        ;
         Mode == out ->
                (UL, IL, OL, CL) = (UL1, IL1, [SEAV|OL1], CL1)
        ;
         Mode == init_elab ->                                           %has been assigned during elaboration but not afterwards
                (UL, IL, OL, CL) = (UL1, IL1, [SEAV|OL1], CL1)          %an 'output' because initialised during elaboration where the context is not ignored
        ;
         Mode == out_elab ->
                (UL, IL, OL, CL) = (UL1, IL1, [SEAV|OL1], [SEAV|CL1])   %an 'output' (as for 'init_elab') because initialised during elaboration where the context is not ignored
                                                                        %a context because since it has been assigned post-elaboration its original initialisation during elaboration will need to be re-applied (it needs to be reset) within the testpoint concerned
        ),
        pmr_partition_seavs(Rest, UL1, IL1, OL1, CL1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%only used in report
%partition variables into atomic integer, real and enumeration variables lists
%used as input for labeling
partition_atomic([], [], [], []).
partition_atomic([Var|Rest], IL, RL, EL) :-
        (mika_seav_atts:mika_seav_atts__is_seav(Var) ->
                mika_seav_atts:mika_seav_atts__get(input_value, Var, Value)
        ;
                Value = Var     %because they may well be not SEAV variables : e.g. record field values
        ),
	midoan_solver__get_type_from_value(Value, Base_type),
	pa_partition(Base_type, Value, IL1, RL1, EL1),
	append(IL1, IL2, IL),
        append(RL1, RL2, RL),
        append(EL1, EL2, EL),
	partition_atomic(Rest, IL2, RL2, EL2).
%%%
pa_partition('ground', _, [], [], []).       %not needed for labeling
pa_partition('type', _, [], [], []).       %not needed for labeling
pa_partition('standard.ads:integer', Value, [Value], [], []).      %an integer var
pa_partition('standard.ads:float', Value, [], [Value], []).        %a float var
pa_partition(modular_integer, Modular, [Value], [], []) :-
        midoan_modular_integer:midoan_modular_integer__get(value, Modular, Value).
pa_partition(base_enumeration, Value, [], [], Out) :-
        (midoan_enum:midoan_enum__ground(Value) ->
                Out = []        %not needed for labeling
        ;
                Out = [Value]
        ).
pa_partition(array(_), Value, IL, RL, EL) :-
	midoan_array:midoan_array__get_all_index_elements(Value, Indice_elements),
	Indice_elements = [(_, V)|_],
	midoan_solver__get_type_from_value(V, Base_type),	%because they are not SEAV variables but solver variables from array components
	pap_partition_array_vars(Indice_elements, Base_type, IL, RL, EL).		%all the variables are of the same type
pa_partition(record, Value, IL, RL, EL) :-
	midoan_record:midoan_record__get_all_field_values(Value, Field_values),
	term_variables_bag(Field_values, Var_list),
	partition_atomic(Var_list, IL, RL, EL).

pap_partition_array_vars([], _, [], [], []).
pap_partition_array_vars([(_, V)|Rest], Base_type, IL, RL, EL) :-
	pa_partition(Base_type, V, IL1, RL1, EL1),         %could itself be an array or a record etc.
	append(IL1, IL2, IL),
        append(RL1, RL2, RL),
        append(EL1, EL2, EL),
	pap_partition_array_vars(Rest, Base_type, IL2, RL2, EL2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Name is a list of codes e.g. "a.ads:2:8:a_type"
%or for standard identifiers e.g. "standard.ads:true" in which case line and column are set to 0
mika_symbolic__parse(Name_atom, File_name, File_extension, Line, Column, Id) :-
        atom_codes(Name_atom, Name),
        (append("standard.ads:", Id_std, Name) ->
		(atom_codes(File_name, "standard"),
		 atom_codes(File_extension, "ads"),
		 atom_codes(Id, Id_std),
		 Line = '0',
		 Column = '0'
		)
	;
		(parse1(Name, File_name1, File_extension1, Line1, Column1, Id1),
                 atom_codes(File_name, File_name1),
		 atom_codes(File_extension, File_extension1),
		 number_codes(Line, Line1),
		 number_codes(Column, Column1),
		 atom_codes(Id, Id1)
		)
	).

%46 = "." -- get the file name
parse1([46|R], [], File_extension, Line, Column, Id) :-
        !,
        parse2(R, File_extension, Line, Column, Id).
parse1([Char|R], [Char|File_name], File_extension, Line, Column, Id) :-
        !,
        parse1(R, File_name, File_extension, Line, Column, Id).

%58 = ":" --get the extension
parse2([58|R], [], Line, Column, Id) :-
        !,
        parse3(R, Line, Column, Id).
parse2([Char|R], [Char|File_extension], Line, Column, Id) :-
        !,
        parse2(R, File_extension, Line, Column, Id).

%58 = ":" --get the line
parse3([58|R], [], Column, Id) :-
        !,
        parse4(R, Column, Id).
parse3([Char|R], [Char|Line], Column, Id) :-
        !,
        parse3(R, Line, Column, Id).

%58 = ":" --get the column
parse4([58|Id], [], Id) :-
        !.
parse4([Char|R], [Char|Column], Id) :-
        !,
        parse4(R, Column, Id).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%take in a list of variables and tries to find the correct subprogram variable
find_sub_var([], Target_source_file_name, Subprogram_name, Line_no, _) :-
        common_util__error(10, "Cannot find in the source file of the subprogram to test for the package name and line number given", no_error_consequences, [(target_source_file_name, Target_source_file_name), (subprogram_name, Subprogram_name), (line_no, Line_no)], 1016555, mika_symbolic, find_sub_var, no_localisation, "the arguments provided are probably erronous").
find_sub_var([Var|R], Target_source_file_name, Subprogram_name, Line_no, Sub_var) :-
        (mika_sub_atts:mika_sub_atts__is_sub_atts(Var) ->      %it is a subprogram
                (mika_sub_atts:mika_sub_atts__get('name', Var, Name),
                 (mika_symbolic__parse(Name, Target_source_file_name, _, Defined_line_no, _, Subprogram_name) ->
                        (((number(Line_no), Defined_line_no == Line_no) ; \+ number(Line_no)) ->
                                (Sub_var = Var,		%found the correct subprogram
                                 !
                                )
                        ;
                                find_sub_var(R, Target_source_file_name, Subprogram_name, Line_no, Sub_var)
                        )
                 ;
                         find_sub_var(R, Target_source_file_name, Subprogram_name, Line_no, Sub_var)
		 )
                )
        ;
                find_sub_var(R, Target_source_file_name, Subprogram_name, Line_no, Sub_var)
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%partition a var list into a list of Seavs vars and a list of unhandled vars (others variables kinds are ignored)
keep_only_seavs([], [], []).
keep_only_seavs([V|Rest], SeavL, UnhandledL) :-
        (mika_seav_atts:mika_seav_atts__is_seav(V) ->
                (SeavL, UnhandledL) = ([V|Rest_seavL], Rest_unhandledL)
        ;
         mika_unhandled_atts:mika_unhandled_atts__is_unhandled_atts(V) ->
                (SeavL, UnhandledL) = (Rest_seavL, [V|Rest_unhandledL])
        ;
                (SeavL, UnhandledL) = (Rest_seavL, Rest_unhandledL)
        ),
        keep_only_seavs(Rest, Rest_seavL, Rest_unhandledL).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%only used during setup_and_run to declare variables as SEAVs to act as arguments
%declared as unused (for 'out' vars) or in (for 'in', 'in_out' vars) SEAVs
%the second argument is the out SEAVs
%the last argument is the out ParamL_driver contains all the parameters in the form driver(Xref_name, Ada_name, Type_name)
%changed 15/04/09 -> Ada 2005
declare_setup_params([], [], []).
declare_setup_params([param(NameL, Mode, Subtype_indication_or_access_definition, _Init)|Rest_param], SeavL, ParamL_driver) :-
        %trace,
        declare_setup_params_list(NameL, Mode, object(not_qualified, Subtype_indication_or_access_definition, _Init), Vars_inner, Params_inner),        %declares parameters to the solver, creates seavs.
        append(Vars_inner, Rest2, SeavL),
	append(Params_inner, Rest5, ParamL_driver),
        declare_setup_params(Rest_param, Rest2, Rest5).
%%%
        declare_setup_params_list([], _Mode, _Decl, [], []).
        declare_setup_params_list([Param|R], Mode, Decl, [Param|Rest], [PDriver|Rest2]) :-
                mika_name_atts:mika_name_atts__unput(Param, Param_name),
                mika_symbolic__parse(Param_name, File_name, File_extension, Line, Column, Id),
                Decl = object(Qualifier, Subtype_indication_or_access_definition, _Init),
                handle_intermediate_types(Subtype_indication_or_access_definition, Type_var, Type_name),
                mika_symbolic__parse(Type_name, _File_name2, _File_extension2, _Line2, _Column2, Id2),
                (((Mode == 'in' ; Mode == 'in_out'), midoan_array:midoan_array__is_unconst_array(Type_var)) ->
                        (%we have an 'in' or 'in_out' parameter for the subprogram under test that is an unconstrained array: we label its range [see issue 14/11/07]
                         %trace,
                         common_util__error(4, "Handling of an unconstrained array at the top level: Mika is only guessing its bounds (may generate false run time errors later in this path)", "May badly affect runtime", no_arguments, 3224252, mika_symbolic, declare_setup_params_list, no_localisation, no_extra_info),
                         common_util__create_dummy_name(Dummy_subtype_name),
                         mika_name_atts:mika_name_atts__create(Dummy_subtype_var, Dummy_subtype_name),
                         midoan_type:midoan_type__get_attribute(Type_var, unconst_array_indexes, Index_list),   %the indices of the unconstrained array (a list of Type_marks)
                         dspl_generate_labelled_range_list(Index_list, Indexes),                     %we label!
                         dspl_generate_type_instantiation(Id2, Indexes, Unconst_array_instantiation),
                         Param_type_name = Unconst_array_instantiation,
                         exec(subtype(Dummy_subtype_var, subtype_indication(_Null_exclusion, indexed(Type_var, Indexes), no_constraint)), carry_on),
                         mika_seav_atts:mika_seav_atts__create(Param, Param_name, object(Qualifier, Dummy_subtype_var, no_init))
                        )
                ;
                        (Param_type_name = Id2,
                         mika_seav_atts:mika_seav_atts__create(Param, Param_name, object(Qualifier, Type_var, no_init))       %initialisations are always ignored
                        )
                ),
                user:file_name_etc_to_ada_var_name(File_name, File_extension, Line, Column, Id, Driver_name),
                PDriver = driver(Param_name, Driver_name, Param_type_name),
                ((Mode == 'in' ; Mode == 'in_out') ->       %it will be read in
                        (symbolically_interpret(Param, _Symbolic, _Constraint, Type_param, _Exception),    %will be declared as in
                         (Type_param == 'unhandled_expression' ->
                                common_util__error(10, "Top level subprogram parameter is not handled", "Cannot proceed", [(param_name, Param_name)], 1023946, mika_symbolic, declare_setup_params_list, no_localisation, no_extra_info)
                         ;
                                true
                         )
                        )
                ;
                        true                                                            %out variables are left as unused
                ),
                declare_setup_params_list(R, Mode, Decl, Rest, Rest2).
%%%
%1st argumnent in in list of type_marks
%2nd argument is out list of SEAVs [Min, Max] for each type_mark
dspl_generate_labelled_range_list([], []).
dspl_generate_labelled_range_list([Type_mark| Rest_type_marks], [[Min, Max]|Rest_ranges]) :-
        common_util__create_dummy_name(Dummy_min_name),
        mika_name_atts:mika_name_atts__create(Min, Dummy_min_name),
        common_util__create_dummy_name(Dummy_max_name),
        mika_name_atts:mika_name_atts__create(Max, Dummy_max_name),
        objects([Min, Max], object('not_qualified', Type_mark, 'no_init')),
        symbolically_interpret(Min, _, Min_const, Min_type, _Exception),
        symbolically_interpret(Max, _, Max_const, Max_type, _Exception),
        symbolically_interpret(-(Max, Min), _, Diff, Diff_type, _Exception),
        ((Min_type == 'unhandled_expression' ; Max_type == 'unhandled_expression' ; Diff_type == 'unhandled_expression') ->
                common_util__error(10, "Range is not handled", "Cannot proceed", [(type_mark, Type_mark)], 1026251, mika_symbolic, dspl_generate_labelled_range_list, no_localisation, "Should never happen")
        ;
                (midoan_solver__sdl(<=(1, Diff)),        %to generate believeable non empty arrays
                 midoan_solver__sdl(<=(Diff, 5)),      %to generate believeable non empty arrays
                 midoan_type:midoan_type__obtain_basetype(Type_mark, Basetype),
                 (Basetype == 'standard.ads:integer'  ->
                        midoan_labeling:midoan_labeling__integers([Min_const, Max_const])
                 ;
                  Basetype == 'base_enumeration' ->
                        midoan_labeling:midoan_labeling__enums([Min_const, Max_const])
                 ;
                        common_util__error(10, "Type is not an enumeration nor an integer", "Cannot proceed", [(type_mark, Type_mark)], 10268134, mika_symbolic, dspl_generate_labelled_range_list, no_localisation, "Should never happen")
                 ),
                 dspl_generate_labelled_range_list(Rest_type_marks, Rest_ranges)
                )
        ).
%%%
%outputs something like '(-10 .. 10, -20 .. 10)' from a list of [[Min, Max], ...] where Min and Max are 'ground' SEAVs
dspl_generate_type_instantiation(Type_name, Indexes, Unconst_array_instantiation) :-
        dsplgti_generate_ada_indices(Indexes, Ada_indices),
        Unconst_array_instantiation =.. [Type_name|Ada_indices].
%%%
        dsplgti_generate_ada_indices([], []).
        dsplgti_generate_ada_indices([[Min, Max]|Rest_indices], ['..'(Min_const, Max_const)|Rest_ada]) :-
                symbolically_interpret(Min, _, Min_const, Min_type, _Exception),
                symbolically_interpret(Max, _, Max_const, Max_type, _Exception),
                ((Min_type == 'unhandled_expression' ; Max_type == 'unhandled_expression') ->
                        common_util__error(10, "Indices are unhandled", "Cannot proceed", no_arguments, 1028953, mika_symbolic, dsplgti_generate_ada_indices, no_localisation, "Should never happen")
                ;
                        dsplgti_generate_ada_indices(Rest_indices, Rest_ada)
                ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%needed for unconstrained returns type instantiation
%Out_return is a constrained SEAV array
generate_type_instantion_from_object(Type_name, Return_var, Unconst_array_instantiation) :-
        symbolically_interpret(tic(Return_var, 'dimensions'), _, Dimensions, _, _Exception),
        gtifo_generate_ada_indices_from_object(Return_var, 1, Dimensions, Ada_indices),
        Unconst_array_instantiation =.. [Type_name|Ada_indices].
%%%
        gtifo_generate_ada_indices_from_object(Return_var, Dim, Dimensions, Ada_indices) :-
                (Dim =< Dimensions ->
                        (symbolically_interpret(indexed(tic(Return_var, first), [Dim]), _, Min_const, Min_type, _Exception),
                         symbolically_interpret(indexed(tic(Return_var, last), [Dim]), _, Max_const, Max_type, _Exception),
                         ((Min_type == unhandled_expression ; Max_type == unhandled_expression) ->
                                common_util__error(10, "Indices are unhandled", "Cannot proceed", no_arguments, 1030554, mika_symbolic, gtifo_generate_ada_indices_from_object, no_localisation, "Should never happen")
                         ;
                                (Dim_1 is Dim + 1,
                                 gtifo_generate_ada_indices_from_object(Return_var, Dim_1, Dimensions, Rest_indices),
                                 Ada_indices = ['..'(Min_const, Max_const)|Rest_indices]
                                )
                         )
                        )
                ;
                        Ada_indices = []
                ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
my_copy_term(Kind, Term, Copy) :-
        (Kind == 'locals' ->
                (term_variables_bag(Term, All_vars),
                 copy_all_locals(All_vars, CopiesL)
                )
        ;
         Kind == 'seavs' ->
                (term_variables_bag(Term, All_vars),
                 copy_all_seavs(All_vars, CopiesL)
                )
        ;
         Kind = single(V) ->
                (copy_term(a(V), a(Vc)),        %should be a free varaible
                 CopiesL = [copy(V, Vc)]
                )
        ),
        copy_term2(Term, CopiesL, Copy).
%%%
        copy_all_locals(All_vars, CopiesL) :-   %copies all the name_atts vars once
                (All_vars == [] ->
                        CopiesL = []
                ;
                        (All_vars = [Var|Rest],
                         (mika_name_atts:mika_name_atts__is_name_atts(Var) ->
                                (mika_name_atts:mika_name_atts__get(name, Var, Name),
                                 mika_name_atts:mika_name_atts__create(Copy, Name),     %a copy name_atts variable is created with the same name
                                 CopiesL = [copy(Var, Copy)|Rest_copiesL]
                                )
                         ;
                                 CopiesL = Rest_copiesL
                         ),
                         copy_all_locals(Rest, Rest_copiesL)
                        )
                ).
%%%
        copy_all_seavs(All_vars, CopiesL) :-   %copies all the Seavs vars once
                (All_vars == [] ->
                        CopiesL = []
                ;
                        (All_vars = [Var|Rest],
                         (mika_seav_atts:mika_seav_atts__is_seav(Var) ->
                                (mika_seav_atts:mika_seav_atts__copy(Var, Copy),
                                 CopiesL = [copy(Var, Copy)|Rest_copiesL]
                                )
                         ;
                                 CopiesL = Rest_copiesL
                         ),
                         copy_all_seavs(Rest, Rest_copiesL)
                        )
                ).
%%%
        copy_term2(Term, CopiesL, Copy) :-
                (compound(Term) ->
                        (Term =.. [Functor|Sub_termL],
                         copy_term2(Functor, CopiesL, Functorc),
                         copy_term_list(Sub_termL, CopiesL, Sub_termLc),
                         Copy =.. [Functorc|Sub_termLc]
                        )
                ;
                 atom(Term) ->
                        Copy = Term
                ;
                 number(Term) ->
                        Copy = Term
                ;
                 var(Term) ->
                        (find_copy(CopiesL, Term, Copy) ->
                                true
                        ;
                                Copy = Term
                        )
                ;
                        common_util__error(10, "Unexpected term", "Cannot proceed", [(term, Term)], 1036004, mika_symbolic, copy_term2, no_localisation, "Should never happen")
                ).
%%%
                copy_term_list(List, CopiesL, Listc) :-
                        (List == [] ->
                                Listc = []
                        ;
                                (List = [Next|Rest],
                                 copy_term2(Next, CopiesL, Nextc),
                                 Listc = [Nextc|Restc],
                                 copy_term_list(Rest, CopiesL, Restc)
                                )
                        ).
%%%
                find_copy(CopiesL, Var, Copy) :-
                        (CopiesL == [] ->
                                fail
                        ;
                                (CopiesL = [copy(Variable, Var_copy)|Rest],
                                 (Var == Variable ->
                                        Copy = Var_copy
                                 ;
                                        find_copy(Rest, Var, Copy)
                                 )
                                )
                        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%list is made up of call(File_name, Functor, Operand)
execute_delayed_calls([], _Subprogram_var) :-
        !.
execute_delayed_calls([call(Operand)|Rest], Subprogram_var) :-
        !,
        handle_subprogram_call(Subprogram_var, Operand, _Symbolic, _Constraint, _Type, _Exception),
        mika_sub_atts:mika_sub_atts__get('name', Subprogram_var, Subprogram_name),
        common_util__error(9, "Delayed call to a subprogram executed", "May be unsound is the package containing his body has an elaboration part", [(name, Subprogram_name)], 9368121, mika_symbolic, cfdsc_lookup_and_execute, no_localisation, "non standard Ada?"),
        execute_delayed_calls(Rest, Subprogram_var).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%