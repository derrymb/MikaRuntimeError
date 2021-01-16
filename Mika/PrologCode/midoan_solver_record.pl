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
% midoan_solver__record.pl
% defines the module record for record meta variables handling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% a record variable R is of the form: R{midoan_record(Type_var, Field_values)}, where Field_values is a list of (Field_name, Value)
% a record meta variable never becomes ground, even if a record is fully known it is still represented as a meta record variable
:- module(midoan_record, []).

:- use_module([	library(atts),	%Sicstus attribute library
		library(lists)	%for append/3
	     ]).

:- attribute midoan_record/2.   %name of the attribute
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%internal predicate (see atts library documentation)
%called prior to unification
%Value is a non-variable or another attributed variable (if Value is a plain variable verify_attributes is not called)
verify_attributes(Var, Value, Goals) :-
        (get_atts(Var, midoan_record(Type_var, Field_values)) ->
                (var(Value) ->  %unification of a record variable with a variable (should always be the case)
                        (midoan_record__is_record(Value) ->
                                (get_atts(Value, midoan_record(Type_var2, Field_values2)),   %two record variables unification
                                 common_util:common_util__error(10, "Unification Error", no_error_consequences, [(type_var, Type_var), (field_values, Field_values), (type_var2, Type_var2), (field_values2, Field_values2)], 105051, midoan_record, verify_attributes, no_localisation, "Unification error in verify_attributes in midoan_solver__record module: should use midoan_solver__controlled_unification (record = record)")
                                )
                        ;
                                %should never happen record are only unified with other record varaibles
                                common_util:common_util__error(10, "Unification Error", no_error_consequences, no_arguments, 105453, midoan_record, verify_attributes, no_localisation, "Unification of a record variable with a non record variable in midoan_solver__record module.")
                        )
                ;
                        fail    %unification of a record var with a nonvar (e.g 'others')
                )
        ;
                Goals = []     %no actions as we are not concerned : not a record variable
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%check if a variable is a record variable
midoan_record__is_record(Var) :-
	var(Var),
	get_atts(Var, midoan_record(_, _)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_record__get_type(Record_var, Type_var) :-
        get_atts(Record_var, midoan_record(Type_var, _Field_values)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%create a record variable or a template (i.e. without initialisations)
midoan_record__create_record(Type_var, Field_list, Template_or_variable, Record_var) :-
        (Field_list == [] ->    %i.e. null_record
                Field_values = []
        ;
                mrcr_extract_fields(Field_list, [], Template_or_variable, Field_values)       %replace the type marks with values
        ),
	put_atts(Record_var, midoan_record(Type_var, Field_values)).
%%%
        % Field_list is the in list of (Inner_field_list, Type_var, Init|no_init)
        % Field_values is the out list of (Field, Value) where Value is a variable of Type_var corresponding to Index_list or the initial value given
        mrcr_extract_fields([], Field_values, _Template_or_variable, Field_values).
        mrcr_extract_fields([(Inner_field_list, Type_var, Init)|Rest], Field_values_i, Template_or_variable, Field_values_o) :-
                mrcref_extract_inner_fields(Inner_field_list, Type_var, Init, Template_or_variable, Inner_field_values),
                append(Field_values_i, Inner_field_values, Field_values_tmp),
                mrcr_extract_fields(Rest, Field_values_tmp, Template_or_variable, Field_values_o).
%%%
                mrcref_extract_inner_fields([], _, _Init, _Template_or_variable, []).
                mrcref_extract_inner_fields([Field_name|Rest_field_list], Type_var, Init, Template_or_variable, [(Field_name, Value)|Rest_field_values]) :-
                        (Template_or_variable == template ->
                                midoan_type:midoan_type__variable_declaration(Value, Type_var)
                        ;
                                (Init == no_init ->
                                        (midoan_type:midoan_type__get_attribute(Type_var, has_fields_with_default, Has_fields_with_default),
                                                (Has_fields_with_default == no ->
                                                        midoan_type:midoan_type__variable_declaration(Value, Type_var)
                                                ;
                                                        midoan_type:midoan_type__created_variable_with_default_initialised_fields(Value, Type_var)
                                                )
                                        )
                                ;
                                        (mika_symbolic:symbolically_interpret(Init, Symbolic_init, Constraint_init, Type_init, _Exception_),
                                         (Type_init == unhandled_expression ->
                                                (common_util:common_util__error(4, "Unhandled expression in initialisation of default record field", no_error_consequences, no_arguments, 49453, midoan_record, mrcref_extract_inner_fields, no_localisation, no_extra_info),
                                                 mika_unhandled_atts:mika_unhandled_atts__create(Value, Field_name, Symbolic_init)
                                                )
                                         ;
                                                (midoan_type:midoan_type__variable_declaration(Value, Type_var),
                                                 mika_symbolic:uvoa_check_range(Constraint_init, Value, Field_name)              %may fail if out of range
                                                )
                                         )
                                        )
                                )
                        ),
                        mrcref_extract_inner_fields(Rest_field_list, Type_var, Init, Template_or_variable, Rest_field_values).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_record__get_all_field_values(Record_var, Field_values) :-
	get_atts(Record_var, midoan_record(_Type_var, Field_values)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
midoan_record__get_field(Record, Field, R, T) :-
	midoan_record__get_all_field_values(Record, Field_values),
	member((Field, Exp), Field_values),
        !,      %added 10/06/08
	midoan_solver:midoan_solver__interpret(Exp, types(_), R, T, _Exception_).	%necessary only to get the type of the field
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%return an anonymous record variable of an updated meta record variable
midoan_record__up_record(Record, Field, Exp, New_record) :-
	midoan_record__is_record(Record),                            %Record is a meta record variable(should always be the case)
        get_atts(Record, midoan_record(Type_var, Field_values)),
	append(Start, [(Field, _)|Rest], Field_values),       %find the position of Field in the field value list of record
	append(Start, [(Field, Exp)|Rest], New_field_values),   %we should probably check Exp against a Template using controlled_unification
        !,      %added 10/06/08
	put_atts(New_record, midoan_record(Type_var, New_field_values)).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%at least one of the elements must be different : easy to check for ground elements, delay otherwise
midoan_record__inequality(X, Y) :-
        midoan_record__get_all_field_values(X, Field_values_X),
        midoan_record__get_all_field_values(Y, Field_values_Y),
        !,
        midoan_solver__inequality_constraint(Field_values_X, Field_values_Y, Outcome, Susp),    %see midoan_solver_main__inequality.pl
        (Outcome ==  yes ->     %at least one of the field value is different
                true
        ;
         Outcome == dontknow ->
                (var(Susp) ->   %the suspension list is still a var: all the field values are ground (but the Outcome is dontknow so we fail)
                        fail
                ;
                        when(Susp, midoan_record__inequality(X, Y))     %all the ground field values are identical but there are some non ground field values
                )
        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%AsgL is of the form: for SPARK ada
%                       [exp, ..., exp]
%          or           [([field], exp), ..., ([field], exp)]
% [but why [field] in the above? (see parsing?)]
midoan_record__create_record_from_agg(Type_var, AsgL, Record) :-
        midoan_type:midoan_type__variable_declaration(Template, Type_var),
	midoan_record__get_all_field_values(Template, Field_values),
	init_field_values(AsgL, Field_values),
	put_atts(Record, midoan_record(Type_var, Field_values)).
%%%
        init_field_values(null_record, _Field_values) :-
                !.
        init_field_values([], _Field_values) :-
                !.
        init_field_values([First|Rest], Field_values) :-
                !,
                ((compound(First), First = named(_Names, _Value)) ->
                        init_field_values_named([First|Rest], Field_values)
                ;
                        init_field_values_positional([First|Rest], Field_values)
                ).
%%%
                init_field_values_named(null_record, []) :-
                        !.
                init_field_values_named([], _) :-
                        !.
                init_field_values_named([named([], _Value)|Rest], Field_values) :-
                        !,
                        init_field_values_named(Rest, Field_values).
                init_field_values_named([named([others], _Value)], []) :-
                        !.
                init_field_values_named([named([others], Value)], [(_Field, Matched)|Rest]) :-
                        !,
                        midoan_solver:midoan_solver__controlled_unification(Value, Matched),
                        init_field_values_named([named([others], Value)], Rest).
                init_field_values_named([named([Field|Rest_field], Value)|Rest], Field_values) :-
                        !,
                        (member((Field, Matched), Field_values) ->
                                (!,
                                 midoan_solver:midoan_solver__controlled_unification(Value, Matched),
                                 init_field_values_named([named(Rest_field, Value)|Rest], Field_values)
                                )
                        ;
                                %not a member : this should an error, but [18/03/08] due to dead code entities not being cross-referenced by gnatxref, and our imperfect way of catching unreferenced entities (problem for standard entities also used as field names (such as 'soh'), and multiple match)
                                %  we allow this proceed, obviously if it issued it will probably create an error later...
                                (%common_util__error(10, "Record creation error, field could not be found", "no proper record can be returned", [(field, Field), (field_values, Field_values)], 1016719, midoan_record, init_field_values_named, no_localisation, "may be due to dead code issue 18-03-08")
                                 common_util:common_util__error(6, "During record creation, a field could not be found", "the value will remain become 'unknown_error616919'", [(field, Field), (field_values, Field_values)], 616919, midoan_record, init_field_values_named, no_localisation, "may be due to dead code issue 18-03-08"),
                                 Matched = unknown_error616919,
                                 init_field_values_named([named(Rest_field, Value)|Rest], Field_values)
                                )
                        ).
%%%
                init_field_values_positional(null_record, []) :-
                        !.
                init_field_values_positional([], R) :-
                        (R == [] ->
                                true
                        ;
                                common_util:common_util__error(10, "Record creation from aggregate failed", no_error_consequences, no_arguments, 140203104, midoan_record, init_field_values_positional, no_localisation, no_extra_info)
                        ).
                init_field_values_positional([Value|Rest], [(_, Matched)|Rest2]) :-
                        midoan_solver:midoan_solver__controlled_unification(Value, Matched),
                        init_field_values(Rest, Rest2). %to allow for mixed record aggregates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%