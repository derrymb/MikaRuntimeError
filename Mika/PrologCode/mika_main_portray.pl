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
% mika_main_portray.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module([mika_sub_atts, mika_name_atts, mika_package_atts, mika_seav_atts, mika_generic_atts, mika_unhandled_atts]).

:- use_module(midoan_solver_main, [	midoan_solver__integer_range/3,
					midoan_solver__real_min/3,
					midoan_solver__real_max/3
			          ]
             ).

%our user defined portray predicate allows us a better output of attributed variables during debugging: as how much of attributed variables is displayed can be set
user:portray(Term) :-
        !,
        (compound(Term) ->
                (Term =.. [Functor|Sub_termL],
                 (Functor == ':' ->                     %we ignore module info
                        (Sub_termL = [_Module|[Goal]],
                         user:portray(Goal)
                        )
                 ;
                  Functor == '.' ->
                        (write('['),
                         portray_subterms(Term),
                         write(']')
                        )
                 ;
		  (Sub_termL = [L, R], (current_op(_, yfx, Functor) ; current_op(_, xfy, Functor) ; current_op(_, xfx, Functor))) ->
			(user:portray(L),
                         write(' '),
                         write(Functor),
                         write(' '),
                         user:portray(R)
                        )
                 ;
                  (Sub_termL = [L], (current_op(_, fy, Functor) ; current_op(_, fx, Functor))) ->
			(write(Functor),
                         write(' '),
                         user:portray(L)
                        )
                 ;
                  Term = midoan_enum(Type_var, Position) ->
                        (write('midoan_enum('),
                         user:portray(Type_var),
                         write(', '),
                         user:portray(Position),
                         write(')')
                        )
                 ;
                  Term = midoan_modular_integer(Modulo, Value) ->
                        (write('modular_integer('),
                         user:portray(Modulo),
                         write(', '),
                         user:portray(Value),
                         write(')')
                        )
                 ;
                  Term = portray(Term2) ->
                        user:portray(Term2)
                 ;
                        (write(Functor),
                         write('('),
                         portray_subterms(Sub_termL),
                         write(')')
                        )
                 )
                )
        ;
         atom(Term) ->
                write(Term)
        ;
         number(Term) ->
                write(Term)
        ;
         var(Term) ->
                (mika_name_atts:mika_name_atts__is_name_atts(Term) ->
                        (mika_name_atts:mika_name_atts__get(name, Term, Name),
                         write('Var_name('),
                         format(Name, []),
                         write(','),
                         write(Term),      %for debugging
                         write(')')
                        )
                ;
                 mika_package_atts:mika_package_atts__is_package_atts(Term) ->
                        (mika_package_atts:mika_package_atts__get(Term, 'name', Xref_name),
                         write('Var_package('),
                         format(Xref_name, []),
                         write(')')
                        )
                ;
                 mika_sub_atts:mika_sub_atts__is_sub_atts(Term) ->
                        (mika_sub_atts:mika_sub_atts__get(name, Term, Name),
                         %mika_sub_atts:mika_sub_atts__get(return, Term, _Return),
                         %mika_sub_atts:mika_sub_atts__get(params, Term, _Params),
                         %mika_sub_atts:mika_sub_atts__get(decls, Term, _Decls),
                         %mika_sub_atts:mika_sub_atts__get(body, Term, _Body),
                         write('Var_sub('),
                         format(Name, []),
                         %write(', '),
                         %user:portray(Return),
                         %write(', '),
                         %user:portray(Params),
                         %write(', '),
                         %user:portray(Decls),
                         %write(', '),
                         %user:portray(Body),
                         write(')')
                        )
                ;
                 mika_seav_atts:mika_seav_atts__is_seav(Term) ->
                        (mika_seav_atts:mika_seav_atts__get(name, Term, Name),
                         mika_seav_atts:mika_seav_atts__get(mode, Term, Mode),
                         mika_seav_atts:mika_seav_atts__get(type, Term, _Type),
                         write('Var_seav('),
                         write(Term),  %may be useful for debug purposes
                         write(', '),
                         format(Name, []),
                         write(', '),
                         write(Mode),
                         %write(', '),
                         %format(Type, []),
                         write(')')
                        )
                ;
                 mika_generic_atts:mika_generic_atts__is_generic_atts(Term) ->
                        (mika_generic_atts:mika_generic_atts__get(name, Term, Name),
                         mika_generic_atts:mika_generic_atts__get(type, Term, Type),
                         write('Var_generic('),
                         format(Name, []),
                         write(', '),
                         format(Type, []),
                         write(')')
                        )
                ;
                 midoan_solver:midoan_solver__is_integer(Term) ->
                        (midoan_solver__integer_range(Term, Min, Max),
                         write('Var_fd('),
                         write(Term),  %may be useful for debug purposes
                         write(', '),
                         write(Min),
                         write('..'),
                         write(Max),
                         write(')')
                        )
                ;
		 midoan_solver:midoan_solver__is_float(Term) ->
                        (midoan_solver__real_min(Term, Inf, _),
			 midoan_solver__real_max(Term, Sup, _),
                         write('Var_fl('),
                         write(Inf),
                         write('..'),
                         write(Sup),
                         write(')')
                        )
                ;
                 midoan_array:midoan_array__is_array(Term) ->
                        (write('Var_array('),
                         midoan_array:midoan_array__get_all_index_elements(Term, Index_valueL),
                         midoan_array:midoan_array__get_type_var(Term, Type),
                         user:portray(Type),
                         write(', '),
                         user:portray(Index_valueL),
                         write(')')
                        )
                ;
                 midoan_record:midoan_record__is_record(Term) ->
                        (write('Var_record('),
                         midoan_record:midoan_record__get_all_field_values(Term, Field_valueL),
                         user:portray(Field_valueL),
                         write(')')
                        )
                ;
                 midoan_type:midoan_type__is_type(Term) ->
                        (write('Var_type('),
                         midoan_type:midoan_type__get_typemark(Term, Type_mark),
                         user:portray(Type_mark),
                         write(')')
                        )
                ;
		 midoan_anon_aggregate:midoan_anon_aggregate__is_anon_aggregate(Term) ->
			(write('anon_aggregate(..)')
			)
                ;
		 midoan_string_literal:midoan_string_literal__is_string_literal(Term) ->
			(write('string_literal(..)')
			)
                ;
                 mika_unhandled_atts:mika_unhandled_atts__is_unhandled_atts(Term) ->
                        (mika_unhandled_atts:mika_unhandled_atts__get('name', Term, Name),
                         mika_unhandled_atts:mika_unhandled_atts__get('comment', Term, Comment),
                         write('Var_unhandled('),
                         write(Name),
                         write(', '),
                         write(Comment),
                         write(')')
                        )
		;
                        (write('Var('),
                         write(Term),
                         write(')')
                        )
                )
	;
		(write('unexpected in portray message id 2:'),
		 display(Term)
		)
        ),
        !.
%%%
% a home made print depth implemented here useful during debugging
        portray_subterms(Term) :-
                portray_subterms(Term, 0).
%%%
                portray_subterms([T|R], N) :-
                        (R == [] ->
                                user:portray(T)
                        ;
                         N < 10 ->			%print depth
                                (user:portray(T),
                                 write(', '),
                                 N1 is N+1,
                                 portray_subterms(R, N1)
                                )
                        ;
                                write('.....')
                        ).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%     END   %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%