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
% mika_list.pl
% defines module mika_list
% extension to list library using Eclipse documentation, other predicates such as intersection are also available in the Eclipse doc
% only used in mika_coverage.pl (could consider using Sicstus ordered set library instead)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- module(mika_list, []).

:- use_module(library(lists)).	%provides the memberchk/2 predicate

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%not suitable for lists of variables union
%union of two lists of terms as per Eclipse documentation
%union([1,2,3], [1,3], L) gives L = [2,1,3]
mika_list__union([], L, L).
mika_list__union([Head|L1tail], L2, L3) :-
        memberchk(Head, L2),
        !,
        mika_list__union(L1tail, L2, L3).
mika_list__union([Head|L1tail], L2, [Head|L3tail]) :-
        mika_list__union(L1tail, L2, L3tail).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%not suitable for lists of variables subtraction
%subtraction of two lists of terms as per Eclipse documentation
%subtract([1,2,3,4], [1], R) gives R = [2,3,4]
%subtract([1,2,3], [3,4], R) gives R = [1,2]
mika_list__subtract([], _, []).
mika_list__subtract([Head|Tail], L2, [Head|Rest]) :-
        (memberchk(Head, L2)->
                fail
        ;
                (!,
                 mika_list__subtract(Tail, L2, Rest)
                )
        ).
mika_list__subtract([_|Tail1], L2, Rest) :-
        mika_list__subtract(Tail1, L2, Rest).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mika_list__intersection(S, T, Result) :-
        mika_list__subtract(S, T, SminusT),
        mika_list__subtract(S, SminusT, Result).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%