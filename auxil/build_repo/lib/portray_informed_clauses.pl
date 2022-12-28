:- use_module(library(lists)).
:- use_module(library(lib)).

:- lib(options).
:- lib(debug_call).

:- lib(stoics_lib:at_con/3).
:- lib(stoics_lib:en_list/2).
:- lib(stoics_lib:portray_clauses/2).
:- lib(stoics_lib:map_succ_list/3).

portray_informed_clauses_defaults(debug(true)).

/** portray_informed_clauses( +Clauses, +InfoPairlist, +File, +Opts ).

Portray db clauses with info predicate to File. 

The name of the info predicate is the functor of the first
element of list Clauses, postfix, extended with =|_info|=.


Opts
  * debug(Dbg=true)
    informational, progress messages

Infos
  * source(Url)
    the source file from which the clauses were generated
  * datetime(datetime(2022,12,18,10,19,53))
    the date and time the dataset was downloaded
  * header(header('ClmName1','ClmName2'))
    a term with the column names
  * data_types(data_types(atom,integer))
    data types for the columns
  * unique_lengths(unique_lengths(Len1,Len2))
    if missing the lengths are calculated from Clauses
  * relation_type(relations_type(1,m))
    1 or m for many

Examples
==
?- portray_informed_clauses([],[],[]).
==

@author nicos angelopoulos
@version  0.1 2022/12/18

*/

portray_informed_clauses( Clauses, InfoS, File, Args ) :-
     Self = portray_informed_clauses,
     options_append( Self, Args, _Opts ),
     en_list( InfoS, Infos ),
     Clauses = [Clause|_],
     functor( Clause, Cname, _ ),
     at_con( [Cname,info], '_', Iname ),
     Known = [source,datetime,header,data_types,unique_lengths],  % last one also picks up relation_type
     map_succ_list( portray_informed(Self,Clauses,Iname,Infos), Known, IClausesNest ),
     flatten( IClausesNest, IClauses ),
     open( File, write, Stream ),
     portray_clauses( IClauses, stream(Stream) ),
     nl( Stream ),
     portray_clauses( Clauses, stream(Stream) ),
     close( Stream ),
     debuc( Self, end, true ).

portray_informed( Self, Clauses, Iname, Infos, Tag, IClause ) :-
     portray_informed_tag( Tag, Self, Clauses, Iname, Infos, IClause ).

portray_informed_tag( unique_lengths, Self, Clauses, Iname, Infos, IClauses ) :-
     !,
     ( memberchk(unique_lengths-ULs,Infos) ->
          debuc( Self, 'Using supplied unique_lengths: ~w', [ULs] )
          ;
          Clauses = [Clause|_],
          functor( Clause, _, Arity ),
          portray_informed_unique_lengths( 1, Arity, Clauses, LengthsL ),
          ULs =.. [unique_lengths|LengthsL]
     ),
     IClause1 =.. [Iname,unique_lengths,ULs],
     ( memberchk(relation_type-RTs,Infos) ->
          debuc( Self, 'Using supplied relation_type: ~w', [RTs] )
          ;
          ( var(LengthsL) ->
               Clauses = [Clause|_],
               functor( Clause, _, Arity ),
               portray_informed_unique_lengths( 1, Arity, Clauses, LengthsL )
               ;
               true
          ),
          length( Clauses, ClausesLen ),
          maplist( portray_informed_relation_lengths(ClausesLen), LengthsL, RTypes ),
          RTs =.. [relation_type|RTypes]
     ),
     IClause2 =.. [Iname,relation_type,RTs],
     IClauses = [IClause1,IClause2].
portray_informed_tag( Tag, Self, _Clauses, Iname, Infos, IClause ) :-
     ( memberchk(Tag-TagInfo,Infos) ->
          IClause =.. [Iname,Tag,TagInfo]
          ;
          debuc( Self, 'Missing info tag: ~w', [Tag] )
     ).

portray_informed_relation_lengths( ClausesLen, ClmLength, RType ) :-
     ( ClausesLen =:= ClmLength -> RType is 1
          ; ( ClausesLen > ClmLength -> RType = m
               ; throw(clm_len(ClmLength),larger_than,clauses_len(ClausesLen))
            )
     ).

portray_informed_unique_lengths( I, Arity, _Clauses, Lengths ) :-
     I > Arity,
     !,
     Lengths = [].
portray_informed_unique_lengths( I, Arity, Clauses, Lengths ) :-
     findall( Ith, (member(Clause,Clauses),arg(I,Clause,Ith)), Iths ),
     sort( Iths, Sths ),
     length( Sths, Length ),
     Lengths = [Length|Tengths],
     J is I + 1,
     portray_informed_unique_lengths( J, Arity, Clauses, Tengths ).
