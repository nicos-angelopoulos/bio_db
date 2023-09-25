% :- lib( replace_non_alphanums/3 ).
:- lib(stoics_lib:at_con/3).

:- ensure_loaded(bio_db_build_organism).

map_predicate_name_defaults( [db(hgnc),org(homs)] ).

%% map_predicate_name( +Cnm1, +Cnm2, -Pname, +Opts ).
%
% Construct a predicate name, first by looking into Opts for predicate/1
% or by downcasing both, and concatenating Cnm2 to Cnm1.
%
%==
% ?- map_predicate_name( prosqlite, 'GeneID', Pname, [] ).
%
% Pname = hgnc_homs_prosqlite_geneid.
% 
% ?- map_predicate_name( prosqlite, 'GeneID', Pname, [predicate(what)] ).
% Pname = what.
% 
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/7/2
%
map_predicate_name( Comp1, Comp2, Pname, Args ) :-
     Self = map_predicate_name,
     options_append( Self, Args, Opts ),
     map_predicate_name_opts( Comp1, Comp2, Pname, Opts ).

map_predicate_name_opts( _Cnm1, _Cnm2, Pname, Opts ) :-
	memberchk( predicate(Pname), Opts ),
	!.
map_predicate_name_opts( Comp1, Comp2, Pname, Opts ) :-
	map_predicate_name_token( Comp1, Tkn1 ),
	map_predicate_name_token( Comp2, Tkn2 ),
     options( org(OrgIn), Opts ),
     bio_db_organism_known( OrgIn, Okn, _Org ),
     options( db(Db), Opts ),
	at_con( [Db,Okn,Tkn1,Tkn2], '_', Pname ),
     !.
     % things should have already error, but to be steadfast: 
map_predicate_name_opts( Comp1, Comp2, _Pname, Opts ) :-
     throw( bio_db_pred_name_fail(Comp1,Comp2), Opts ).

map_predicate_map_prefix( map, Opts ) :-
	options( map_prefix(true), Opts ),
	!.
map_predicate_map_prefix( '', _Opts ).

map_predicate_name_token( Atom, Comp ) :-
     ( cnm_token(Atom,_,Comp) -> true; downcase_atom(Atom,Comp) ).
	% replace_non_alphanums( Down, 0'_, CompCs ),
	% atom_codes( Comp, CompCs ).

%% map_predicate_name_stem( Pname, Stem, Opts ).
%
% Stem is either the argument to stem/1 appearing in Opts or Pname.
% 
%==
% ?- map_predicate_name( pname, Stem, [abc(d),stem(file_stem)] ).
% Stem = file_stem.
%==
%
% @author nicos angelopoulos
% @version  0.1 2014/7/2
%
map_predicate_name_stem( _Pname, Stem, Opts ) :-
	memberchk( stem(Stem), Opts ),
	!.
map_predicate_name_stem( Pname, Pname, _Opts ).
