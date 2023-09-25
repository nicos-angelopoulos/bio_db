/** go_obo( +OboF, -Term ).

Parses an OboF(ile) to a Term of the form obo(Infos,Terms).

Each Info=Name-Value and each Term is of the form obo_term(Id,Name,NSpc,Obs,Props), with Prop = Name-Value.

Args
  * Id
    the unique (numeric id), derived from atomic go id: GO:0000001 -> 1
  * Name
    name of the term (short description like)
  * NSpc
    (shorthand) of the namespace (one of: [bp, cc, mf]).
  * Obs
    obsolete flag
  * Props
    Name-Value property pairs ('Name: ' being a line starter in the go.obo input file: OboF)

==
?- use_module(library(by_unix)).
?- @ wget('http://purl.obolibrary.org/obo/go/go.obo').
?- go_obo(go.obo, obo(_,Terms) ), length( Terms, Len), write(length(Len)), nl.
?- shell('grep Term go.obo| wc -l').
==
@author nicos angelopoulos
@version  0:1 2021/05/08

*/ 
go_obo( OboF, Term ) :-
    SectOpts = [separator_call(obo_separating_line),terminating_separator(false),
                include_separator(true)],
    io_sections( OboF, [OboInfoSect|OboTermSects], SectOpts ),
    debuc( std_graphs_gont, length, obo_term_sects/OboTermSects ),
    debuc( std_maps_gont, length, obo_term_sects/OboTermSects ),
    go_obo_section( OboInfoSect, OboInfo ),
    go_obo_terms( OboTermSects, OboTerms ),
    Term = obo(OboInfo,OboTerms).

go_obo_terms( [], [] ).
go_obo_terms( [[Sep|Sect]|Sects], OboTerms ) :-
    ( Sep == `[Term]` ->
        ( go_obo_section(Sect,Pairs) -> true
                                  ;  maplist(atom_string,SectAtms,Sect), 
                                     throw(cannot_map_to_pairs(SectAtms))
        ),
        ( select(is_obsolete-Obs,Pairs,Pairs1) -> true ; Obs = false, Pairs1 = Pairs ),
        ( (select(id-IdPrv,Pairs1,Pairs2),go_obo_id(IdPrv,Id)) -> true ; throw(no_id(Pairs)) ),
        ( select(name-Name,Pairs2,Pairs3) -> true ; throw(no_name(Pairs)) ),
        ( (select(namespace-SpcPrv,Pairs3,Pairs4),go_obo_name_space(SpcPrv,Spc)) ->
                true 
                ; 
                throw(no_name_space(Pairs)) 
        ),
        ( (member(SecId,[is_obsolete,id,name,namespace]),memberchk(SecId-_,Pairs4)) -> 
                throw( multiple_name_of_pair(SecId,Pairs) )
                ;
                true
        ),
        OboTerms = [obo_term(Id,Name,Spc,Obs,Pairs4)|ToboTerms]
        ;
        ToboTerms = OboTerms
    ),
    go_obo_terms( Sects, ToboTerms ).

go_obo_section( [], [] ).
go_obo_section( [Ln|Lns], Pairs ) :-
    ( Ln == [] -> 
        Pairs = Tairs
        ;
        ( append(NameCs,[0':,0' |ValCs],Ln) ->
            atom_string( Name, NameCs ),
            atom_string( Val, ValCs ),
            Pairs = [Name-Val|Tairs]
            ;
            atom_string(AtmLn,Ln),
            throw(cannot_parse_line(AtmLn))
        )
    ),
    go_obo_section( Lns, Tairs ).

go_obo_non_obs( GoObo, GoOboCurr ) :-
    GoObo = obo(Info, OboTerms),
    go_obo_non_obs_terms( OboTerms, OboCurrTerms ),
    GoOboCurr = obo(Info, OboCurrTerms ).

go_obo_non_obs_terms( [], [] ).
go_obo_non_obs_terms( [O|Os], Ps ) :-
    O = obo_term(Id,Name,Nspc,Obs,Props),
    ( Obs == true ->
        Ps = TPs
        ;
        Ps = [obo_term(Id,Name,Nspc,Obs,Props)|TPs]
    ),
    go_obo_non_obs_terms( Os, TPs ).

obo_separating_line( [0'[|_] ).

go_obo_id( GoT, InT ) :-
    atomic_list_concat( ['GO',IntAtm], ':', GoT ),
    atom_number( IntAtm, InT ),
    !.
go_obo_id( GoT, InT ) :-
    throw( not_a_go_id(GoT,InT) ).

go_obo_name_space(biological_process, bp).
go_obo_name_space(cellular_component, cc).
go_obo_name_space(molecular_function, mf).
