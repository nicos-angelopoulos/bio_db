
:- set_prolog_flag(stack_limit, 2 000 000 000).

:- use_module(library(apply)).
% if library(lib) is missing, install via pack_install(lib).
:- use_module(library(lib)).

% stoics packs, lib knowns how to deal with these (will install if missing)
:- lib(mtx).
:- lib(os_lib).
:- lib(by_unix).
:- lib(debug_call).
:- lib(stoics_lib).

% bio_db loads
:- ensure_loaded('../../lib/bio_db_build_aliases').  % /1.  also adds lib alias to that dir

:- lib(link_to_bio_sub/3).
:- lib(bio_db_dnt_times/3).
:- lib(bio_db_add_infos/1).             % bio_db_add_infos_to/2
:- lib(build_dnload_loc/3).
:- lib(bio_db_source_url/3).
:- lib(bio_db_build_organism/3).
:- lib(url_file_local_date_mirror/3).

std_maps_reac_defaults( Def ) :-
     Def = [ db(reac),
             debug(true),
             debug_fetch(true),
             debug_url(false),
             iactive(true),
             org(human),
             reac_file('NCBI2Reactome_PE_All_Levels.txt')
     ].

/** std_maps_reac( +Opts ).

Uses the PE_All_Levels file with NCBI identfiers.

The species is identified by 'Homo sapiens' at the last arg of each row.
The PE file contains more detailed info that All_levels.txt .

Opts
  * db(Db=reac)
    database token, also rel dir name
  * debug(Dbg=true)
    progress, informational message
  * debug_fetch(Fbg=true)
    whether to debug the fetching of the url (via url_file_local_date_mirror/3)
  * debug_url(Ubg=false)
    whether to debug the concatenation of the url (via bio_db_source_url/3)
  * iactive(Iact=true)
    whether the session is interactive, otherwise wget gets --no-verbose
  * org(Org=human)
    as recognised by db_organism. note here human maps are given hs token
  * reac_file(ReacF='NCBI2Reactome_PE_All_Levels.txt')
    the url base for the Reactome Url

Tokens
  * ncbi(Ncbi)
    NCBI gene identfier (integer)
  * reac(Reac)
    reactome product, the integer postfix only ('R-HSA-' removed)
  * reap(Reactome(Pathway))
    reactome pathway, the integer postfix only ('R-HSA-' removed)
  * recn(Reac(Name))
    reactome product name
  * recl(Reac(Loc))
    localisation for reactome products (controlled vocab)

Currently we map
  * map_reac_ncbi_reac(2)
    Ncbi -> reac-tome product (1 to many)
  * map_reac_ncbi_reap(2)
    Ncbi -> reactome pathway: many-many
  * map_reac_reac_reap(3)
    Reactome product to pathway, many-many, incl inference
  * map_reac_reac_recn(2)
    Reactome product to its name
  * map_reac_reac_recl(2)
    Reactome product to localisation
  * map_reap_reap_repn(2)
    Reactome pathway (id, int part only) to pathway name

Note that we start using ncbi rather than entz, as these are now known.

@author nicos angelopoulos
@version  0:1 2022/12/24

*/
std_maps_reac( Args ) :-
     Self = std_maps_reac,
     options_append( Self, Args, Opts ),
     bio_db_build_aliases( Opts ),
     build_dnload_loc( Self, DnDir, Opts ),
     bio_db_source_url( Url, [debug_url-debug,reac_file-url_file], Opts ),
     url_file_local_date_mirror( Url, DnDir, [date(prefix)|Opts] ),
     os_path( _, Local, Url ),
     debuc( Self, 'Local: ~p', [Local] ),
     os_path( Dir, Local, InP ),
     mtx( InP, Mtx, sep(tab) ),
     debuc( Self, dims, mtx/Mtx ),
     % Forg = 'Homo sapiens', Org = hsp, 
     Stems= [ncbi_reac,ncbi_reap,reac_reap,reac_recn,reac_recl,reap_repn],
     options( org(OrgIn), Opts ),
     bio_db_organism( OrgIn, Otkn, Org ),
     std_maps_reac_org( Org, Roid, Rorg ),
     maplist( std_maps_reac_postfix_pname(Otkn), Stems, Pnames ),
     std_maps_reac_rows( Mtx, Self, Rorg, Roid, Pnames, 0, Cnt, Neac, Neap, Recp, Recn, Recl, Repn ),
     debuc( Self, '~d records belonged to: ~w', [Cnt,Rorg] ),

     Maps = [Neac, Neap, Recp, Recn, Recl, Repn],
     os_path( Dir, maps, MapsP ),
     os_make_path( MapsP ),
     bio_db_dnt_times( InP, DnDt, _DnEn ),
     maplist( std_maps_reac_portray(Self,Otkn,MapsP,Url,DnDt), Stems, Maps, Files ),
     debuc( Self, enum, files/Files ),
     debuc( Self, end, true ).

std_maps_reac_postfix_pname( Dorg, Psfx, Stem ) :-
     at_con( [reac,Dorg,Psfx], '_', Stem ).

std_maps_reac_portray( Self, Dorg, Dir, Url, DnDt, Psfx, Map, File ) :-
     at_con( [reac,Dorg,Psfx], '_', Stem ),
     os_dir_stem_ext( File, [ext(pl),odir(Dir),stem(Stem)] ),
     debuc( Self, 'Output: ~p', [File] ),
     sort( Map, Ord ),
     portray_clauses( Ord, file(File) ),
     std_maps_react_header( Psfx, Hdr ),
     bio_db_add_infos_to( [header(Hdr),source(Url),datetime(DnDt)], File ),
     link_to_bio_sub( reac, File, [org(hs),type(maps)] ).

std_maps_react_header( ncbi_reac, hdr('NCBI gene id','Reactome product id') ).
std_maps_react_header( ncbi_reap, hdr('NCBI gene id','Evidence','Reactome pathway id') ).
std_maps_react_header( reac_reap, hdr('Reactome product id','Evidence','Reactome pathway id') ).
std_maps_react_header( reac_recn, hdr('Reactome product id','Reactome product name') ).
std_maps_react_header( reac_recl, hdr('Reactome product id','Reactome localisation') ).
std_maps_react_header( reap_repn, hdr('Reactome pathway id','Reactome pathway name') ).

  % * map_reac_hsap_ncbi_reac(2)
  % * map_reac_hsap_ncbi_reap(3)
  % * map_reac_hsap_reac_reap(3)
  % * map_reac_hsap_reac_recn(2)
  % * map_reac_hsap_reac_recl(2)
  % * map_reac_hsap_reap_repn(2)
    
std_maps_reac_rows( [], _Self, _Rrg, _Roid, _Pnames, Cnt, Cnt, [], [], [], [], [], [] ).
std_maps_reac_rows( [Rw|Rws], Self, Rorg, Roid, Pnames, I, Cnt, NeacRs, NeapRs, RecpRs, RecnRs, ReclRs, RepnRs ) :-
     ( (arg(8,Rw,Rorg),arg(2,Rw,Feac),catch(std_maps_reac_id(Feac,Roid,Reac),_,fail)) ->
          Rw = row(Ncbi,Feac,RnAc,Feap,_Url,Repn,Reev,Rorg),
          J is I + 1,
          % std_maps_reac_id( Feac, Reac ),
          std_maps_reac_id( Feap, Roid, Reap ),
          std_maps_reac_recn_recl( RnAc, Recn, Recl ),
          Pnames=   [NeacPn,NeapPn,RecpPn,RecnPn,ReclPn,RepnPn],
          NeacH =.. [NeacPn,Ncbi,Reac],      NeacRs = [NeacH|NeacT],
          NeapH =.. [NeapPn,Ncbi,Reev,Reap], NeapRs = [NeapH|NeapT],
          RecpH =.. [RecpPn,Reac,Reev,Reap], RecpRs = [RecpH|RecpT],
          RecnH =.. [RecnPn,Reac,Recn],      RecnRs = [RecnH|RecnT],
          ReclH =.. [ReclPn,Reac,Recl],      ReclRs = [ReclH|ReclT],
          RepnH =.. [RepnPn,Reap,Repn],      RepnRs = [RepnH|RepnT]
          ;
          J is I,
          NeacRs = NeacT,
          NeapRs = NeapT,
          RecpRs = RecpT,
          RecnRs = RecnT,
          ReclRs = ReclT,
          RepnRs = RepnT
     ),
     std_maps_reac_rows( Rws, Self, Rorg, Roid, Pnames, J, Cnt, NeacT, NeapT, RecpT, RecnT, ReclT, RepnT ).

std_maps_reac_recn_recl( RnAc, Recn, Recl ) :-
     at_con( [Left,Right], '[', RnAc ),
     atom_concat( Recl, ']', Right ),
     atom_concat( Recn, ' ', Left ),
     !.
std_maps_reac_recn_recl( RnAc, _Recn, _Recl ) :-
     throw( cannot_recn_recl(RnAc) ).

std_maps_reac_id( Feac, Roid, Reac ) :-
     at_con( ['R', Roid,Aeac], '-', Feac ),
     atom_number( Aeac, Reac ),
     !.
std_maps_reac_id( Feac, Roid, _Reac ) :-
     thrown( could_not_convert_reactome_token_to_int_id(Feac,Roid) ).
     
std_maps_reac_org( chicken, 'GGA', 'Gallus gallus' ).
std_maps_reac_org( gallus, 'GGA', 'Gallus gallus' ).
std_maps_reac_org( hs, 'HSA', 'Homo sapiens' ).
std_maps_reac_org( human, 'HSA', 'Homo sapiens' ).
std_maps_reac_org( mouse, 'MMU', 'Mus musculus' ).
std_maps_reac_org( musm, 'MMU', 'Mus musculus' ).
