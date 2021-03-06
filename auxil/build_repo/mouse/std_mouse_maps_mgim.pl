

:- use_module(library(apply)).  % maplist/2.
:- use_module(library(lists)).  % member/2.

% if library(lib) is missing, install via pack_install(lib).
%
:- use_module(library(lib)).

% external code, lib knowns how to deal with these (will install if missing)
:- lib(mtx).
:- lib(os_lib).
:- lib(options).
:- lib(debug_call).
:- lib(stoics_lib:portray_clauses/2).


% also sets lib alias to that dir
:- ensure_loaded('../../lib/bio_db_build_aliases').  % /1.

% local libs & sources
:- lib(url_file_local_date_mirror/3).
:- lib(pfx_by/4).
:- lib(csv_ids_map/6).
:- lib(link_to_bio_sub/4).
:- lib(bio_db_add_infos/1).  % bio_db_add_infos_file/2.

mgim_url( 'http://www.informatics.jax.org/downloads/reports' ).

% mgim_report_seq('MRK_Sequence.rpt').
mgim_report(symb, 'MRK_List1').   % List2  only included non-widrawn ones.
mgim_report(seq,  'MRK_Sequence').
mgim_report(prot, 'MRK_SwissProt_TrEMBL').
mgim_report(swiss_prot, 'MRK_SwissProt').
mgim_report(entrez, 'MGI_EntrezGene').

std_mouse_maps_mgim_defaults(debug(true)).

/**  std_mouse_maps_mgim.

Create bio_db map files from the Mouse Genome Informatics Marker datasets.

Opts
  * opt(Opt=_)
     is a...

==
?- std_mouse__map_mgim.
==

@author nicos angelopoulos
@version  0.1 2018/11/2
@version  0.2 2018/2/11, added: map_mouse_mgim_mgim_entz/2.

*/
std_mouse_maps_mgim( Args ) :-
    Self = std_mouse_maps_mgim,
    options_append( Self, Args, Opts ),
    bio_db_build_aliases( Opts ),
    mgim_get_report( symb, Self, SymbUrl, DnDir, _SymbInF, SymbMtx, SymbDnt ),
    working_directory( Old, DnDir ),
    os_make_path( maps ),   % fixme: make sure it doesn't trip if dir exists already
    working_directory( _, maps ),
    Sims = [    cnm_transform(mouse_cnm), to_value_1(pfx_by_num(true,'MGI:')),
                prefix(mgim_mouse), datetime(SymbDnt) ],
    csv_ids_map( _, 'MGI Accession ID', 'Marker Symbol', SymbMtx, SymbMapF, Sims ),
    SymbMapFs = [SymbMapF],
    Cpts = call_options([org(mouse),type(maps)]),
    map_list_options( link_to_bio_sub(mgim), SymbMapFs, Cpts ),
    SymbMtx = [SymbHdr|SymbRows],
    findall( map_mgim_mouse_mgim_chrl(RMgi,RChr,RStart,REnd,RSign), (  member(SymbRow,SymbRows),
                                                arg(1,SymbRow,RMgiMFull),
                                                atomic_list_concat(['MGI',RMgiAtm],':',RMgiMFull),
                                                atom_number( RMgiAtm, RMgi ),
                                                arg(2,SymbRow,RChr),
                                                arg(4,SymbRow,RStart),
                                                arg(5,SymbRow,REnd),
                                                arg(6,SymbRow,RSign)
                                             ), MapChrlRows ),
    ChrlF = 'map_mgim_mouse_mgim_chrl.pl',
    portray_clauses( MapChrlRows, file(ChrlF) ),
    findall( ChrlHdrArg, (member(I,[1,2,4,5,6]),arg(I,SymbHdr,ChrlHdrArg)), ChrlHdrArgs ),
    ChrlHdr =.. [hdr|ChrlHdrArgs],
    bio_db_add_infos_file( ChrlF, [source(SymbUrl),header(ChrlHdr),datetime(SymbDnt)] ),
    % here( DnDir, SymbInF, ChrlF ),
    mgim_get_report( seq, Self, SeqUrl, DnDir, _SeqRelF, SeqMtx, SeqDnt ),
    mtx_column_values_select( SeqMtx, 'Marker Type', 'Gene', GenMtx, _, [] ),
    debuc( Self, dims, gene/GenMtx ),
    GenMtx = [_|GenRows],
    findall( Unip-Tremb, (member(GenRow,GenRows),arg(14,GenRow,Unip),arg(15,GenRow,Tremb),Unip\=='',Tremb\==''), UTs ),
    debuc( Self, length, uTs/UTs ),
	% absolute_file_name( bio_db_downloads(mgim), MgimD ),
    Cims = [cnm_transform(mouse_cnm),to_value_1(pfx_by_num(true,'MGI:')),prefix(mgim_mouse),to_value_2(sep_by('|')),
            source(SeqUrl), datetime(SeqDnt)
           ],
    csv_ids_map( _, 'MGI Marker Accession ID', 'GenBank IDs', GenMtx, GenBMapF, Cims ),
    csv_ids_map( _, 'MGI Marker Accession ID', 'UniProt IDs', GenMtx, UnipMapF, Cims ),

    % entezid ( no header !)
    mgim_get_report( entrez, Self, EntzUrl, DnDir, _EntzRelF, EntzMtx, EntzDnt ),
    EntzHdr = hdr('MGI Marker Accession ID','Entrez ID'),
    % EntzOpts = [cnm_transform(mgi_entrez_idx_header),to_value_1(pfx_by_num(true,'MGI:')),prefix(mgim_mouse),to_value_2(atom_number),
    EntzOpts = [cnm_transform(mgi_entrez_idx_header),to_value_1(pfx_by_num(true,'MGI:')),prefix(mgim_mouse),to_value_2(=),
            source(EntzUrl), datetime(EntzDnt), has_header(false),header(EntzHdr)
           ],
    csv_ids_map( _, 1, 9, EntzMtx, MapEntzF, EntzOpts ),


    % symbol & synonyms:
    findall( map_mgim_mouse_mgim_symb(RMgi,RSymb), ( member(SymbRow,SymbRows),
                                                    arg(1,SymbRow,RMgiFull),
                                                    atomic_list_concat(['MGI',RMgiAtm], ':', RMgiFull),
                                                    atom_number( RMgiAtm, RMgi ),
                                                    arg(7,SymbRow,RSymb)
                                                  ),
                                                    MapSymbRows ),
    MapSymbF = 'map_mgim_mouse_mgim_symb.pl',
    portray_clauses( MapSymbRows, file(MapSymbF) ),
    MapSymbHdr = hdr('MGI Marker Accession ID','Symbol'), % fixme: use arg ?
    bio_db_add_infos_file( MapSymbF, [source(SymbUrl),header(MapSymbHdr),datetime(SymbDnt)] ),
    SynoOpts = [cnm_transform(mouse_cnm),to_value_2(pfx_by_num(true,'MGI:')),prefix(mgim_mouse),to_value_1(sep_by('|')),
            source(SymbUrl), datetime(SymbDnt)
           ],
    csv_ids_map( _, 'Marker Synonyms (pipe-separated)', 'MGI Accession ID', SymbMtx, MapSynoF, SynoOpts ),

    % withdrawn
    WdraOpts = [cnm_transform(mouse_cnm_withdrawn),to_value_2(withdrawn),prefix(mgim_mouse),% to_value_1(sep_by('|')),
            source(SymbUrl), datetime(SymbDnt)
           ],
    csv_ids_map( _, 'Marker Symbol', 'Marker Name', SymbMtx, MapWdraF, WdraOpts ),
    MapFs = [GenBMapF,ChrlF,UnipMapF,MapSynoF,MapWdraF,MapEntzF],
    Cpts = call_options([org(mouse),type(maps)]),
    map_list_options( link_to_bio_sub(mgim), MapFs, Cpts ),

    working_directory( _, Old ),
    % here( here(GenBMapF,DnDir,SeqRelF) ).
    debuc( Self, end, true ).

mgi_entrez_idx_header( 1, mgim ).
mgi_entrez_idx_header( 9, entz ).

/* atom_number/2 does the same, except for exception...
mgi_entrez_id( '', _ ) :- !, fail.
mgi_entrez_id( Atom, Number ) :- 
    atom_number( Atom, Number ),
    !.
mgi_entrez_id( Atom, Number ) :- 
    throw( cannot_entrez_id_this(Atom,Number) ).
    */

mgim_get_report( Which, Self, Url, DnDir, RelF, Mtx, DntStamp ) :-
    mgim_url( Base ),
    mgim_report( Which, Stem ),
    % atomic_list_concat( [Base,'/MRK_',Stem,'.rpt'], Url ),
    atomic_list_concat( [Base,'/',Stem,'.rpt'], Url ),
    mgim_dnload_dir( DnDir ),
	UrlOpts = [debug(true),interface(wget),file(RelF),dnt_stamp(DntStamp)],
    url_file_local_date_mirror( Url, DnDir, UrlOpts ),
    os_path( DnDir, RelF, AbsF ),
    mtx( AbsF, Mtx, sep(tab) ),
    debuc( Self, dims, Which/Mtx ).

mouse_cnm( 'MGI Accession ID', mgim ).
mouse_cnm( 'MGI Marker Accession ID', mgim ).
mouse_cnm( 'GenBank IDs', genb ).
mouse_cnm( 'UniProt IDs', unip ).
mouse_cnm( 'Marker Symbol', symb ).
mouse_cnm( 'Marker Synonyms (pipe-separated)', syno ).

mgim_dnload_dir( Loc ) :-
    absolute_file_name( bio_db_build_downloads(mgim), Loc ),
    debuc( mouse, 'Loc: ~p', Loc ),
	os_make_path( Loc, debug(true) ).

sep_by( _, '', _ ) :- !, fail. % do not include empties
sep_by( Sep, Atom, List ) :-
    atomic_list_concat(  List, Sep, Atom ).

withdrawn( Full, Rem ) :-
    atom_concat( 'withdrawn, = ', Rem, Full ).

pfx_by_num( true, Pfx, Full, Rem ) :-
    atom_concat( Pfx, RemPrv, Full ),
    ( atom_number(RemPrv,Rem) -> true; Rem=RemPrv ).
pfx_by_num( false, Pfx, Full, Full ) :-
    atom_concat( Pfx, _, Full ).
    
mouse_cnm_withdrawn( 'Marker Symbol', symb ).
mouse_cnm_withdrawn( 'Marker Name',  wdra ).
