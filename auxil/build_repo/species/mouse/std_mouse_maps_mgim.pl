
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
:- lib(bio_db_source_url/3).
:- lib(build_dnload_loc/3).

% mgim_url( 'http://www.informatics.jax.org/downloads/reports' ).

% mgim_report_seq('MRK_Sequence.rpt').
mgim_report_stem(symb, 'MRK_List1').   % List2  only included non-widrawn ones.
mgim_report_stem(seq,  'MRK_Sequence').
mgim_report_stem(prot, 'MRK_SwissProt_TrEMBL').
mgim_report_stem(swiss_prot, 'MRK_SwissProt').
mgim_report_stem(ncbi, 'MGI_EntrezGene').

std_mouse_maps_mgim_defaults( [  
                                 db(mgim),
                                 debug(true),
                                 debug_url(false),
                                 iactive(true),
                                 mgim_file(symb),
                                 org(mouse)
                              ] ).

/**  std_mouse_maps_mgim.

Create bio_db map files from the Mouse Genome Informatics Marker datasets.

Opts
  * db(Db=mgim)
    source database
  * debug(Dbg=true)
    informational, progress messages
  * debug_url(Ubg=false)
    whether to debug the concatenation of the url (via bio_db_source_url/3)
  * iactive(Iact=true)
    when false, wget reports less
  * org(Org=mouse)
    colloquial name for the organism

==
?- std_mouse__map_mgim([]).

ορέστης;build_repo/mouse% date; pupsh std_mouse_maps_mgim.pl; date
Tue 27 Dec 14:39:32 GMT 2022
% Building at: '/home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27'
% Creating dated local basename.
% Using local directory: '/home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/dnloads/mgim'
% File with today's date exists: '/home/nicos/.local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/dnloads/mgim/MRK_List1-22.12.27.rpt', so skipping download of:'http://www.informatics.jax.org/downloads/reports/MRK_List1.rpt'.
...
Tue 27 Dec 14:42:47 GMT 2022

ορέστης;dnloads/mgim% ls ../../data/
galg/  homs/  musm/
ορέστης;dnloads/mgim% cd maps
ορέστης;mgim/maps% date
Tue 27 Dec 14:45:59 GMT 2022
ορέστης;mgim/maps% pwd
/usr/local/users/nicos/local/share/swi-prolog/pack/Downloads/bio_db_repo-22.12.27/dnloads/mgim/maps

ορέστης;mgim/maps% wc -l *
   675857 mgim_musm_mgim_chrl.pl
   277235 mgim_musm_mgim_genb.pl
   718464 mgim_musm_mgim_ncbi.pl
   675857 mgim_musm_mgim_symb.pl
    16966 mgim_musm_mgim_unip.pl
    58676 mgim_musm_symb_wdra.pl
   256471 mgim_musm_syno_mgim.pl
==

@author nicos angelopoulos
@version  0.1 2018/11/2
@version  0.2 2018/2/11, added: map_mouse_mgim_mgim_entz/2.
@version  0.3 2022/12/27, new naming of db predicates and entz -> ncbi

*/
std_mouse_maps_mgim( Args ) :-
    Self = std_mouse_maps_mgim,
    options_append( Self, Args, Opts ),
    bio_db_build_aliases( Opts ),
    mgim_dnload_report( symb, Self, SymbUrl, DnDir, _SymbInF, SymbMtx, SymbDnt, Opts ),
    working_directory( Old, DnDir ),
    os_make_path( maps ),               % fixme: make sure it doesn't trip if dir exists already
    working_directory( _, maps ),
    Sims = [    to_value_1(pfx_by_num(true,'MGI:')), datetime(SymbDnt)
                | Opts
           ],
    csv_ids_map( _, 'MGI Accession ID', 'Marker Symbol', SymbMtx, SymbMapF, Sims ),
    % 23.09.25: Marker Symbol was mapping to symb, this is not specific enough, there are 677692 lines, 
    %           renamed it to mrks (at bio_db_cnm_token/2,3 and cnm_token/2,3).
    %
    SymbMapFs = [SymbMapF],
    Cpts = call_options([org(mouse),type(maps)]),
    map_list_options( link_to_bio_sub(mgim), SymbMapFs, Cpts ),
    SymbMtx = [SymbHdr|SymbRows],
    findall( mgim_musm_mgim_chrl(RMgi,RChr,RStart,REnd,RSign), (  member(SymbRow,SymbRows),
                                                arg(1,SymbRow,RMgiMFull),
                                                atomic_list_concat(['MGI',RMgiAtm],':',RMgiMFull),
                                                atom_number( RMgiAtm, RMgi ),
                                                arg(2,SymbRow,RChr),
                                                arg(4,SymbRow,RStart),
                                                arg(5,SymbRow,REnd),
                                                arg(6,SymbRow,RSign)
                                             ), MapChrlRows ),
    ChrlF = 'mgim_musm_mgim_chrl.pl',
    portray_clauses( MapChrlRows, file(ChrlF) ),
    findall( ChrlHdrArg, (member(I,[1,2,4,5,6]),arg(I,SymbHdr,ChrlHdrArg)), ChrlHdrArgs ),
    ChrlHdr =.. [hdr|ChrlHdrArgs],
    bio_db_add_infos_file( ChrlF, [source(SymbUrl),header(ChrlHdr),datetime(SymbDnt)] ),
    % here( DnDir, SymbInF, ChrlF ),
    mgim_dnload_report( seq, Self, SeqUrl, DnDir, _SeqRelF, SeqMtx, SeqDnt, Opts ),
    mtx_column_values_select( SeqMtx, 'Marker Type', 'Gene', GenMtx, _, [] ),
    debuc( Self, dims, gene/GenMtx ),
    GenMtx = [_|GenRows],
    findall( Unip-Tremb, (member(GenRow,GenRows),arg(14,GenRow,Unip),arg(15,GenRow,Tremb),Unip\=='',Tremb\==''), UTs ),
    debuc( Self, length, uTs/UTs ),
	% absolute_file_name( bio_db_downloads(mgim), MgimD ),
    Cims = [ to_value_1(pfx_by_num(true,'MGI:')), to_value_2(sep_by('|')),
             source(SeqUrl), datetime(SeqDnt) | Opts
           ],
    csv_ids_map( _, 'MGI Marker Accession ID', 'GenBank IDs', GenMtx, GenBMapF, Cims ),
    csv_ids_map( _, 'MGI Marker Accession ID', 'UniProt IDs', GenMtx, UnipMapF, Cims ),

    % entezid ( no header !)
    mgim_dnload_report( ncbi, Self, NcbiUrl, DnDir, _NcbiRelF, NcbiMtx, NcbiDnt, Opts ),
    NcbiHdr = hdr('MGI Marker Accession ID','NCBI ID'),
    % EntzOpts = [cnm_transform(mgi_entrez_idx_header),to_value_1(pfx_by_num(true,'MGI:')),prefix(mgim_mouse),to_value_2(atom_number),
    NcbiOpts = [ cnm_transform(mgi_ncbi_idx_header),
                 to_value_1(pfx_by_num(true,'MGI:')),to_value_2(=),
                 source(NcbiUrl), datetime(NcbiDnt), has_header(false), header(NcbiHdr)
                 | Opts
           ],
    csv_ids_map( _, 1, 9, NcbiMtx, MapNcbiF, NcbiOpts ),
    % cnm_token('Marker Symbol', _, mrks ).  Succeeds.
    % 23.9.25 this previously seems to have been re-doing 'Marker Symbol'
    findall( mgim_musm_mgim_symb(RMgi,RSymb), ( member(SymbRow,SymbRows),
                                                    arg(10,SymbRow,'Gene'),       % added 23.9.25
                                                    arg(1,SymbRow,RMgiFull),
                                                    atomic_list_concat(['MGI',RMgiAtm], ':', RMgiFull),
                                                    atom_number( RMgiAtm, RMgi ),
                                                    arg(7,SymbRow,RSymb)
                                                  ),
                                                    MapSymbRows ),
    MapMrksF = 'mgim_musm_mgim_symb.pl',
    portray_clauses( MapSymbRows, file(MapMrksF) ),
    MapSymbHdr = hdr('MGI Marker Accession ID','Marker Symbol (Gene - only)'), % fixme: use arg ?
    bio_db_add_infos_file( MapMrksF, [source(SymbUrl),header(MapSymbHdr),datetime(SymbDnt)] ),
    SynoOpts = [ to_value_2(pfx_by_num(true,'MGI:')), to_value_1(sep_by('|')),
                 source(SymbUrl), datetime(SymbDnt)
                 | Opts
               ],
    csv_ids_map( _, 'Marker Synonyms (pipe-separated)', 'MGI Accession ID', SymbMtx, MapSynoF, SynoOpts ),

    % withdrawn
    WdraOpts = [to_value_2(withdrawn), source(SymbUrl), datetime(SymbDnt) | Opts],
    csv_ids_map( _, 'Marker Symbol', 'Marker Name', SymbMtx, MapWdraF, WdraOpts ),
    MapFs = [GenBMapF,ChrlF,UnipMapF,MapSynoF,MapWdraF,MapNcbiF],
    Cpts = call_options([org(mouse),type(maps)]),
    map_list_options( link_to_bio_sub(mgim), MapFs, Cpts ),

    working_directory( _, Old ),
    % here( here(GenBMapF,DnDir,SeqRelF) ).
    debuc( Self, end, true ).

mgi_ncbi_idx_header( 1, mgim ).
mgi_ncbi_idx_header( 9, ncbi ).

mgim_dnload_report( Which, Self, Url, DnDir, BaseF, Mtx, DntStamp, Opts ) :-
    mgim_report_stem( Which, Stem ),
    os_ext( rpt, Stem, Uname ),
    bio_db_source_url( Url, [debug_url-debug], [url_file(Uname)|Opts] ),
    build_dnload_loc( Self, DnDir, Opts ),
    UrlOpts = [interface(wget),file(BaseF),dnt_stamp(DntStamp)|Opts],
    url_file_local_date_mirror( Url, DnDir, UrlOpts ),
    os_path( DnDir, BaseF, AbsF ),
    mtx( AbsF, Mtx, sep(tab) ),
    debuc( Self, dims, Which/Mtx ).

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
