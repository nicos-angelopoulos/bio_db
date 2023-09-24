
:- lib(bio_db_organism/2).
:- lib(bio_db_source_url/2).

bio_db_string_version_base_name( Defs ) :-
                                   Defs = [ 
                                            relation(links),
                                            org(human)
                                          ].

/** bio_db_string_version_base_name(+Vers, -VersD, -Bname, -Url, +Opts).

Generate remote base name, remote version directory and URL from string Version Vers and options Opts.

Opts
  * relation(links)
    or =info=
  * org(Org=human)
    or =chicken=

==
?- bio_db_string_version_base_name( 12, Bname, Rem, [] ).

?- Opts = [relation(info),org(chicken)],
   bio_db_string_version_
==

@author nicos angelopoulos
@version  0:1 2023/09/22

*/
bio_db_string_version_base_name( VersionPrv, VersD, Bname, Remote, Opts ) :-
     ( atom_concat(v,Vers,VersionPrv)->true;Vers=VersionPrv ),
     options( [relation(Relt),debug_url(Dbg),org(KnownAs)], Opts ),
     bio_db_organism( KnownAs, Org ),
     bio_db_taxo( Org, Taxo ),
     % atomic_list_concat( ['protein.',Relt,'.v',Vers,'/',Taxo,'.protein.',Relt,'.v',Vers,'.txt.gz'], Bname ),
     atomic_list_concat( ['protein.',Relt,'.v',Vers], VersD ),
     atomic_list_concat( [Taxo,'.protein.',Relt,'.v',Vers,'.txt.gz'], Bname ),
     atomic_list_concat( [VersD,Bname], '/', RelP ),
     Upts = [url_file(RelP),url_base(strg),debug(Dbg)],
     bio_db_source_url( Remote, Upts ).

% fixme: ideally we want to hook this to NCBI taxonomy (see organism=multi)
bio_db_taxo(human, 9606).
bio_db_taxo(chicken, 9031).
