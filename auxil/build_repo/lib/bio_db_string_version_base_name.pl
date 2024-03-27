
:- lib(bio_db_taxo/3).
:- lib(bio_db_source_url/2).

bio_db_string_version_base_name( Defs ) :-
                                   Defs = [
                                            relation(links)
                                          ].

/** bio_db_string_version_base_name(+Vers, -VersD, -Bname, -Url, +Opts).

Generate remote base name, remote version directory and URL from string Version Vers and options Opts.

Opts
  * relation(links)
    or =info=

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
     options( [relation(Relt),debug_url(Dbg)], Opts ),
     bio_db_taxo( _Org, Taxo, Opts ),
     % atomic_list_concat( ['protein.',Relt,'.v',Vers,'/',Taxo,'.protein.',Relt,'.v',Vers,'.txt.gz'], Bname ),
     atomic_list_concat( ['protein.',Relt,'.v',Vers], VersD ),
     atomic_list_concat( [Taxo,'.protein.',Relt,'.v',Vers,'.txt.gz'], Bname ),
     atomic_list_concat( [VersD,Bname], '/', RelP ),
     Upts = [url_file(RelP),url_base(strg),debug(Dbg)|Opts],
     bio_db_source_url( Remote, [], Upts ).


