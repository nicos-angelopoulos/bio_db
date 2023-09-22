
:- use_module(library(lib)).

:- lib(options).
:- lib(debug_call).

bio_db_source_url_defaults( [ debug(false),
                              url_type('Source')
                            ]
                          ).

/** bio_db_source_url(-Url, +Opts).

Create a full Url from options passed in Opts.

Opts
  * debug(Dbg=false)
    informational, progress messages
  * base_url(BuTkn)
    the base url token passed as 1st arg to bio_db_source_base_url/2, or Url pointing to directory
  * url_file(BaseF)
    the (relative path) name of the file to be added to BuTkn expansion
  * url_type(UrlType='Source')
    Only used in reporting what kind of Url was constructed

Note, that likely Dbg will be overriden when the predicate is
called with Opts transferred from the caller.

Examples
==
?- bio_db_source_url(Url,[]).
?- bio_db_source_url(Url,[base_url(gont_obo),url_file()]).
?- bio_db_source_url(Url,[base_url('https://purl.obolibrary.org/obo/')]). 
==

@author nicos angelopoulos
@version  0.1 2023/09/22
@see bio_db_source_base_url/2
*/

bio_db_source_url( Url, Args ) :-
     Self = bio_db_source_url,
     options_append( Self, Args, Opts ),
     options( base_url(BuTkn), Opts ),
     bio_db_source_url_base( BuTkn, BsUrl ),
     options( url_file(SrcF),  Opts ),
     atom_concat( BsUrl, SrcF, Url ),
     options( url_type(Utype), Opts ),
     debuc( Self, '~w URL: ~p', [Utype,Url] ).
