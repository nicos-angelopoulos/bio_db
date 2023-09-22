
:- use_module(library(lib)).

% external code, lib knowns how to deal with these (will install if missing)
:- lib(options).
:- lib(debug_call).
:- lib(pack_errors).  % throw/2.

:- ensure_loaded('bio_db_build_aliases' ).  % /1, also sets the lib dir

% local libs & sources
:- lib(bio_db_build_messages/0).             % loads the error messages pretty printing
:- lib(bio_db_source_base_url/2).

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

Opts are also passed to throw/2 if a URL cannot be constructed.

Examples
==
?- bio_db_source_url(Url,[]).
ERROR: options:options/2: Required option: base_url(_6930), not present in options: [$restore(bio_db_source_url,debug,false),$restore(bio_db_source_url,debug,false),debug(false),url_type(Source)]

?- bio_db_source_url(Url,[base_url(gont_obo),url_file('go.obo')]).
Url = 'https://purl.obolibrary.org/obo/go.obo'.

?- bio_db_source_url(Url,[base_url('https://purl.obolibrary.org/obo/'),url_file('go.obo')]). 
Url = 'https://purl.obolibrary.org/obo/go.obo'.

?- bio_db_source_url(Url,[base_url(false),bio_db:example/1]).
ERROR: bio_db:bio_db_source_url/2: Not a valid URL token (1st arg of bio_db_source_base_url/2) or Url. Got: false.
ERROR: Trail: [bio_db:example/1]
==

@author nicos angelopoulos
@version  0.1 2023/09/22
@see bio_db_source_base_url/2
*/

bio_db_source_url( Url, Args ) :-
     Self = bio_db_source_url,
     options_append( Self, Args, Opts ),
     options( base_url(BuTkn), Opts ),
     bio_db_source_url_base( BuTkn, BsUrl, Opts ),
     options( url_file(SrcF),  Opts ),
     atom_concat( BsUrl, SrcF, Url ),
     options( url_type(Utype), Opts ),
     debuc( Self, '~w URL: ~p', [Utype,Url] ).

bio_db_source_url_base( Tkn, BaseUrl, _Opts ) :-
     bio_db_source_base_url( Tkn, BaseUrl ),
     !.
bio_db_source_url_base( Url, BaseUrl, _Opts ) :-
     atom_concat( 'http', _, Url ),
     !,
     BaseUrl = Url.
bio_db_source_url_base( Url, _BaseUrl, Opts ) :-
     throw( base_url(not_a(Url)), [bio_db:bio_db_source_url/2|Opts] ). 
