
:- use_module(library(lib)).

% external code, lib knowns how to deal with these (will install if missing)
:- lib(options).
:- lib(debug_call).
:- lib(pack_errors).  % throw/2.

:- ensure_loaded('bio_db_build_aliases' ).  % /1, also sets the lib dir

% local libs & sources
:- lib(org_ense_dir/4).         
:- lib(bio_db_build_messages/0).             % loads the error messages pretty printing
:- lib(bio_db_source_base_url/2).

bio_db_source_url_defaults( [ debug(false),
                              url_type('Source')
                            ]
                          ).

/** bio_db_source_url(-Url, +Renames +Opts).

Create a full Url from options passed in Opts with possible -Pairs of option name Renames.

Opts
  * db(Db)
    database, if given and matches a 1st arg of bio-db_source_base_url/2, it is used to find the base url (overrides BuTkn)
  * debug(Dbg=false)
    informational, progress messages
  * url_base(BuTkn)
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
?- bio_db_source_url(Url,[],[]).
ERROR: options:options/2: Required option: url_base(_6930), not present in options: [$restore(bio_db_source_url,debug,false),$restore(bio_db_source_url,debug,false),debug(false),url_type(Source)]

?- bio_db_source_url(Url,[],[url_base(gont_obo),url_file('go.obo')]).
Url = 'https://purl.obolibrary.org/obo/go.obo'.

?- bio_db_source_url(Url,[],[url_base('https://purl.obolibrary.org/obo/'),url_file('go.obo')]). 
Url = 'https://purl.obolibrary.org/obo/go.obo'.

?- bio_db_source_url(Url,[],[url_base(false),bio_db:example/1]).cls

ERROR: bio_db:bio_db_source_url/2: Not a valid URL token (1st arg of bio_db_source_base_url/2) or Url. Got: false.
ERROR: Trail: [bio_db:example/1]
==

@author nicos angelopoulos
@version  0.1 2023/09/22
@see bio_db_source_base_url/2

*/
bio_db_source_url( Url, RnmS, Args ) :-
     Self = bio_db_source_url,
     options_append( Self, Args, OptsPrv ),
     ( is_list(RnmS) -> RnmS = Rnms; ( Rnms == true-> Rnms = []; Rnms = [RnmS] ) ),
     ( Rnms == [] -> OptsPrv = Opts
                  ;  options_rename( OptsPrv, Rnms, Opts, true )
     ),
     ( memberchk(url_base(BuTkn), Opts) ->
          true
          ;
          ( memberchk(db(BuTkn),Opts) ->
                    true
                    ;
                    throw(multi_option_miss([url_base/1,db/1]), Opts)
          )
     ),
     bio_db_source_url_base( BuTkn, BsUrl, Opts ),
     options( url_file(SrcFPrv),  Opts ),
     options( org(Org), Opts ),
     bio_db_source_url_file( SrcFPrv, BsUrl, Org, SrcF, Opts ),
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
     throw( base_url(not_a(Url)), [bio_db:bio_db_source_url/3|Opts] ). 

bio_db_source_url_file( call(Goal), BsUrl, Org, SrcF, Opts ) :-
     !,
     call( Goal, BsUrl, Org, SrcF, [bio_db_build:bio_db_source_url/3|Opts] ).
bio_db_source_url_file( SrcF, _BsUrl, _Org, SrcF, _Opts ).

ense_url_file( Url, Org, SrcF, Opts ) :-
     org_ense_dir( Org, Eir, Stem, Opts ),
     atomic_list_concat( [Url,Eir,'/'], Orl ),  % organism specific sub-directory
     Found @@ curl( -l, '--no-progress-meter', Orl ),
     at_con( Stemics, '.', Stem ),
     findall( HsGtfRel-Amb-Rel, ( member(HsGtf,Found),
                                  at_con(Parts,'.',HsGtf),
                                  ense_url_file_parts( Org, Stemics, Parts, Amb, Rel ),
                                  atomic_list_concat( [Eir,HsGtf], '/', HsGtfRel )
                        ),
                            HsGtfs ),
     ( HsGtfs = [SrcF-_Amb-_Rel] ->
          true
          ;
          throw( non_unique_auto_ided_ense_gtf_files(Orl,HsGtfs) )
     ).

ense_url_file_parts( _Org, Stemics, Parts, Amb, Rel ) :-
     once( append(Stemics,[GRChTkn,RelAtm,gtf,gz],Parts) ),
     atom_concat('GRC',Amb,GRChTkn),
     atom_number(RelAtm,Rel),
     !.
ense_url_file_parts( pig, Stemics, Parts, Amb, Rel ) :-
     once( append(Stemics,[Scrofa,Edit,RelAtm,gtf,gz],Parts) ),
     atom_concat( 'Sscrofa', Vers, Scrofa ), 
     atomic_list_concat( [Vers,Edit], '.', Amb ),
     atom_number(RelAtm,Rel),
     !.
