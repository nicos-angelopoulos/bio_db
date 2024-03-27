
:- use_module(library(lib)).

:- lib(options).
:- lib(debug_call).
:- lib(stoics_lib:known/1).

% local
:- ensure_loaded(bio_db_build_organism).

bio_db_taxo_defaults([org(hs)]).

/** bio_db_taxo(-Org, -Tax, +Opts).
    bio_db_taxo(+Org, -Tax).

Tax-onomy is the NCBI tax_id of bio_db Org-anism.

In the /2 version Org is returned in the canonical name (via bio_db_organism/2).
Opts
  * debug(Dbg=false)
    informational, progress messages
  * org(Org=hs)
    any recognised alias for the organism

Examples
==
?- bio_db_taxo(Org, Tax, []).
Org = human,
Tax = 9606.

?- bio_db_taxo(hs, Tax).
Tax = 9606.

?- bio_db_taxo(Org, Tax, [org(gallus)]).
Org = chicken,
Tax = 9031.
==

@author nicos angelopoulos
@version  0.1 2024/03/27
@see bio_db_organism/2
*/

bio_db_taxo( Org, Tax, Args ) :-
     Self = bio_db_taxo,
     options_append( Self, Args, Opts ),
     options( org(KnownAs), Opts ),
     bio_db_organism( KnownAs, Org ),
     known( bio_db_canon_taxo(Org,Tax) ).
 
bio_db_taxo( KnownAs, Tax ) :-
     bio_db_organism( KnownAs, Org ),
     known( bio_db_canon_taxo(Org,Tax) ).

% fixme: ideally we want to hook this to NCBI taxonomy (see organism=multi)
bio_db_canon_taxo(chicken, 9031).
bio_db_canon_taxo(human,   9606).
bio_db_canon_taxo(mouse,  10090).
bio_db_canon_taxo(pig,     9823).
