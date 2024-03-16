
bio_db_org_in_opts_defaults( Defs ) :-
          Defs = [
                    debug(false),
                    org(hs),
                    org_tkn(_)
                 ].

/** bio_db_org_in_opts(-Org,+Opts).

Select organism in option list and convert to canonical name.

Opts
  * debug(Dbg=false)
    informational, progress messages
  * org(OrgOpt=hs)
    organism of which to return the canonical name as Org
  * org_tkn(Tkn)
    returns the db token for the organism

Examples
==
?- bio_db_org_in_opts(Org,[]).
Org = human.

?- bio_db_org_in_opts(Org,[org(gallus),org_tkn(Tkn)]).
Org = chicken,
Tkn = galg.
==

@author nicos angelopoulos
@version  0.1 2024/03/16

*/
bio_db_org_in_opts( Org, Args ) :-
     Self = bio_db_org_in_opts,
     options_append( Self, Args, Opts ),
     options( org(OrgOpt), Opts ),
     bio_db_organism( OrgOpt, Token, Org ),
     memberchk( org_tkn(Token), Opts ).
