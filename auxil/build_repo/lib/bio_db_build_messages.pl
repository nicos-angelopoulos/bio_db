
:- use_module(library(lib)).

:- lib(pack_errors).

/** bio_db_build_messages.

Doc predicate, for pack(pack_errors) messages.

Examples
==
?- [pack(bio_db/auxil/build_repo/lib/bio_db_source_url)].
?- bio_db_source_url(Url,[base_url(false),predicate(example/1),pack(bio_db)]).
==

@author nicos angelopoulos
@version  0.1 2023/09/22

*/
bio_db_build_messages.

:- multifile(pack_errors:message/3).

pack_errors:message( base_url(not_a(NaUrl)) ) -->
    ['Not a valid URL token (1st arg of bio_db_source_base_url/2) or Url. Got: ~w.'-[NaUrl]].

pack_errors:message( org_ense_miss(Org,Eir) ) -->
    ['Not a valid organism, for mapping to an Ensembl directory: ~w (2nd arg: ~w)'-[Org,Eir]].

pack_errors:message( not_a_cnm(Cnm,Ctx) ) -->
   ['Not a valid column name or db predicate token (len=4): ~w, (context: ~w).'-[Cnm,Ctx]].

pack_errors:message( multi_option_miss(List) ) -->
   ['One of the following options should be give: ~w'-[List]].

pack_errors:message( bio_db_pred_name_fail(Comp1,Comp2) ) -->
   ['Failed to map fields: ~w and/or ~w, to bio_db predicate name tokens.'-[Comp1,Comp2]].
