:- module(bio_db_repo, [bio_db_repo/0, bio_db_repo_version/2]).

:- use_module(library(bio_db)).
:- ensure_loaded(bio_db_repo_version).

/** <module> Data package for bio_db.

This contains all the prolog database predicates
served by pack(bio_db) (see bio_db_paths/0).

*/

/** bio_db_repo.

This is the data pack for bio_db.
Please see bio_db/0 for more information.

*/

bio_db_repo.

/** bio_db_repo_version( -Version, -Date ).

	Version is a Mj:MM:DD term with integer components and date
	is a date(YYYY,MM,DD) term.

	Version is the date the data were downloaded from primary sources and
	Date is the date the pack was published.

Previously:

  bio_db_repo_version( 16:9:10, date(16,9,11) ).<br>
  bio_db_repo_version( 17:3:3, date(17,3,4) ).<br>

*/
