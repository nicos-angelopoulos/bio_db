
:- use_module(library(lib)).

% stoics
:- lib(os_lib).
:- lib(options).
:- lib(debug_call).

% bio_db/auxil
:- lib(bio_db_taxo/3).
:- lib(os_grep_mtx/4).

ncbi_species_grep_defaults([]).

/** ncbi_species_grep(+File, -OutF, +Opts).

Greps the species information from an NCBI downloaded (tsv) file to an output file (OutF).

The species information is on the first column by numeric taxonomy. 
The output file is produced by postfixing File by Org as returned by 1st argument of bio_db_taxo/3.

The predicate prepends a custom hdr if one is given as on option, or replicates the input header 
(and returnes it in that option term).

Opts are passed to bio_db_taxo/3 and os_grep_mtx/4.

Examples
==
?- ncbi_species_grep(a,b,c).
==

@author nicos angelopoulos
@version  0.1 2024/03/27
@see bio_db_taxo/3
@see os_grep_mtx/4

*/
ncbi_species_grep( TsvF, GreF, Args ) :-
     Self = ncbi_species_grep,
     options_append( Self, Args, Opts ),
     bio_db_taxo( Org, Tax, Opts ),
     atomic_concat( '^', Tax, Patt ),
     os_postfix( Org, TsvF, GreF )
     os_grep_mtx( TsvF, Patt, GreF, Opts ).
