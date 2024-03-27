
:- use_module(library(lib)).

:- lib(options).
:- lib(debug_call).

ncbi_species_grep_defaults([hde(_)]).

/** ncbi_species_grep(+File, +Opts).

Greps the species information from an NCBI downloaded (tsv) file.

The species information is on the first column by numeric taxonomy. 

The predicate prepends a custom hdr if one is given as on option, or replicates the input header 
(and returnes it in that option term).

Opts
  * debug(Dbg=false)
    informational, progress messages
  * hdr(Hdr) 
    if given is taken to be in row format and added as 1st line to output (via mtx/2).
    if not given, the 1st line of the input line is returned.
  * org(
Examples
==
?- ncbi_species_grep([]).
==

@author nicos angelopoulos
@version  0.1 2024/03/27

*/
ncbi_species_grep( +TsvF, Args ) :-
     Self = ncbi_species_grep,
     options_append( Self, Args, Opts ),
     options( hdr(Hdr), Opts ),
     ( var(Hdr) -> 
     grep( Stem, '^9606', HsStem ),
     debuc( Self, end, true ).
