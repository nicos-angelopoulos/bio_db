
:- use_module(library(readutil)).   % read_line_to_codes/2.
:- lib(stoics_lib:at_con/3).

os_grep_mtx_defaults( Defs ) :- 
          Defs =  [  append(true),
                     iface(shell),
                     options_types(iface-oneof([shell,process]))
                  ].

/** os_grep_mtx( +InF, +Patt, +OutF ).

Greps a pattern in file InF with output added into OutF.

The defaults facilitate greps on matrices (.tsv and .csv), thus the name,
however it can be used more generally when called with non default options.

Opts
  * debug(Dbg=false)
    informational, progress messages
  * append(App=true)
    whether to prepend a header
  * iface(Iface=shell)
    default uses shell/1, (=|process|= to use process_create/3)
  * hdr(Hdr)
    if given the header is prepended (via mtx/2).
    if not given the input files header is prepended

Opts are passed to mtx/2 (iff =|App = true |= and =|\+ var(Hdr)|=.

Examples
==
?- os_grep_mtx([]).
==

@author nicos angelopoulos
@version  0.1 2024/03/27

*/
os_grep_mtx(File, Pattern, OutF, Args ) :-
     Self = os_grep_mtx,
     options_append( Self, Args, Opts ),
     options( append(App), Opts ),
     options( iface(Ifc), Opts ),
     options( hdr(Hdr), Opts ),
     ( App \== false -> 
                ( var(Hdr) -> 
                              at_con( ['head -1',File,'>',OutF], ' ', Shell ),
                              os_shell(Shell)
                              ;
                              mtx( OutF, [Hdr], Opts )
                )
     ),
     os_grep_mtx( Ifc, App, Hdr, File, Pattern, OutF ).

os_grep_mtx( shell, App, File, Pattern, OutF ) :-
    ( App == false ->
          at_con( [grep,Pattern,File,'>',OutF], ' ', Shell )
          ;
          at_con( [grep,Pattern,File,'>>',OutF], ' ', Shell )
     ),
     os_shell( Shell ).
os_grep_mtx( process, App, File, Pattern, OutF ) :-
     % read_lines(Out, Lines).
     process_create(path(grep), [ Pattern, file(File) ],
                       [ stdout(pipe(Out))
                       ]),
     ( App == false -> Mode = write; Mode = append ),
     open( OutF, Mode, Write ),
     write_lines_out(Out, Write),
     close( Write ).

write_lines_out(Out, Write) :-
        read_line_to_codes( Out, Line1 ),
        write_lines(Line1, Out, Write ).

os_shell( Shell ) :-
     shell( Shell ),
     !.
os_shell( Shell ) :-
     throw( shell_failure(Shell) ).
