
:- lib(stoics_lib:at_con/3).

os_grep_defaults( Defs ) :- 
          Defs =  [  append(true),
                     iface(shell),
                     options_types(iface-oneof(shell,process))
                  ].

/** os_grep( +InF, +Patt, +OutF ).

Greps a pattern in file InF with output added into OutF.

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
?- os_grep([]).
==

@author nicos angelopoulos
@version  0.1 2024/03/27

*/
os_grep(File, Pattern, OutF, Args ) :-
     Self = os_grep,
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
     os_grep( Ifc, App, Hdr, File, Pattern, OutF ).

os_grep( shell, App, File, Pattern, OutF ) :-
    ( App == false ->
          at_con( [grep,Pattern,File,'>',OutF], ' ', Shell )
          ;
          at_con( [grep,Pattern,File,'>>',OutF], ' ', Shell )
     ),
     os_shell( Shell ).
os_grep( process, App, File, Pattern, OutF ) :-
     % read_lines(Out, Lines).
     process_create(path(grep), [ Pattern, file(File) ],
                       [ stdout(pipe(Out))
                       ]),
     ( App == false -> Mode = write; Mode = append ),
     open( OutF, Mode, Write ),
     write_lines_out(Out, Write),
     close( Write ).

os_shell( Shell ) :-
     shell( Shell ),
     !.
os_shell( Shell ) :-
     throw( shell_failure(Shell) ).
