
:- use_module(library(filesex)).    % link_file/3.
:- use_module(library(os_lib)).
:- use_module(library(debug_call)).

:- lib(bio_db_build_organism/3).
%
:- lib(stoics_lib:en_list/2).
:- lib(stoics_lib:message_report/3).

link_to_bio_sub_defaults( Defs ) :-
    Defs = [
        debug(true),
        org(homs),
        type(maps)
    ].

%% link_to_bio_sub( +Sub, +FileS ).
%  link_to_bio_sub( +Sub, +FileS, +Opts ).
%
% Create a symbolic link from File to bio_Type(Sub(File)) if one does not exist.
% 
% FileS can be a list of files.
% Deletes with a warning an existing link at that location if one exists pointing
% to a different location and an error if a regular file exists.
%
% Opts 
%  * debug(Dbg=true)
%    listens to debug(link_to_bio_sub)
%  * org(Org=hs)
%    organism (defines first level of directory structure)
%  * type(Type=maps)
%    type of db table, second level of directory structure
%
%==
% ?- expand_file_name( '$local/../work/2014/hgnc', [Local] ), cd( Local ).
% ?- link_to_bio_sub( hgnc, hgnc_approved_symbol ).
%==
%
% Old: link_to_maps_sub( hs, gont, graphs, IsaF ).
%
% New: link_to_bio_sub( gont, IsaF, [org(hs),type(graphs)] ),
%
% @author nicos angelopoulos
% @version  0.1 2014/7/2
% @version  0.2 2020/9/13
% @version  0.3 2023/10/2,  allow for multiple FileS
%
link_to_bio_sub( Sub, File ) :-
     link_to_bio_sub( Sub, File, [] ).

link_to_bio_sub( Sub, FileS, Args ) :-
     Self = link_to_bio_sub,
     options_append( Self, Args, Opts ),
     options( [org(Org),type(Type)], Opts ),
     en_list( FileS, Files ),
     link_to_bio_sub_files( Files, Sub, Self, Org, Type, Opts ).
     
link_to_bio_sub_files( [], _Sub, _Self, _Org, _Type, _Opts ).
link_to_bio_sub_files( [File|Files], Sub, Self, Org, Type, Opts ) :-
     link_to_bio_sub_file( File, Sub, Self, Org, Type, Opts ),
     link_to_bio_sub_files( Files, Sub, Self, Org, Type, Opts ).

link_to_bio_sub_file( File, Sub, Self, Org, Type, _Opts ) :-
     absolute_file_name( File, AbsFile, [access(exist)] ),
     bio_db_organism( Org, Tkn, _ ),
     directory_file_path( Tkn, Type, Rel ),
     absolute_file_name( bio_db_build_data(Rel), Dir ),
     directory_file_path( Dir, Sub, ToDir ),
     file_base_name( AbsFile, Base ),
     os_make_path( ToDir, debug(true) ),
     directory_file_path( ToDir, Base, Dest ),
     link_to_bio_sub_read( Dest, AbsFile, Self ),
     !.
link_to_bio_sub_file( File, Sub, _Self, Org, Type, Opts ) :-
     throw( linking_failed(File,Sub,Org,Type), [bio_db_build:link_to_sub/3|Opts] ).

link_to_bio_sub_read( Dest, AbsFile, _Self ) :-
     read_link( Dest, _Link, Target ),
     !,
     link_to_bio_sub_target( Target, AbsFile, Dest ).
link_to_bio_sub_read( Dest, AbsFile, _Self ) :-
     exists_file( Dest ),
     !,
     Mess = 'Destination:~w already exists & is a file. Refusing to connect to:~w', 
     message_report( Mess, [Dest,AbsFile], error ),
     fail.
link_to_bio_sub_read( Dest, AbsFile, Self ) :-
     debuc( Self, 'Symbolic linking, ~p, to ~p', [AbsFile,Dest] ),
     link_file( AbsFile, Dest, symbolic ).

link_to_bio_sub_target( Target, Target, _ ) :- !.
link_to_bio_sub_target( Target, AbsF, Dest ) :- !,
     Mess = 'Destination:~w was pointing to:~w, repointing to:~w', 
     message_report( Mess, [Dest,Target,AbsF], warning ),
     delete_file( Dest ),
     link_file( AbsF, Dest, symbolic ).
