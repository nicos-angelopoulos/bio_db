
:- use_module(library(filesex)).
:- use_module(library(lib)).

:- lib(os_lib).
:- lib(options).
:- lib(ansi_term).
:- lib(debug_call).
:- lib( stoics_lib:date_two_digit_dotted/1 ).

:- prolog_load_context(directory, Lib ),
   lib(Lib),
   directory_file_path( Pa, lib, Lib ),
   directory_file_path( GPa, build_repo, Pa ),
   directory_file_path( GPa, lib, PaLib ),
   lib(PaLib).

:- debuc(bio_db_build_aliases).

bio_db_build_aliases_defaults( date_stem(Dotted) ) :-
	date_two_digit_dotted( Dotted ).
	
:- ( current_predicate(user:bio_db_repo_build_root/1) ->
        true ; 
        absolute_file_name(pack('Downloads'),PackDns),
        os_make_path(PackDns),
        os_path(PackDns,bio_db_repo,BioRoot),
        assert( user:bio_db_repo_build_root(BioRoot) )
   ).
    
    /*
:- ( ( getenv( 'USER', Uname ),
       % atomic_list_concat( ['/usr/local/users/',Uname,'/work/bio_db_repo'], BioRootUsr ),
       directory_file_path( '/usr/local/users', Uname, PfxU ),
       directory_file_path( PfxD, work, WorkD ),
       exists_directory(Work
     )
        ->

     )
   directory_file_path( Work, bio_db_repo, BioRootUsr ),
   assert( build_aliases_root_stem(BioRoot) ).
   */

/*
:- getenv( 'USER', Uname ),
   atomic_list_concat( ['/usr/local/users/',Uname,'/work/bio_db_repo'], BioRoot ),
   assert( user:bio_db_repo_build_root(BioRoot) ).
   */
%
% keeps the abolute locations for the bio_db build location to a single place.
% to deploy, simply change absolute location(s) of these aliases 
% and the scripts should work fine.
%
bio_db_build_aliases( Args ) :-
	Self = bio_db_build_aliases,
	options_append( Self, Args, Opts ),
	% Dotted = '15.09.14',
	user:bio_db_repo_build_root( Stem ),
	options( date_stem(Dotted), Opts ),
	atomic_list_concat( [Stem,Dotted], '-', Dir ),
	debuc( bio_db_build_aliases, 'Building at: ~p', Dir ),
	os_make_path( Dir ),
	( file_search_path(bio_db_build,_) ->
		% write warning
		true
		;
		assert( file_search_path(bio_db_build,Dir) ),
		directory_file_path( Dir, dnloads, Dnloads ),
		directory_file_path( Dir, data, Data ),
		assert( file_search_path(bio_db_build_downloads,Dnloads) ),
		os_make_path( Dnloads ),
		assert( file_search_path(bio_db_build_data,Data) ),
		os_make_path( Data )
	).

% include this to loader... most std_... files expect the initialisation here
% :- initialization( bio_db_build_aliases([]) ).
