
:- use_module( library(apply) ). % maplist/3,...
:- use_module( library(lists) ). % append/3,...

:- use_module( library(lib) ).

% :- lib(real).
:- lib(mtx).
:- lib(options).
:- lib(stoics_lib:holds/2).
:- lib(stoics_lib:portray_clauses/2).

:- lib(bio_db_add_infos/1).

:- ensure_loaded(bio_add_info_kvs_lengths_rel).
:- ensure_loaded(map_predicate_name).             % /4.

mtx_map_defaults( Defs ) :-
            Defs = [ 
                        add_info(true),
                        has_header(true),
                        interface(prolog),
                        odir(maps),
                        sort_by(1), 
                        source('not_known')
                    ].

/** mtx_map( +Mtx, +Ccts, ?OutF, +Opts ).

Create data predicates from the columns of a matrix.

Generalises csv_ids_map/6. New code should be using this predicate.
This version uses mtx(Mtx) option to do away with the extra argument.
Mtx can be any representation of a matrix (also see Ctx in options).

Ccts is a list of (Cid, Cid:TransfG or Cid:MTkn:TransfG) terms, that identify the source column in Mtx,
the output map token and any transform of the column values before depositing them to OutF. 
If TransfG is missing, then it taken to be =|false|= for no transform.
If MTkn is missing, it is generated by calling fixme: on Cid corresponding Cnm.
Cid, should be numeric if Mtx does not have a header (HasH), else it can be either a column number or a column name in Mtx.

The predicate name for the map relation is produced by map_predicate_name/4 and is saved at OutF.
The prediate names expect only two column names, the first and last in Cnms are taken to be these,
except if primary and secondary options are given.

If OutF is unbound, then the predicate name +.pl is used.

Options
  * add_info(Ainf=true)
    whether to add info terms
  * datetime(DnDt)
    download datime, for Mtx
  * dir(Dir='.')
    directory for the result
  * has_header(HasH=true)
    false allows for non header matrices, in which case Cid(s) should be numeric.
  * header(Hdr)
    a header term, something like row(CnmDesc1,CnmDesc2)
  * interface(Ifc=prolog)
    also known is sqlite
  * mtx(Ctx)
    Canonical representation of Mtx. If Ctx is ground, Mtx is ignored
  * source(Src='not_known'
    source info tag
  * sort_by(Sby=1)
    sort clauses by Nth argument (fixme: make sure sort/4 is used)

Options are passed to mtx/2 and map_predicate_name/4.

==
fixme: add example, small download from HGNC, etc
==

@author nicos angelopoulos
@version  0.1 2023/9/16
@see map_predicate_name/4

*/
mtx_map( Mtx, ClmsIn, OutF, Args ) :-
     % Tbl is Mtx
     Self = mtx_map,
     options_append( Self, Args, Opts ),
     options( mtx(Ctx), Opts ),
     ( var(Ctx) ->
          mtx( Mtx, Ctx, Opts )
          ;
          true
     ),
     options( has_header(HasH), Opts ),
     mtx_map_column_names( Ctx, HasH, ClmsIn, Htx, Cnms ),
     mtx_map_columns( Cnms, Htx, Clms, Hargs ), % fixme: create Hdr from Hargs which you can pass to _info
     memberchk( interface(Iface), Opts ),
     ( memberchk(primary(Prim),Opts) -> true; Hargs=[Prim|_] ),
     ( memberchk(secondary(Seco),Opts) -> true; last(Hargs,Seco) ),
     map_predicate_name( Prim, Seco, Pname, Opts ),
     map_predicate_name_stem( Pname, Stem, Opts ),
     memberchk( odir(Dir), Opts ),
     directory_file_path( Dir, Stem, Dtem ),
     mtx_map_rows( Clms, Pname, Otx ),
     % mtx_lists( Otx, Clms ),
     % fixme: add the infos before writing out
     mtx_map_iface( Iface, Self, Otx, Dtem, OutF ),

     ( options(add_info(false),Opts) -> true
                      ; 
                         Hdr =.. [hdr|Hargs],
                         bio_db_add_infos([OutF,header(Hdr)|Opts]) 
     ).

mtx_map_rows( [[]|_], _Pname, Otx ) :-
     !,
     Otx = [].
mtx_map_rows( Clms, Pname, Otx ) :-
     mtx_map_row_args( Clms, Args, Tlms ),
     Row =.. [Pname|Args],
     Otx = [Row|Ttx],
     mtx_map_rows( Tlms, Pname, Ttx ).

mtx_map_row_args( [], [], [] ).
mtx_map_row_args( [[H|T]|R], [H|Args], [T|M] ) :-
     mtx_map_row_args( R, Args, M ).

mtx_map_iface( prolog, Self, Otx, Dtem, OutF ) :-
     !,
     os_ext( pl, Dtem, OutF ),
     portray_clauses( Otx, file(OutF) ),
     debuc( Self, wrote, OutF ).
mtx_map_iface( Other, _Self, _Otx, _Dtem, _OutF ) :-
     throw( mtx_map(4,interface_not_yet_implemented(Other)) ).

mtx_map_columns( [], _Mtx, [], [] ).
mtx_map_columns( [Cid:_Nid:Transf|Cnms], Mtx, [Clm|Clms], [Cid|Targs]) :-
     mtx_column( Mtx, Cid, Mlm ),
     mtx_map_column_transform( Transf, Mlm, Clm ),
     % fixme: do we need to add Nid to Clm ?
     mtx_map_columns( Cnms, Mtx, Clms, Targs ).
     
mtx_map_column_transform( false, Mlm, Clm ) :- !,
     Mlm = Clm.
mtx_map_column_transform( Poal, Mlm, Clm ) :- !,
     maplist( Poal, Mlm, Clm ).
     % call( Poal, Mlm, Clm ).

mtx_map_column_names( Ctx, HasH, ClmsIn, Htx, Clms ) :-
     ( HasH == false ->   % makes true defaulty
          Ctx = [Row|_Rows],
          functor( Row, F, Rarity ),
          findall( Harg, between(1,Rarity,Harg), Hargs ),
          Hdr =.. [F|Hargs],
          Htx = [Hdr|Ctx]
          ;
          Ctx = Htx
     ),
     maplist( mtx_map_column_term(HasH), ClmsIn, Clms ).

mtx_map_column_term( false, Cin, Clm ) :-
     ( Cin = _:_:_ -> Clm = Cin ; throw(header_less_mtx_column_no_token(Cin)) ).
mtx_map_column_term( true, Cin, Clm ) :-
     ( Cin = _:_:_ -> Cin = Clm
               ; ( Cin = ClmId:Transf -> Clm = ClmId:ClmId:Transf 
                       ;  Cin = ClmId -> Clm = ClmId:ClmId:false
                 )
     ).