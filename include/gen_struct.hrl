%%--------------------------------------------------------------------------------------------------
-compile([{parse_transform, gen_struct_transform}]).
%%--------------------------------------------------------------------------------------------------
-include_lib("gen_struct/include/gen_struct_export.hrl").
%%--------------------------------------------------------------------------------------------------
-define(PRIMARY_KEY(Key), '=pk'() -> Key).
-define(SERIAL_FIELD(Key), '=serial'() -> Key).
%%--------------------------------------------------------------------------------------------------
%% Constructors
%%--------------------------------------------------------------------------------------------------

new() ->
  init( gen_struct:new(?MODULE) ).

%%--------------------------------------------------------------------------------------------------

new(Proplist) ->
  init( gen_struct:new(?MODULE, Proplist) ).

%%--------------------------------------------------------------------------------------------------

new_from_list(Proplist) ->
  init( gen_struct:new_from_list(?MODULE, Proplist) ).

%%--------------------------------------------------------------------------------------------------

new_from_tuple(List) ->
  init( gen_struct:new_from_tuple(?MODULE, List) ).    

%%--------------------------------------------------------------------------------------------------
%% Set/Get
%%--------------------------------------------------------------------------------------------------

fset(Field, Value, Struct) ->
  gen_struct:fset(?MODULE, Struct, Field, Value).

%%--------------------------------------------------------------------------------------------------

fset(Proplist, Struct) ->
  gen_struct:fset(?MODULE, Struct, Proplist).

%%--------------------------------------------------------------------------------------------------

fget(Field, Struct) ->
  gen_struct:fget(?MODULE, Struct, Field).

%%--------------------------------------------------------------------------------------------------

fget(Field, DefVal, Struct) ->
  gen_struct:fget(?MODULE, Struct, Field, DefVal).

%%--------------------------------------------------------------------------------------------------
%% Exports
%%--------------------------------------------------------------------------------------------------

to_json(Struct) ->
  gen_struct:to_json(?MODULE, Struct).

%%--------------------------------------------------------------------------------------------------

to_list(Struct) ->
  gen_struct:to_list(?MODULE, Struct).

%%--------------------------------------------------------------------------------------------------

to_proplist(Struct) ->
  gen_struct:to_proplist(?MODULE, Struct).

%%--------------------------------------------------------------------------------------------------

to_proplist(Opts, Struct) ->
  gen_struct:to_proplist(?MODULE, Struct, Opts).

%%--------------------------------------------------------------------------------------------------
%% DBI
%%--------------------------------------------------------------------------------------------------

insert(Conn, Struct) ->
  gen_dbi_struct:insert(?MODULE, Struct, Conn).

%%--------------------------------------------------------------------------------------------------

lookup(Conn) ->
  gen_dbi_struct:lookup(?MODULE, Conn).

%%--------------------------------------------------------------------------------------------------