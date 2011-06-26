%%--------------------------------------------------------------------------------------------------
-compile([{parse_transform, gen_struct_transform}]).
%%--------------------------------------------------------------------------------------------------
-include_lib("gen_struct/include/gen_struct_export.hrl").
%%--------------------------------------------------------------------------------------------------
-define(PRIMARY_KEY(Key), '=pk'() -> Key).
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

fset(Field,Value,Struct) ->
  gen_struct:fset(?MODULE, Struct, Field, Value).

%%--------------------------------------------------------------------------------------------------

fset(Proplist,Struct) ->
  gen_struct:fset(?MODULE, Struct, Proplist).

%%--------------------------------------------------------------------------------------------------

fget(Field,Struct) ->
  gen_struct:fget(?MODULE, Struct, Field).

%%--------------------------------------------------------------------------------------------------

fget(Field,DefVal,Struct) ->
  gen_struct:fget(?MODULE, Struct, Field, DefVal).

%%--------------------------------------------------------------------------------------------------
