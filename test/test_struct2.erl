%%--------------------------------------------------------------------------------------------------
-module(test_struct2).
%%--------------------------------------------------------------------------------------------------
-export([fun1/1]).
%%--------------------------------------------------------------------------------------------------
-include_lib("gen_struct/include/gen_struct.hrl").
%%--------------------------------------------------------------------------------------------------
-record(test_struct2,{
    field1 = 1,
    field2 = undefined,
    field3 = 11.11,
    field4 = <<"12345">>,
    field5 = {1,2,3,4,5},
    field6 = [1,2,3,4,5],
    field7 = "12345",
    field8 = {test_record,1,2,3,4,5}
  }).
%%--------------------------------------------------------------------------------------------------
%% ?PRIMARY_KEY(field1).
%%--------------------------------------------------------------------------------------------------
init(S) ->
  S:fset(field2,init).
%%--------------------------------------------------------------------------------------------------
fun1(_S) -> ok.
%%--------------------------------------------------------------------------------------------------