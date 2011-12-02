%%--------------------------------------------------------------------------------------------------
-module(gen_struct_tests).
%%--------------------------------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
-include("test_struct1.hrl").
%%--------------------------------------------------------------------------------------------------
-define(ATOM, undefined).
-define(INT, 1).
-define(FLOAT,11.11).
-define(BIN, <<"12345">>).
-define(STR, "12345").
-define(REC, {test_record,1,2,3,4,5}).
-define(LIST, [1,2,3,4,5]).
-define(TUPLE, {1,2,3,4,5}).
%%--------------------------------------------------------------------------------------------------
%% TODO test record not found
%% TODO test not proplist
%%--------------------------------------------------------------------------------------------------

%%-record(test_struct,{
%%    field1 = ?INT,
%%    field2 = ?ATOM,
%%    field3 = ?FLOAT,
%%    field4 = ?BIN,
%%    field5 = ?TUPLE,
%%    field6 = ?LIST,
%%    field7 = ?STR,
%%    field8 = ?REC
%%  }).

%%--------------------------------------------------------------------------------------------------

new_test_() ->
  M1 = test_struct1,
  M2 = test_struct2,
  S1 = M1:new(),
  S2 = M1:new([{field1,1},{field2,2},{field3,3},{field4,4},{field5,5},
               {field6,6},{field7,7},{field8,8}]),
  S3 = M1:new([{field6,6},{field7,7},{field8,8}]),

  S4 = M2:new(),
  S5 = M2:new([{field1,1},{field2,2},{field3,3},{field4,4},{field5,5},
               {field6,6},{field7,7},{field8,8}]),
  S6 = M2:new([{field6,6},{field7,7},{field8,8}]),
  
  [
    ?_assertEqual( M1:'=fields'(), [field1,field2,field3,field4,field5,field6,field7,field8] ),

    ?_assertEqual( S1 , {M1,?INT,?ATOM,?FLOAT,?BIN,?TUPLE,?LIST,?STR,?REC} ),
    ?_assertEqual( S2 , {M1,1,2,3,4,5,6,7,8} ),
    ?_assertEqual( S3 , {M1,?INT,?ATOM,?FLOAT,?BIN,?TUPLE,6,7,8} ),

    ?_assertEqual( S4 , {M2,?INT,init,?FLOAT,?BIN,?TUPLE,?LIST,?STR,?REC} ),
    ?_assertEqual( S5 , {M2,1,init,3,4,5,6,7,8} ),
    ?_assertEqual( S6 , {M2,?INT,init,?FLOAT,?BIN,?TUPLE,6,7,8} )
  ].

%%--------------------------------------------------------------------------------------------------

fget_test_() ->
  M1 = test_struct1,
  M2 = test_struct2,
  S1 = M1:new(),
  S2 = M2:new(),

  [
    ?_assertEqual( S1:fget(field1) , ?INT ), 
    ?_assertEqual( S1:fget(field2) , ?ATOM ), 
    ?_assertEqual( S1:fget(field3) , ?FLOAT ), 
    ?_assertEqual( S1:fget(field4) , ?BIN ), 
    ?_assertEqual( S1:fget(field5) , ?TUPLE ), 
    ?_assertEqual( S1:fget(field6) , ?LIST ), 
    ?_assertEqual( S1:fget(field7) , ?STR ), 
    ?_assertEqual( S1:fget(field8) , ?REC ),

    ?_assertEqual( S2:fget(field2) , init )
  ].

%%--------------------------------------------------------------------------------------------------

fget_default_test_() ->
  M1 = test_struct1,
  M2 = test_struct2,
  S1 = M1:new(),
  S2 = M2:new(),

  [
    ?_assertEqual( S1:fget(field2,default), default),
    ?_assertEqual( S2:fget(field2,default), init)
  ].

%%--------------------------------------------------------------------------------------------------

multi_fget_test_() ->
  M1 = test_struct1,
  M2 = test_struct2,
  S1 = M1:new(),
  S2 = M2:new(),

  [
    ?_assertEqual( S1:fget([field1,field2,field3]), [?INT,?ATOM,?FLOAT] ),
    ?_assertEqual( S1:fget([]), [] ),
    ?_assertEqual( S2:fget([field1,field2,field3]), [?INT,init,?FLOAT] ),
    ?_assertEqual( S2:fget([]), [] )
  ].

%%--------------------------------------------------------------------------------------------------

fset_test_() ->
  M1 = test_struct1,
  M2 = test_struct2,
  S1 = M1:new(),
  S2 = M2:new(),

  S11 = S1:fset(field1,2),
  S21 = S2:fset(field2,init),

  [
    ?_assertEqual( S11:fget(field1) , 2 ),
    ?_assertEqual( S21:fget(field2) , init)
  ].

%%--------------------------------------------------------------------------------------------------

multi_fset_test_() ->
  M1 = test_struct1,
  M2 = test_struct2,
  S1 = M1:new(),
  S2 = M2:new(),

  S11 = S1:fset([{field1,2},{field3,22.22}]),
  S21 = S2:fset([{field1,2},{field3,22.22}]),

  [
    ?_assertEqual( S11:fget([field1,field2,field3]) , [2,undefined,22.22] ),
    ?_assertEqual( S21:fget([field1,field2,field3]) , [2,init,22.22] )
  ].

%%--------------------------------------------------------------------------------------------------

new_from_tuple_test_() ->
  M1 = test_struct1,
  Tuple1 = {1,2,3,4,5,6,7,8},
  Tuple2 = {1,2,3,4,5,6,7,8,9},

  [
    ?_assertMatch(X when is_tuple(X), M1:new_from_tuple(Tuple1) ),
    ?_assertException(throw, 
                      {invalid_number_of_arguments,[{expected, 8}, {actual, 9}]},
                      M1:new_from_tuple(Tuple2))
  ].

%%--------------------------------------------------------------------------------------------------

new_from_list_test_() ->
  M1 = test_struct1,
  List1 = [1,2,3,4,5,6,7,8],
  List2 = [1,2,3,4,5,6,7,8,9],

  [
    ?_assertMatch(X when is_tuple(X), M1:new_from_list(List1) ),
    ?_assertException(throw, 
                      {invalid_number_of_arguments,[{expected, 8}, {actual, 9}]},
                      M1:new_from_list(List2))
  ].

%%--------------------------------------------------------------------------------------------------

new_from_proplist_test() ->
  M1 = test_struct1,
  S1 = M1:new_from_proplist([{field1, 2}]),

  ?assertMatch(2, S1:fget(field1)).

%%--------------------------------------------------------------------------------------------------

meta_functions_test_() ->
  M1 = test_struct1,
  M2 = test_struct2,
  Record1 = {test_struct1,1,undefined,11.11,<<"12345">>,
              {1,2,3,4,5},
              [1,2,3,4,5],
              "12345",
              {test_record,1,2,3,4,5}},

  [
    ?_assertException(throw, {invalid_field,[{field,42}]}, M1:'=field'(42) ),
    ?_assertEqual(2, M1:'=field'(field1) ),
    ?_assertEqual([field1, field2, field3, field4, field5, field6, field7, field8], M1:'=fields'() ),
    ?_assertEqual(Record1, M1:'=record'() ),
    ?_assertEqual(field1, M1:'=pk'() ),
    ?_assertEqual(undefined, M2:'=pk'() ),
    ?_assertEqual(8, M1:'=fields_num'() )
  ].

%%--------------------------------------------------------------------------------------------------

to_list_test() ->
  M1 = test_struct1,
  S1 = M1:new(),
  List = [1,undefined,11.11,<<"12345">>,
    {1,2,3,4,5},
    [1,2,3,4,5],
    "12345",
    {test_record,1,2,3,4,5}],

  ?assertMatch(List, S1:to_list()).

%%--------------------------------------------------------------------------------------------------

to_tuple_test() ->
  M1 = test_struct1,
  S1 = M1:new(),
  Tuple = {1,undefined,11.11,<<"12345">>,
    {1,2,3,4,5},
    [1,2,3,4,5],
    "12345",
    {test_record,1,2,3,4,5}},

  io:format("~p\n",[S1:to_tuple()]),
  ?assertMatch(Tuple, S1:to_tuple()).

%%--------------------------------------------------------------------------------------------------  

to_proplist_test_() ->
  M1 = test_struct1,
  S1 = M1:new(),
  Proplist1 = [{field1,1},
    {field2,undefined},
    {field3,11.11},
    {field4,<<"12345">>},
    {field5,{1,2,3,4,5}},
    {field6,[1,2,3,4,5]},
    {field7,"12345"},
    {field8,{test_record,1,2,3,4,5}}],

  Proplist2 = [{field8,{test_record,1,2,3,4,5}},
    {field7,"12345"},
    {field6,[1,2,3,4,5]},
    {field5,{1,2,3,4,5}},
    {field4,<<"12345">>},
    {field3,11.11},
    {field1,1}],

  [
    ?_assertMatch(Proplist1, S1:to_proplist()),
    ?_assertMatch(Proplist2, S1:to_proplist(filter_undefined))
  ].

%%--------------------------------------------------------------------------------------------------  

to_json_test() ->
  M1 = test_struct1,
  S1 = M1:new(),
  Proplist = {[{field1,1},
    {field2,undefined},
    {field3,11.11},
    {field4,<<"12345">>},
    {field5,{1,2,3,4,5}},
    {field6,[1,2,3,4,5]},
    {field7,"12345"},
    {field8,{test_record,1,2,3,4,5}}]},

  ?assertMatch(Proplist, S1:to_json()).

%%--------------------------------------------------------------------------------------------------  

start_stop_test() ->
  application:stop(gen_struct),
  ok = application:start(gen_struct),
  ok = application:stop(gen_struct),
  ok = application:start(gen_struct),
  ok.

%%--------------------------------------------------------------------------------------------------

blank_record_test_() ->
  [?_assertMatch(#test_struct1{field1=undefined},gen_struct:blank_record(test_struct1))].
%%--------------------------------------------------------------------------------------------------
new_from_dict_test_() ->
  [?_assertMatch(#test_struct1{field1=undefined},gen_struct:new_from_dict(test_struct1,dict:new())),
   ?_assertMatch(#test_struct1{field1=1,
                              field2=2},gen_struct:new_from_dict(test_struct1,dict:from_list([{field1,1},{field2,2}])))
  ].
