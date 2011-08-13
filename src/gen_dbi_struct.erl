%%--------------------------------------------------------------------------------------------------
-module(gen_dbi_struct).
%%--------------------------------------------------------------------------------------------------
-include_lib("gen_dbi/include/gen_dbi.hrl").
-include_lib("eunit/include/eunit.hrl").
%%--------------------------------------------------------------------------------------------------
-export([
  insert/3,
  lookup/2,
  lookup/3,
  delete/3,
  update/4,
  test1/0
]).
%%--------------------------------------------------------------------------------------------------
-include_lib("osw_common/include/testhelper.hrl").
%%--------------------------------------------------------------------------------------------------
-define(FIELD(M, K),    M:'=field'(K) ).
-define(FIELDS(M),      M:'=fields'() ).
-define(FIELDS_NUM(M),  M:'=fields_num'() ).
-define(INDEX(M,F),     M:'=field'(F) ).
-define(RECORD(M),      M:'=record'() ).
-define(PK(M),          M:'=pk'() ).
-define(SERIAL(M),      M:'=serial'() ).
%%--------------------------------------------------------------------------------------------------
-define(PROPLIST_VALUES(P), [V || {_K,V} <- Proplist]).
-define(PROPLIST_KEYS(P), [K || {K,_V} <- Proplist]).
%%--------------------------------------------------------------------------------------------------
%% TODO returning is a pg specific? or sql standard, make it also river specific?
%%--------------------------------------------------------------------------------------------------

%% get all
lookup(Module, DBH) 
when 
  is_atom(Module), 
  is_record(DBH, gen_dbi_dbh) 
->
  SQLTable = get_table_name(Module),
  SQL = "SELECT * FROM "++ SQLTable ++" ;",
  gen_dbi:fetch_structs(DBH, SQL, Module).

%%--------------------------------------------------------------------------------------------------

lookup_all_test() ->
  F = fun(_DBH, SQL, _Module) ->
    ?assertEqual( SQL, "SELECT * FROM db_currency ;")
  end,

  mock(gen_dbi),
  meck:expect(gen_dbi, fetch_structs, F),

  DBH = #gen_dbi_dbh{driver = pg, driver_config = [], handle = {}},

  lookup(db_currency, DBH).
  
%%--------------------------------------------------------------------------------------------------

%% get on pk, !!NOTE!! must be a tuple !!NOTE!!

lookup(_Module, _DBH, {}) ->
  throw(tuple_is_empty);

lookup(Module, DBH, PrimaryKey) 
when 
  is_atom(Module), 
  is_record(DBH, gen_dbi_dbh), 
  is_tuple(PrimaryKey)
->
  Keys = get_primary_columns(Module),
  Values = tuple_to_list(PrimaryKey),

  SQLTable = get_table_name(Module),
  SQLWhere = format_placeholders_where(Keys),

  SQL = lists:flatten(
    io_lib:format("SELECT * FROM ~s WHERE ~s ;", 
    [SQLTable, SQLWhere])),

  case gen_dbi:fetch_structs(DBH, SQL, Values, Module) of
    Err when element(1,Err) =:= error -> Err;
    {ok, Res} -> 
      {ok, hd(Res)}
  end;

%%--------------------------------------------------------------------------------------------------

lookup(_Module, _DBH, []) ->
  throw(proplist_is_empty);

lookup(Module, DBH, Proplist) 
when 
  is_atom(Module), 
  is_record(DBH, gen_dbi_dbh), 
  is_list(Proplist)
->
  Keys = ?PROPLIST_KEYS(Proplist),
  Values = ?PROPLIST_VALUES(Proplist),

  SQLTable = get_table_name(Module),
  SQLWhere = format_placeholders_where(Keys),

  SQL = lists:flatten(
    io_lib:format("SELECT * FROM ~s WHERE ~s ;", 
    [SQLTable, SQLWhere])),

  case gen_dbi:fetch_structs(DBH, SQL, Values, Module) of
    Err when element(1,Err) =:= error -> Err;
    {ok, Res} -> 
      {ok, hd(Res)}
  end.

%%--------------------------------------------------------------------------------------------------

lookup_on_proplist_test_() ->
  F = fun(_DBH, SQL, _Values, _Module) ->
    {ok, [SQL]}
  end,

  mock(gen_dbi),
  meck:expect(gen_dbi, fetch_structs, F),

  DBH = #gen_dbi_dbh{driver = pg, driver_config = [], handle = {}},

  [ 
    ?_assertException(throw, proplist_is_empty, lookup(db_currency, DBH, []) ),
    ?_assertEqual( lookup(db_currency, DBH, [{u_code, 987}]), {ok, "SELECT * FROM db_currency WHERE u_code = $1 ;"})
  ].

%%--------------------------------------------------------------------------------------------------

lookup_on_pk_test_() ->
  F = fun(_DBH, SQL, _Values, _Module) ->
    {ok, [SQL]}
  end,

  mock(gen_dbi),
  meck:expect(gen_dbi, fetch_structs, F),

  DBH = #gen_dbi_dbh{driver = pg, driver_config = [], handle = {}},

  [ 
    ?_assertException(throw, tuple_is_empty, lookup(db_currency, DBH, {}) ),
    ?_assertEqual( lookup(db_currency, DBH, {1}), {ok, "SELECT * FROM db_currency WHERE u_id = $1 ;"})
  ].

%%--------------------------------------------------------------------------------------------------

insert(Module, Struct, DBH) 
when
  is_atom(Module), 
  is_tuple(Struct), 
  element(1,Struct) =:= Module, 
  is_record(DBH, gen_dbi_dbh)
->
  Proplist = Struct:to_proplist(filter_undefined),

  Columns = ?PROPLIST_KEYS(Proplist),
  ColumnsLen = length(Columns),
  Values = ?PROPLIST_VALUES(Proplist),

  SQLInto = format_columns_list(Columns),
  SQLPlaceholders = format_placeholders_list(ColumnsLen),
  SQLTable = get_table_name(Module),

  SQL = lists:flatten(
    io_lib:format("INSERT INTO ~s ~s VALUES ~s RETURNING * ;", 
    [SQLTable, SQLInto, SQLPlaceholders])),

  case gen_dbi:execute(DBH, SQL, Values) of
    Err when element(1,Err) =:= error -> Err;
    {ok, 1, _Columns, [Inserted]} -> {ok, Module:new_from_tuple(Inserted)}
  end.

%%--------------------------------------------------------------------------------------------------

insert_test() ->
  Proplist = [{u_id,1}, {u_code, 987}, {u_name,"NAN"}],

  F = fun(_DBH, SQL, _Values) ->
    ?assertEqual(SQL, "INSERT INTO db_currency ( u_name, u_code, u_id ) VALUES ( $1, $2, $3 ) RETURNING * ;"),
    {ok, 1, [], [{1,987,"NAN"}]}
  end,

  mock(gen_dbi),
  meck:expect(gen_dbi, execute, F),

  Currency = db_currency:new(Proplist),
  DBH = #gen_dbi_dbh{driver = pg, driver_config = [], handle = {}},

  ?assertEqual( insert(db_currency, Currency, DBH), {ok, Currency}).

%%--------------------------------------------------------------------------------------------------

%% TODO: update from a proplist

update(Module, Struct, DBH, Fields) 
when 
  is_atom(Module), 
  is_tuple(Struct), 
  element(1,Struct) =:= Module, 
  is_record(DBH, gen_dbi_dbh), 
  is_list(Fields)
->
  Values = [Struct:fget(F) || F <- Fields],

  Primary = get_primary_columns(Module),
  Proplist = [{K, Struct:fget(K)} || K <- Primary ],
  PKFields = ?PROPLIST_KEYS(Proplist),
  PKValues = ?PROPLIST_VALUES(Proplist),

  AssertPKVal = fun(V) ->
    case V of 
      undefined -> throw(primary_key_value_is_undefined); 
      _  -> ok
    end
  end,
  [ AssertPKVal(PKV) || PKV <- PKValues],

  SQLTable = get_table_name(Module),
  SQLSet = format_placeholders_where(Fields),
  SQLWhere = format_placeholders_where(length(Fields)+1, PKFields),

  SQL = lists:flatten(
    io_lib:format("UPDATE ~s SET ~s WHERE ~s RETURNING *;",
    [SQLTable, SQLSet, SQLWhere])),

  case gen_dbi:execute(DBH, SQL, Values++PKValues) of
    Err when element(1,Err) =:= error -> Err;
    {ok, 1, _Columns, [Updated]} -> {ok, Module:new_from_tuple(Updated)}
  end.

%%--------------------------------------------------------------------------------------------------

update_test() ->
  Proplist = [{u_id,1}, {u_code, 987}, {u_name,"NAN"}],

  F = fun(_DBH, SQL, _Values) ->
    ?assertEqual(SQL, "UPDATE db_currency SET u_code = $1 WHERE u_id = $2 RETURNING *;"),
    {ok, 1, [], [{1,987,"NAN"}]}
  end,

  mock(gen_dbi),
  meck:expect(gen_dbi, execute, F),

  Currency = db_currency:new(Proplist),
  Currency2 = Currency:fset(u_code, <<"999">>),
  DBH = #gen_dbi_dbh{driver = pg, driver_config = [], handle = {}},

  ?assertEqual( update(db_currency, Currency2, DBH, [u_code]), {ok, Currency}).

%%--------------------------------------------------------------------------------------------------

delete(Module, Struct, DBH) 
when 
  is_atom(Module), 
  is_tuple(Struct), 
  element(1,Struct) =:= Module, 
  is_record(DBH, gen_dbi_dbh)
->
  Primary = get_primary_columns(Module),
  Proplist = [{K, Struct:fget(K)} || K <- Primary ],

  Fields = ?PROPLIST_KEYS(Proplist),
  Values = ?PROPLIST_VALUES(Proplist),
  SQLWhere = format_placeholders_where(Fields),
  SQLTable = get_table_name(Module),

  SQL = lists:flatten(
    io_lib:format("DELETE FROM ~s WHERE ~s RETURNING * ;",
    [SQLTable, SQLWhere])),

  case gen_dbi:execute(DBH, SQL, Values) of
    Err when element(1,Err) =:= error -> Err;
    Res -> 
      {ok, hd(gen_dbi:result_to_structs(DBH, Res, Module))}
  end.

%%--------------------------------------------------------------------------------------------------

delete_test() ->
  Proplist = [{u_id,1}, {u_code, 987}, {u_name,"NAN"}],

  F1 = fun(_DBH, SQL, _Values) ->
    ?assertEqual(SQL, "DELETE FROM db_currency WHERE u_id = $1 RETURNING * ;"),
    ok
  end,

  F2 = fun(_DBH, Res, _Module) ->
    [Res]
  end,

  mock(gen_dbi),
  meck:expect(gen_dbi, execute, F1),
  meck:expect(gen_dbi, result_to_structs, F2),

  Currency = db_currency:new(Proplist),
  DBH = #gen_dbi_dbh{driver = pg, driver_config = [], handle = {}},

  ?assertEqual( delete(db_currency, Currency, DBH), {ok, ok}).

%%--------------------------------------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------------------------------------

get_table_name(Module) ->
  case application:get_env(gen_dbi, erl_table_prefix) of
    undefined -> atom_to_list(Module);
    {ok, Val} -> atom_to_list(Module) -- Val
  end.

%%--------------------------------------------------------------------------------------------------

format_columns_list(Columns) ->
  Names = "(" ++ [ ", " ++ atom_to_list(C) || C <- Columns] ++ " )",
  Names1 = lists:flatten(Names) -- ",",
  Names1.

%%-------------------------------------------------------------------------------------------------- 

%% TODO: get driver placeholder char and put it in the conn record
%% maybe better add them to the driver?
format_placeholders_list(NumOfFileds) ->
  format_placeholders_list(1, NumOfFileds).

format_placeholders_list(FromNum, NumOfFileds) ->
  Placeholders = "(" ++ [ ", $" ++ integer_to_list(N) || N <- lists:seq(FromNum, FromNum+NumOfFileds-1)] ++ " )",
  Placeholders1 = lists:flatten(Placeholders) -- ",",
  Placeholders1.

%%--------------------------------------------------------------------------------------------------

format_placeholders_where(Columns) ->
  format_placeholders_where(1, Columns).

format_placeholders_where(FromNum, Columns) ->
  ColumnsNum = length(Columns),
  Placeholders  = lists:seq(FromNum, FromNum+ColumnsNum-1),
  Set = lists:zipwith(fun(A,B) -> {A,B} end, Columns, Placeholders ),

  Where = [ " AND" ++ atom_to_list(K) ++" = $" ++ integer_to_list(N) || {K, N} <- Set],
  Where1 = lists:flatten(Where) -- " AND",
  Where1.

%%--------------------------------------------------------------------------------------------------

get_primary_columns(Module) ->
  Pri = ?PK(Module),

  Pri2 = case is_list(Pri) of
    true  -> Pri;
    false -> [Pri]
  end,

  Pri2.

%%--------------------------------------------------------------------------------------------------

get_serial_columns(Module) ->
  Serial = ?SERIAL(Module),
  
  Serial2 = case is_list(Serial) of
    true  -> Serial;
    false -> [Serial]
  end,

  Serial2.

%%--------------------------------------------------------------------------------------------------
%% Test
%%--------------------------------------------------------------------------------------------------

test1() ->
  F = fun(DBH) ->

    %% create struct
    Currency = db_currency:new([{u_code, <<"216">>},
                             {u_name, <<"IPH">>}]),

    %% get field
    %% !!! note that we use Currency:fget not currency:fget(Currency) !!!
    <<"216">> = Currency:fget(u_code),

    %% set field
    NewCurrency = Currency:fset(u_code, <<"217">>),
    <<"217">> = NewCurrency:fget(u_code),
    
    %% insert into db
    %% !!! not that DBH is a database handle, this is functional, unlike process dict hack!!!
    %% so every DB function, will have to get DBH as a first parameter
    {ok, Currency1} = Currency:insert(DBH),


    %% db update, note that struct has to be read from db before we update it
    NewCurrency1 = Currency1:fset(u_code, <<"217">>),
    <<"217">> = NewCurrency1:fget(u_code),
    {ok, Currency2} = NewCurrency1:update(DBH, [u_code]),  
    Currency1:fget(u_id) =:= Currency2:fget(u_id),

    %% select all from table
    %% only on lookups we don't use Currency (note the upper C), but we use currency
    %% because we select new struct, we don't work on one
    {ok, Currencies1} = db_currency:lookup(DBH),
    io:format(" Currencies1: ~p\n\n", [Currencies1]),
   
    %% select pk from table
    {ok, Currency2} = db_currency:lookup(DBH, {Currency1:fget(u_id)}),

    %% select by where from table
    {ok, Currencies2} = db_currency:lookup(DBH, [{u_code, "EUR"}]),
    io:format(" Currencies2: ~p\n\n", [Currencies2]),

    %% delete currency
    Currency1:delete(DBH)

    %% TODO update
  end,
  gen_dbi:trx(F).

  

% test() ->
%    F = fun(DBH) ->
%      currency:lookup(DBH)
%    end,
%    gen_dbi:trx(F).


%%--------------------------------------------------------------------------------------------------  