%%--------------------------------------------------------------------------------------------------
-module(gen_dbi_struct).
%%--------------------------------------------------------------------------------------------------
-include_lib("gen_dbi/include/gen_dbi.hrl").
%%--------------------------------------------------------------------------------------------------
-export([
  insert/3,
  lookup/2,
  lookup/3,
  delete/3,
  test/0
  % update
  % delete
]).
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
lookup(Module, DBH) when is_atom(Module), is_record(DBH, gen_dbi_dbh) ->
  SQL = "SELECT * FROM "++ atom_to_list(Module) ++";",
  io:format("* SELECT * FROM ~s; \n",[atom_to_list(Module)]),
  gen_dbi:fetch_structs(DBH, SQL, Module).

%%--------------------------------------------------------------------------------------------------

%% get on pk, !!NOTE!! must be a tuple !!NOTE!!

lookup(_Module, _DBH, {}) ->
  throw(tuple_is_empty);

lookup(Module, DBH, PrimaryKey) when is_tuple(PrimaryKey) ->
  Keys = get_primary_columns(Module),
  Values = tuple_to_list(PrimaryKey),

  SQLTable = atom_to_list(Module),
  SQLWhere = format_placeholders_where(Keys),

  SQL = lists:flatten(
    io_lib:format("SELECT * FROM ~s WHERE ~s;", 
    [SQLTable, SQLWhere])),
  io:format("* ~s\n",[SQL]),

  case gen_dbi:fetch_structs(DBH, SQL, Values, Module) of
    Err when element(1,Err) =:= error -> Err;
    {ok, Res} -> 
      {ok, hd(Res)}
  end;

%%--------------------------------------------------------------------------------------------------

lookup(_Module, _DBH, []) ->
  throw(proplist_is_empty);

lookup(Module, DBH, Proplist) when is_list(Proplist) ->
  Keys = ?PROPLIST_KEYS(Proplist),
  Values = ?PROPLIST_VALUES(Proplist),

  SQLTable = atom_to_list(Module),
  SQLWhere = format_placeholders_where(Keys),

  SQL = lists:flatten(
    io_lib:format("SELECT * FROM ~s WHERE ~s;", 
    [SQLTable, SQLWhere])),
  io:format("* ~s\n",[SQL]),

  case gen_dbi:fetch_structs(DBH, SQL, Values, Module) of
    Err when element(1,Err) =:= error -> Err;
    {ok, Res} -> 
      {ok, hd(Res)}
  end.

%%--------------------------------------------------------------------------------------------------

insert(Module, Struct, DBH) ->
  Proplist = Struct:to_proplist(filter_undefined),

  Columns = ?PROPLIST_KEYS(Proplist),
  ColumnsLen = length(Columns),
  Values = ?PROPLIST_VALUES(Proplist),

  SQLInto = format_columns_list(Columns),
  SQLPlaceholders = format_placeholders_list(ColumnsLen),
  SQLTable = atom_to_list(Module),

  SQL= lists:flatten(
    io_lib:format("INSERT INTO ~s ~s VALUES ~s RETURNING *;", 
    [SQLTable, SQLInto, SQLPlaceholders])),
  io:format("* ~p\n",[SQL]),

  {ok, Sth} = gen_dbi:prepare(DBH, SQL),
  case gen_dbi:execute(Sth, Values) of
   {ok, 1, [Inserted]} -> {ok, Module:new_from_tuple(Inserted)};
   Err -> Err
  end.

%%--------------------------------------------------------------------------------------------------

% update(Module, Struct, C, Fileds) ->
%   ok.

%%--------------------------------------------------------------------------------------------------

delete(Module, Struct, DBH) ->
  Primary = get_primary_columns(Module),
  Proplist = [{K, Struct:fget(K)} || K <- Primary ],

  Fields = ?PROPLIST_KEYS(Proplist),
  Values = ?PROPLIST_VALUES(Proplist),
  SQLWhere = format_placeholders_where(Fields),
  SQLTable = atom_to_list(Module),

  SQL = lists:flatten(
    io_lib:format("DELETE FROM ~s WHERE ~s RETURNING *;",
    [SQLTable, SQLWhere])),
  io:format("* ~s\n", [SQL]),

  case gen_dbi:execute(DBH, SQL, Values) of
    Err when element(1,Err) =:= error -> Err;
    Res -> 
      {ok, hd(gen_dbi:result_to_structs(DBH, Res, Module))}
  end.

%%--------------------------------------------------------------------------------------------------
%% Internal
%%--------------------------------------------------------------------------------------------------

format_columns_list(Columns) ->
  Names = "(" ++ [ ", " ++ atom_to_list(C) || C <- Columns] ++ " )",
  Names1 = lists:flatten(Names) -- ",",
  Names1.

%%-------------------------------------------------------------------------------------------------- 

%% TODO: get driver placeholder char and put it in the conn record
%% maybe better add them to the driver?
format_placeholders_list(NumOfFileds) ->
  Placeholders = "(" ++ [ ", $" ++ integer_to_list(N) || N <- lists:seq(1, NumOfFileds)] ++ " )",
  Placeholders1 = lists:flatten(Placeholders) -- ",",
  Placeholders1.

%%--------------------------------------------------------------------------------------------------

format_placeholders_where(Columns) ->
  ColumnsNum = length(Columns),
  Placeholders  = lists:seq(1, ColumnsNum),
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

test() ->
  F = fun(DBH) ->

    %% create struct
    Currency = currency:new([{u_code, <<"216">>},
                             {u_name, <<"IPH">>}]),

    %% get field
    %% !!! note that we use Currency:fget not currency:fget(Currency) !!!
    <<"216">> = Currency:fget(u_code),

    %% set field
    NewCurrency = Currency:fset(u_code, 217),
    217 = NewCurrency:fget(u_code),
    
    %% insert into db
    %% !!! not that DBH is a database handle, this is functional, unlike process dict hack!!!
    %% so every DB function, will have to get DBH as a first parameter
    {ok, Currency1} = Currency:insert(DBH),


    %% select all from table
    %% only on lookups we don't use Currency (note the upper C), but we use currency
    %% because we select new struct, we don't work on one
    {ok, Currencies1} = currency:lookup(DBH),
    io:format(" Currencies1: ~p\n\n", [Currencies1]),
   
    %% select pk from table
    {ok, Currency1} = currency:lookup(DBH, {Currency1:fget(u_id)}),

    %% select by where from table
    {ok, Currencies2} = currency:lookup(DBH, [{u_code, "EUR"}]),
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