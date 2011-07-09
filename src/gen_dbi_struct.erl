%%--------------------------------------------------------------------------------------------------
-module(gen_dbi_struct).
%%--------------------------------------------------------------------------------------------------
-include_lib("gen_dbi/include/gen_dbi.hrl").
%%--------------------------------------------------------------------------------------------------
-export([
  insert/3,
  lookup/2,
  test/0
  % update
  % lookup
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

%% TODO serial?

insert(Module, Struct, DBH) ->
  Proplist = Struct:to_proplist(filter_undefined),

  FieldNames = " ( " ++ [ ", " ++ atom_to_list(F) || {F,_} <- Proplist] ++ " ) ",
  FieldNames1 = lists:flatten(FieldNames) -- ",",
  FieldsNum = length(Proplist),
  Placeholders = " ( " ++ [ ", $" ++ integer_to_list(N) || N <- lists:seq(1, FieldsNum)] ++ " ) ",
  Placeholders1 = lists:flatten(Placeholders) -- ",",
  Values = [V || {_,V} <- Proplist],

  SQL= "INSERT INTO "++ atom_to_list(Module) ++ FieldNames1 ++" VALUES "++ Placeholders1 ++ "RETURNING * ;",
  io:format("* insert * ~p\n",[SQL]),

  {ok, Sth} = gen_dbi:prepare(DBH, SQL),
  case gen_dbi:execute(Sth, Values) of
   {ok, 1, [Inserted]} -> {ok, Module:new_from_tuple(Inserted)};
   Err -> Err
  end.

%%--------------------------------------------------------------------------------------------------

% update(Module, Struct, C, Fileds) ->
%   ok.

% %%--------------------------------------------------------------------------------------------------

lookup(Module, C) when is_atom(Module), is_record(C, gen_dbi_dbh) ->
  SQL = "SELECT * FROM "++ atom_to_list(Module) ++";",
  gen_dbi:fetch_structs(C, SQL, Module).

% lookup(Module, Struct, C, PrimaryKey, ) when is_tuple(PrimaryKey) ->
%   ok;

% lookup(Module, C, Proplist) ->
%   ok.

% %%--------------------------------------------------------------------------------------------------

% delete(Module, C, Struct) ->
%   ok;

% delete(Module, C, PrimaryKey) when is_tuple(PrimaryKey) ->
%   ok.

%%--------------------------------------------------------------------------------------------------

% test() ->
%   F = fun(C) ->
%     Currency = currency:new([{u_code, <<"931">>},
%                              {u_name, <<"IPH">>}]),
%     Currency:insert(C)
%   end,
%   gen_dbi:trx(F).

test() ->
   F = fun(C) ->
     currency:lookup(C)
   end,
   gen_dbi:trx(F).


%%--------------------------------------------------------------------------------------------------  