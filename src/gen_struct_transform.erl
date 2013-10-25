%%--------------------------------------------------------------------------------------------------
-module(gen_struct_transform).
%%--------------------------------------------------------------------------------------------------
-export([
  parse_transform/2,
  has_function/3,
  insert_before_eof/3,
  from_str_to_ast/2,
  get_last_line_number/1,
  get_module_name/1,
  get_record/2,
  get_record_fields/1,
  insert_field_ast_fun/2,
  insert_fields_ast_fun/2,
  insert_num_of_fields_ast_fun/2,
  insert_record_ast_fun/2,
  insert_init_ast_fun/1,
  insert_pk_ast_fun/1,
  insert_serial_ast_fun/1
]).
%%--------------------------------------------------------------------------------------------------
%% TODO: return old AST if no record name = module name
%%--------------------------------------------------------------------------------------------------
-type ast() :: list(term()).
-type proplist() :: list(tuple()).
%%--------------------------------------------------------------------------------------------------

%% @doc This function will convert AST

-spec parse_transform(ast(), proplist()) -> ast().
parse_transform(AST, _Options) ->
  % io:format("AST: \n~p\n",[AST]),
  NewAST = try
    LastLineNumber = get_last_line_number(AST),
    ModuleName     = get_module_name(AST),
    ModuleRecord   = get_record(ModuleName, AST),
    RecordFields   = get_record_fields(ModuleRecord),
    % io:format("LastLineNumber: ~p\n",[LastLineNumber]),
    % io:format("ModuleName: ~p\n",[ModuleName]),
    % io:format("ModuleRecord: ~p\n",[ModuleRecord]),
    % io:format("RecordFields: ~p\n",[RecordFields]),

    {AST1, _} = insert_field_ast_fun(AST, RecordFields),
    {AST2, _} = insert_fields_ast_fun(AST1, RecordFields),
    {AST3, _} = insert_num_of_fields_ast_fun(AST2, RecordFields),  
    {AST4, _} = insert_record_ast_fun(AST3, ModuleName),
    {AST5, _} = insert_init_ast_fun(AST4),
    {AST6, _} = insert_pk_ast_fun(AST5),
    {AST7, _} = insert_serial_ast_fun(AST6),

    %%io:format("NewAST: ~p\n",[AST7]),
    AST7

  catch
    throw:{invalid_record_field, Params} ->
      io:format("Unable to parse record field: ~p\n",[proplists:get_value(field,Params)]),
      throw(stop);

    throw:{record_does_not_exist, Params} ->
      io:format("Record: ~p does not exist!\n",[proplists:get_value(record,Params)]),
      AST;

    C:E ->
      io:format("Class: ~p, Excaption: ~p\nStacktrace: ~p\n",[C,E,erlang:get_stacktrace()]),
      throw(stop)
  end,
  NewAST.

%%--------------------------------------------------------------------------------------------------

%% @doc Checks if AST has function/arity

-spec has_function(atom(), integer(), ast()) -> boolean().
has_function(_FunctionName, _Arity, []) -> 
  false;
has_function(FunctionName, Arity, [{function, _, FunctionName, Arity, _}|_]) -> 
  true;
has_function(FunctionName, Arity, [_|AST]) -> 
  has_function(FunctionName, Arity, AST).

%%--------------------------------------------------------------------------------------------------

%% @doc Returns last line number from AST

-spec get_last_line_number(ast()) -> integer().
get_last_line_number([{eof,LineNum}|_]) -> 
  LineNum;
get_last_line_number([_|AST]) -> 
  get_last_line_number(AST).

%%--------------------------------------------------------------------------------------------------

%% @doc Returns module name from AST

-spec get_module_name(ast()) -> atom().
get_module_name([{attribute,_,module,ModuleName}|_]) -> 
  ModuleName;
get_module_name([_|AST]) -> 
  get_module_name(AST).

%%--------------------------------------------------------------------------------------------------

%% @doc Returns record AST that has the same name as the module name

-spec get_record(atom(), ast()) -> ast().
get_record(RecordName,[]) -> 
  throw({record_does_not_exist,[{record,RecordName}]});
get_record(RecordName,[{attribute,_,record,{RecordName,_}=Record}|_]) -> 
  Record;
get_record(RecordName,[_|AST]) -> 
  get_record(RecordName, AST).

%%--------------------------------------------------------------------------------------------------

%% @doc Returns record fields, record that has the same name as the module name

-spec get_record_fields(ast()) -> list(atom()).
get_record_fields({_RecordName, RecordFields}=ASTElement) ->
  get_record_fields_loop(RecordFields, []).

get_record_fields_loop([],Acc) -> 
  lists:reverse(Acc);
get_record_fields_loop([{record_field,_,{atom,_,FieldName}} | AST],Acc) -> 
  get_record_fields_loop(AST, [FieldName|Acc]);
get_record_fields_loop([{record_field,_,{atom,_,FieldName},_DefaultValue} | AST],Acc) -> 
  get_record_fields_loop(AST,[FieldName|Acc]);
get_record_fields_loop([Field|_AST],_Acc) -> 
  throw({invalid_record_field,[{field,Field}]}).

%%--------------------------------------------------------------------------------------------------

%% @doc Inserts '=field(Field)' function that returns field index

-spec insert_field_ast_fun(ast(), list(atom())) -> tuple(ast(), integer()).
insert_field_ast_fun(AST, Fields) ->
  LastLineNumber = get_last_line_number(AST),
  {InsertAST, NewLastLineNumber} = insert_field_ast_fun_loop(Fields, LastLineNumber, 2, []),
  insert_before_eof(AST, InsertAST, NewLastLineNumber).

insert_field_ast_fun_loop([], LastLineNumber, _, Acc) -> 
  Acc1 = Acc ++ "'=field'(Field) -> throw({invalid_field,[{field, Field}]}). \n",
  from_str_to_ast(Acc1, LastLineNumber);
  
insert_field_ast_fun_loop([Field|Fields], LastLineNumber, Index, Acc) ->
  Acc1 = Acc ++ "'=field'(" ++ atom_to_list(Field) ++") -> "++ integer_to_list(Index) ++"; \n",
  insert_field_ast_fun_loop(Fields, LastLineNumber, Index+1, Acc1).

%%--------------------------------------------------------------------------------------------------

%% @doc Inserts '=fields'() function that returns a list of fields

-spec insert_fields_ast_fun(ast(), list(atom())) -> tuple(ast(), integer()).
insert_fields_ast_fun(AST, Fields) ->
  LastLineNumber = get_last_line_number(AST),
  Lst = "'=fields'() -> [ " ++ (lists:flatten([ ", " ++ atom_to_list(F) || F <- Fields]) -- "," ) ++ " ].",
  {InsertAST, NewLastLineNumber} = from_str_to_ast(Lst,LastLineNumber),
  insert_before_eof(AST, InsertAST, NewLastLineNumber).

%%--------------------------------------------------------------------------------------------------

%% @doc Inserts '=record'() function that returns a record

-spec insert_record_ast_fun(ast(), atom()) -> tuple(ast(), integer()).
insert_record_ast_fun(AST, RecordName) ->
  LastLineNumber = get_last_line_number(AST),
  Str = "'=record'() -> #"++ atom_to_list(RecordName) ++"{} .",
  {InsertAST, NewLastLineNumber} = from_str_to_ast(Str, LastLineNumber),
  insert_before_eof(AST, InsertAST, NewLastLineNumber).

%%--------------------------------------------------------------------------------------------------

%% @doc Check if init/1 function exists, if not it will insert one

-spec insert_init_ast_fun(ast()) -> tuple(ast(), integer()).
insert_init_ast_fun(AST) ->
  LastLineNumber = get_last_line_number(AST),

  case has_function(init, 1, AST) of
    false -> 
      Str = "init(Struct) -> Struct.",
      {InsertAST, NewLastLineNumber} = from_str_to_ast(Str, LastLineNumber),
      insert_before_eof(AST, InsertAST, NewLastLineNumber);
    true ->
      {AST, LastLineNumber}
  end.

%%--------------------------------------------------------------------------------------------------

%% @doc Inserts a function '=fields_num'() that will return number of fields defined in a record

-spec insert_num_of_fields_ast_fun(ast(), list(atom())) -> tuple(ast(), integer()).
insert_num_of_fields_ast_fun(AST, RecordFields) ->
  LastLineNumber = get_last_line_number(AST),

  Len = length(RecordFields),
  Str = "'=fields_num'() -> "++ integer_to_list(Len) ++".",
  {InsertAST, NewLastLineNumber} = from_str_to_ast(Str, LastLineNumber),
  insert_before_eof(AST, InsertAST, NewLastLineNumber).

%%--------------------------------------------------------------------------------------------------

%% @doc Check if '=pk'() functions exists, if not it will insert one

-spec insert_pk_ast_fun(ast()) -> tuple(ast(), integer()).
insert_pk_ast_fun(AST) ->
  LastLineNumber = get_last_line_number(AST),

  case has_function('=pk', 0, AST) of
    false -> 
      Str = "'=pk'() -> undefined.",
      {InsertAST, NewLastLineNumber} = from_str_to_ast(Str, LastLineNumber),
      insert_before_eof(AST, InsertAST, NewLastLineNumber);
    true ->
      {AST, LastLineNumber}
  end.

%%--------------------------------------------------------------------------------------------------

%% @doc Check if '=serial'() functions exists, if not it will insert one

-spec insert_serial_ast_fun(ast()) -> tuple(ast(), integer()).
insert_serial_ast_fun(AST) ->
  LastLineNumber = get_last_line_number(AST),

  case has_function('=serial', 0, AST) of
    false -> 
      Str = "'=serial'() -> undefined.",
      {InsertAST, NewLastLineNumber} = from_str_to_ast(Str, LastLineNumber),
      insert_before_eof(AST, InsertAST, NewLastLineNumber);
    true ->
      {AST, LastLineNumber}
  end.

%%--------------------------------------------------------------------------------------------------

%% @doc Takes erlang code as a stirng, and LastLineNumber, and returns an AST and new last line number

-spec from_str_to_ast(string(), integer()) -> tuple(ast(), integer()).
from_str_to_ast(Str, LastLineNumber) ->
  {ok, Tokens, NewLastLineNumber} = erl_scan:string(Str, LastLineNumber),
  {ok, AST} = erl_parse:parse_form(Tokens),                             
  {AST, NewLastLineNumber}.

%%--------------------------------------------------------------------------------------------------

%% @doc Inserts AST before end of file

-spec insert_before_eof(ast(), ast(), integer()) -> tuple(ast(), integer()).
insert_before_eof(AST, InsertAST, NewLastLineNumber) ->
  insert_before_eof_loop(AST, InsertAST, NewLastLineNumber,[]).

insert_before_eof_loop([{eof,_LastLineNumber}|_], InsertAST, NewLastLineNumber, Acc) -> 
  Acc1 = [{eof,NewLastLineNumber},InsertAST|Acc],
  {lists:reverse(Acc1), NewLastLineNumber};

insert_before_eof_loop([ASTElement | AST], InsertAST, NumberOfLines, Acc) -> 
  insert_before_eof_loop(AST, InsertAST, NumberOfLines, [ASTElement|Acc]).

%%--------------------------------------------------------------------------------------------------
