%%----------------------------------------------------------------------------------------------------
-module(gen_struct_transform).
%%----------------------------------------------------------------------------------------------------
-export([parse_transform/2]).
%%----------------------------------------------------------------------------------------------------
%% TODO create state
-record(state,{eof, ast, opt, record_name, module_name, record_fields}).
%%----------------------------------------------------------------------------------------------------

parse_transform(AST,Options) ->
  % io:format("AST: \n~p\n",[AST]),
  NewAST = try
%%    State = #state{ast=AST, opt=Options},
    LastLineNumber = get_last_line_number(AST),
    ModuleName = get_module_name(AST),
    ModuleRecord = get_record(ModuleName,AST),
    RecordFields = get_record_fields(ModuleRecord),
    % io:format("LastLineNumber: ~p\n",[LastLineNumber]),
    % io:format("ModuleName: ~p\n",[ModuleName]),
    % io:format("ModuleRecord: ~p\n",[ModuleRecord]),
    % io:format("RecordFields: ~p\n",[RecordFields]),

    {InsertAST1,LastLineNumber1} = gen_fields_idx_fun_ast(RecordFields,LastLineNumber),
    {AST1,LastLineNumber2} = insert_before_eof(AST,InsertAST1,LastLineNumber1),

    {InsertAST2,LastLineNumber3} = gen_fields_lst_fun_ast(RecordFields,LastLineNumber2),
    {AST2,LastLineNumber4} = insert_before_eof(AST1,InsertAST2,LastLineNumber3),

    {AST3,LastLineNumber5} = insert_record_ast_fun(AST2,ModuleName,LastLineNumber4),
    {AST4,LastLineNumber6} = insert_init_ast_fun(AST3,LastLineNumber5),

%%    io:format("AST: ~p\n",[AST4]),
    AST4

  catch
    throw:{invalid_record_field,Params} ->
      io:format("Unable to parse record field: ~p\n",[proplists:get_value(field,Params)]),
      throw(stop);

    throw:{record_does_not_exist,Params} ->
      io:format("Record: ~p does not exist!\n",[proplists:get_value(record,Params)]),
      AST;

    C:E ->
      io:format("Class: ~p, Excaption: ~p\nStacktrace: ~p\n",[C,E,erlang:get_stacktrace()]),
      throw(stop)
  end,
  NewAST.

%%----------------------------------------------------------------------------------------------------

has_function(FunctionName,Arity,[])                                         -> false;
has_function(FunctionName,Arity,[{function,_,FunctionName,Arity,_}=_Fun|_]) -> true;
has_function(FunctionName,Arity,[_|Rest])                                   -> has_function(FunctionName,Arity,Rest).

%%----------------------------------------------------------------------------------------------------

get_last_line_number([{eof,LineNum}|_]) -> LineNum;
get_last_line_number([_|Rest])          -> get_last_line_number(Rest).

%%----------------------------------------------------------------------------------------------------

get_module_name([{attribute,_,module,ModuleName}|_]) -> ModuleName;
get_module_name([_|Rest])                            -> get_module_name(Rest).

%%----------------------------------------------------------------------------------------------------

get_record(RecordName,[])                                             -> throw({record_does_not_exist,[{record,RecordName}]});
get_record(RecordName,[{attribute,_,record,{RecordName,_}=Record}|_]) -> Record;
get_record(RecordName,[_|Rest])                                       -> get_record(RecordName,Rest).

%%----------------------------------------------------------------------------------------------------

get_record_fields({_RecordName,RecordFields}) ->
  get_record_fields_loop(RecordFields,[]).

get_record_fields_loop([],Acc)                                                       -> lists:reverse(Acc);
get_record_fields_loop([{record_field,_,{atom,_,FieldName}}|Rest],Acc)               -> get_record_fields_loop(Rest,[FieldName|Acc]);
get_record_fields_loop([{record_field,_,{atom,_,FieldName},_DefaultValue}|Rest],Acc) -> get_record_fields_loop(Rest,[FieldName|Acc]);
get_record_fields_loop([Field|_Rest],_Acc)                                           -> throw({invalid_record_field,[{field,Field}]}).


%%----------------------------------------------------------------------------------------------------

gen_fields_idx_fun_ast(Fields,LastLineNumber) ->
  gen_fields_idx_fun_ast_loop(Fields,LastLineNumber,2,[]). %% 2 because first element is module_name


gen_fields_idx_fun_ast_loop([],LastLineNumber,_,Acc) -> 
  Acc1 = Acc ++ "'=field'(_) -> throw(invalid_field). \n",
  from_str_to_ast(Acc1,LastLineNumber);

gen_fields_idx_fun_ast_loop([Field|Fields],LastLineNumber,Index,Acc) ->
  Acc1 = Acc ++ "'=field'(" ++ atom_to_list(Field) ++") -> "++ integer_to_list(Index) ++"; \n",
  gen_fields_idx_fun_ast_loop(Fields,LastLineNumber,Index+1,Acc1).

%%----------------------------------------------------------------------------------------------------

gen_fields_lst_fun_ast(Fields,LastLineNumber) ->
  Lst = "'=fields'() -> [ " ++ (lists:flatten([ ", " ++ atom_to_list(F) || F <- Fields]) -- "," ) ++ " ].",
  from_str_to_ast(Lst,LastLineNumber).

%%----------------------------------------------------------------------------------------------------

insert_record_ast_fun(AST,RecordName,LastLineNumber) ->
  Str = "'=record'() -> #"++ atom_to_list(RecordName) ++"{} .",
  {InsertAST,NewLastLineNumber} = from_str_to_ast(Str,LastLineNumber),
  insert_before_eof(AST,InsertAST,NewLastLineNumber).

%%----------------------------------------------------------------------------------------------------

insert_init_ast_fun(AST,LastLineNumber) ->
  case has_function(init,1,AST) of
    false -> 
      Str = "init(Struct) -> Struct.",
      {InsertAST, NewLastLineNumber} = from_str_to_ast(Str,LastLineNumber),
      insert_before_eof(AST,InsertAST,NewLastLineNumber);
    true ->
      {AST, LastLineNumber}
  end.

%%----------------------------------------------------------------------------------------------------

from_str_to_ast(Str,LastLineNumber) ->
  {ok, Tokens, NewLastLineNumber} = erl_scan:string(Str,LastLineNumber),
  {ok, AST} = erl_parse:parse_form(Tokens),                             
  {AST, NewLastLineNumber}.

%%----------------------------------------------------------------------------------------------------

insert_before_eof(AST,InsertAST,NewLastLineNumber) ->
  insert_before_eof_loop(AST,InsertAST,NewLastLineNumber,[]).


insert_before_eof_loop([{eof,_LastLineNumber}|_],InsertAST,NewLastLineNumber,Acc) -> 
  Acc1 = [{eof,NewLastLineNumber},InsertAST|Acc],
  {lists:reverse(Acc1),NewLastLineNumber};

insert_before_eof_loop([ASTElemnt|Rest],InsertAST,NumberOfLines,Acc) -> 
  insert_before_eof_loop(Rest,InsertAST,NumberOfLines,[ASTElemnt|Acc]).

%%----------------------------------------------------------------------------------------------------
