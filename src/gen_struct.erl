%%--------------------------------------------------------------------------------------------------
-module(gen_struct).
%%--------------------------------------------------------------------------------------------------
-include_lib("eunit/include/eunit.hrl").
%%--------------------------------------------------------------------------------------------------
-export([
         new/1,
         new/2,
         new_from_list/2,
         new_from_tuple/2,
         new_from_proplist/2,
         new_from_dict/2,
         fset/3,
         fset/4,
         fget/3,
         fget/4,
         to_list/2,
         to_tuple/2,
         to_proplist/2,
         to_proplist/3,
         to_json/2,
         blank_record/1
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
-type gen_struct() :: tuple().
-type gen_struct_module() :: atom().
%%--------------------------------------------------------------------------------------------------

new(Module) when is_atom(Module) ->
  ?RECORD(Module).

%%--------------------------------------------------------------------------------------------------

new(Module, Proplist) when is_atom(Module), is_list(Proplist) ->
  Struct = ?RECORD(Module),
  Keys = proplists:get_keys(Proplist),
  lists:foldl(fun(K,S) -> S:fset(K, proplists:get_value(K,Proplist)) end, Struct, Keys).

%%--------------------------------------------------------------------------------------------------

new_from_proplist(Module, Proplist) when is_atom(Module), is_list(Proplist) ->
  new(Module, Proplist).

%%--------------------------------------------------------------------------------------------------

new_from_list(Module, List) when is_atom(Module), is_list(List) ->
  Fields = ?FIELDS(Module),
  StructLen = length(Fields),
  ListLen = length(List),
  case StructLen =:= ListLen of
    true -> list_to_tuple([Module] ++ List);
    false -> throw({invalid_number_of_arguments,[{expected, StructLen}, {actual, ListLen}]})
  end.

%%--------------------------------------------------------------------------------------------------

new_from_tuple(Module, Tuple) when is_atom(Module), is_tuple(Tuple) ->
  Fields = ?FIELDS(Module),
  StructLen = length(Fields),
  TupleLen = size(Tuple),
  case StructLen =:= TupleLen of
    true -> list_to_tuple([Module] ++ tuple_to_list(Tuple));
    false -> throw({invalid_number_of_arguments,[{expected, StructLen}, {actual, TupleLen}]})
  end.

%%--------------------------------------------------------------------------------------------------
-spec new_from_dict(Module :: gen_struct_module(), Dictionary :: dict()) -> gen_struct().
% @doc Initialises a gen_struct from the dictionary Dictionary. Missing elements initialised to 'undefined'.                       

new_from_dict(Module, Dictionary) when is_atom(Module) ->
  Fields = ?FIELDS(Module),
  BlankRecord = blank_record(Module),
  dict_set(Module,BlankRecord,Fields,Dictionary).

%%--------------------------------------------------------------------------------------------------
-spec dict_set(Module :: gen_struct_module(),
               Record :: gen_struct(),
               Fields :: list(atom()),
               Dictionary :: dict()) -> gen_struct().
% @doc set the fields Fields in the gen_struct Record to the values associated with each field in the 
%  dictionary Dict.
% @end
dict_set(Module,Record,Fields,Dictionary) when is_atom(Module),
                                               is_tuple(Record),
                                               is_list(Fields) ->
  dict_set(Module,Record,2,Fields,Dictionary).
%%--------------------------------------------------------------------------------------------------
dict_set(_Module,Record,_Index,[],_Dictionary) ->
  Record;
dict_set(Module,Record,Index,[Field|Rest],Dictionary) ->
  case dict:find(Field,Dictionary) of
    {ok,Value} ->
      NewRecord = erlang:setelement(Index,Record,Value),
      dict_set(Module,NewRecord,Index+1,Rest,Dictionary);
    error ->
      dict_set(Module,Record,Index+1,Rest,Dictionary)
  end.


%%--------------------------------------------------------------------------------------------------
-spec blank_record(Module :: gen_struct_module()) -> gen_struct().
%@doc Creates a new and blank gen_struct with the elements initialised to 'undefined'.                              
blank_record(Module) ->
  Fields = ?FIELDS(Module),
  Arity = length(Fields) + 1,
  erlang:setelement(1,erlang:make_tuple(Arity,undefined),Module).
  
%%--------------------------------------------------------------------------------------------------

fset(Module,Struct,Field,Value) when is_atom(Module), is_tuple(Struct), is_atom(Field) ->
  Index = ?INDEX(Module,Field),
  erlang:setelement(Index,Struct,Value).

fset(Module,Struct,Proplist) when is_atom(Module), is_tuple(Struct), is_list(Proplist) ->
  Keys = proplists:get_keys(Proplist),
  lists:foldl(fun(K,S) -> S:fset(K, proplists:get_value(K,Proplist)) end, Struct, Keys).

%%--------------------------------------------------------------------------------------------------

fget(Module,Struct,Field) when is_atom(Module), is_tuple(Struct), is_atom(Field) ->
  Index = ?INDEX(Module,Field),
  erlang:element(Index,Struct);

fget(Module,Struct,Fields) when is_atom(Module), is_tuple(Struct), is_list(Fields) ->
  [Struct:fget(F) || F <- Fields].

%%--------------------------------------------------------------------------------------------------

fget(Module,Struct,Field,DefVal) when is_atom(Module), is_tuple(Struct), is_atom(Field) ->
  Index = ?INDEX(Module,Field),
  case erlang:element(Index,Struct) of
    undefined -> DefVal;
    Ret       -> Ret
  end.

%%--------------------------------------------------------------------------------------------------

to_list(Module, Struct) ->
  Fields = ?FIELDS(Module),
  Data = [ fget(Module, Struct, Field) || Field <- Fields],
  Data.

%%--------------------------------------------------------------------------------------------------

to_tuple(Module, Struct) ->
  Fields = ?FIELDS(Module),
  Data = [fget(Module, Struct, Field) || Field <- Fields],
  list_to_tuple(Data).  
 
%%--------------------------------------------------------------------------------------------------

to_proplist(Module, Struct) ->
  Fields = ?FIELDS(Module),
  Data = [{Field, fget(Module, Struct, Field)} || Field <- Fields],
  Data.  

to_proplist(Module, Struct, filter_undefined) ->
  F = fun(V, Acc) ->
    case V of
      {_, undefined} -> Acc;
      _              -> [V|Acc]
    end
  end,
  lists:foldl(F, [], to_proplist(Module, Struct) ).

%%--------------------------------------------------------------------------------------------------

to_json(Module, Struct) ->
  Fields = ?FIELDS(Module),
  Data = [{Field, fget(Module, Struct, Field)} || Field <- Fields],
  {Data}.  

%%--------------------------------------------------------------------------------------------------

%% TODO: new from json
%% TODO utils module: to_dict
%% TODO dumper function
%% TODO basic built in erlang validation
%% TODO validation for the web?
%%--------------------------------------------------------------------------------------------------
