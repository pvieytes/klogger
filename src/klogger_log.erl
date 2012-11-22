%%-------------------------------------------------------------------
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
%%
%% @author Pablo Vieytes <pvieytes@openshine.com>
%% @copyright (C) 2012, Openshine S.L.
%% @doc
%% Functions to crate dynamically the logger code
%%
%% @end
%% Created : 22 Nov 2012 by Pablo Vieytes <pvieytes@openshine.com>
%%-------------------------------------------------------------------

-module(klogger_log).

-include_lib("klogger/include/klogger.hrl").


%% API
-export([create_logger/2,
	 set_log_level/2
	 ]).

%% Logger funs
-export([do_log/4
	 ]).

-define(EVENTCHILD(Name), {Name, {gen_event, start_link, [{local, Name}]}, permanent, 5000, worker, dynamic}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a new logger interface
%%
%% @spec compile_logger(LoggerName::atom(), 
%%                      [{BackendName::atom(), Type::atom(), Level::integer()}]) -> 
%%           string()
%% @end
%%--------------------------------------------------------------------
create_logger(LoggerName, Backends) ->
    Specs = ?EVENTCHILD(LoggerName),
    case supervisor:start_child(klogger_sup, Specs) of
	{ok, _Pid} -> 
	    add_handlers(LoggerName, Backends),
	    compile_logger(LoggerName, Backends),
	    ok;
	Error -> Error
    end.



set_log_level(Logger, NewLevels)->
   PreviousBackends=
	try
	    Logger:get_backends()
	catch _:_ ->
		error
	end,
    case PreviousBackends of
	error -> {error, lists:flatten(io_lib:format("logger: ~p not found", [Logger]))};
	PreviousBackends ->
	    Right = 
		lists:all(
			  fun({Name, Level}) when is_integer(Level);
						  Level >= ?DEBUG;
						  Level =< ?FATAL ->
				  case lists:keyfind(Name, 2, PreviousBackends) of
				      false -> false;
				      _ -> true
				  end;
			     (_) ->
				  false
			  end,
			  NewLevels),
	    case Right of
		false->
		    {error, "backends lists is not valid"};
		    true ->
		    NewBackends = 
			lists:foldl(
			  fun({Name, Level}, Acc) ->
				  case lists:keyfind(Name, 2, Acc) of
				      {Type, Name, _OldLevel, Path} ->
					  [{Type, Name, Level, Path}|lists:keydelete(Name, 2, Acc)];
				      {Type, Name, _OldLevel} ->
					 [{Type, Name, Level}|lists:keydelete(Name, 2, Acc)]
				  end				 
			  end,
			  PreviousBackends,
			  NewLevels),    
		    compile_logger(Logger, NewBackends)
	    end
    end.
		    






%%%===================================================================
%%% Internal functions
%%%===================================================================


%% compiling funs
%%=========================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Compile a new logger interface
%%
%% @spec compile_logger(LoggerName::atom(), 
%%                      [{BackendName::atom(), Type::atom(), Level::integer()}]) -> 
%%           string()
%% @end
%%--------------------------------------------------------------------
compile_logger(Name, Backends) ->
    CodeString = get_code(Name, Backends),
    {Module,Code} = dynamic_compile:from_string(CodeString),
    case code:load_binary(Module, lists:concat([Name, ".erl"]), Code) of
    	{error, _} = E-> E;
    	_ -> ok 
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This funciton returns a string with the logger code
%%
%% @spec backends_to_str([{Name::atom(), Type::atom(), Level::integer()}]) -> 
%%           string()
%% @end
%%--------------------------------------------------------------------
get_code(LoggerName, Backends) ->
    ModuleStr = atom_to_list(LoggerName),
    BackendsString = backends_to_str(Backends),
    "-module(" ++ ModuleStr ++ ").

     -export([log/2,          
              debug/1,
              info/1,
              warning/1,
              error/1,
              fatal/1,
              get_backends/0
            ]).  

       -define(BACKENDS, " ++ BackendsString ++ ").

       get_backends() -> ?BACKENDS.

       debug(Msg) -> log("++ lists:flatten(io_lib:format("~p", [?DEBUG])) ++ ", Msg).
       info(Msg) -> log("++ lists:flatten(io_lib:format("~p", [?INFO])) ++ ", Msg).
       warning(Msg) -> log("++ lists:flatten(io_lib:format("~p", [?WARNING])) ++ ", Msg).
       error(Msg) -> log("++ lists:flatten(io_lib:format("~p", [?ERROR])) ++ ", Msg).
       fatal(Msg) -> log("++ lists:flatten(io_lib:format("~p", [?FATAL])) ++ ", Msg).

      
       log(Action, Msg) ->
           " ++ atom_to_list(?MODULE) ++":do_log(" ++ ModuleStr ++ ", Action, Msg, ?BACKENDS).
      ".



%%--------------------------------------------------------------------
%% @private
%% @doc
%% This funciton convert a list of backend info tuples in a string
%%
%% @spec backends_to_str([]) -> 
%%           string()
%% backend() = {Name::atom(), , Level::integer()}
%% @end
%%--------------------------------------------------------------------
backends_to_str(L)->
    "[" ++ backends_to_str_loop(L, "") ++ "]".

backends_to_str_loop([Tuple|[]], Acc)->
    Acc ++ convert_tuple_to_string(Tuple);

backends_to_str_loop([Tuple|Rest], Acc) -> 
    convert_tuple_to_string(Tuple) ++ ", " ++  backends_to_str_loop(Rest, Acc).

convert_tuple_to_string(Tuple)->
    List = 
	lists:map(
	  fun(Atom) when is_atom(Atom) ->
		  atom_to_list(Atom);
	     (Integer) when is_integer(Integer) ->
		  lists:flatten(io_lib:format("~p", [Integer]));
	     (List) when is_list(List) -> "\"" ++ List ++ "\""
	  end,
	  tuple_to_list(Tuple)),
    "{" ++ string:join(List, ", ") ++ "}".     
	     


%% event server funs
%%=========================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This funciton add a handler to the event server for each backend
%%
%% @spec backends_to_str([{Name::atom(), Type::type() , Level::integer()}]) -> 
%%           ok | {error, Reason}
%% type() = console_backend | file_backend
%% @end
%%--------------------------------------------------------------------
add_handlers(LoggerName, Backends)->
    lists:foreach(
      fun({file_backend, BackendName, _Level, Path})->
	     ok = gen_event:add_handler(LoggerName, klogger_file_backend, [{LoggerName, BackendName, Path}]);
	  ({console_backend,BackendName,_Level})->	     
	     ok = gen_event:add_handler(LoggerName, klogger_console_backend, [{LoggerName, BackendName}])
      end,
      Backends).



%% logging 
%%=========================================

do_log(LoggerName, Action, Msg, Backends) ->
    ActionCode = 
	case Action of
	    Action when is_atom(Action) -> ?LEVELCODE(Action);
	    Action when 
		  is_integer(Action),
		  Action >= ?NONE,
		  Action =< ?DEBUG ->
		Action
	end,
    Log = 
	fun(Event) -> 
		%% io:format("dbg notify event: ~p~n", [Event]),
		gen_event:notify(LoggerName, Event) 
	end,
    lists:foreach(
      fun({_, BackendName, BackendLevel, _}) when ActionCode =< BackendLevel  ->
	      Log({log, BackendName, ActionCode, Msg, now()});
	 ({_, BackendName, BackendLevel}) when ActionCode =< BackendLevel  ->
	      Log({log, BackendName, ActionCode, Msg, now()});
	 (_E) ->	      
	      ignore
      end, 
      Backends).


