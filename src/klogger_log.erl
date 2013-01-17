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
%%
%% Functions to crate dynamically the logger code and create log events
%%
%% @end
%% Created : 22 Nov 2012 by Pablo Vieytes <pvieytes@openshine.com>
%%-------------------------------------------------------------------

-module(klogger_log).

-include("include/klogger.hrl").

%% API
-export([create_logger/2,
	 create_backend_record_list/1,
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
%% @spec create_logger(LoggerName::atom(), 
%%                      [{BackendName::atom(), Type::atom(), Level::integer()}]) -> 
%%           string()
%% @end
%%--------------------------------------------------------------------
create_logger(LoggerName, B) ->
    Backends = 
	if
	    is_tuple(B) -> [B];
	    is_list(B) -> B
	end,
    %% check specs
    case create_backend_record_list(Backends) of
	{error, _} = E ->
	    E;
	BackendsRecords ->
	    Specs = ?EVENTCHILD(LoggerName),
	    case supervisor:start_child(klogger_sup, Specs) of
		{ok, _Pid} -> 
		    add_handlers(LoggerName, BackendsRecords),
		    compile_logger(LoggerName, BackendsRecords),
		    ok;
		Error -> Error
	    end
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
		  fun({Name, _Level}) ->
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
					  [{Type, Name, ?LEVELCODE(Level), Path}|lists:keydelete(Name, 2, Acc)];
				      {Type, Name, _OldLevel} ->
					 [{Type, Name, ?LEVELCODE(Level)}|lists:keydelete(Name, 2, Acc)]
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
    file:write_file("/tmp/logger.erl", list_to_binary(CodeString)),
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
    BackendsString = backends_records_to_str(Backends),
    "-module(" ++ ModuleStr ++ ").

     -export([log/2,          
              log/3, 
              debug/1,
              debug/2,
              info/1,
              info/2,
              warning/1,
              warning/2,
              error/1,
              error/2,
              fatal/1,
              fatal/2,
              get_backends/0,
              is_klogger/0
            ]).  

       -define(BACKENDS, " ++ BackendsString ++ ").

       is_klogger() -> true.

       get_backends() -> ?BACKENDS.
 
       debug(Format, D) ->
             debug(lists:flatten(io_lib:format(Format, D))).  
    
       debug(Msg) -> log("++ lists:flatten(io_lib:format("~p", [?DEBUG])) ++ ", Msg).


       info(Format, D) ->
             info(lists:flatten(io_lib:format(Format, D))).
       info(Msg) -> log("++ lists:flatten(io_lib:format("~p", [?INFO])) ++ ", Msg).

      
       warning(Format, D) ->
            warning(lists:flatten(io_lib:format(Format, D))).
       warning(Msg) -> log("++ lists:flatten(io_lib:format("~p", [?WARNING])) ++ ", Msg).

      
       error(Format, D) ->
             error(lists:flatten(io_lib:format(Format, D))).
       error(Msg) -> log("++ lists:flatten(io_lib:format("~p", [?ERROR])) ++ ", Msg).


       fatal(Format, D) -> 
             fatal(lists:flatten(io_lib:format(Format, D))).
       fatal(Msg) -> log("++ lists:flatten(io_lib:format("~p", [?FATAL])) ++ ", Msg).


       log(Action, Format, D) ->
            log(Action, lists:flatten(io_lib:format(Format, D))).
       log(Action, Msg) when is_list(Msg) ->
           " ++ atom_to_list(?MODULE) ++":do_log(" ++ ModuleStr ++ ", Action, Msg, ?BACKENDS);
       log(Action, Variable) ->
           log(Action, lists:flatten(io_lib:format(\"~p\"))).
      ".


%% {BackendName :: atom(), Level :: integer()}
backends_records_to_str(Backends) ->
   "[" ++  backends_records_to_str(Backends, "") ++ "]".

backends_records_to_str([Backend=#file_backend{}|Rest] , String) ->
    TupleStr =
	"{ " ++ atom_to_list(Backend#file_backend.name) ++ ", " ++
 	lists:flatten(io_lib:format("~p", [Backend#file_backend.level])) ++
	"}",
    case Rest of
	[] -> String ++ TupleStr;
	Rest -> 
	    backends_records_to_str(Rest, String  ++ TupleStr ++ ", ")
    end;

backends_records_to_str([Backend=#ram_backend{}|Rest] , String) ->
    TupleStr =
	"{ " ++ atom_to_list(Backend#ram_backend.name) ++ ", " ++
 	lists:flatten(io_lib:format("~p", [Backend#ram_backend.level])) ++
	"}",
    case Rest of
	[] -> String ++ TupleStr;
	Rest -> 
	    backends_records_to_str(Rest, String  ++ TupleStr ++ ", ")
    end;

backends_records_to_str([Backend=#console_backend{}|Rest] , String) ->
    TupleStr =
	"{ " ++ atom_to_list(Backend#console_backend.name) ++ ", " ++
 	lists:flatten(io_lib:format("~p", [Backend#console_backend.level])) ++
	"}",
    case Rest of
	[] -> String ++ TupleStr;
	Rest -> 
	    backends_records_to_str(Rest, String  ++ TupleStr ++ ", ")
    end.

	     

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
add_handlers(LoggerName, BackendsRecords)->
    lists:foreach(
      fun(BackendRecord) ->
	      ok = gen_event:add_handler(LoggerName, klogger_handler, [LoggerName, BackendRecord, []])
      end,
      BackendsRecords).



%% logging 
%%=========================================

do_log(LoggerName, Action, Msg, Backends) ->
    ActionCode =  ?LEVELCODE(Action),   
    lists:foreach(
      fun({BackendName, BackendLevel}) when ActionCode =< BackendLevel  ->
	      Event = {log, BackendName, ActionCode, Msg, now()},
	      gen_event:notify(LoggerName, Event);
	 (_E) ->	      
	      ignore
      end, 
    Backends).



create_backend_record_list(Backends)->
    create_backend_record_list(Backends,[]).

create_backend_record_list([],Acc)->
    Acc;

create_backend_record_list([{backend, BackendList}|Rest],Acc)->   
    case proplists:get_value(name, BackendList) of
	undefined -> {error, "name is mandatory"};
	Name ->
	    Level =  
		case proplists:get_value(loglevel, BackendList) of 
		    undefined -> ?DEBUG;
		    L -> ?LEVELCODE(L)
		end,
	    GetErLog = 
		case proplists:get_value(get_error_logger, BackendList) of 
		    undefined -> disable;
		    ErL -> ErL
		end,
	    case proplists:get_value(type, BackendList) of 
		console_backend ->
		    Record = 
			#console_backend{name=Name,
					 level=Level,
					 get_error_logger=GetErLog},
		    create_backend_record_list(Rest,[Record|Acc]);
		ram_backend ->
		    Record = 
			#ram_backend{name=Name,
				     level=Level,
				     get_error_logger=GetErLog},
		    create_backend_record_list(Rest,[Record|Acc]);
		file_backend ->
		    case proplists:get_value(path, BackendList) of 
			undefined -> 
			    {error, "path is mandatory for file_backend type"};
			Path -> 
			    Record =
				#file_backend{name=Name,
					      level=Level,
					      get_error_logger=GetErLog,
					      path=Path},
			    create_backend_record_list(Rest,[Record|Acc])
		    end
	    end
    end.
