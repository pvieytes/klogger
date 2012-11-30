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
%% gen_event handler
%%
%% @end
%% Created : 26 Nov 2012 by Pablo Vieytes <pvieytes@openshine.com>
%%-------------------------------------------------------------------

-module(klogger_handler).

-behaviour(gen_event).

-include("include/klogger.hrl").

%% API
-export([get_error_logger/3,
	 transfer_ram/3
	]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {name,
		level,
		backend,
		file=[],
		get_error_logger=disable, 
		logger_name,
		ram}).



%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @spec 
%% @end
%%--------------------------------------------------------------------
get_error_logger(Logger, BackendName, Mode)->
    Logger ! {get_error_logger, BackendName, Mode}.


transfer_ram(Logger,  BackendName, NewBackends)->
    Logger ! {transfer_ram, Logger, BackendName, NewBackends}.

%%%===================================================================
%%% gen_event callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a new event handler is added to an event manager,
%% this function is called to initialize the event handler.
%%
%% @spec init(Args) -> {ok, State}
%% @end
%%--------------------------------------------------------------------
init([LoggerName, Backend=#file_backend{}, PreviousData]) ->
    process_flag(trap_exit, true),
    case open_log_file(Backend#file_backend.path, LoggerName, Backend#file_backend.name) of
	{ok, File} ->
	    case Backend#file_backend.get_error_logger of
		enable ->
		    klogger_integration_error_logger(LoggerName, Backend#file_backend.get_error_logger);
		_ ->
		    ignore
	    end,
	    State = #state{name=Backend#file_backend.name,
			level=Backend#file_backend.level,
			backend=Backend,
			get_error_logger=Backend#file_backend.get_error_logger,
			logger_name=LoggerName,
			file=File},
	    transfer_previous_data(State, PreviousData),
	    {ok, State};
	{error, _} = E ->
	    E
    end;

init([LoggerName, Backend=#console_backend{}, PreviousData]) ->
    process_flag(trap_exit, true),
    case Backend#console_backend.get_error_logger of
	enable ->
	    klogger_integration_error_logger(LoggerName, Backend#console_backend.get_error_logger);
	_ ->
	    ignore
    end,
    State =  #state{name=Backend#console_backend.name,
		    level=Backend#console_backend.level,
		    get_error_logger=Backend#console_backend.get_error_logger,
		    logger_name=LoggerName,
		    backend=Backend},
    transfer_previous_data(State, PreviousData),
    {ok, State};

init([LoggerName, Backend=#ram_backend{}, _PreviousData]) ->
    process_flag(trap_exit, true),
    Ram = init_ram_storage(),

    case Backend#ram_backend.get_error_logger of
	enable ->
	    klogger_integration_error_logger(LoggerName, Backend#ram_backend.get_error_logger);
	_ ->
	    ignore
    end,
    {ok, #state{name=Backend#ram_backend.name,
		level=Backend#ram_backend.level,
		get_error_logger=Backend#ram_backend.get_error_logger,
		logger_name=LoggerName,
		backend=Backend,
		ram=Ram}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives an event sent using
%% gen_event:notify/2 or gen_event:sync_notify/2, this function is
%% called for each installed event handler to handle the event.
%%
%% @spec handle_event(Event, State) ->
%%                          {ok, State} |
%%                          {swap_handler, Args1, State1, Mod2, Args2} |
%%                          remove_handler
%% @end
%%--------------------------------------------------------------------
handle_event({log, BackendName, ActionCode, Msg, TimeStamp}, State=#state{name=BackendName}) ->   
    log(ActionCode, Msg, TimeStamp, State),
    {ok, State};

handle_event({error_logger_event, Event}, State=#state{get_error_logger=enable}) ->   
    manage_error_logger_event(Event, State),
    {ok, State};

handle_event(_Event, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event manager receives a request sent using
%% gen_event:call/3,4, this function is called for the specified
%% event handler to handle the request.
%%
%% @spec handle_call(Request, State) ->
%%                   {ok, Reply, State} |
%%                   {swap_handler, Reply, Args1, State1, Mod2, Args2} |
%%                   {remove_handler, Reply}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for each installed event handler when
%% an event manager receives any other message than an event or a
%% synchronous request (or a system message).
%%
%% @spec handle_info(Info, State) ->
%%                         {ok, State} |
%%                         {swap_handler, Args1, State1, Mod2, Args2} |
%%                         remove_handler
%% @end
%%--------------------------------------------------------------------
handle_info({get_error_logger, BackendName, Mode}, State=#state{name=BackendName}) ->
    Status = State#state.get_error_logger,
    case Mode of
	Status ->
	    {ok, State};
	Mode ->
	    klogger_integration_error_logger(State#state.logger_name, Mode),   
	    {ok, State#state{get_error_logger=Mode}}
    end;

handle_info({transfer_ram, LoggerName, BackendName, NewBackends}, State=#state{name=BackendName}) ->
    case State#state.backend of
	_B = #ram_backend{} ->
	    case klogger_log:create_backend_record_list(NewBackends) of
		{error, _}->
		    do_nothing;
		BackendsRecords ->
		    Data = pop_all_elements_ram_storage(State#state.ram),
		    lists:foreach(
		      fun(BackendRecord) ->
			      spawn(
				fun() -> 
					gen_event:add_handler(LoggerName, 
							      klogger_handler, 
							      [LoggerName, BackendRecord, Data])
				end)
		      end,
		      BackendsRecords),
		    destroy_ram_storage(State#state.ram)
	    end;
	_ ->
	    ignore
    end,
    %%TODO how to erase the instace??
    {ok, []};

handle_info(debug_show_ram, State) ->
    io:format("show ram~n"),
    case State#state.backend of
	_B = #ram_backend{} ->	    
	    lists:foreach(
	      fun({ActionCode, Bin}) -> io:format("{~p, ~p}~n", [ActionCode, binary_to_term(Bin)]) end,
	    	  read_all_elements_ram_storage(State#state.ram));
	_ ->
	    ignore
    end,	    
    {ok, State};

handle_info(_Info, State) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever an event handler is deleted from an event manager, this
%% function is called. It should be the opposite of Module:init/1 and
%% do any necessary cleaning up.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    case State#state.get_error_logger of
	enable ->
	    klogger_integration_error_logger(State#state.logger_name, disable);
	disable ->
	    ignore
    end,
    case State#state.backend of
	_Backend = #file_backend{} ->  
	    %% close file
	    close_log_file(State#state.file);
	_Backend = #ram_backend{} ->  
	    to_do;%%show info
	_ ->
	    ok
    end,
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%%===================================================================
%%% Internal functions
%%%===================================================================

log(ActionCode, Msg, TimeStamp, State) ->    
    LogMsg = klogger_msg:create_log_msg(ActionCode, Msg, TimeStamp),
    case State#state.backend of
	_Backend = #file_backend{} ->
	    write_msg_on_disk(State#state.file, LogMsg);
	_Backend = #console_backend{} ->
	    io:format("~s~n", [LogMsg]);
	_Backend = #ram_backend{} ->
	    write_msg_on_ram(LogMsg, ActionCode, State#state.ram)
    end.

manage_error_logger_event(Event, State) ->
    case Event of
	{Error, _, Data} when Error == error ->
	    if 
		State#state.level >= ?ERROR ->
		    Msg = create_error_logger_msg(Data),
		    log(?ERROR, Msg, now(), State);
		true -> 
		    ok
	    end;
	{Warning, _, Data} when Warning == warning_msg ->
	    if 
		State#state.level >= ?WARNING ->
		    Msg = create_error_logger_msg(Data),
		    log(?WARNING, Msg, now(), State);
		true -> 
		    ok
	    end;
	{Info, _, Data} when Info == info_msg ->
	    if 
		State#state.level >= ?INFO ->
		    Msg = create_error_logger_msg(Data),
		    log(?INFO, Msg, now(), State);
		true -> 
		    ok
	    end;

	_ ->
	    ignore	
    end.

create_error_logger_msg(Data) ->
    try 
	Spaces = "             ",
	case Data of
	    {_, Std, List} when 
		  Std == std_error;
		  Std == std_warning;
		  Std == std_info ->
		lists:foldl(
		  fun({F, D}, Acc) ->
			  Acc ++ lists:flatten(io_lib:format("~n~s~p: ~p", [Spaces, F, D]));
		     (Term, Acc) -> 
			  Acc ++  lists:flatten(io_lib:format("~n~s~p", [Spaces, Term]))
		  end,
		  "",
		  List);
	    {_, F, D} ->
		lists:flatten(io_lib:format(F, D))
	end
    catch _:_ ->
	    lists:flatten(io_lib:format("~p", [Data]))
    end.



klogger_integration_error_logger(Logger, enable) ->
    %% add the klogger handler to error_logger
    case lists:member(error_logger_klogger_handler, 
		      gen_event:which_handlers(error_logger)) of
	true->
	    error_logger ! {add_klogger, Logger};
	false ->
	    gen_event:add_handler(error_logger, 
				  error_logger_klogger_handler, 
				  [Logger])
    end;

klogger_integration_error_logger(Logger, disable) ->
    error_logger ! {delete_klogger, Logger}.


transfer_previous_data(State, Data) ->
    case Data of
	[]->
	    ignore;
	Data ->
	    Level = State#state.level,
	    FLog = 
		case State#state.backend of
		    _Backend = #file_backend{} ->
			fun(LMsg) -> write_msg_on_disk(State#state.file, LMsg) end;
		    _Backend = #console_backend{} ->
			fun(LMsg) -> io:format("~s~n", [LMsg]) end
		end,   
	    lists:foreach(
	      fun({ActionCode, BinData}) when ActionCode =< Level ->
		      FLog(binary_to_term(BinData));
		 (_) ->
		      ignore
	      end,
	      Data)
    end.

%%%===================================================================
%%% File backend - Internal functions
%%%===================================================================


%% file log 
%%=========================================

open_log_file(Path, LoggerName, BackendName) -> 
    Opts = [{name, lists:concat([LoggerName, "_", BackendName])},
	    {format, external},
	    {file, Path}],
    case disk_log:open(Opts) of
	{ok, Log} -> {ok, Log};
	{repaired, Log, _, _} -> {ok, Log};
	Else -> Else
    end.

write_msg_on_disk(Log, LogMsg)->
    disk_log:blog_terms(Log, [LogMsg++"\n"]).

close_log_file(Log) ->
    disk_log:close(Log).


%% ram log 
%%=========================================
init_ram_storage() ->
    Tab = ets:new(ram, [ordered_set, private]),
    ets:insert(Tab, {index, 1}),
    Tab.

destroy_ram_storage(Tab) ->
    ets:delete(Tab).

read_all_elements_ram_storage(Tab) ->
    read_all_elements_ram_storage(ets:last(Tab), Tab, []).

read_all_elements_ram_storage('$end_of_table', _Tab, Acc)->
    Acc;

read_all_elements_ram_storage(Key, Tab, Acc)->
    case ets:lookup(Tab, Key) of
	[{index,_}]->
	    read_all_elements_ram_storage(ets:prev(Tab, Key), Tab, Acc);  
	[{_, LogCodem, LogsgBin}] ->
	    read_all_elements_ram_storage(ets:prev(Tab, Key), Tab, [{LogCodem, LogsgBin}|Acc])
    end.  
 
pop_all_elements_ram_storage(Tab) ->
    Elements = read_all_elements_ram_storage(Tab),
    ets:delete_all_objects(Tab),
    Elements.

write_msg_on_ram(LogMsg, LogCode, Tab) -> 
    [{index,Index}|_] = ets:lookup(Tab, index),
    ets:insert_new(Tab, {Index, LogCode, term_to_binary(LogMsg)}),
    ets:insert(Tab, {index, Index+1}).
 
