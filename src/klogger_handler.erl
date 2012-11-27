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

-include_lib("klogger/include/klogger.hrl").
%% API
-export([get_error_logger/3]).

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 


-record(state, {name,
		level,
		backend,
		file=[],
		get_error_logger=disable, 
		logger_name}).



%%%===================================================================
%%% gen_event callbacks
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
init([LoggerName, Backend=#file_backend{}]) ->
    process_flag(trap_exit, true),
    case open_log_file(Backend#file_backend.path, LoggerName, Backend#file_backend.name) of
	{ok, File} ->
	    case Backend#file_backend.get_error_logger of
		enable ->
		    klogger_integration_error_logger(LoggerName, Backend#file_backend.get_error_logger);
		_ ->
		    ignore
	    end,
	    {ok, #state{name=Backend#file_backend.name,
			level=Backend#file_backend.level,
			backend=Backend,
			get_error_logger=Backend#file_backend.get_error_logger,
			logger_name=LoggerName,
			file=File}};
	{error, _} = E ->
	    E
    end;

init([LoggerName, Backend=#console_backend{}]) ->
    process_flag(trap_exit, true),
    case Backend#console_backend.get_error_logger of
	enable ->
	    klogger_integration_error_logger(LoggerName, Backend#console_backend.get_error_logger);
	_ ->
	    ignore
    end,
    {ok, #state{name=Backend#console_backend.name,
		level=Backend#console_backend.level,
		get_error_logger=Backend#console_backend.get_error_logger,
		logger_name=LoggerName,
		backend=Backend}}.



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
    case State#state.backend of
	_Backend = #file_backend{} ->  
	    %% close file
	    close_log_file(State#state.file);
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
	    io:format("~s~n", [LogMsg])
    end.

manage_error_logger_event(Event, State) ->
    case Event of
	{Error, _, Data} when Error == error; 
			      Error == error_report  ->
	    if 
		State#state.level >= ?ERROR ->
		    Msg = create_error_logger_msg(Data),
		    log(?ERROR, Msg, now(), State);
		true -> 
		    ok
	    end;
	{Warning, _, Data} when Warning == warning_msg; 
				Warning == warning_report ->
	    if 
		State#state.level >= ?WARNING ->
		    Msg = create_error_logger_msg(Data),
		    log(?WARNING, Msg, now(), State);
		true -> 
		    ok
	    end;
	{Info, _, Data} when Info == info_msg; 
			     Info == info_report ->
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
    end.



klogger_integration_error_logger(Logger, enable) ->
    %% add the klogger handler to error_logger
    case lists:member(error_logger_klogger_handler, 
		      gen_event:which_handlers(error_logger)) of
	true->
	    error_logger ! {add_klogger, Logger};
	false ->
	    error_logger:tty(false),
	    gen_event:add_handler(error_logger, 
				  error_logger_klogger_handler, 
				  [Logger])
    end;

klogger_integration_error_logger(Logger, disable) ->
    error_logger ! {delete_klogger, Logger}.




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
