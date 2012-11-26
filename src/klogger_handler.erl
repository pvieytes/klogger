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

%% gen_event callbacks
-export([init/1, handle_event/2, handle_call/2, 
	 handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 


-record(file_backend_state, {name,
			     backend,
			     file}).

-record(console_backend_state, {name,
				backend}).


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
	    {ok, #file_backend_state{name=Backend#file_backend.name,
				     backend=Backend,
				     file=File}};
	{error, _} = E ->
	    E
    end;

init([_LoggerName, Backend=#console_backend{}]) ->
    process_flag(trap_exit, true),
    {ok, #console_backend_state{name=Backend#console_backend.name,
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
handle_event({log, BackendName, ActionCode, Msg, TimeStamp}, State=#file_backend_state{}) ->
    if
	BackendName == 	State#file_backend_state.name ->
	    LogMsg = klogger_msg:create_log_msg(ActionCode, Msg, TimeStamp),
	    write_msg(State#file_backend_state.file, LogMsg);
	true ->
	    ignore
    end,
    {ok, State};

handle_event({log, BackendName, ActionCode, Msg, TimeStamp}, State=#console_backend_state{}) ->
    if
	BackendName == 	State#console_backend_state.name ->
	    LogMsg = klogger_msg:create_log_msg(ActionCode, Msg, TimeStamp),
	    io:format("~s~n", [LogMsg]);
	true ->
	    ignore
    end,
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
terminate(_Reason, State=#file_backend_state{}) ->
    %% close file
    close_log_file(State#file_backend_state.file),
    ok;

terminate(_Reason, _State=#console_backend_state{}) ->
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

write_msg(Log, LogMsg)->
    disk_log:blog_terms(Log, [LogMsg++"\n"]).

close_log_file(Log) ->
    disk_log:close(Log).
