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
%%
%% @end
%% Created : 19 Nov 2012 by Pablo Vieytes <pvieytes@openshine.com>
%%-------------------------------------------------------------------

-module(klogger_logger).

-behaviour(gen_server).

-include_lib("klogger/include/klogger.hrl").


%% API
-export([start_link/1
	 ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {name, 
		backends}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Params::term()) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link({LoggerName, BackendSpecs}) ->
    gen_server:start_link({local, LoggerName}, ?MODULE, [{LoggerName, BackendSpecs}], []).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([{LoggerName, BackendSpecs}]) ->
    {ok, #state{name=LoggerName,
		backends=BackendSpecs}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({set_log_level, List}, _From, State) ->
    _Reply = set_log_level(State, List),
    {reply, ok, State};


handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast({log, Backend, ActionCode, Msg, TimeStamp}, State) ->
    Mod = State#state.name,
    LogMsg = Mod:create_log_msg(ActionCode, Msg, TimeStamp),
    io:format("dbg backend: ~p -> msg: ~p~n", [Backend, LogMsg]),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
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

set_log_level(State, List) ->
    _StoredBackends = State#state.backends,
    Name = State#state.name,
    CodeString = get_code(Name, List),
    {Module,Code} = dynamic_compile:from_string(CodeString),
    case  code:load_binary(Module, atom_to_list(Name) ++ ".erl", Code) of
    	{error, _Error}=E->
    	    E;
    	_ -> ok
    end.

   
get_code(LoggerName, List)->
    ModuleStr = atom_to_list(LoggerName),
    ListStr = create_list(List),
    "-module(" ++ ModuleStr ++ ").
     -export([log/2,
              create_log_msg/3,
              debug/1,
              info/1,
              warning/1,
              error/1,
              fatal/1
            ]).  

     -define(DEBUG, "++ lists:flatten(io_lib:format("~p", [?DEBUG])) ++ ").
     -define(INFO, "++ lists:flatten(io_lib:format("~p", [?INFO])) ++ ").
     -define(WARNING, "++ lists:flatten(io_lib:format("~p", [?WARNING]))  ++ ").
     -define(ERROR, "++ lists:flatten(io_lib:format("~p", [?ERROR])) ++ ").
     -define(FATAL, "++ lists:flatten(io_lib:format("~p", [?FATAL])) ++ ").
     -define(NONE, "++ lists:flatten(io_lib:format("~p", [?NONE])) ++ ").


     -define(LEVELCODE(Level),
        case Level of
            debug -> ?DEBUG;
            info -> ?INFO;
            warning -> ?WARNING;
            error -> ?ERROR;
            fatal -> ?FATAL;
            none -> ?NONE
        end).


       debug(Msg) -> log(?DEBUG, Msg).

       info(Msg) -> log(?INFO, Msg).

       warning(Msg) -> log(?WARNING, Msg). 

       error(Msg) -> log(?ERROR, Msg).

       fatal(Msg) -> log(?FATAL, Msg).

      
      log(Action, Msg) ->
         List = " ++ ListStr ++ ",
         ActionCode = case Action of
                         Action when is_atom(Action) -> ?LEVELCODE(Action);
                         Action -> Action
                      end,

         lists:foreach(
             fun({'console_log_backend', BackendLevel}) when ActionCode =< BackendLevel  ->
                    io:format(\"~s~n\", [create_log_msg(ActionCode, Msg, now())]);
                ({'console_log_backend', _Level})->
                    io:format(\"dbg console ActionLevel: ~p; BackendLevel: ~p~n\", [ActionCode,  _Level]),
                    ok;
                ({Backend, BackendLevel}) when ActionCode =< BackendLevel ->
                    gen_server:cast(" ++ ModuleStr ++ ", {log, Backend, ActionCode, Msg, now()});
                (_Else) ->
                    ok
             end,
             List). 

      create_log_msg(ActionCode, Msg, TimeStamp) ->
          H = case ActionCode of
                   ?DEBUG  -> \"DBG\";
                   ?INFO -> \"INFO\";
                   ?WARNING  -> \"WARN\";
                   ?ERROR  -> \"ERROR\";
                   ?FATAL  -> \"** FATAL**\"
               end,
          {{Y, Mt, D}, {Ho, Mn, S}} = calendar:now_to_local_time(TimeStamp),
          Date = lists:flatten(io_lib:format(\"~p/~p/~p - ~p:~p:~p\" , [D, Mt, Y, Ho, Mn, S])),
          lists:flatten(io_lib:format(\"~s - ~s -- ~s\", [H, Date, Msg])).

      ".

create_list(L)->
    "[" ++ create_list_loop(L, "") ++ "]".

create_list_loop([{BackEnd, Level}|[]], Acc)->
    B = atom_to_list(BackEnd),
    L = lists:flatten(io_lib:format("~p", [Level])),
    Acc ++ "{'" ++ B ++ "', " ++ L ++ "}";

create_list_loop([{BackEnd, Level}|Rest], Acc) ->
    B = atom_to_list(BackEnd),
    L = lists:flatten(io_lib:format("~p", [Level])),
    "{'" ++ B ++ "', " ++ L ++ "}, " ++ create_list_loop(Rest, Acc).
