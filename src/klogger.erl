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

-module(klogger).


%% API
-export([start/0,
	 stop/0,
	 add_logger/2,
	 set_log_level/2,
	 log/3]).



-define(LOGGERSERVER(Id, Params), {Id, {klogger_logger, start_link, [Params]}, permanent, 5000, worker, dynamic}).

%% ===================================================================
%% API
%% ===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% start klogger application
%%
%% @spec start() -> ok | {error | Error}
%%
%% @end
%%--------------------------------------------------------------------
start()->
    application:start(?MODULE).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% stop klogger application
%%
%% @spec stop() -> ok | {error | Error}
%%
%% @end
%%--------------------------------------------------------------------
stop()->
    application:stop(?MODULE).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% add logger to the app
%%
%% @spec add_logger(Logger::atom(), BackendSpecs::[backend()]) -> ok | {error | Error}
%%
%% @end
%%--------------------------------------------------------------------
add_logger(Logger, BackendSpecs)->
    LoggerServerSpecs =?LOGGERSERVER(Logger, {Logger, BackendSpecs}),
    case supervisor:start_child(klogger_sup, LoggerServerSpecs) of
	{ok, _Pid} -> ok;
	Error -> Error
    end.
    


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%%
%% @spec set_log_level(Logger::atom(), List::[{backend::atom(), Level::integer}]) -> ok | {error | Error}
%%
%% @end
%%--------------------------------------------------------------------
set_log_level(Logger, List) ->
    gen_server:call(Logger, {set_log_level, List}).

	    
	    

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%%
%% @spec log(Logger::atom(), Msg::string()) -> ok | {error | Error}
%%
%% @end
%%--------------------------------------------------------------------
log(Logger, Action, Msg) ->
    Logger:log(Action, Msg).

	    
%% ===================================================================
%% Internal functions
%% ===================================================================

