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

-include_lib("klogger/include/klogger.hrl").

%% API
-export([start/0,
	 stop/0,
	 add_logger/1,
	 add_logger/2,
	 set_log_level/2,
	 get_error_logger/3]).



-define(LOGGERCHILD(Id, Params), {Id, {klogger_logger, start_link, [Params]}, permanent, 5000, worker, dynamic}).

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
%% @spec add_logger(Logger::atom()) -> ok | {error | Error}
%%
%% @end
%%--------------------------------------------------------------------
add_logger(Logger)->
    BackendSpecs = [{console_backend, console_log, ?DEBUG}],
    add_logger(Logger, BackendSpecs).

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
    klogger_log:create_logger(Logger, BackendSpecs).


%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%%
%% @spec set_log_level(Logger::atom(), List::[{backend::atom(), Level::integer}]) -> ok | {error | Error}
%%
%% @end
%%--------------------------------------------------------------------
set_log_level(Logger, Tuple) when is_tuple(Tuple) ->
    set_log_level(Logger, [Tuple]);

set_log_level(Logger, LevelList) ->
    klogger_log:set_log_level(Logger, LevelList).

get_error_logger(Logger, Backend, Mode) ->
    %% check logger
    case code:is_loaded(Logger) of
	true ->    
	    case Mode of
		enable ->
		    %% add the klogger handler to error_logger
		    case lists:member(error_logger_klogger_handler, 
				      gen_event:which_handlers(error_logger)) of
			true->
			    ignore;
			false ->
			    gen_event:add_handler(error_logger, 
						  error_logger_klogger_handler, 
						  [])
		    end,	    
		    Logger ! {get_error_logger, Backend},
		    error_logger ! {add_klogger, Logger},
		    ok;	  
		disable ->

		    ok
	    end;
	false ->
	    {error, "logger not found"}
    end.
		

