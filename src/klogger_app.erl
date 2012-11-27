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

-module(klogger_app).

-behaviour(application).

%% Application callbacks
-export([start/2, prep_stop/1, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    klogger_sup:start_link().

prep_stop(_State) ->
    %% delete handler from error_logger
    gen_event:delete_handler(error_logger, error_logger_klogger_handler, []),

    %% enable error_logger tty
    error_logger:tty(false),

    %% delete loggers modules
    lists:foreach(fun({Mod, _, _, _}) ->
			  code:delete(Mod)
		  end,
		  supervisor:which_children(klogger_sup)),	     
    ok.



stop(_State) ->
    ok.
