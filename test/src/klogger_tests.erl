%%-------------------------------------------------------------------
%% @author Pablo Vieytes <mail@pablovieytes.com>
%% @copyright (C) 2012, Pablo Vieytes
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
%% @doc
%
%% @end
%% Created : 26 Nov 2012 by Pablo Vieytes <mail@pablovieytes.com>
%%-------------------------------------------------------------------
-module(klogger_tests).
-include_lib("eunit/include/eunit.hrl").
-export([start/0]).

start() ->
    eunit:test(klogger).

general_test() ->
    ?debugMsg("General tests"),
    LogFilePath =  "./test.log",
    %%delete previous logs
    file:delete(LogFilePath),   
    ?assertMatch(ok, klogger:start()),
    Options = [{console_backend, console_log, debug}, {file_backend, file_log, debug, LogFilePath}],
    ?assertMatch(ok, klogger:add_logger(logger, Options)),
    ?assertMatch(ok, logger:debug("text message")),
    ?assertMatch(ok, logger:info("text message")),
    ?assertMatch(ok, logger:warning("text message")),
    ?assertMatch(ok, logger:error("text message")),
    ?assertMatch(ok, logger:fatal("text message")),
    ?assertMatch(ok, klogger:stop()),
    ?assertMatch(ok, ok).
