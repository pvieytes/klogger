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
   
    %%delete previous logs
    LogFilePath =  "./test.log",
    file:delete(LogFilePath),   

    %%start app
    ?assertMatch(ok, klogger:start()),

    %% add logger
    ?assertMatch(ok, klogger:add_logger(logger)),
    ?assertMatch(ok, klogger:delete_logger(logger)),
    Backend = {backend, [{name, console_log}, 
			 {type, console_backend},
			 {loglevel, debug},
			 {get_error_logger, enable}
			]},
    ?assertMatch(ok, klogger:add_logger(logger, Backend)),
    ?assertMatch(true, 
		 lists:member(error_logger_klogger_handler,
			      gen_event:which_handlers(error_logger))),

    %% add logger
    Backends = [
		{backend, [{name, console_log}, 
			   {type, console_backend},
			   {loglevel, debug},
			   {get_error_logger, enable}
			  ]},
		{backend,  [{name, file_log}, 
			    {type, file_backend},
			    {loglevel, debug},
			    {path, LogFilePath},
			    {get_error_logger, enable}
			   ]}
	       ],

    %% logging
    ?assertNot(ok == klogger:add_logger(logger, Backends)),
    ?assertMatch(ok, klogger:delete_logger(logger)),
    ?assertMatch(ok, klogger:add_logger(logger, Backends)),
    ?assertMatch(ok, logger:debug("text message")),
    ?assertMatch(ok, logger:info("text message")),
    ?assertMatch(ok, logger:warning("text message")),
    ?assertMatch(ok, logger:error("text message")),
    ?assertMatch(ok, logger:fatal("text message")),

    %% error logger
    ?debugMsg("error logger"),
    ?assertMatch(ok, error_logger:info_msg("info msg in error logger")),    
    ?assertMatch(ok, error_logger:info_msg("info msg in error logger; data: ~p", [data_atom])),    
    ?assertMatch(ok, error_logger:info_report([{info,data1},a_term,{tag2,data}])),    
    ?assertMatch(ok, error_logger:warning_msg("warning msg in error logger")),
    ?assertMatch(ok, error_logger:warning_msg("warning msg in error logger: ~p", [data_atom])),
    ?assertMatch(ok, error_logger:warning_report([{warning,data1},a_term,{tag2,data}])),
    ?assertMatch(ok, error_logger:error_msg("error msg in error logger")),
    ?assertMatch(ok, error_logger:error_msg("error msg in error logger: ~p", [data_atom])),
    ?assertMatch(ok, error_logger:error_report([{error,data1},a_term,{tag2,data}])),
    ?assertMatch(ok, klogger:get_error_logger(logger, console_log, disable)),
    ?assertMatch(ok, klogger:get_error_logger(logger, file_log, disable)),
    timer:sleep(100),
    ?assertMatch(ok, error_logger:info_msg("info msg in error logger")),    

    %%stop klogger
    ?assertMatch(ok, klogger:stop()).

