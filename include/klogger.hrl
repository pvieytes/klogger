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
%% Created : 20 Nov 2012 by Pablo Vieytes <pvieytes@openshine.com>
%%-------------------------------------------------------------------

-define(DEBUG, 5).
-define(INFO, 4).
-define(WARNING, 3).
-define(ERROR, 2).
-define(FATAL, 1).
-define(NONE, 0).

-define(LEVELCODE(L),
	case L of
	    debug -> ?DEBUG;
	    info -> ?INFO;
	    warning -> ?WARNING;
	    error -> ?ERROR;
	    fatal -> ?FATAL;
	    none -> ?NONE;
	    L when is_integer(L),  L >= ?NONE,  L =< ?DEBUG ->
		L		
	end).

-record(file_backend, {name, 
		       level,
		       path,
		       get_error_logger=disable}).

-record(console_backend, {name,
			  level,
			  get_error_logger=disable}).

-record(ram_backend, {name,
		      level,
		      get_error_logger=disable}).

