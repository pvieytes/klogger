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

-module(ituklog).


%% API
-export([start/0,
	 stop/0]).


%% ===================================================================
%% API
%% ===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% start ituklog application
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
%% stop ituklog application
%%
%% @spec stop() -> ok | {error | Error}
%%
%% @end
%%--------------------------------------------------------------------
stop()->
    application:stop(?MODULE).
