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
%% Functions to crate dynamically the logger code
%%
%% @end
%% Created : 22 Nov 2012 by Pablo Vieytes <pvieytes@openshine.com>
%%-------------------------------------------------------------------

-module(klogger_msg).

-include_lib("klogger/include/klogger.hrl").

-export([create_log_msg/3]).

-define(DAYNAME(D),
	case D of
	    1 -> "Mon";
	    2 -> "Tue";
	    3 -> "Wed";
	    4 -> "Thr";
	    5 -> "Fri";
	    6 -> "Sat";
	    7 -> "Sun"
	end).

-define(MONTHNAME(M),
	case M of
	    1 -> "Jan";
	    2 -> "Feb";
	    3 -> "Mar";
	    4 -> "Apr";
	    5 -> "May";
	    6 -> "Jun";
	    7 -> "Jul";
	    8 -> "Aug";
	    9 -> "Sep";
	    10 -> "Oct";
	    11 -> "Nov";
	    12 -> "Dec"
	end).		



create_log_msg(ActionCode, Msg, TimeStamp) ->
    Act = 
	case ActionCode of
	    ?DEBUG  -> "DBG";
	    ?INFO -> "INFO";
	    ?WARNING  -> "WARN";
	    ?ERROR  -> "*ERROR*";
	    ?FATAL  -> "**FATAL**"
	end,
    {{Y, Mt, D}, {Ho, Mn, S}} = calendar:now_to_local_time(TimeStamp),
    DayName = ?DAYNAME(calendar:day_of_the_week(Y, Mt, D)),
    MonthName = ?MONTHNAME(Mt),
    Min =	
	case Mn of
	    Mn when Mn < 10 -> 
		lists:flatten(io_lib:format("0~p", [Mn]));
	    _ ->
		lists:flatten(io_lib:format("~p", [Mn]))
	end,
    Seg = 
	case S of
	    S when S < 10 -> 
		lists:flatten(io_lib:format("0~p", [S]));
	    _ ->
		lists:flatten(io_lib:format("~p", [S]))
	end,
    {_, _, Micro} = TimeStamp,
    Mili = 
	case trunc(Micro/1000) of
	    M when M < 10 ->
		lists:flatten(io_lib:format("00~p", [M]));		       
	    M when M < 100 ->
		lists:flatten(io_lib:format("0~p", [M]));	
	    M when M < 1000 ->
		lists:flatten(io_lib:format("~p", [M]))
	end,
    Date = lists:flatten(io_lib:format("~s ~s ~p ~p:~s:~s ~s" , [DayName, MonthName, Y, Ho, Min, Seg, Mili])),
    lists:flatten(io_lib:format("[~s] [~s]  ~s", [Date, Act, Msg])).
