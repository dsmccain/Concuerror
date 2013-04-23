%%%----------------------------------------------------------------------
%%% Copyright (c) 2011, Alkis Gotovos <el3ctrologos@hotmail.com>,
%%%                     Maria Christakis <mchrista@softlab.ntua.gr>
%%%                 and Kostis Sagonas <kostis@cs.ntua.gr>.
%%% All rights reserved.
%%%
%%% This file is distributed under the Simplified BSD License.
%%% Details can be found in the LICENSE file.
%%%----------------------------------------------------------------------
%%% Author      : Daniel S. McCain <dsmccain@acm.org>
%%% Description : Action logging interface for graph construction
%%%----------------------------------------------------------------------

-module(concuerror_graph).
%% Non gen_evt exports.
-export([internal/1, internal/2]).
%% Log API exports.
-export([start/0, stop/0, action/2, action/3, backtrack/2, error/1, cycle/1]).
%% Log callback exports.
-export([init/1, terminate/2, handle_call/2, handle_info/2,
         handle_event/2, code_change/3]).

-behaviour(gen_event).

-include("gen.hrl").

%%%----------------------------------------------------------------------
%%% Callback types
%%%----------------------------------------------------------------------

-type backtrack_type() :: 'normal' | 'deadlock' | 'sleep_set_block'.
%% -type event() ::
%%     {'backtrack', backtrack_type()} |
%%     {'action', string()} |
%%     'error' |
%%     'cycle'.
-type event() :: {'log', string()}.
-type state() :: file:fd().

-export_type([backtrack_type/0, event/0, state/0]).

%%%----------------------------------------------------------------------
%%% Non gen_evt functions.
%%%----------------------------------------------------------------------

%% @spec internal(string()) -> no_return()
%% @doc: Print an internal error message and halt.
-spec internal(string()) -> no_return().
internal(String) ->
    internal(String, []).

%% @spec internal(string(), [term()]) -> no_return()
%% @doc: Like `internal/1', but prints a formatted message using arguments.
-spec internal(string(), [term()]) -> no_return().
internal(String, Args) ->
    InitPid = whereis(init),
    group_leader(InitPid, self()),
    io:format("(Internal) " ++ String, Args),
    halt(?RET_INTERNAL_ERROR).

%%%----------------------------------------------------------------------
%%% API functions
%%%----------------------------------------------------------------------

%% @spec start(atom(), term()) -> {'ok', pid()} |
%%                                  {'error', {'already_started', pid()}}
%% @doc: Starts the log event manager.
%%
%% `Mod' is the module containing the callback functions.
%% `Args' are the arguments given to the callback function `Mod:init/1'.
-spec start() -> {'ok', pid()} | {'error', {'already_started', pid()}}.

start() ->
    gen_event:start({local, concuerror_graph}).

%% @spec stop() -> 'ok'
%% @doc: Terminates the log event manager.
-spec stop() -> 'ok'.

stop() ->
    gen_event:stop(concuerror_graph).

%% ##########
%% TODO:
-spec action(non_neg_integer(), string()) -> 'ok'.
action(ExploreN, String) when is_integer(ExploreN), is_list(String) ->
    action(ExploreN, String, []).

-spec action(non_neg_integer(), string(), [term()]) -> 'ok'.
action(ExploreN, String, Args) when is_integer(ExploreN), is_list(String) ->
    Msg = io_lib:format(String, Args),
    LogArgs = [ExploreN, Msg],
    LogMsg = io_lib:format("~p: action :: ~p\n", LogArgs),
    gen_event:notify(concuerror_graph, {log, LogMsg}).

-spec backtrack(non_neg_integer(), backtrack_type()) -> 'ok'.
backtrack(ExploreN, Type) when is_integer(ExploreN), is_atom(Type) ->
    LogArgs = [ExploreN, Type],
    LogMsg = io_lib:format("~p: backtrack :: ~p\n", LogArgs),
    gen_event:notify(concuerror_graph, {log, LogMsg}).

-spec error(non_neg_integer()) -> 'ok'.
error(ExploreN) when is_integer(ExploreN) ->
    LogMsg = io_lib:format("~p: error\n", [ExploreN]),
    gen_event:notify(concuerror_graph, {log, LogMsg}).

-spec cycle(non_neg_integer()) -> 'ok'.
cycle(ExploreN) when is_integer(ExploreN) ->
    LogMsg = io_lib:format("~p: cycle\n", [ExploreN]),
    gen_event:notify(concuerror_graph, {log, LogMsg}).

%%%----------------------------------------------------------------------
%%% Callback functions
%%%----------------------------------------------------------------------

-spec init(term()) -> {'ok', state()}.

init(FileName) ->
	{ok, Fd} = file:open(FileName, [write]),
	{ok, Fd}.

-spec terminate(term(), state()) -> 'ok'.

terminate(_Reason, File) ->
    file:close(File),
    ok.

-spec handle_event(event(), state()) -> {'ok', state()}.

handle_event({log, String}, File) ->
	file:write(File, String),
    {ok, File}.

-spec code_change(term(), term(), term()) -> no_return().

code_change(_OldVsn, _State, _Extra) ->
    internal("~p:~p: code_change~n", [?MODULE, ?LINE]).

-spec handle_info(term(), term()) -> no_return().

handle_info(_Info, _State) ->
    internal("~p:~p: handle_info~n", [?MODULE, ?LINE]).

-spec handle_call(term(), term()) -> no_return().

handle_call(_Request, _State) ->
    internal("~p:~p: handle_call~n", [?MODULE, ?LINE]).
