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
%% Action log API exports.
-export([start/0, stop/0, action/2, backtrack/2, error/1, cycle/1]).
%% Action log callback exports.
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
%%% Definitions
%%%----------------------------------------------------------------------

%% Printing depth of terms like messages or exit reasons.
-define(PRINT_DEPTH, 4).
-define(PRINT_DEPTH_EXIT, 10).

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


action_string({Proc, {error, [Type, Kind|_]}})
    when Type =:= error; Type =:= throw ->
    Msg = concuerror_error:type(concuerror_error:new({Kind, foo})),
    concuerror_util:flat_format("~s exits (~P)",
        [concuerror_lid:to_string(Proc), Msg, ?PRINT_DEPTH_EXIT]);
action_string({Proc, {exit, _Extra}}) ->
    concuerror_util:flat_format("~s exits (~P)",
        [concuerror_lid:to_string(Proc), normal, ?PRINT_DEPTH_EXIT]);
action_string({Proc, block}) ->
    concuerror_util:flat_format("~s blocks", [concuerror_lid:to_string(Proc)]);
action_string({Proc, {send, {Orig, Dest, Msg}}}) ->
    NewDest =
    case is_atom(Orig) of
        true -> {name, Orig};
        false -> Dest
    end,
    concuerror_util:flat_format("~s sends message `~W` to process ~s",
        [concuerror_lid:to_string(Proc), Msg, ?PRINT_DEPTH,
            concuerror_lid:to_string(NewDest)]);
action_string({Proc, {'receive', Extra}}) ->
    {_Tag, Origin, Msg} = Extra,
    concuerror_util:flat_format("~s receives message `~W` from process ~s",
        [concuerror_lid:to_string(Proc), Msg, ?PRINT_DEPTH,
            concuerror_lid:to_string(Origin)]);
action_string({Proc, {'after', _Extra}}) ->
    concuerror_util:flat_format("~s receives no matching messages",
        [concuerror_lid:to_string(Proc)]);
action_string({Proc, {is_process_alive, Extra}}) ->
    concuerror_util:flat_format("~s checks if process ~s is alive",
        [concuerror_lid:to_string(Proc), concuerror_lid:to_string(Extra)]);
action_string({Proc, {register, {Name, TLid}}}) ->
    concuerror_util:flat_format("~s registers process ~s as `~p`",
        [concuerror_lid:to_string(Proc), concuerror_lid:to_string(TLid), Name]);
action_string({Proc, {whereis, {Name, TLid}}}) ->
    concuerror_util:flat_format("~s requests the pid of process `~p` (~s)",
        [concuerror_lid:to_string(Proc), Name, concuerror_lid:to_string(TLid)]);
action_string({Proc, {process_flag, Extra}}) ->
    {Flag, Value, _Links} = Extra,
    concuerror_util:flat_format("~s sets flag `~p` to `~p`",
                  [concuerror_lid:to_string(Proc), Flag, Value]);
action_string({Proc, {monitor, {TLid, _RefLid}}}) ->
    concuerror_util:flat_format("~s monitors process ~s",
        [concuerror_lid:to_string(Proc), concuerror_lid:to_string(TLid)]);
action_string({Proc, {spawn_monitor, {TLid, _RefLid}}}) ->
    concuerror_util:flat_format("~s spawns and monitors process ~s",
        [concuerror_lid:to_string(Proc), concuerror_lid:to_string(TLid)]);
action_string({Proc, {ets, Extra}}) ->
    case Extra of
        {insert, [_EtsLid, Tid, _K, _KP, Objects, _Status]} ->
            concuerror_util:flat_format("~s: ~p ~p",
                [concuerror_lid:to_string(Proc), ets_insert, {Tid, Objects}]);
        {insert_new, [_EtsLid, Tid, _K, _KP, Objects, _Status]} ->
            concuerror_util:flat_format("~s: ~p ~p",
                [concuerror_lid:to_string(Proc), ets_insert_new, {Tid,
                        Objects}]);
        {delete, [_EtsLid, Tid]} ->
            concuerror_util:flat_format("~s: ~p ~p",
                [concuerror_lid:to_string(Proc), ets_delete, Tid]);
        {C, [_EtsLid | Options]} ->
            ListC = atom_to_list(C),
            AtomC = list_to_atom("ets_" ++ ListC),
            concuerror_util:flat_format("~s: ~p ~p",
                [concuerror_lid:to_string(Proc), AtomC, list_to_tuple(Options)])
    end;
action_string({Proc, {demonitor, Extra}}) ->
    concuerror_util:flat_format("~s demonitors process ~s",
        [concuerror_lid:to_string(Proc), concuerror_lid:to_string(Extra)]);
action_string({Proc, {link, Extra}}) ->
    concuerror_util:flat_format("~s links to process ~s",
        [concuerror_lid:to_string(Proc), concuerror_lid:to_string(Extra)]);
action_string({Proc, {receive_no_instr, Msg}}) ->
    concuerror_util:flat_format("~s receives message `~W` from unknown process",
        [concuerror_lid:to_string(Proc), Msg, ?PRINT_DEPTH]);
action_string({Proc, {spawn, Child}}) ->
    concuerror_util:flat_format("~s spawns process ~s",
        [concuerror_lid:to_string(Proc), concuerror_lid:to_string(Child)]);
action_string({Proc, {spawn_link, Child}}) ->
    concuerror_util:flat_format("~s spawns and links to process ~s",
        [concuerror_lid:to_string(Proc), concuerror_lid:to_string(Child)]);
action_string({Proc, {spawn_opt, Child}}) ->
    concuerror_util:flat_format("~s spawns with opts to process ~s",
        [concuerror_lid:to_string(Proc), concuerror_lid:to_string(Child)]);
action_string({Proc, {unlink, Extra}}) ->
    concuerror_util:flat_format("~s unlinks from process ~s",
        [concuerror_lid:to_string(Proc), concuerror_lid:to_string(Extra)]);
action_string({Proc, {unregister, RegName}}) ->
    concuerror_util:flat_format("~s unregisters process `~p`",
        [concuerror_lid:to_string(Proc), RegName]);
action_string({Proc, Other}) ->
    concuerror_util:flat_format("~s: ~p", [concuerror_lid:to_string(Proc),
            Other]).

%%%----------------------------------------------------------------------
%%% API functions
%%%----------------------------------------------------------------------

%% @spec start(atom(), term()) -> {'ok', pid()} |
%%                                  {'error', {'already_started', pid()}}
%% @doc: Starts the action log event manager.
%%
%% `Mod' is the module containing the callback functions.
%% `Args' are the arguments given to the callback function `Mod:init/1'.
-spec start() -> {'ok', pid()} | {'error', {'already_started', pid()}}.

start() ->
    gen_event:start({local, concuerror_graph}).

%% @spec stop() -> 'ok'
%% @doc: Terminates the action log event manager.
-spec stop() -> 'ok'.

stop() ->
    gen_event:stop(concuerror_graph).

%% @spec action(non_neg_integer(), {concuerror_lid:lid(), term()}) -> 'ok'.
%% @doc: Logs an action.
-spec action(non_neg_integer(), {concuerror_lid:lid(), term()}) -> 'ok'.

action(ExploreN, {Lid, Instr}) when is_integer(ExploreN) ->
    Action = action_string({Lid, Instr}),
    LogMsg = concuerror_util:flat_format("~p: action :: ~p~n",
        [ExploreN, Action]),
    gen_event:notify(concuerror_graph, {log, LogMsg}).

%% @spec backtrack(non_neg_integer(), backtrack_type()) -> 'ok'.
%% @doc: Logs a backtrack.
-spec backtrack(non_neg_integer(), backtrack_type()) -> 'ok'.

backtrack(ExploreN, Type) when is_integer(ExploreN), is_atom(Type) ->
    LogArgs = [ExploreN, Type],
    LogMsg = io_lib:format("~p: backtrack :: ~p\n", LogArgs),
    gen_event:notify(concuerror_graph, {log, LogMsg}).

%% @spec error(non_neg_integer()) -> 'ok'.
%% @doc: Logs an error.
-spec error(non_neg_integer()) -> 'ok'.

error(ExploreN) when is_integer(ExploreN) ->
    LogMsg = io_lib:format("~p: error\n", [ExploreN]),
    gen_event:notify(concuerror_graph, {log, LogMsg}).

%% @spec cycle(non_neg_integer()) -> 'ok'.
%% @doc: Logs a cycle.
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
