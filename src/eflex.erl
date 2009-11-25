-module(eflex).
-compile([export_all]).

-include("eflex.hrl").

start() ->
    %% start([{auto_increment, 3}]). % Add 3 days for each mouse move
    start([]).

start(Options) ->
    process_flag(trap_exit, true),

    Pid = start_link(Options),
    receive
	{'EXIT', Pid, Reason} ->
	    io:format("EXIT: ~p\n", [Reason]),
	    init:stop(1)
    end.

%% app_start_link(KeyVals) ->
%%     OldKeyVals = application:get_all_env(?APPLICATION),
%%     [ok = application:unset_env(?APPLICATION, Key) ||
%% 	{Key, _} <- OldKeyVals,
%% 	Key =/= included_applications],
%%     [ok = application:set_env(?APPLICATION, Key, Val) ||
%% 	{Key, Val} <- KeyVals],
%%     application:start(?APPLICATION).

start_link(Options) ->
    eflex_wx:start_link(Options).

stop() ->
    application:stop(?APPLICATION).

debug() ->
    start([{debug, 2}]).

batch() ->
    start([{window, false}]).
