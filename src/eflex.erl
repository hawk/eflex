%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2009 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(eflex).
-export([main/1]).
-include("eflex.hrl").

usage() ->
    io:format("Eflex - a flextime calculation tool implemented with Erlang/OTP \n"),
    io:format("\nusage:\n"),
    io:format("    eflex                     (run the graphical tool)\n"),
    halt(1).

main([]) -> start();
main(["--help"]) ->
    File = "README.md",
    case file:read_file(File) of
	{ok, Bin} ->
	    io:format("~s\n", [binary_to_list(Bin)]);
	{error, Reason} ->
	    fatal_error(3, "~s: ~s\n", [File, file:format_error(Reason)])
    end;
main(_) ->
    usage().

fatal_error(RetCode, Format, Args) ->
    io:format(Format, Args),
    halt(RetCode).


start() ->
    start([]).

start(Options) ->
    process_flag(trap_exit, true),
    Pid = start_link(Options),
    receive
	{'EXIT', Pid, Reason} ->
	    io:format("EXIT: ~p\n", [Reason]),
	    init:stop(1)
    end.

start_link(Options) ->
    eflex_wx:start_link(Options).

stop() ->
    application:stop(?APPLICATION).

debug() ->
    start([{debug, 2}]).

batch() ->
    start([{window, false}]).
