%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2009 Dan Gudmundsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(eflex_break).

-compile(export_all).

-include_lib("wx/include/wx.hrl").

start(Minutes) when Minutes =< 0 -> 
    case whereis(eflex_mini_break) of
	undefined -> ok;
	Pid -> exit(Pid, kill)
    end;
start(Minutes) -> 
    io:format("Mini Break ~p~n",[Minutes]),
    case whereis(eflex_mini_break) of
	undefined -> ok;
	Pid -> exit(Pid, kill)
    end,

    spawn(fun() ->
		  register(eflex_mini_break, self()),
		  wx:new(),
		  loop(Minutes, wx_misc:getMousePosition(), erlang:now())
	  end).

loop(Min, Pos, Time) ->
    timer:sleep(timer:minutes(3)),
    case wx_misc:getMousePosition() of
	Pos ->
	    loop(Min, Pos, erlang:now());
	New ->	    
	    Active = timer:now_diff(erlang:now(), Time),
	    case minutes(Active) > Min of
		true ->
		    Env = wx:get_env(),
		    spawn_link(fun() ->
				       wx:set_env(Env),
				       break()
			       end),
		    loop(Min, New, now());
		_ ->
		    loop(Min, New, Time)
	    end
    end.


minutes(Milli) ->
    Milli div (60*1000*1000).

break() ->
    wx_misc:bell(),
    timer:sleep(500),
    wx_misc:bell(),
    Dlg = wxMessageDialog:new(wx:null(), paus_msg(), 
			      [{style, ?wxSTAY_ON_TOP bor ?wxCENTER 
				bor ?wxDIALOG_NO_PARENT bor ?wxOK}]),
    wxMessageDialog:showModal(Dlg).

paus_msg() ->
    "Now it's time to take a break, says the ergonomist.\n" 
	"Change the position, grab a cup of coffee go talk to your nice working mates.".



