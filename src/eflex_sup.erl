-module(eflex_sup).

-behaviour(application).
-behaviour(supervisor).

-export([start/0, start/2, init/1, stop/1]).

-define(APPLICATION, eflex).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% application and supervisor callback functions

start(normal, Args) ->
    SupName = {local, ?MODULE},
    case supervisor:start_link(SupName, ?MODULE, [Args]) of
        {ok, Pid} ->
            {ok, Pid, {normal, Args}};
        Error -> 
            Error
    end;
start(_, _) ->
    {error, badarg}.

start() ->
    SupName = {local, ?MODULE},
    supervisor:start_link(SupName, ?MODULE, []).

stop(_StartArgs) ->
    ok.

init([]) -> % Supervisor
    init();
init([[]]) -> % Application
    init();
init(BadArg) ->
    {error, {badarg, BadArg}}.
    
init() ->
    RestartStrategy = one_for_one,
    MaxR = 0,
    MaxT = timer:seconds(1),
    KillAfter = timer:seconds(3),
    Args = application:get_all_env(?APPLICATION),
    Args2 = lists:keydelete(included_applications, 1, Args),
    Mod = eflex_wx,
    MFA = {Mod, start_link, [Args2]},
    Modules = [Mod, proc_lib],
    ChildSpecs = [{Mod, MFA, permanent, KillAfter, worker, Modules}],
    {ok, {{RestartStrategy, MaxR, MaxT}, ChildSpecs}}.
