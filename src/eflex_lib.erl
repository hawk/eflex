%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2009 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(eflex_lib).
-compile([export_all]).

-include("eflex.hrl").

-define(DIR_NAME, ".eflex").
-define(FILE_PREFIX, "eflex_").
-define(FILE_SUFFIX, ".xml").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parse options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

parse_options(Options) ->
    Dir = dir_name(),
    Cookie = term_to_binary({node(), self(), erlang:now()}),
    O = #options{window         = true,
                 dir            = Dir,
                 read_only      = false,
                 lock_file      = filename:join([Dir, "eflex.lock"]),
                 lock_cookie    = Cookie,
                 arrived_file   = filename:join([Dir, "arrived_timestamp.txt"]),
                 left_file      = filename:join([Dir, "left_timestamp.txt"]),
                 debug          = 0,
                 date           = date(),
                 auto_increment = undefined,
                 n_rows         = 20},
    parse_options(Options, O).

parse_options([{Key, Val} | Options], O) when is_record(O, options) ->
    case Key of
        dir when is_list(Val) ->
            parse_options(Options, O#options{dir = Val});
        read_only when Val =:= true; Val =:= false ->
            parse_options(Options, O#options{read_only = Val});
        window when Val =:= true; Val =:= false ->
            parse_options(Options, O#options{window = Val});
        debug when is_integer(Val) ->
            parse_options(Options, O#options{debug = Val});
        date when is_tuple(Val), size(Val) =:= 3 ->
            parse_options(Options, O#options{date = Val});
        auto_increment when is_integer(Val) ->
            parse_options(Options, O#options{auto_increment = Val});
        n_rows when is_integer(Val) ->
            parse_options(Options, O#options{n_rows = Val});
        absence_all_day when is_list(Val) ->
            parse_options(Options, O#options{absence_all_day = Val});
        unspecified_work when is_list(Val) ->
            parse_options(Options, O#options{unspecified_work = Val});
        _ -> 
            exit({badarg, {Key, Val}})
    end;
parse_options([], O) ->
    O.

dir_name() ->
    case os:getenv("HOME") of
        false ->
            {ok, Pwd} = file:get_cwd(),
            filename:join([Pwd, ?DIR_NAME]);
        Home ->
            filename:join([Home, ?DIR_NAME])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Make a default year record
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_initial_year(YearNo, Options) ->
    Econfig = make_initial_config(Options),
    #eflex_year{no     = YearNo,
                weeks  = make_initial_weeks(YearNo, Econfig),
                config = Econfig}.

make_initial_config(O) ->
    WorkingTime =
        #eflex_working_time{monday    = 7 * 60 + 45,
                            tuesday   = 7 * 60 + 45,
                            wednesday = 7 * 60 + 45,
                            thursday  = 7 * 60 + 45,
                            friday    = 7 * 60 + 45,
                            saturday  = 0,
                            sunday    = 0},
    LunchDuration = 40,
    WorkdayBreak =
        #eflex_break{duration          = 30,
                     time_before_break = 4 * 60},
    FreedayBreak =
        #eflex_break{duration          = 30,
                     time_before_break = 5 * 60},
    Att = #eflex_activity_type{initial    = 0,
			       unit       = decimal,
			       category   = attendance,
			       sign       = positive,
			       visibility = visible,
			       share      = 0},
    Proj = Att#eflex_activity_type{category = project},
    ActivityTypes =
        [
         Att#eflex_activity_type{name       = ?DATE_ACT,
				 unit       = date,
				 visibility = hidden},          
         Att#eflex_activity_type{name       = ?ARRIVED_ACT,
				 unit       = minutes,
				 visibility = hidden},
         Att#eflex_activity_type{name       = ?LEFT_ACT,
				 unit       = minutes,
				 visibility = hidden},
         Att#eflex_activity_type{name       = ?BREAK_ACT,
				 unit       = minutes,
				 visibility = hidden},
         Att#eflex_activity_type{name       = ?ADJUST_ACT,
				 unit       = minutes,
				 visibility = hidden},
         Att#eflex_activity_type{name       = ?WORK_ACT,
				 visibility = hidden},
         Att#eflex_activity_type{name       = ?FLEX_ACT,
				 visibility = hidden},
         Att#eflex_activity_type{name       = ?OVERTIME_ACT,
				 sign       = negative,
				 visibility = visible},
         Att#eflex_activity_type{name       = ?PARENTAL_ACT,
				 unit       = decimal},
         Att#eflex_activity_type{name       = ?ABSENCE_ACT},
         Att#eflex_activity_type{name       = ?VACATION_ACT},
         Proj#eflex_activity_type{name       = ?UNSPEC_ACT,
				  visibility = hidden},
         Proj#eflex_activity_type{name       = "OTP maintenance"},
         Proj#eflex_activity_type{name       = "OTP development"},
         Proj#eflex_activity_type{name       = "SIS"}
        ],
    Holidays =
        [
         #eflex_holiday{day = 1, month = 1},
         #eflex_holiday{day = 1, month = 5},
         #eflex_holiday{day = 6, month = 6},
         #eflex_holiday{day = 24,month = 12},
         #eflex_holiday{day = 25,month = 12},
         #eflex_holiday{day = 26,month = 12},
         #eflex_holiday{day = 31,month = 12}
        ],
    {AbsName, ActivityTypes2} =
	case O#options.absence_all_day of
	    undefined ->
		{undefined, ActivityTypes};
	    A ->
		{A,
		 [Att#eflex_activity_type{name = A} |
		  ActivityTypes]}
	end,
    {UnspecName, ActivityTypes3} =
	case O#options.unspecified_work of
	    undefined ->
		{undefined, ActivityTypes2};
	    U ->
		{U, [Proj#eflex_activity_type{name = U} |
		     ActivityTypes2]}
	end,
    ActivityTypes4 = sort_activity_types(ActivityTypes3),
    #eflex_config{unit             = minutes,
		  working_time     = WorkingTime,
                  lunch_duration   = LunchDuration,
                  workday_break    = WorkdayBreak,
                  freeday_break    = FreedayBreak,
                  activity_types   = ActivityTypes4,
                  holidays         = Holidays,
		  absence_all_day  = AbsName,
		  unspecified_work = UnspecName,
		  mini_break       = 0}.

sort_activity_types(ActivityTypes) ->
    {[Unspec | Mandatory], Attendance, Project} =
        multi_split_activity_types(ActivityTypes),
    Pos = #eflex_activity_type.name,
    Mandatory ++ lists:keysort(Pos, Attendance) ++
        [Unspec] ++ lists:keysort(Pos, Project).

make_initial_weeks(YearNo, Econfig) when is_integer(YearNo) ->
    A = #eflex_activity{year_sum  = 0,
                        week_sum  = 0,
                        monday    = undefined,
                        tuesday   = undefined,
                        wednesday = undefined,
                        thursday  = undefined,
                        friday    = undefined,
                        saturday  = undefined,
                        sunday    = undefined},
    MakeWeek =
        fun(WeekNo) ->
                MondayDate = monday_of_the_week({YearNo, WeekNo}),
                MondayDayNo = calendar:date_to_gregorian_days(MondayDate),
                Eactivities =
                    [
                     A#eflex_activity{name      = ?DATE_ACT,
                                      year_sum  = YearNo,
                                      week_sum  = WeekNo,
                                      monday    = MondayDayNo + 0,
                                      tuesday   = MondayDayNo + 1,
                                      wednesday = MondayDayNo + 2,
                                      thursday  = MondayDayNo + 3,
                                      friday    = MondayDayNo + 4,
                                      saturday  = MondayDayNo + 5,
                                      sunday    = MondayDayNo + 6},
                     A#eflex_activity{name = ?ARRIVED_ACT},
                     A#eflex_activity{name = ?LEFT_ACT},
                     A#eflex_activity{name = ?BREAK_ACT},
                     A#eflex_activity{name = ?ADJUST_ACT},
                     A#eflex_activity{name = ?WORK_ACT},
                     A#eflex_activity{name = ?FLEX_ACT},
                     A#eflex_activity{name = ?UNSPEC_ACT}
                    ],
		Eactivities2 = sort_activities(Eactivities, Econfig),
                #eflex_week{no = WeekNo, activities = Eactivities2}
        end,
    Eweeks = lists:map(MakeWeek, 
		       lists:seq(1, last_week_no_of_the_year(YearNo))),
    list_to_tuple(Eweeks).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Initialize files
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init_files(#options{dir = Dir,
                    read_only = true,
                    date = Date} = O) ->
    Eyear = read_year(Dir, Date),
    {O, Eyear};
init_files(#options{dir = Dir,
                    read_only = false,
                    date = Date,
                    arrived_file = ArrFile,
                    left_file = LeftFile} = O) ->
    NewEyears = eflex_flextool:init(O, false),
    ?SAFE(ensure_dir(Dir), Dir),
    [write_year(Dir, Y) || Y <- NewEyears],
    Eyear = ensure_year(Dir, Date, O),
    Now = calendar:now_to_local_time(erlang:now()),
    StrContents = "See file modification file time.",
    ArrMtime = safe_ensure_file(ArrFile, StrContents, Now),
    LeftMtime = safe_ensure_file(LeftFile, StrContents, ArrMtime),
    O2 = O#options{arrived_mtime = ArrMtime,
                   left_mtime = LeftMtime},
    incr_timestamp(O2, Eyear).

read_year(Dir, Date) ->
    YearFile = date_to_year_file(Dir, Date),
    case file:read_file(YearFile) of
        {ok, Bin} ->
            case catch eflex_xml:decode(binary_to_list(Bin)) of
                {'EXIT', Reason} = Error ->
                    error_logger:error_msg("~p: Cannot parse file ~p "
					   "because\n\t~p\n",
                                           [?MODULE, YearFile, Error]),
                    exit(Reason);
                Eyear when is_record(Eyear, eflex_year) ->
                    Eyear
            end;
        {error, Reason} = Error ->
            error_logger:error_msg("~p: Cannot read file ~p "
				   "because\n\t~p\n",
                                   [?MODULE, YearFile, Error]),
            exit(Reason)
    end.

ensure_year(Dir, Date, Options) ->
    YearFile = date_to_year_file(Dir, Date),
    case file:read_file(YearFile) of
        {ok, Bin} ->
            case catch eflex_xml:decode(binary_to_list(Bin)) of
                {'EXIT', Reason} = Error ->
                    {{Y, Mo, D}, {H, Mi, S}} =
                        calendar:now_to_local_time(erlang:now()),
                    SaveFile = lists:concat([YearFile, "_",
                                             Y, "_", Mo, "_", D, "_",
                                             H, "_", Mi, "_", S]),
                    error_logger:error_msg("~p: Renamed ~p to ~p "
					   "because\n\t~p\n",
                                           [?MODULE, YearFile,
					    SaveFile, Error]),
                    ?SAFE(file:rename(YearFile, SaveFile),
                          {file, rename, [YearFile, SaveFile]}),
                    copy_latest_year(Dir, Date, Options);
                Eyear when is_record(Eyear, eflex_year) ->
                    Eyear
            end;
        {error, enoent} ->
            copy_latest_year(Dir, Date, Options);
        {error, Reason} = Error ->
            error_logger:error_msg("~p: Cannot read file ~p "
				   "because\n\t~p\n",
                                   [?MODULE, YearFile, Error]),
            exit(Reason)
    end.

copy_latest_year(Dir, Date, Options) ->
    {YearNo, _} = week_of_the_year(Date),
    Fun = fun(File, Acc) -> [File | Acc] end,
    RegExp = ?FILE_PREFIX ++ ".*" ++ ?FILE_SUFFIX,
    AllFiles = filelib:fold_files(Dir, RegExp, false, Fun, []),
    Eyear = 
        case lists:reverse(lists:sort(AllFiles)) of
            [] ->
                make_initial_year(YearNo, Options);
            [LatestYearFile | _] ->
                {ok, Bin} = ?SAFE(file:read_file(LatestYearFile),
                                  {file, read_file, [LatestYearFile]}),
                LatestEyear = eflex_xml:decode(binary_to_list(Bin)),
                strip_year(YearNo, LatestEyear)
        end,
    write_year(Dir, Eyear),
    Eyear.

read_previous_config(#options{dir = Dir, date = Date}) ->
    {YearNo, _} = week_of_the_year(Date),
    case catch read_year(Dir, {YearNo - 1, 1, 1}) of
	{'EXIT', Reason} ->
	    {error, Reason};
	PrevEyear ->
	    MaxWeekNo = size(PrevEyear#eflex_year.weeks),
	    SumEyear = sum_activities(PrevEyear, MaxWeekNo),
	    {ok, SumEyear#eflex_year.config}
    end.

strip_year(YearNo, LatestEyear) ->
    MaxWeekNo = size(LatestEyear#eflex_year.weeks),
    LatestEyear2 = sum_activities(LatestEyear, MaxWeekNo),
    Econfig = LatestEyear2#eflex_year.config,
    Eweeks = make_initial_weeks(YearNo, Econfig),
    LatestEyear#eflex_year{no     = YearNo,
                           weeks  = Eweeks,
                           config = Econfig}.

write_year(Dir, Eyear) ->    
    Xml = eflex_xml:encode(Eyear),
    YearFile = date_to_year_file(Dir, Eyear#eflex_year.no),
    safe_write_to_file(YearFile, Xml).

incr_timestamp(#options{dir            = Dir,
                        %% date           = DisplayDate,
                        arrived_mtime  = {ArrDate, ArrTime},
                        left_mtime     = {LeftDate, LeftTime} = LeftMtime,
                        arrived_file   = ArrFile,
                        left_file      = LeftFile,
                        auto_increment = AutoIncr} = O,
               Eyear) ->
    CurrMtime = {CurrDate, CurrTime} = compute_now(AutoIncr),
    if
        ArrDate =:= CurrDate ->
            %% Same day
            Eyear2 = update_activity(Eyear, ?ARRIVED_ACT, 
				     ArrDate, ArrTime),
            Eyear3 = 
                if
                    CurrMtime < LeftMtime ->
                        ?SAFE(file:change_time(LeftFile, LeftMtime),
			      LeftFile),
                        update_activity(Eyear2, ?LEFT_ACT,
					LeftDate, LeftTime);
                    true ->
                        ?SAFE(file:change_time(LeftFile, CurrMtime),
			      LeftFile),
                        update_activity(Eyear2, ?LEFT_ACT,
					CurrDate, CurrTime)
                end,
            write_year(Dir, Eyear3),
            {O, Eyear3};
        true ->
            %% Yet another day
            {ArrYearNo,  _} = week_of_the_year(ArrDate),
            {CurrYearNo, _} = week_of_the_year(CurrDate),
            {_OldEyear, NewEyear} =
                if
                    ArrYearNo =:= CurrYearNo ->
                        Eyear2 = update_activity(Eyear, ?ARRIVED_ACT,
						 CurrDate, CurrTime),
                        Eyear3 = update_activity(Eyear2, ?LEFT_ACT,
						 CurrDate, CurrTime),
                        write_year(Dir, Eyear3),
                        {Eyear3, Eyear3};
                    true ->
                        %% Yet another year
                        Eyear2 = update_activity(Eyear, ?ARRIVED_ACT,
						 ArrDate, ArrTime),
                        Eyear3 = update_activity(Eyear2, ?LEFT_ACT,
						 LeftDate, LeftTime),
                        write_year(Dir, Eyear),
                        Eyear4 = strip_year(CurrYearNo, Eyear3),
                        Eyear5 = update_activity(Eyear4, ?ARRIVED_ACT,
						 CurrDate, CurrTime),
                        Eyear6 = update_activity(Eyear5, ?LEFT_ACT,
						 CurrDate, CurrTime),
                        write_year(Dir, Eyear6#eflex_year{no = CurrYearNo}),
                        {Eyear3, Eyear6}
                end,
            ?SAFE(file:change_time(ArrFile, CurrMtime), ArrFile),
            ?SAFE(file:change_time(LeftFile, CurrMtime), LeftFile),
            %% DisplayDate2 =
            %%     if
            %%         CurrDate =/= ArrDate ->
            %%             %% Switch display date
            %%             io:format("Switch display date: ~p\n",
	    %%                       [CurrDate]),
            %%             CurrDate;
            %%         true ->
            %%             DisplayDate
            %%     end,
            {O#options{%%date = DisplayDate2,
                       arrived_mtime = CurrMtime,
                       left_mtime = CurrMtime},
             NewEyear}
    end.

ensure_action(Name, Eactivities, Econfig) ->
    case lists:keysearch(Name, #eflex_activity.name, Eactivities) of
        false ->
            Act = #eflex_activity{name      = Name,
				  year_sum  = 0,
				  week_sum  = 0,
				  monday    = undefined,
				  tuesday   = undefined,
				  wednesday = undefined,
				  thursday  = undefined,
				  friday    = undefined,
				  saturday  = undefined,
				  sunday    = undefined},
	    patch_sign(Act, Econfig);
        {value, Act} ->
            Act
    end.

compute_now(AutoIncr) ->
    Now = calendar:now_to_local_time(erlang:now()),
    if
        AutoIncr =:= undefined ->
            Now;
        true ->
            DictKey = eflex_auto_increment,
            case get(DictKey) of
                undefined ->
                    put(DictKey, AutoIncr),
                    Now;
                OldAutoIncr->
                    {DateNow, T} = Now,
                    NewAutoIncr = OldAutoIncr + AutoIncr,
                    put(DictKey, NewAutoIncr),
                    Days = calendar:date_to_gregorian_days(DateNow),
		    Days2 = Days + NewAutoIncr,
                    {calendar:gregorian_days_to_date(Days2), T}
            end
    end.

make_activity_tuple(N, Eactivities) ->
    {Attendance, Project} = split_activities(Eactivities),
    AttendanceFun = 
	fun(A, {Pos, T}) ->
		{Pos + 1, setelement(Pos, T, A)}
	end,
    ProjectFun =
	fun(A, {Pos, T}) ->
		{Pos - 1, setelement(Pos, T, A)}
	end,
    Tuple  = erlang:make_tuple(N, undefined),
    {_, Tuple2} = lists:foldl(AttendanceFun,
			      {1, Tuple},
			      Attendance),
    {_, Tuple3} = lists:foldl(ProjectFun,
			      {N, Tuple2},
			      Project),
    Tuple3.

split_activities(Eactivities) ->
    Pred = fun(A) -> A#eflex_activity.name =/= ?UNSPEC_ACT end,
    lists:splitwith(Pred, Eactivities).

sort_activities(Eactivities,
		#eflex_config{activity_types = Types}) ->
    NamePos = #eflex_activity_type.name,
    Category =
        fun(#eflex_activity{name = Name}) ->
                T = ?KEYSEARCH(Name, NamePos, Types),
                case T#eflex_activity_type.category of
                    project    -> true;
                    attendance -> false
                end
        end,
    Mandatory = pick_mandatory_activities(Eactivities,
					  NamePos),
    Optional = Eactivities -- Mandatory,
    Project  = lists:filter(Category, Optional),
    Attendance = Optional -- Project,
    [Unspec | Mandatory2] = Mandatory,
    Pos = #eflex_activity.name,
    Mandatory2 ++ lists:keysort(Pos, Attendance) ++
	[Unspec] ++ 
	lists:reverse(lists:keysort(Pos, Project)).

pick_mandatory_activities(Eactivities, NamePos) ->
    [?KEYSEARCH(Name, NamePos, Eactivities) ||
        Name <- mandatory_activity_names()].

mandatory_activity_names() ->
    %% Significant order
    [
     ?UNSPEC_ACT,
     ?DATE_ACT,
     ?ARRIVED_ACT,
     ?LEFT_ACT,
     ?BREAK_ACT,
     ?ADJUST_ACT,
     ?WORK_ACT,
     ?FLEX_ACT
    ].

multi_split_activity_types(ActivityTypes) ->
    NamePos = #eflex_activity_type.name,
    Mandatory = pick_mandatory_activities(ActivityTypes,
					  NamePos),
    Optional = ActivityTypes -- Mandatory,
    CategoryFun =
        fun(#eflex_activity_type{name = Name}) ->
                T = ?KEYSEARCH(Name, NamePos, ActivityTypes),
                case T#eflex_activity_type.category of
                    project    -> true;
                    attendance -> false
                end
        end,
    Project  = lists:filter(CategoryFun, Optional),
    Attendance = Optional -- Project,
    {Mandatory, Attendance, Project}.

opt_update_activity(Val,
                    ActivityPos,
                    DayNo,
                    #options{date = Date} = Options,
                    #eflex_year{weeks = Eweeks,
				config = Econfig} = Eyear,
                    N) ->
    {_, WeekNo} = week_of_the_year(Date),
    Eweek = element(WeekNo, Eweeks),
    Eactivities = Eweek#eflex_week.activities,
    Tuple = make_activity_tuple(N, Eactivities),
    case element(ActivityPos, Tuple) of
        undefined ->
            {Options, Eyear};
        Eactivity ->
            Name = Eactivity#eflex_activity.name,
            {Eactivity2, OptInt} =
		update_changed_activity(Eactivity, 
					Val,
					Eyear,
					Name,
					DayNo),
            Eactivities2 =
		lists:keystore(Name,
			       #eflex_activity.name,
			       Eactivities,
			       Eactivity2),
            {YearNo, WeekNo} = week_of_the_year(Date),
            MondayDate = monday_of_the_week({YearNo, WeekNo}),
            MondayDayNo = calendar:date_to_gregorian_days(MondayDate),
            Eactivities3 =
		update_derived_activities(MondayDayNo,
					  Eactivities2,
					  Econfig),
            Eweek2 = Eweek#eflex_week{activities = Eactivities3},
            Eweeks2 = setelement(WeekNo, Eweeks, Eweek2),
            Eyear2 = Eyear#eflex_year{weeks = Eweeks2},
            ChangeDayNo = MondayDayNo + DayNo - 1,
            ChangeDate  = calendar:gregorian_days_to_date(ChangeDayNo),
            Options2 = 
		opt_change_timestamp(Name, OptInt, ChangeDate, Options),
            {Options2, Eyear2}
    end.

update_changed_activity(OldEactivity, Str, Eyear, Name, DayNo) ->
    Empty = "",
    Stripped = strip_time(Str),
    OptInt = 
        case Stripped =:= Empty of
            true ->
                undefined;
            false ->
                Converted = convert_activity_time(Stripped, Eyear, Name),
                convert_time(Converted, integer)
        end,
    FieldPos = #eflex_activity.monday + DayNo - 1,
    OldWeekSum = OldEactivity#eflex_activity.week_sum,
    NewEactivity = setelement(FieldPos, OldEactivity, OptInt),
    WeekSum = calc_week_sum(NewEactivity),
    YearSum = NewEactivity#eflex_activity.year_sum + WeekSum - OldWeekSum,
    Eactivity = NewEactivity#eflex_activity{year_sum = YearSum,
                                            week_sum = WeekSum},
    {Eactivity, OptInt}.

update_derived_activities(MondayDayNo, Eactivities, Econfig) ->
    Patch = fun(Act) -> patch_sign(Act, Econfig) end,
    Eactivities2 = lists:map(Patch, Eactivities),
    [_DateAct, ArrivedAct, LeftAct, BreakAct,
     AdjustAct, WorkAct, FlexAct | Extra] = Eactivities2,
    {Attendance, [UnspecAct | Project]} = split_activities(Extra),
    calc_derived(1, MondayDayNo, Econfig, Eactivities,
		 ArrivedAct, LeftAct, BreakAct, AdjustAct,
		 WorkAct, FlexAct, UnspecAct, Attendance, Project).

patch_sign(Act, Econfig) ->
    #eflex_activity_type{sign = Sign} =
        ?KEYSEARCH(Act#eflex_activity.name,
                   #eflex_activity_type.name,
                   Econfig#eflex_config.activity_types),
    Act#eflex_activity{sign = Sign}.    

calc_derived(WeekDayNo, MondayDayNo, Econfig, Eactivities,
	     ArrivedAct, LeftAct, BreakAct, AdjustAct,
	     WorkAct, FlexAct, UnspecAct, Attendance, Project)
  when WeekDayNo =< 7 ->
    #eflex_config{lunch_duration = OptLunchDuration,
                  workday_break  = WorkdayBreak,
                  freeday_break  = FreedayBreak} = Econfig,
    OptGetActElem =
        fun(Pos, Tuple) when is_record(Tuple, eflex_activity) ->
                case element(Pos, Tuple) of
                    undefined ->
                        0;
                    Int when is_integer(Int) ->
                        case element(#eflex_activity.sign, Tuple) of
                            positive -> Int;
                            negative -> 0 - Int
                        end
                end
        end,
    ExpectedWorkTime =
	expected_worktime(MondayDayNo, WeekDayNo, Econfig),
    {DayBreak, LunchDuration} =
        if
            ExpectedWorkTime > 0 -> 
		{WorkdayBreak, OptLunchDuration};
            true ->
		{FreedayBreak, FreedayBreak#eflex_break.duration}
        end,
    ActPos = #eflex_activity.monday + WeekDayNo - 1,
    Arrived = OptGetActElem(ActPos, ArrivedAct),
    Left = OptGetActElem(ActPos, LeftAct),
    BrutWorkTime = Left - Arrived,
    BreakDuration = DayBreak#eflex_break.duration,
    TimeBeforeBreak = DayBreak#eflex_break.time_before_break,
    TimeAfterFirstBreak = 
	lists:max([0, BrutWorkTime - TimeBeforeBreak - LunchDuration]),
    LunchBreak =
        if
            TimeAfterFirstBreak > 0 ->
                LunchDuration; 
            true -> 
                0
        end,
    Adjust = OptGetActElem(ActPos, AdjustAct),
    PosWorkTime = lists:max([0, BrutWorkTime - LunchBreak]),
    BrutOvertime = 
	lists:max([0, (PosWorkTime + Adjust) - ExpectedWorkTime]),
    Nbreaks = BrutOvertime div (TimeBeforeBreak + BreakDuration),
    TotalBreak = LunchBreak + (BreakDuration * Nbreaks),
    NetWorkTime = BrutWorkTime - TotalBreak + Adjust,
    AttendanceSum = 
	lists:sum([OptGetActElem(ActPos, A) || A <- Attendance]),
    ProjectSum = 
	lists:sum([OptGetActElem(ActPos, A) || A <- Project]),
    Flex = NetWorkTime - ExpectedWorkTime + AttendanceSum,
    Unspec = NetWorkTime - ProjectSum,

    %% io:format("\n", []),
    %% io:format("WeekDayNo:        ~p\n", [WeekDayNo]),
    %% io:format("ExpectedWorkTime: ~p\n", [ExpectedWorkTime]),
    %% io:format("WorkdayBreak      ~p\n", [WorkdayBreak]),
    %% io:format("FreedayBreak      ~p\n", [FreedayBreak]),
    %% io:format("DayBreak          ~p\n", [DayBreak]),
    %% io:format("LunchDuration     ~p\n", [LunchDuration]),
    %% io:format("BrutWorkTime      ~p\n", [BrutWorkTime]),
    %% io:format("BreakDuration:    ~p\n", [BreakDuration]),
    %% io:format("TimeBeforeBreak:  ~p\n", [TimeBeforeBreak]),
    %% io:format("Nbreaks:          ~p\n", [Nbreaks]),
    %% io:format("NetWorkTime:      ~p\n", [NetWorkTime]),
    %% io:format("LunchBreak:       ~p\n", [LunchBreak]),
    %% io:format("TotalBreak        ~p\n", [TotalBreak]),
    %% io:format("Adjust            ~p\n", [Adjust]),
    %% io:format("NetWorkTime:      ~p\n", [NetWorkTime]),
    %% io:format("AttendanceSum        ~p\n", [AttendanceSum]),
    %% io:format("ProjectSum         ~p\n", [ProjectSum]),
    %% io:format("Flex:             ~p\n", [Flex]),
    %% io:format("Unspec:           ~p\n", [Unspec]),

    OptSetElem =
        fun(Pos, Tuple, Val) ->
                case Val of
                    0    -> setelement(Pos, Tuple, undefined);
                    Int  -> setelement(Pos, Tuple, Int)
                end
        end,
    BreakAct2   = OptSetElem(ActPos, BreakAct, TotalBreak),
    WorkAct2    = OptSetElem(ActPos, WorkAct, NetWorkTime),
    FlexAct2    = OptSetElem(ActPos, FlexAct, Flex),
    UnspecAct2  = OptSetElem(ActPos, UnspecAct, Unspec),
    calc_derived(WeekDayNo + 1, MondayDayNo, Econfig, Eactivities,
		 ArrivedAct, LeftAct, BreakAct2, AdjustAct,
		 WorkAct2, FlexAct2, UnspecAct2, Attendance, Project);
calc_derived(_WeekDayNo, _MondayDayNo, _Econfig, Eactivities,
	     ArrivedAct, LeftAct, BreakAct, _AdjustAct,
	     WorkAct, FlexAct, UnspecAct, _Attendance, _Project) ->
    Replace =
        fun(Eactivity, ActivitiesAcc) ->
                lists:keystore(Eactivity#eflex_activity.name,
                         #eflex_activity.name,
                         ActivitiesAcc, Eactivity)
        end,
    lists:foldl(Replace, 
		Eactivities,
		[ArrivedAct, LeftAct, BreakAct,
		 WorkAct, FlexAct, UnspecAct]).

expected_worktime(MondayDayNo, WeekDayNo, Econfig) ->
    case is_holiday(MondayDayNo, WeekDayNo, Econfig) of
        true  -> 0;
        false -> lookup_worktime(WeekDayNo, Econfig)
    end.

is_holiday(MondayDayNo, 
	   WeekDayNo,
	   #eflex_config{holidays = Holidays}) ->
    {_YearNo, MonthNo, DayNo} =
	calendar:gregorian_days_to_date(MondayDayNo + WeekDayNo - 1),
    [H || #eflex_holiday{day = D, month = M} = H <- Holidays,
          D =:= DayNo,
          M =:= MonthNo] =/= [].

lookup_worktime(WeekDayNo, #eflex_config{working_time = WorkTime})
  when WeekDayNo >= 1, WeekDayNo =< size(WorkTime) ->
    Pos = #eflex_working_time.monday + WeekDayNo - 1,
    case element(Pos, WorkTime) of
        undefined -> 0;
        Int when is_integer(Int) -> Int
    end.

update_activity(Eyear, Name, Date, {H, M, _S}) ->
    Minutes = (H * 60) + M,
    {_YearNo, WeekNo} = week_of_the_year(Date),
    Eweeks = Eyear#eflex_year.weeks,
    Eweek = element(WeekNo, Eweeks),
    Eactivities = Eweek#eflex_week.activities,
    ActPos = #eflex_activity.name,
    Eactivity = ?KEYSEARCH(Name, ActPos, Eactivities),
    WeekDayNo = calendar:day_of_the_week(Date), % 1..7
    DayPos = #eflex_activity.monday + WeekDayNo - 1,
    Eactivity2 = setelement(DayPos, Eactivity, Minutes),
    Eactivities2 = lists:keystore(Name, ActPos, Eactivities, Eactivity2),
    Eweek2 = Eweek#eflex_week{activities = Eactivities2},
    Eweeks2 = setelement(WeekNo, Eweeks, Eweek2),
    Eyear#eflex_year{weeks = Eweeks2}.

date_to_year_file(Dir, YearNo) when is_integer(YearNo) ->
    Name = lists:concat([?FILE_PREFIX, YearNo, ?FILE_SUFFIX]),
    filename:join([Dir, Name]);
date_to_year_file(Dir, {_, _, _} = Date) ->
    {YearNo, _WeekNo} = week_of_the_year(Date),
    date_to_year_file(Dir, YearNo).

safe_ensure_file(File, IoList, Mtime) ->
    case file:read_file_info(File) of
        {ok, _} ->
            filelib:last_modified(File);
        {error, enoent} ->
            safe_write_to_file(File, IoList),
            ?SAFE(file:change_time(File, Mtime), File),
            Mtime;
        {error, Reason} ->
            exit({Reason, File})
    end.

safe_write_to_file(File, IoList) ->
    TmpFile = lists:concat([File, ".tmp"]),
    ?SAFE(file:write_file(TmpFile, IoList), TmpFile),
    ?SAFE(file:rename(TmpFile, File), {TmpFile, File}).

ensure_dir(Dir) ->
    case filelib:is_dir(Dir) of
        true ->
            ok;
        false ->
            ensure_dir(filename:dirname(Dir)),
            file:make_dir(Dir)
    end.

opt_change_timestamp(Name, 
		     ChangeTime,
		     ChangeDate, 
		     #options{arrived_mtime = {ArrDate, _},
			      arrived_file  = ArrFile} = O)
  when Name =:= ?ARRIVED_ACT,
       ChangeDate =:= ArrDate,
       ChangeTime =/= undefined ->
    Hours = ChangeTime div 60,
    Minutes = ChangeTime rem 60,
    ChangeMtime = {ChangeDate, {Hours, Minutes, 0}},
    ?SAFE(file:change_time(ArrFile, ChangeMtime), ArrFile),
    O#options{arrived_mtime = ChangeMtime};
opt_change_timestamp(Name,
		     ChangeTime,
		     ChangeDate,
		     #options{left_mtime = {LeftDate, _},
			      left_file  = LeftFile} = O)
  when Name =:= ?LEFT_ACT,
       ChangeDate =:= LeftDate,
       ChangeTime =/= undefined ->
    Hours = ChangeTime div 60,
    Minutes = ChangeTime rem 60,
    ChangeMtime = {ChangeDate, {Hours, Minutes, 0}},
    ?SAFE(file:change_time(LeftFile, ChangeMtime), LeftFile),
    O#options{left_mtime = ChangeMtime};
opt_change_timestamp(_Name,
		     _ChangeTime,
		     _ChangeDate,
		     #options{} = O) ->
    O.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Calculations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sum_activities(Eyear, MaxWeekNo) ->
    Econfig = Eyear#eflex_year.config,
    EweekList = tuple_to_list(Eyear#eflex_year.weeks),
    Pred = fun(#eflex_week{no = WeekNo}) -> WeekNo =< MaxWeekNo end,
    EweekList2 =  lists:takewhile(Pred, EweekList),
    Etypes = Econfig#eflex_config.activity_types,
    Tab = ets:new(eflex_activity_type, 
		  [set, {keypos, #eflex_activity_type.name}]),
    ActInsert = fun(Eactivity_type) -> ets:insert(Tab, Eactivity_type) end, 
    lists:foreach(ActInsert, Etypes),
    do_sum_activities(Tab, EweekList2),
    ActLookup = fun(#eflex_activity_type{name = Name}) ->
                        hd(ets:lookup(Tab, Name)) 
                end,
    Etypes2 = lists:map(ActLookup, Etypes),
    Econfig2 = Econfig#eflex_config{activity_types = Etypes2},
    Eyear2 = Eyear#eflex_year{config = Econfig2},
    ets:delete(Tab),
    Eyear2.

do_sum_activities(Tab, Eweeks) ->
    Pos = #eflex_activity_type.initial,
    SumActivity =
        fun(#eflex_activity{name = Name} = Eactivity) ->
           Sum = calc_week_sum(Eactivity),
           ets:update_counter(Tab, Name, {Pos, Sum})
        end,
    SumWeek =
        fun(#eflex_week{activities = Eactivities}) ->
                lists:foreach(SumActivity, Eactivities)
        end,
    lists:foreach(SumWeek, Eweeks).

calc_week_sum(#eflex_activity{monday    = Mon,
                              tuesday   = Tue,
                              wednesday = Wed,
                              thursday  = Thu,
                              friday    = Fri,
                              saturday  = Sat,
                              sunday    = Sun}) ->
    Add = fun(OptInt, Acc) ->
                  case OptInt of
                      undefined -> Acc;
                      Int       -> Acc + Int
                  end
          end,
    lists:foldl(Add, 0, [Mon, Tue, Wed, Thu, Fri, Sat, Sun]).

convert_time(Date, ToUnit) ->
    convert_time(Date, ToUnit, 0).
    
convert_time(Date, date, _Default) when is_list(Date) ->
    Date;
convert_time(Time, ToUnit, _Default) when is_integer(Time) ->
    AbsTime = abs(Time),
    case ToUnit of
        integer ->
            Time;
        minutes ->
            Hours = integer_to_list(AbsTime div 60),
            Minutes = integer_to_list(AbsTime rem 60),
            opt_negate(Time, Hours ++ ":" ++ 
		       string:right(Minutes, 2, $0));
        decimal ->
            Hours = integer_to_list(AbsTime div 60),
            Decimal =
		integer_to_list(round(AbsTime rem 60 * 100 / 60)),
            opt_negate(Time, Hours ++ "," ++
		       string:right(Decimal, 2, $0));
        date ->
            {_Year, Month, Day} = 
		calendar:gregorian_days_to_date(AbsTime),
            integer_to_list(Day) ++ "/" ++ integer_to_list(Month)
    end;
convert_time(Time, ToUnit, Default) when is_list(Time) ->
    Time2 = strip_time(Time),
    case string:tokens(Time2, ",:") of
        [Hours, Minutes] ->
            Hours2 = abs(list_to_integer(Hours)),
            Minutes3 =
                case lists:member($:, Time) of
                    true  ->
                        abs(list_to_integer(Minutes));
                    false ->
                        Decimal = list_to_float("0." ++ Minutes),
                        abs(round(Decimal * 60))
                end,
            convert_time(opt_negate(Hours, (Hours2 * 60) + Minutes3), 
			 ToUnit,
			 Default);
        [Hours] ->
            Hours2 = abs(list_to_integer(Hours)),
            convert_time(opt_negate(Hours, Hours2 * 60), 
			 ToUnit,
			 Default);
        _ when Time =/= Default -> % Assert
            %% Bad type. Set to default.
            convert_time(Default, ToUnit, Default)
    end.

opt_negate(List, Int) when is_list(List), is_integer(Int) ->
    case List of
        [$- | _] -> 0 - abs(Int);
        _        -> abs(Int)
    end;
opt_negate(Int, List) when is_integer(Int), is_list(List) ->
    case Int < 0 of
        true  -> [$- | List];
        false -> List
    end.

convert_activity_time(Time, #eflex_year{config = Econfig}, Name) ->
    Etypes = Econfig#eflex_config.activity_types,
    #eflex_activity_type{unit = Unit} =
        ?KEYSEARCH(Name, #eflex_activity_type.name, Etypes),
    convert_time(Time, Unit).

strip_time([$- | Time]) ->
    [$- | strip_time(Time)];
strip_time(Time) when is_list(Time) ->
    AllowedChars = ",:0123456789",
    [Char || Char <- Time, lists:member(Char, AllowedChars)].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

unify_shares(#eflex_year{config = Econfig} = Eyear, Undef, Share) ->
    case Econfig of
	#eflex_config{unspecified_work = Name,
		      activity_types = Etypes}
	when Name =/= Undef ->
	    Filter = fun(A) ->
			     case A#eflex_activity_type.name =:= Name of
				 true  -> A#eflex_activity_type{share = Share};
				 false -> A#eflex_activity_type{share = 0}
			     end
		     end,
	    Etypes2 = lists:map(Filter, Etypes),
	    Econfig2 = Econfig#eflex_config{activity_types = Etypes2},
	    Eyear#eflex_year{config = Econfig2};
	#eflex_config{} ->
	    Eyear
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list_years(#options{dir = Dir}) ->
    Fun = 
        fun(File, Acc) ->
                Base = filename:basename(File),
		case string:tokens(Base, "_.") of
		    [_, YearStr, _] ->
			[list_to_integer(YearStr) | Acc];
		    _ ->
			Acc
		end
        end,
    RegExp = ?FILE_PREFIX ++ ".*" ++ ?FILE_SUFFIX,
    lists:sort(filelib:fold_files(Dir, RegExp, false, Fun, [])).

is_week_within_range({YearNo, WeekNo}, Options) ->
    Years = list_years(Options),
    [FirstYearNo | _] = Years,
    FirstYearAndWeekNo = week_of_the_year({FirstYearNo, 1, 1}),
    LastYearNo = lists:last(Years),
    LastWeekNo = last_week_no_of_the_year(LastYearNo),
    ({YearNo, WeekNo} >= FirstYearAndWeekNo) andalso
    ({YearNo, WeekNo} =< {LastYearNo, LastWeekNo}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% week_of_the_year(Date) -> {Year, Week}
%%
%% Date   = {Year, Month, Day}
%% Year  = int()
%% Month = 1..12
%% Day   = 1..31
%% Week  = 1..53
%%
%% This function computes the week number of a given day
%% according to the ISO8601 standard.
%%
%% In ISO8601 the week number is defined by these rules:
%% - weeks start on a monday
%% - week 1 of a given year is the one that includes the
%%   4th of January. Or, equivalently, week 1 is the week
%%   that includes the first  Thursday of that year.
%% 
%% Normally the returned Year is the same as the given Year,
%% but the first and last days of a year can actually
%% belong to the previous respective next year. For example,
%% the day {2012, 1, 1} is included in week 52 in 2011 and
%% the day {2012, 12, 31} is included in week 1 in 2012.
%% The day {2009, 12, 28} is included in week 53 in 2009.
%%
%% The ISO860 standard is used by most European countries.
%% There are also several other week numbering systems that
%% are used in various parts of the world. Some have monday
%% the first day of the week while others have wednesday,
%% saturday or sunday. Some defines the first week of the
%% year as the week containing the 1st of january, while
%% others uses the 4th or 7th.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

week_of_the_year({Year, _Month, _Day} = Date) ->
    MondayInFirstWeek = monday_of_the_first_week(Year),
    ActualDay = calendar:date_to_gregorian_days(Date),
    Diff = ActualDay - MondayInFirstWeek,
    Week = (Diff div 7) + 1,
    if
        Diff < 0 ->
            %% The day belongs to the last week of the previous year
            week_of_the_year({Year - 1, 12, 31});
        Week > 52 ->
            MondayInFirstWeek2 = monday_of_the_first_week(Year + 1),
            if
                ActualDay >= MondayInFirstWeek2 ->
                    {Year + 1, 1};
                true ->
                    {Year, Week}
            end;
        true ->
            {Year, Week}
    end.

monday_of_the_first_week(Year) ->
    PivotDate = {Year, 1, 4},
    PivotDay = calendar:date_to_gregorian_days(PivotDate),
    PivotDay + 1 - calendar:day_of_the_week(PivotDate).

monday_of_the_week({Year, Week}) when is_integer(Year), is_integer(Week) ->
    MondayInFirstWeek = monday_of_the_first_week(Year),
    calendar:gregorian_days_to_date((MondayInFirstWeek + ((Week - 1) * 7))).

last_week_no_of_the_year(YearNo) ->
    case week_of_the_year({YearNo, 12, 31}) of
        {Y, WeekNo} when Y =:= YearNo ->
            WeekNo;
        {_, _} ->
            {_, WeekNo} = week_of_the_year({YearNo, 12, 31 - 7}),
            WeekNo
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

safe_keysearch(Key, Pos, List, Mod, Line) ->
    case lists:keysearch(Key, Pos, List) of
        false ->
            io:format("~p(~p): lists:keysearch(~p, ~p, ~p) -> false\n",
                      [Mod, Line, Key, Pos, List]),
            erlang:error({Mod, Line, lists, keysearch, [Key, Pos, List]});
        {value, Val} ->
            Val
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lock(#options{lock_file = File, lock_cookie = Cookie}) ->
    case file:read_file(File) of
        {error, enoent} ->
	    case filelib:is_dir(filename:dirname(File)) of
		true  -> safe_write_to_file(File, Cookie);
		false -> ok
	    end;
        {ok, StoredCookie} when StoredCookie =:= Cookie ->
            ok;
        _ ->
            {error, "Lock file exists: " ++ File}
    end.

unlock(#options{lock_file = File}) ->
    file:delete(File),
    case filelib:is_file(File) of
        false -> ok;
        true  -> exit("Lock file exists: " ++ File)
    end.
