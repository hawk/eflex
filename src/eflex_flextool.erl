%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2009 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(eflex_flextool).
-compile([export_all]).

-include("eflex.hrl").

-record(day, {year,
              month,
              day,
              arrived_left,
              auto_codes = [],
              user_codes = [],
              derived_flex = 0}).

-record(props, {year_no,
		lunch_duration,
		freeday_break_after,
		freeday_break_duration,
		workday_break_after,
		workday_break_duration,
		early_overtime_ends,
		late_overtime_starts}).

init(Options, Force) ->
    case Force of
        false ->
            case filelib:is_dir(Options#options.dir) of
                true  -> [];
                false -> init(Options, true)
            end;
        true ->
            Dir =
                case os:getenv("HOME") of
                    false -> {ok, Cwd} = file:get_cwd(), Cwd;
                    Home  -> filename:join([Home, ".FlexTool"])         
                end,
            io:format("Initiate ~p from ~p...\n", [Options#options.dir, Dir]),
            {Days, Props} = read_dir(Dir),
	    io:format("Props: ~p\n", [Props]),
	    case Days of
                [#day{year = YearNo} | _] ->
                    Eyear = eflex_lib:make_initial_year(YearNo, Options),
		    Econfig = Eyear#eflex_year.config,
                    Default = Econfig#eflex_config.holidays,
                    SystemDates =
                        [{all, M, D} ||
                            #eflex_holiday{month = M, day = D} <- Default],
		    io:format("System holidays: ~p\n", [SystemDates]),
                    UserDates = read_holidays(Dir),
		    io:format("User holidays: ~p\n", [UserDates]),
		    HolidayDates = lists:usort(SystemDates ++ UserDates),
		    Eyear2 = init_config(Eyear, HolidayDates, Props),
                    init_years(Days, [Eyear2], HolidayDates, Props);
                [] ->
                    []
            end
    end.

init_years([#day{year = YearNo, month = MonthNo, day = DayNo} = Day| Days],
	   [Eyear | Eyears], HolidayDates, Props) ->
    Date = {YearNo, MonthNo, DayNo},
    %% io:format("Init ~p\n", [Date]),
    {NewYearNo, WeekNo} = eflex_lib:week_of_the_year(Date),
    if
        NewYearNo =:= Eyear#eflex_year.no ->
            Eyear2 = init_day(Day, WeekNo, Eyear),
            init_years(Days, [Eyear2 | Eyears], HolidayDates, Props);
        true ->
            NewEyear  = eflex_lib:strip_year(NewYearNo, Eyear),
	    NewEyear2  = init_config(NewEyear, HolidayDates, Props),
            NewEyear3 = init_day(Day, WeekNo, NewEyear2),
            init_years(Days, [NewEyear3, Eyear | Eyears], HolidayDates, Props)
    end;
init_years([], Eyears, _HolidayDates, _Props) ->
    lists:reverse(Eyears).

init_config(#eflex_year{no = YearNo, config = Econfig} = Eyear,
	    HolidayDates, Props) ->
    Econfig2 = init_props(Props, Econfig, YearNo),
    Holidays = dates_to_holidays(HolidayDates, YearNo),
    Econfig3= Econfig2#eflex_config{holidays = Holidays},
    Eyear#eflex_year{config = Econfig3}.

init_props(Props, Econfig, YearNo) ->
    Pick =
	fun(#props{year_no = PropsNo} = P, _Acc)
	   when PropsNo =< YearNo ->
		P;
	   (_, Acc) ->
		Acc
	end,
    #props{lunch_duration = NewLunchDur,
	   freeday_break_after = NewFreeAfter,
	   freeday_break_duration = NewFreeDur,
	   workday_break_after = NewWorkAfter,
	   workday_break_duration = NewWorkDur} =
	lists:foldl(Pick, undefined, Props),
    #eflex_config{lunch_duration = OldLunchDur,
		  freeday_break = OldFree,
		  workday_break = OldWork} = Econfig,
    #eflex_break{duration = OldFreeDur, 
		 time_before_break = OldFreeAfter} = OldFree,
    #eflex_break{duration = OldWorkDur, 
		 time_before_break = OldWorkAfter} = OldWork,
    FreeDur = opt_val(NewFreeDur, OldFreeDur),
    FreeAfter = opt_val(NewFreeAfter, OldFreeAfter),
    WorkDur = opt_val(NewWorkDur, OldWorkDur), 
    WorkAfter = opt_val(NewWorkAfter, OldWorkAfter),
    LunchDur = opt_val(NewLunchDur, OldLunchDur),
    Free = #eflex_break{duration = FreeDur,
			time_before_break = FreeAfter},
    Work = #eflex_break{duration = WorkDur,
			time_before_break = WorkAfter},
    Econfig#eflex_config{lunch_duration = LunchDur, 
			 freeday_break = Free,
			 workday_break = Work}.			 

opt_val(Val, Default) ->
    case Val of
	undefined -> Default;
	_         -> Val
    end.
	    
init_day(#day{year         = YearNo,
              month        = MonthNo,
              day          = DayNo,
              arrived_left = ArrivedLeftStr,
              user_codes   = UserCodesStr,
              auto_codes   = AutoCodesStr,
              derived_flex = FlexStr},
         WeekNo,
         #eflex_year{weeks = Eweeks,
                     config = Econfig} = Eyear) ->
    Date = {YearNo, MonthNo, DayNo},
    %% io:format("DATE  ~p\n", [Date]),
    if 
	AutoCodesStr =/= [], UserCodesStr =/= [] ->
            io:format("~p\tError Double codes\n",  [Date]); 
	true ->
	    ok
    end,
    MondayDate = eflex_lib:monday_of_the_week(eflex_lib:week_of_the_year(Date)),
    MondayDayNo = calendar:date_to_gregorian_days(MondayDate),
    WeekDayNo = calendar:day_of_the_week(Date),
    DayPos = #eflex_activity.monday + WeekDayNo - 1,
    NamePos = #eflex_activity.name,
    Update =
        fun({ActName, NewVal}, Acc) ->
		update(ActName, NewVal, Acc, Date, NamePos, DayPos, Econfig)
	end,
    UserActTimes = pick_codes(UserCodesStr, Date, MondayDayNo, WeekDayNo, Econfig),
    AutoActTimes = pick_codes(AutoCodesStr, Date, MondayDayNo, WeekDayNo, Econfig),
    MandActTimes = pick_arr_left(ArrivedLeftStr, Date),
    AllActTimes = UserActTimes ++ AutoActTimes ++ MandActTimes,
    %% io:format("INPUT ~p\n", [AllActTimes]),
    Eweek = element(WeekNo, Eweeks),
    Eactivities = Eweek#eflex_week.activities,
    Eactivities2 = lists:foldl(Update, Eactivities, AllActTimes),
    %% io:format("FOLDL ~p\n", [Eactivities2]),
    Eactivities3 = eflex_lib:update_derived_activities(MondayDayNo,
						       Eactivities2, 
						       Econfig),
    %% io:format("DERIV ~p\n", [Eactivities3]),
    Eactivities4 = update_adjust(FlexStr, Eactivities3, Update, NamePos, DayPos,
				MondayDayNo, Econfig),
    %% io:format("ADJUS ~p\n", [Eactivities4]),
    Eactivities5 = absence_all_day(Eactivities4, Update, NamePos, DayPos,
				   Date, MondayDayNo, WeekDayNo, Econfig),
    %% io:format("ADJUS ~p\n", [Eactivities5]),
    Eactivities6 = unspecified_work(Eactivities5, Update, NamePos, DayPos,
				    Date, MondayDayNo, Econfig),
    %% io:format("UNSPEC ~p\n", [Eactivities6]),
    Eweek2 = Eweek#eflex_week{activities = Eactivities6},
    Eweeks2 = setelement(WeekNo, Eweeks, Eweek2),
    Eyear#eflex_year{weeks = Eweeks2}.

update(?ADJUST_ACT, 0, Acc, _Date, _NamePos, _DayPos, _Econfig) ->
    Acc;
update(ActName, NewVal, Acc, Date, NamePos, DayPos, Econfig) ->
    Act = eflex_lib:ensure_action(ActName, Acc, Econfig),
    case element(DayPos, Act) of
	%% OldVal when is_integer(OldVal), OldVal =/= NewVal ->
	OldVal when OldVal =/= NewVal ->
	    if
		%% ActName =:= ?ARRIVED_ACT ->
		%%     ok;
		%% ActName =:= ?LEFT_ACT ->
		%%     ok;
		%% ActName =:= ?VACATION_ACT ->
		%%     ok;
		%% ActName =:= ?ADJUST_ACT, NewVal =:= 0 ->
		%%     ok;
		OldVal =:=  undefined ->
		    io:format("~p\tSet ~s\t to   ~p\n",
			      [Date, ActName, NewVal]);
		is_integer(OldVal) ->
		    io:format("~p\tInc ~s\t with ~p\n",
			      [Date, ActName, NewVal])
	    end,
	    Act2 = setelement(DayPos, Act, NewVal),
	    Acc2 = lists:keystore(ActName, NamePos, Acc, Act2),
	    eflex_lib:sort_activities(Acc2, Econfig);
	_ ->
	    Acc
    end.

update_adjust(FlexStr, Eactivities, Update, NamePos, DayPos, MondayDayNo, Econfig) ->
    case catch list_to_integer(FlexStr) of
        {'EXIT', _} ->
            Eactivities;
        FlexInt ->
            FlexAct = ?KEYSEARCH(?FLEX_ACT, NamePos, Eactivities),
            OldFlex = element(DayPos, FlexAct),
            if
                OldFlex =:= undefined ->
                    Eactivities;
                OldFlex =:= FlexInt ->
                    Eactivities;
                true ->
                    AdjustAct = ?KEYSEARCH(?ADJUST_ACT, NamePos, Eactivities),
                    Diff = FlexInt - OldFlex,
		    AdjustTime = 
			case element(DayPos, AdjustAct) of
			    undefined -> Diff;
			    Tmp -> Tmp + Diff
			end,
                    Eactivities2 = Update({?ADJUST_ACT, AdjustTime}, Eactivities),
		    eflex_lib:update_derived_activities(MondayDayNo,
							Eactivities2,
							Econfig)
	    end
    end.

absence_all_day(Eactivities, Update, NamePos, DayPos,
		Date, MondayDayNo, WeekDayNo, Econfig) ->
    FlexAct = ?KEYSEARCH(?FLEX_ACT, NamePos, Eactivities),
    Flex = element(DayPos, FlexAct),
    ExpectedWorkTime =
	eflex_lib:expected_worktime(MondayDayNo,
				    WeekDayNo,
				    Econfig),
    AbsName = Econfig#eflex_config.absence_all_day,
    if
	Flex =:= -ExpectedWorkTime, AbsName =/= undefined ->
	    io:format("~p\tAbs ~s\t with ~p\n",
		      [Date, AbsName, ExpectedWorkTime]),
	    Eactivities2 = Update({AbsName, ExpectedWorkTime}, Eactivities),
	    eflex_lib:update_derived_activities(MondayDayNo,
						Eactivities2,
						Econfig);
	true ->
	    Eactivities
    end.

unspecified_work(Eactivities, Update, NamePos, DayPos,
		 Date,MondayDayNo, Econfig) ->
    UnspecAct = ?KEYSEARCH(?UNSPEC_ACT, NamePos, Eactivities),
    WorkName = Econfig#eflex_config.unspecified_work,
    Unspec = element(DayPos, UnspecAct),
    if
	Unspec =/= undefined, WorkName =/= undefined ->
	    WorkAct = eflex_lib:ensure_action(WorkName, Eactivities, Econfig),
	    Work =
		case element(DayPos, WorkAct) of
		    undefined -> Unspec;
		    Diff      -> Unspec + Diff
		end, 
	    io:format("~p\tUns ~s\t with ~p\n",
		      [Date, WorkName, Unspec]),
	    Eactivities2 = Update({WorkName, Work}, Eactivities),
	    eflex_lib:update_derived_activities(MondayDayNo,
						Eactivities2,
						Econfig);
	true ->
	    Eactivities
    end.

pick_arr_left(undefined, _Date) ->
    [{?ARRIVED_ACT, undefined}, {?LEFT_ACT, undefined}, {?ADJUST_ACT, undefined}];
pick_arr_left(ArrivedLeftStr, Date) ->
    Times = string:tokens(ArrivedLeftStr, "-,"),
    [ArrTime | Times2] = pick_time(Times, undefined),
    [LeftTime | RevTimes3] = pick_time(lists:reverse(Times2), ArrTime),
    AdjustTime = pick_adjust(Date, lists:reverse(RevTimes3), 0),
    [{?ARRIVED_ACT, ArrTime}, {?LEFT_ACT, LeftTime}, {?ADJUST_ACT, AdjustTime}].

pick_codes(CodeStr, Date, MondayDayNo, WeekDayNo, Econfig) ->
    Pick = fun(CodeTime, Acc) ->
                   {Code2, Time3} =
                       case string:tokens(CodeTime, "=") of
                           [Code, Time] ->
                               [Time2] = pick_time([Time], 0),
                               {Code, Time2};
                           [Code] ->
                               ExpectedWorkTime =
                                   eflex_lib:expected_worktime(MondayDayNo,
                                                               WeekDayNo,
                                                               Econfig),
                               {Code, ExpectedWorkTime}
                       end,
		   case convert_code(Code2, Date) of
		       false ->
			   Acc;
		       ActName when is_list(ActName) -> 
			   [{ActName, Time3} | Acc]
		   end
           end,
    Codes = string:tokens(CodeStr, ","),
    lists:foldl(Pick, [], Codes).

convert_code(CodeStr, Date) ->
    case CodeStr of
	"01" -> ?ABSENCE_ACT;     % Hurt
        "02" -> ?ABSENCE_ACT;     % Illness
        "03" -> ?ABSENCE_ACT;     % Illness half day
        "04" -> ?VACATION_ACT;    % Vacation
        "05" -> ?PARENTAL_ACT;    % Parental leave
        "06" -> ?ABSENCE_ACT;     % Military service
        "07" -> ?ABSENCE_ACT;     % Other absence
        "08" -> ?ADJUST_ACT;      % Permission. Leave with pay.
        "09" -> ?ADJUST_ACT;      % Education with pay
        "10" -> ?ADJUST_ACT;      % Travel
        "11" -> ?PARENTAL_ACT;    % Parental leave
        "12" -> ?ABSENCE_ACT;     % Union work without pay
	"13" -> ?ABSENCE_ACT;     % Hurt part of day
	"18" -> ?PARENTAL_ACT;    % Parental leave
        "16" -> ?ABSENCE_ACT;     % Abroad work without pay
        "20" -> ?ADJUST_ACT;      % Union work with pay
        "21" -> ?ADJUST_ACT;      % Education without vacation pay
        "22" -> ?ADJUST_ACT;      % Education with pay
        "23" -> ?PARENTAL_ACT;    % Pregnancy
        "27" -> ?PARENTAL_ACT;    % Parental leave
        "28" -> ?ABSENCE_ACT;     % Military service
	"29" -> ?ABSENCE_ACT;     % Unknown absence
        "30" -> ?OVERTIME_ACT;    % Work on free day
        "32" -> ?OVERTIME_ACT;    % B-time for time
        "33" -> ?OVERTIME_ACT;    % C-time for time
        "36" -> ?OVERTIME_ACT;    % A-time 
        "41" -> ?OVERTIME_ACT;    % Time wage
        "42" -> ?OVERTIME_ACT;    % B-time for pay
        "43" -> ?OVERTIME_ACT;    % C-time for pay
        "45" -> false;            % Travel
        "46" -> false;            % Travel
        "50" -> ?ABSENCE_ACT;     % Absence on non-free day
        "52" -> ?ABSENCE_ACT;     % Absence B-time and C-time
        "53" -> ?ABSENCE_ACT;     % Absence I-time
        "56" -> ?ABSENCE_ACT;     % Absence A-time
        "80" -> false;            % Transfer of B-time or C-time to pay
        _    -> io:format("~p\t\Error unknown code: ~p -> skipped",
                          [Date, CodeStr]),
                false
    end.
            
pick_time([H | T], _Default) ->
    [eflex_lib:convert_time(H, integer) | T];
pick_time([], Default) ->
    [Default].

pick_adjust(Date, [ArrStr, LeftStr | T], Adjust) ->
    LeftTime = eflex_lib:convert_time(LeftStr, integer),
    ArrTime = eflex_lib:convert_time(ArrStr, integer),
    Diff = ArrTime - LeftTime,
    io:format("~p\tInc ~s\t with ~p\n", [Date, ?ABSENCE_ACT, Diff]),
    pick_adjust(Date, T, Adjust + Diff);
pick_adjust(Date, [Str], Adjust) ->
    io:format("~p\tError skipped: ~p\n", [Date, Str]),
    Adjust;
pick_adjust(_Date, [], Adjust) ->
    Adjust.

read_holidays(Dir) ->
    File = filename:join([Dir, "FlexTool.user"]),
    case file:read_file(File) of
        {ok, Bin} ->
            Simple = simplify(Bin),
            Pred = fun({Str, _}) -> Str =/= "HOLIDAYS" end,
            {_, [_ | Holidays]} = lists:splitwith(Pred, Simple),
            simplify_dates(Holidays);
        {error, Reason} ->
            io:format("Failed to load data from ~p: ~p\n", [File, Reason]),
            []
    end.

simplify_dates([{DateStr, _} | Tail]) ->
    case string:tokens(DateStr, "-") of
        [DayStr, MonthStr, YearStr] ->
            MonthNo = month_str_to_no(MonthStr),
            DayNo = list_to_integer(DayStr),
            case YearStr of
                "All" ->
                    [{all, MonthNo, DayNo} | simplify_dates(Tail)];
                _ ->
                    YearNo = list_to_integer(YearStr),
                    [{YearNo, MonthNo, DayNo} | simplify_dates(Tail)]
            end;
        ["END_HOLIDAYS"] ->
            [];
	_ ->
            io:format("Skipped holiday: ~p\n", [DateStr]),
	    simplify_dates(Tail)
    end.
    
dates_to_holidays(Dates, YearNo) ->
    lists:usort([#eflex_holiday{day = D, month = M} || {all, M, D} <- Dates] ++
                [#eflex_holiday{day = D, month = M} || {Y, M, D} <- Dates,
                                                       Y =:= YearNo]).
    
month_str_to_no(MonthStr) ->
    case MonthStr of
        "January" -> 1;
        "February" -> 2;
        "March" -> 3;
        "April" -> 4;
        "May" -> 5;
        "June" -> 6;
        "July" -> 7;
        "August" -> 8;
        "September" -> 9;
        "October" -> 10;
        "November" -> 11;
        "December" -> 12
    end.

read_dir(Dir) ->
    MonthFiles = lists:sort(read_files(Dir)),
    {Days, Props} = read_months(MonthFiles, [], []),
    {lists:reverse(Days), lists:reverse(Props)}.

read_files(Dir) ->
    case file:list_dir(Dir) of
        {ok, Files} ->
            ParseName =
                fun(Base) ->
                        case string:tokens(Base, ".") of
                            [MonthStr, YearStr] ->
                                YearNo = list_to_integer(YearStr),
                                MonthNo = month_str_to_no(MonthStr),
                                File = filename:join([Dir, Base]),
                                {YearNo, MonthNo, File}
                        end
                end,
            Filter =
                fun(File) ->
                        case catch ParseName(File) of
                            {'EXIT', _} -> false;
                            Parsed      -> {true, Parsed}
                        end
                end,
            lists:zf(Filter, Files);
        {error, Reason} ->
            io:format("Failed to load data from ~p: ~p\n", [Dir, Reason]),
            []
    end.

read_months([{YearNo, MonthNo, File} | MonthFiles], Days, Props) ->
    %% io:format("Read ~s...\n", [File]),
    {ok, Bin} = file:read_file(File),
    Simple = simplify(Bin),
    {Days2, Props2} = read_month(YearNo, MonthNo, Simple, Days, Props),
    read_months(MonthFiles, Days2, Props2);
read_months([], Days, Props) ->
    {Days, Props}.

read_month(YearNo, MonthNo, [KeyVal | Simple], Days, Props) ->
    case KeyVal of
        {"Day", DayStr} ->
            DayNo = list_to_integer(DayStr),
            D = #day{year = YearNo, month = MonthNo, day = DayNo},
            {D2, Simple2} = read_day(D, Simple),
            read_month(YearNo, MonthNo, Simple2, [D2 | Days], Props);
        {"Properties", _} ->
	    case Props of
		[#props{year_no = PropsYearNo} | _] when PropsYearNo =:= YearNo ->
		    read_props(YearNo, MonthNo, Simple, Days, Props);
		[P | _] ->
		    %% Use prev year as default
		    Props2 = [P#props{year_no = YearNo}] ++ Props,
		    read_props(YearNo, MonthNo, Simple, Days, Props2);
		[] ->
		    Props2 = [#props{year_no = YearNo}] ++ Props,
		    read_props(YearNo, MonthNo, Simple, Days, Props2)
	    end;
        _ ->
            read_month(YearNo, MonthNo, Simple, Days, Props)
    end;
read_month(_YearNo, _MonthNo, [], Days, Props) ->
    {Days, Props}.

read_props(YearNo, MonthNo, [{Key, Val0} | Simple], Days, [P | Props] = All) ->
    Val = lists:map(fun($\.) -> $\: ;
		       (C) -> C end, Val0),
    case Key of
        "EndProperties" ->
	    read_month(YearNo, MonthNo, Simple, Days, All);
	"OTBFreeDayAfter" ->
	    P2 = P#props{freeday_break_after =
			 eflex_lib:convert_time(Val, integer)},
	    read_props(YearNo, MonthNo, Simple, Days, [P2 | Props]);
	"OTBFreeDayLength" ->
	    P2 = P#props{freeday_break_duration =
			 eflex_lib:convert_time(Val, integer)},
	    read_props(YearNo, MonthNo, Simple, Days, [P2 | Props]);
	"OTBWorkDayAfter" ->
	    P2 = P#props{workday_break_after =
			 eflex_lib:convert_time(Val, integer)},
	    read_props(YearNo, MonthNo, Simple, Days, [P2 | Props]);
	"OTBWorkDayLength" ->
	    P2 = P#props{workday_break_duration =
			 eflex_lib:convert_time(Val, integer)},
	    read_props(YearNo, MonthNo, Simple, Days, [P2 | Props]);
	"LunchTime" ->
	    P2 = P#props{lunch_duration =
			 eflex_lib:convert_time(Val, integer)},
	    read_props(YearNo, MonthNo, Simple, Days, [P2 | Props]);
	"EarlyCOverTimeEnds" ->
	    P2 = P#props{early_overtime_ends =
			 eflex_lib:convert_time(Val, integer)},
	    read_props(YearNo, MonthNo, Simple, Days, [P2 | Props]);
	"LateCOverTimeStarts" ->
	    P2 = P#props{late_overtime_starts =
			 eflex_lib:convert_time(Val, integer)},
	    read_props(YearNo, MonthNo, Simple, Days, [P2 | Props]);
        _ ->
	    read_props(YearNo, MonthNo, Simple, Days, All)
    end.

read_day(D, [{Key, Val} | Simple]) ->
    case Key of
        "EndDay" ->
            {D, Simple};
        "ArriveLeft" ->
            read_day(D#day{arrived_left = Val}, Simple);
        "AutoCodes" ->
            read_day(D#day{auto_codes = Val}, Simple);
        "UserCodes" ->
            read_day(D#day{user_codes = Val}, Simple);
        "Flex" ->
            read_day(D#day{derived_flex = Val}, Simple);
        _ ->
            read_day(D, Simple)
    end;
read_day(D, []) ->
    D.

simplify(Bin) when is_binary(Bin) ->
    simplify(binary_to_list(Bin));
simplify(String) ->
    Split =
        fun(Row) ->
                case string:tokens(Row, [$=]) of
                    []    ->
                        empty;
                    [Tag] ->
                        {string:strip(Tag), ""};
                    [Tag, Val] -> 
                        {string:strip(Tag),
                         string:strip(Val)};
                    [Tag, Val | Extra] -> 
                        %% Value contains at least one equal sign
                        Val2 = lists:flatten([Val ++ [[$= | E] || E <- Extra]]),
                        {string:strip(Tag),
                         string:strip(Val2)}
                end
        end,
    lists:map(Split, string:tokens(String, [$\n])).
