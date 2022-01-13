%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2009 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(eflex_wx).
-compile([export_all]).

%% sys callback functions
-export([system_continue/3,
         system_terminate/4,
         system_code_change/4
        ]).

-include("eflex.hrl").
-include_lib("wx/include/wx.hrl").

-define(AWAY, "Away a while").
-define(BACK, "Cancel").

-record(state,
        {parent,
         wx,
         options,
         mouse_check_interval,
         mouse_pos,
         main_frame,
         main_panel,
	 main_status_bar,
         main_grid,
         main_grid_size,
         activity_col_width,
         year,
         popup,
         selected_time,
	 activity_frame,
	 activity_panel,
	 holiday_frame,
	 holiday_panel,
	 holiday_cals,
	 config_frame,
	 config_panel,
	 absence_all_day,
	 unspecified_work,
	 away_a_while,
	 away_button}).

-record(activity_selected, {items}).
-record(week_selected, {items}).
-record(holiday_toggled, {items, date}).
-record(activity_data, {label, type, inner, outer}).
-record(config_data, {label, text, initial}).
-record(activity_list, {name}).
-record(calendar_data, {month}).
-record(main_button_data, {action}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Init
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(RawOptions) ->
    spawn_link(?MODULE, init, [[{parent, self()} | RawOptions]]).

init([{parent, Parent} | RawOptions])
  when is_pid(Parent); Parent =:= undefined ->
    Options  = eflex_lib:parse_options(RawOptions),
    case eflex_lib:lock(Options) of
	ok -> 
	    ok;
	{error, Reason} ->
	    Q = Reason ++ ". \nDo you want to remove the lock file and continue? ",
	    wx:new(),
	    wx:debug(Options#options.debug),
	    case ask_question(Q) of
		true ->
		    %% wx:destroy(),
		    ok;
		false ->
		    exit(Reason)
	    end
    end,
    {Options2, Eyear} = eflex_lib:init_files(Options),
    S = #state{parent  = Parent,
	       options = Options2,
               year    = Eyear,
               mouse_check_interval = timer:minutes(1),
	       holiday_cals = []},
    case Options2#options.window of
        false -> no_window_init(S);
        true  -> window_init(S)
    end,
    eflex_lib:unlock(Options).

ask_question(Question) ->
    Dialog = wxMessageDialog:new(wx:null(), Question,
				 [{style, ?wxYES_NO bor ?wxICON_HAND}]),
    Answer = wxMessageDialog:showModal(Dialog),
    wxMessageDialog:destroy(Dialog),
    case Answer of
	?wxID_YES -> true;
	?wxID_NO  -> false
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Batch
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

no_window_init(#state{options = Options} = S) ->
    wx:new(),
    wx:debug(Options#options.debug),
    no_window_loop(S).

no_window_loop(#state{mouse_check_interval = MouseCheckInterval} = S) ->
    S2 = check_mouse_pos(S),
    receive
        {system, From, Msg} ->
            %% io:format("~p got {system, ~p, ~p}~n", [?MODULE, From, Msg]),
            sys:handle_system_msg(Msg, From, S#state.parent, ?MODULE, [], S2);
        Msg ->
            error_logger:error_msg("~p~p got unexpected message: ~p~n",
				   [?MODULE, self(), Msg]),
            ?MODULE:no_window_loop(S2)
    after MouseCheckInterval ->
            ?MODULE:no_window_loop(S2)
    end.

check_mouse_pos(#state{options = OldOptions,
		       mouse_check_interval = MouseCheckInterval,
		       mouse_pos = OldPos} = S) ->
    Now = calendar:now_to_local_time(erlang:now()),
    MinWait = (MouseCheckInterval div 30000),
    {OldS, Diff} = 
	case S#state.away_a_while of
	    undefined ->
		{S, 0};
	    {Date, Time} ->
		case date() of
		    D when D =:= Date ->
			Away = calendar:datetime_to_gregorian_seconds({Date, Time}),
			Back = calendar:datetime_to_gregorian_seconds(Now),
			Minutes = (Back - Away) div 60,
			Bar = S#state.main_status_bar,
			case MinWait - Minutes of
			    Wait when Wait > 0 ->
				Status = "Away a while - stop moving the mouse within " ++
				    integer_to_list(Wait) ++ " minutes...",
				wxStatusBar:setStatusText(Bar, Status);
			    _Wait ->
				DiffStr =  eflex_lib:convert_time(Minutes,
								  minutes),
				Status = "Away a while - adjust " ++ DiffStr ++
				    " when you are back...",
				wxStatusBar:setStatusText(Bar, Status)
			end,
			{S, Minutes};
		    _ ->
			%% New day. Cancel AFK
			wxStatusBar:setStatusText(S#state.main_status_bar,
						  "New day - adjust cancelled"),
			{away_a_while(S), 0}
		end
	end,
    NewPos = wx_misc:getMousePosition(),
    if
	NewPos =:= OldPos ->
	    OldS;
	OldOptions#options.read_only ->
	    io:format("Mouse move: ~p\n", [Now]),
	    OldS;
	true ->
	    %% io:format("Mouse move: ~p\n", [CurrMtime]),
	    case OldS#state.away_a_while of
		undefined ->
		    incr(OldS, NewPos);
		_ when Diff > MinWait ->
		    %% Back
		    NewS = incr(OldS, NewPos),
		    back_again(NewS, Now, Diff);
		_ ->
		    %% Too soon
		    OldS#state{mouse_pos = NewPos}
	    end
    end.

incr(#state{options = OldOptions,
	    year = OldEyear} = OldS, NewPos) ->
    {NewOptions, NewEyear} =
	eflex_lib:incr_timestamp(OldOptions, OldEyear),
    OldS#state{options   = NewOptions,
	       mouse_pos = NewPos,
	       year      = NewEyear}.
    
away_a_while(S) ->
    case S#state.away_a_while of
	undefined ->
	    wxStatusBar:setStatusText(S#state.main_status_bar,
				      "Away a while... "),
	    wxButton:setLabel(S#state.away_button, ?BACK),
	    DateTime = calendar:now_to_local_time(erlang:now()),
	    S#state{away_a_while = DateTime};
	_Date ->
	    wxButton:setLabel(S#state.away_button, ?AWAY),
	    S#state{away_a_while = undefined}
    end.

back_again(S, {Date, _Time}, Minutes) ->
    DiffStr =  eflex_lib:convert_time(Minutes, minutes),
    wxStatusBar:setStatusText(S#state.main_status_bar,
			      "Back again - adjusted " ++ DiffStr),
    Name = ?ADJUST_ACT,
    {_YearNo, WeekNo} = eflex_lib:week_of_the_year(Date),
    Eyear= S#state.year,
    Eweeks = Eyear#eflex_year.weeks,
    Eweek = element(WeekNo, Eweeks),
    Eactivities = Eweek#eflex_week.activities,
    ActPos = #eflex_activity.name,
    Eactivity = ?KEYSEARCH(Name, ActPos, Eactivities),
    WeekDayNo = calendar:day_of_the_week(Date), % 1..7
    DayPos = #eflex_activity.monday + WeekDayNo - 1,
    NewAdjust =
	case element(DayPos, Eactivity) of
	    undefined -> -Minutes;
	    OldAdjust -> OldAdjust - Minutes
	end,
    Eactivity2 = setelement(DayPos, Eactivity, NewAdjust),
    Eactivities2 = 
	lists:keystore(Name, ActPos, Eactivities, Eactivity2),
    Eweek2 = Eweek#eflex_week{activities = Eactivities2},
    Eweeks2 = setelement(WeekNo, Eweeks, Eweek2),
    Eyear2 = Eyear#eflex_year{weeks = Eweeks2},
    S2 = S#state{year = Eyear2},

    away_a_while(S2).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Interative
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(DATE_ROW, 0).
-define(BREAK_ROW, 3).
-define(WORK_ROW, 5).
-define(FLEX_ROW, 6).
-define(ACTIVITY_COL, 0).
-define(YEAR_SUM_COL, 1).
-define(WEEK_SUM_COL, 2).
-define(MONDAY_COL, 3).
-define(SUNDAY_COL, 9).
-define(WEEK_NO_COL, ?ACTIVITY_COL).
-define(MAIN_FRAME_ID, 100).
-define(MAIN_FLEX_ID,  101).
-define(ACT_FRAME_ID, 102).
-define(CONFIG_FRAME_ID, 103).
-define(HOLIDAY_FRAME_ID, 104).
-define(LIST_FRAME_ID, 105).
-define(CLONE_BUTTON,    200).
-define(DELETE_BUTTON,  201).
-define(VIEW_BUTTON,  202).
-define(ACT_TYPE_EDIT_ITEM, 300).
-define(HOLIDAYS_EDIT_ITEM, 301).
-define(CONFIG_EDIT_ITEM, 302).
-define(CONFIG_COPY_ITEM, 303).
-define(ACT_COPY_INITIAL_ITEM, 304).
-define(HOLIDAY_COPY_ITEM, 305).
-define(CLOSE_ITEM,  ?wxID_EXIT).   %% Use OS specific version if available
-define(ABOUT_ITEM, ?wxID_ABOUT).   %% Use OS specific 
-define(CONTENTS_ITEM, 310).
-define(MIN_ACT_NAME_LEN, 255).

-define(GENERAL, "General").
-define(UNIT_CONFIG_ITEM, "Unit").
-define(WORK_TIME_PER_DAY, "Work time per day").
-define(WORKDAY, "Workday").
-define(FREEDAY, "Freeday").
-define(MINIBREAK, "Mini break").
-define(AUTO_CODE, "Auto code").
-define(MON_CONFIG_ITEM, "Monday").
-define(TUE_CONFIG_ITEM, "Tuesday").
-define(WED_CONFIG_ITEM, "Wednesday").
-define(THU_CONFIG_ITEM, "Thursday").
-define(FRI_CONFIG_ITEM, "Friday").
-define(SAT_CONFIG_ITEM, "Saturday").
-define(SUN_CONFIG_ITEM, "Sunday").
-define(LUNCH_CONFIG_ITEM, "Lunch duration").
-define(BREAK_CONFIG_ITEM, "Break duration").
-define(BEFORE_CONFIG_ITEM, "Time before break").
-define(ABSENCE_CONFIG_ITEM, "Absence all day").
-define(UNSPEC_CONFIG_ITEM, "Unspecified work").

window_init(#state{options = Options, year=#eflex_year{config=Config}} = S) ->
    Wx = wx:new(),
    wx:debug(Options#options.debug),
    #eflex_config{mini_break = MiniBreak} = Config,
    if 
        MiniBreak > 0 -> 
	    spawn_link(eflex_break, start, [MiniBreak]);
        true -> 
	    ignore
    end,    
    S2 = wx:batch(fun() -> create_main_window(S#state{wx = Wx}) end),
    window_loop(S2, undefined).

window_loop(#state{mouse_check_interval = MouseCheckInterval} = S, OldS) ->
    S2 = update_year(S, OldS),
    receive
        Event ->
	    %% io:format("Received: ~p\n", [Event]),
            S3 = 
                case Event of
		    #wx{id = ?MAIN_FRAME_ID} ->
			handle_main(S2, Event);
		    #wx{id = ?MAIN_FLEX_ID} ->
			handle_main(S2, Event);
		    #wx{id = ?ACT_FRAME_ID} ->
			handle_action_type(S2, Event);
		    #wx{userData = #activity_data{}} ->
			handle_action_type(S2, Event);
		    #wx{userData = #activity_list{}} ->
			handle_action_list(S2, Event);
		    #wx{userData = week_selected} ->
			handle_action_list(S2, Event);
		    #wx{id = ?CONFIG_FRAME_ID} ->
			handle_config(S2, Event);
		    #wx{id = ?HOLIDAY_FRAME_ID} ->
			handle_holiday(S2, Event);
		    #wx{userData = #calendar_data{}} ->
			handle_holiday(S2, Event);
		    #wx{userData = #config_data{}} ->
			handle_config(S2, Event);
		    #wx{userData = #main_button_data{}} ->
			handle_main_button(S2, Event);
		    _ ->
			handle_other(S2, Event)
                end,
            ?MODULE:window_loop(S3, S2)
    after MouseCheckInterval ->
	    %% Check mouse pos
            ?MODULE:window_loop(S2, S2)
    end.

handle_main(#state{options = O,
                   main_grid = MainGrid} = S,
	    Event) ->
    %% io:format("Received main event: ~p\n", [Event]),
    case Event of
	#wx{id = ?MAIN_FLEX_ID,
	    event = #wxGrid{type = grid_cell_change,
			    row = Row,
			    col = Col}} ->
	    S2 = opt_update_activity(S, MainGrid, Row, Col),
	    Time = wxGrid:getCellValue(MainGrid, Row, Col),
	    S2#state{selected_time = Time};
	#wx{id = ?MAIN_FLEX_ID,
	    event = #wxSize{type = size} = NewSize} ->
	    resize(S, MainGrid, NewSize);
	#wx{id = ?MAIN_FRAME_ID,
	    event = #wxClose{type = close_window}} ->
	    close_window(O);
	#wx{id = ?MAIN_FRAME_ID,
	    event = #wxIconize{type = iconize}} ->
	    S;
	    %% case wxFrame:isIconized(Frame) of
	    %% 	true ->
	    %% 	    %% io:format("Window iconized\n", []),
	    %% 	    S;
	    %% 	false ->
	    %% 	    S
	    %% 	    %% io:format("Window de-iconized\n", []),
	    %% 	    %% check_mouse_pos(S)
	    %% 	    %% %% Switch to current week
	    %% 	    %% {YearNo, WeekNo} = eflex_lib:week_of_the_year(date()),
	    %% 	    %% week_selected(S, {YearNo, WeekNo})
	    %% end;
	_ ->
	    io:format("Unknown eflex main event: ~p\n", [Event]),
	    S
    end.
	
handle_other(#state{main_frame = Mframe,
		    activity_frame = Aframe,
		    holiday_frame = Hframe,
		    config_frame = Cframe,
		    options = O,
		    main_grid = MainGrid} = S,
	     Event) ->
    %% io:format("Received other event: ~p\n", [Event]),
    case Event of
	{select_activity, Row, _Col} ->
	    wx:batch(fun() -> activity_popup(S, Row) end);
	{copy_cell, Row, Col} ->
	    case wxGrid:getCellValue(MainGrid, Row, Col) of
		[$- | Time] -> ok; % Use abs val
		Time -> ok
	    end,
	    clipboard_copy(Time),
	    S#state{selected_time = Time};
	{paste_cell, Row, Col} ->
	    S2 = 
		case clipboard_paste() of
		    {ok, T} ->
			S#state{selected_time = T};
		    {error, _} ->
			S
		end,
	    case S2#state.selected_time of
		undefined ->
		    S;
		Time ->
		    wxGrid:setCellValue(MainGrid, Row, Col, Time),
		    opt_update_activity(S2, MainGrid, Row, Col)
	    end;
	{toggle_holiday, Row, Col} ->
	    wx:batch(fun() -> holiday_popup(S, Row, Col) end);
	auto_code ->
	    auto_code(S);
	#wx{event = #wxMenu{type = menu_open}} ->
	    S#state{popup = undefined};
	#wx{id = ItemPos,
	    event = #wxCommand{type = command_menu_selected},
	    userData = UserData} ->
	    case S#state.popup of
		_  when ItemPos =:= 0 ->
		    S;
		#holiday_toggled{items = Items, date = Date}
		when ItemPos =< length(Items) ->
		    Item = lists:nth(ItemPos, Items),
		    holiday_toggled(S#state{popup = undefined}, Date, Item);
		#week_selected{items = Items}
		when ItemPos =< length(Items) ->
		    Item = lists:nth(ItemPos, Items),
		    week_selected(S#state{popup = undefined}, Item);
		#activity_selected{items = Items}
		when ItemPos =< length(Items) ->
		    Item = lists:nth(ItemPos, Items),
		    activity_selected(S#state{popup = undefined}, Item);
		undefined ->
		    case ItemPos of
			?ACT_TYPE_EDIT_ITEM ->
			    wx:batch(fun() -> create_activity_type_window(S) end);
			?ACT_COPY_INITIAL_ITEM ->
			    case eflex_lib:read_previous_config(O) of
				{ok, PrevEconfig} ->
				    PrevTypes = PrevEconfig#eflex_config.activity_types,
				    Eyear = S#state.year,
				    Econfig = Eyear#eflex_year.config,
				    Types = Econfig#eflex_config.activity_types,
				    NamePos = #eflex_activity_type.name,
				    Copy =
					fun(#eflex_activity_type{name = Name} = AT) ->
						case lists:keysearch(Name,
								     NamePos,
								     PrevTypes) of
						    false ->
							AT;
						    {value, Type} ->
							I = Type#eflex_activity_type.initial,
							AT#eflex_activity_type{initial = I}
						end
					end,
				    Types2 = lists:map(Copy, Types),
				    Econfig2 = Econfig#eflex_config{activity_types = Types2},
				    Eyear2 = Eyear#eflex_year{config = Econfig2},
				    S2 = S#state{year = Eyear2},
				    recreate_activity_type_window(S2);
				{error, _} ->
				    S
			    end;
			?HOLIDAY_COPY_ITEM ->
			    case eflex_lib:read_previous_config(O) of
				{ok, PrevEconfig} ->
				    PrevHolidays = PrevEconfig#eflex_config.holidays,
				    Eyear = S#state.year,
				    Econfig = Eyear#eflex_year.config,
				    Econfig2 = Econfig#eflex_config{holidays = PrevHolidays},
				    Eyear2 = Eyear#eflex_year{config = Econfig2},
				    S2 = S#state{year = Eyear2},
				    recreate_holiday_window(S2);
				{error, _} ->
				    S
			    end;
			?HOLIDAYS_EDIT_ITEM ->
			    wx:batch(fun() -> create_holiday_window(S) end);
			?CONFIG_EDIT_ITEM ->
			    wx:batch(fun() -> create_config_window(S) end);
			?CONFIG_COPY_ITEM ->
			    case eflex_lib:read_previous_config(S#state.options) of
				{ok, #eflex_config{unit = Unit,
						   working_time = Work,
						   workday_break = Wbreak,
						   freeday_break = Fbreak}} ->
				    Eyear = S#state.year,
				    Econfig = Eyear#eflex_year.config,
				    Econfig2 =
					Econfig#eflex_config{unit = Unit,
							     working_time = Work,
							     workday_break = Wbreak,
							     freeday_break = Fbreak},
				    Eyear2 = Eyear#eflex_year{config = Econfig2},
				    Str = "Previous config copied.\n",
				    Status = lists:flatten(io_lib:format(Str, [])),
				    wxStatusBar:setStatusText(S#state.main_status_bar, Status),
				    S2 = S#state{year = Eyear2},
				    recreate_config_window(S2);
				{error, Reason} ->
				    Str = "<ERROR> Previous config not copied: ~p \n",
				    Status = lists:flatten(io_lib:format(Str, [Reason])),
				    wxStatusBar:setStatusText(S#state.main_status_bar, Status),
				    S
			    end;
			?CLOSE_ITEM when UserData =:= main_window ->
			    close_window(O);
			?CLOSE_ITEM when UserData =:= activity_type_editor ->
			    wxFrame:destroy(Aframe),
			    S#state{activity_frame = undefined, 
				    activity_panel = undefined};
			?CLOSE_ITEM when UserData =:= holiday_editor ->
			    wxFrame:destroy(Hframe),
			    S#state{holiday_frame = undefined,
				    holiday_panel = undefined,
				    holiday_cals  = []};
			?CLOSE_ITEM when UserData =:= config_editor ->
			    wxFrame:destroy(Cframe),
			    S#state{config_frame = undefined,
				    config_panel = undefined};
			?CONTENTS_ITEM ->
			    {file, BeamFile} = code:is_loaded(?MODULE),
			    EbinDir = filename:dirname(BeamFile),
			    AppDir = filename:dirname(EbinDir),
			    HelpFile = filename:join([AppDir, "README.md"]),
			    Url = "file://" ++ filename:absname(HelpFile),
			    wx_misc:launchDefaultBrowser(Url),
			    S;
			?ABOUT_ITEM ->
			    AboutStr = about(),
			    MD = wxMessageDialog:new(Mframe, 
						     AboutStr,
						     [{style, ?wxOK bor ?wxICON_INFORMATION}, 
						      {caption, "About Eflex"}]),
			    wxDialog:showModal(MD),
			    wxDialog:destroy(MD),
			    S;
			_ ->
			    io:format("Item ~p in system menu is ignored. \n",
				      [ItemPos]),
			    S
		    end;
		Popup ->
		    io:format("Item ~p in popup menu ~p is ignored. \n",
			      [ItemPos, Popup]),
		    S
	    end;
	{week_selected, What} ->
	    week_selected(S, What);
	{select_year, _Row, _Col} ->
	    wx:batch(fun() -> year_popup(S) end);
	{select_week, _Row, _Col} ->
	    wx:batch(fun() -> week_popup(S) end);
	_ ->
	    io:format("Unknown eflex other event: ~p\n", [Event]),
	    S
    end.

about() ->
    Time =
	case lists:keysearch(compile, 1, module_info()) of
	    {value, {compile, I}} ->
		case lists:keysearch(time, 1, I) of
		    {value, {time, {Year, Mon, Day, Hour, Min, Sec}}} ->
			lists:concat([Year, "-", Mon, "-", Day, " ",
				      Hour, ":", Min, ":", Sec]);
		    false ->
			"?"
		end;
	    false ->
		"?"
	end,
    "Eflex - a flextime calculation tool implemented with Erlang/OTP\n"
	++  "by Håkan Mattsson\n\n" ++
	"Latest compilation: " ++ Time.

handle_action_type(S, Event) ->
    %% io:format("Received action event: ~p\n", [Event]),
    case Event of
   	#wx{id = ?ACT_FRAME_ID,
	    event = #wxClose{type = close_window}} ->
	    wxFrame:destroy(S#state.activity_frame),
	    S#state{activity_frame = undefined, activity_panel = undefined};
	#wx{obj = ObjRef,
	    userData = #activity_data{label = Label, type = Etype} = AD,
	    event = #wxCommand{type = command_text_enter = CmdType,
			       cmdString = ValStr0}} ->
	    ValStr = string:join(string:tokens(ValStr0, " "), " "),
	    if
		ValStr =:= "" ->
		    %% Invalid name. Recover the name.
		    OldName = Etype#eflex_activity_type.name,
		    io:format("~p is not valid as activity type name. "
			      "Reset to old value ~p\n",
			      [ValStr, OldName]),
		    wxTextCtrl:setValue(ObjRef, OldName),
		    S;
		Label =:= name ->
		    Eyear = S#state.year,
		    Econfig = Eyear#eflex_year.config,
		    Etypes = Econfig#eflex_config.activity_types,
		    OldName = Etype#eflex_activity_type.name,
		    NamePos = #eflex_activity_type.name,
		    case lists:keysearch(ValStr, NamePos, Etypes) of
			{value, _} ->
			    %% Already exists. Recover the name.
			    io:format("Activity type ~p already exists. "
				      "Reset to old value ~p\n",
				      [ValStr, OldName]),
			    wxTextCtrl:setValue(ObjRef, OldName),
			    S;
			false ->
			    %% Rename
			    Etype2 = Etype#eflex_activity_type{name = ValStr},
			    replace_user_data(ObjRef, CmdType, AD, Etype2),
			    Etypes2 = lists:keyreplace(OldName,
						       NamePos,
						       Etypes,
						       Etype2),
			    Etypes3 = eflex_lib:sort_activity_types(Etypes2),
			    AbsenceAllDay = Econfig#eflex_config.absence_all_day,
			    AbsenceAllDay2 = 
				if
				    AbsenceAllDay =:= OldName ->
					ValStr;
				    true ->
					AbsenceAllDay
				end,
			    UnspecWork = Econfig#eflex_config.unspecified_work,
			    UnspecWork2 = 
				if
				    UnspecWork =:= OldName -> ValStr;
				    true -> UnspecWork
				end,
			    Econfig2 =
				Econfig#eflex_config{activity_types = Etypes3,
						     absence_all_day = AbsenceAllDay2,
						     unspecified_work = UnspecWork2},
			    Eweeks = Eyear#eflex_year.weeks,
			    Eweeks2 = [replace_name(Eweek, Econfig2, OldName, ValStr)
				       || Eweek <- tuple_to_list(Eweeks)],
			    Eweeks3 = list_to_tuple(Eweeks2),
			    Eyear2 = Eyear#eflex_year{weeks = Eweeks3, config = Econfig2},
			    recreate_config_window(S#state{year = Eyear2})
		    end;
		Label =:= initial ->
		    InitInt = eflex_lib:convert_time(ValStr,
						     integer,
						     Etype#eflex_activity_type.initial),
		    InitStr = eflex_lib:convert_time(InitInt,
						     Etype#eflex_activity_type.unit,
						     Etype#eflex_activity_type.initial),
		    wxTextCtrl:setValue(ObjRef, InitStr),
		    Etype2 = Etype#eflex_activity_type{initial = InitInt},
		    replace_user_data(ObjRef, CmdType, AD, Etype2),
		    replace_type(S, Etype2);
		Label =:= share ->
		    case catch list_to_integer(ValStr) of
			{'EXIT', _} ->
			    Default = integer_to_list(Etype#eflex_activity_type.share),
			    wxTextCtrl:setValue(ObjRef, Default),
			    S;
			Share ->
			    wxTextCtrl:setValue(ObjRef, integer_to_list(Share)),
			    Etype2 = Etype#eflex_activity_type{share = Share},
			    replace_user_data(ObjRef, CmdType, AD, Etype2),
			    S2 = replace_type(S, Etype2),
			    Eyear = unify_shares(S2#state.year),
			    recreate_config_window(S2#state{year = Eyear})
		    end
	    end;
	#wx{obj = ObjRef,
	    userData = #activity_data{label = Label, type = Etype} = AD,
	    event = #wxCommand{type = command_choice_selected = CmdType,
			       cmdString = ValStr}} ->
	    case Label of
		unit ->
		    Unit =
			case ValStr of
			    "Minutes" -> minutes;
			    "Decimal" -> decimal
			end,
		    Etype2 = Etype#eflex_activity_type{unit = Unit},
		    replace_user_data(ObjRef, CmdType, AD, Etype2),
		    replace_type(S, Etype2);
		sign ->
		    Sign =
			case ValStr of
			    "Positive" -> positive;
			    "Negative" -> negative
			end,
		    Etype2 = Etype#eflex_activity_type{sign = Sign},
		    replace_user_data(ObjRef, CmdType, AD, Etype2),
		    replace_type(S, Etype2);
		visibility ->
		    Visibility =
			case ValStr of
			    "Visible" -> visible;
			    "Hidden"  -> hidden
			end,
		    Etype2 = Etype#eflex_activity_type{visibility = Visibility},
		    replace_user_data(ObjRef, CmdType, AD, Etype2),
		    replace_type(S, Etype2);
		category ->
		    Category =
			case ValStr of
			    "Attendance" -> attendance;
			    "Project"    -> project
			end,
		    Etype2 = Etype#eflex_activity_type{category = Category},
		    replace_user_data(ObjRef, CmdType, AD, Etype2),
		    replace_type(S, Etype2)
	    end;
	#wx{userData = #activity_data{label = Label, type = Etype},
	    event = #wxCommand{type = command_button_clicked}} ->
	    Eyear = S#state.year,
	    Econfig = Eyear#eflex_year.config,
	    Etypes = Econfig#eflex_config.activity_types,
	    case Label of
		clone ->
		    Name = Etype#eflex_activity_type.name ++ "(clone)",
		    case lists:keysearch(Name, #eflex_activity_type.name, Etypes) of
			{value, _} ->
			    io:format("Activity type ~s already exists. "
				      "Cloning rejected.\n",
				      [Name]),
			    S;
			false ->
			    Etype2 = Etype#eflex_activity_type{name = Name},
			    %% Eweeks = Eyear#eflex_year.weeks,
			    %% EweekList = tuple_to_list(Eweeks),
			    %% Frame = S#state.activity_frame,
			    %% Panel = S#state.activity_panel,
			    %% OuterSizer = AD#activity_data.outer,
			    %% create_activity_type_row(Panel, OuterSizer, Etype2, EweekList, false),
			    %% refresh_sizer(Frame, Panel, OuterSizer),
			    Etypes2 = 
				eflex_lib:sort_activity_types([Etype2 | Etypes]),
			    Econfig2 =
				Econfig#eflex_config{activity_types = Etypes2},
			    Eyear2 = Eyear#eflex_year{config = Econfig2},
			    Eyear3 = unify_shares(Eyear2),
			    S2 = S#state{year = Eyear3},
			    S3 = recreate_activity_type_window(S2),
			    recreate_config_window(S3)
		    end;
		delete ->
		    %% OuterSizer = AD#activity_data.outer,
		    %% InnerSizer = AD#activity_data.inner,
		    %% wxSizer:detach(OuterSizer, InnerSizer),
		    %% Frame = S#state.activity_frame,
		    %% Panel = S#state.activity_panel,
		    %% refresh_sizer(Frame, Panel, OuterSizer),
		    Etypes2 = Etypes -- [Etype],
		    Econfig2 = Econfig#eflex_config{activity_types = Etypes2},
		    Eyear2 = Eyear#eflex_year{config = Econfig2},
		    Eyear3 = unify_shares(Eyear2),
		    S2 = S#state{year = Eyear3},
		    S3 = recreate_activity_type_window(S2),
		    recreate_config_window(S3);
		{view, Name} ->
		    wx:batch(fun() ->
				     create_activity_list_window(S, Name)
			     end)
	    end;
	_ ->
	    io:format("Unknown eflex action event: ~p\n", [Event]),
	    S
    end.

unify_shares(#eflex_year{config =#eflex_config{activity_types = Etypes} = Econfig} = Eyear) ->
    Count = fun(A, {Sum, N}) ->
		    case A#eflex_activity_type.share =/= 0 of
			true  -> {Sum + 1, A#eflex_activity_type.name};
			false -> {Sum, N}
		    end
	    end,
    Name = 
	case lists:foldl(Count, {0, undefined}, Etypes) of
	    {1, N} -> N;
	    {_, _} -> undefined
	end,
    Econfig2 = Econfig#eflex_config{unspecified_work = Name},
    Eyear#eflex_year{config = Econfig2}.

handle_action_list(S, Event) ->
    %% io:format("Received activity list event: ~p\n", [Event]),
    case Event of
   	#wx{id = ?CLOSE_ITEM,
	    userData = #activity_list{name = ActName}} ->
	    close_action_list(S, ActName);
   	#wx{userData = #activity_list{name = ActName},
	    event = #wxClose{type = close_window}} ->
	    close_action_list(S, ActName);
   	#wx{obj = Grid,
	    userData = week_selected,
	    event = #wxGrid{row = Row}} ->
	    Val = wxGrid:getCellValue(Grid, Row, 0),
	    case catch list_to_integer(Val) of
		{'EXIT', _} ->
		    S;
		WeekNo ->
		    Eyear = S#state.year,
		    YearNo = Eyear#eflex_year.no,
		    week_selected(S, {YearNo, WeekNo})
	    end;
	_ ->
	    io:format("Unknown eflex activity list event: ~p\n", [Event]),
	    S
    end.

close_action_list(S, ActName) ->
    Eyear = S#state.year,
    Econfig = Eyear#eflex_year.config,
    Etypes = Econfig#eflex_config.activity_types,
    Etype = ?KEYSEARCH(ActName,
		       #eflex_activity_type.name,
		       Etypes),
    wxFrame:destroy(Etype#eflex_activity_type.frame),
    Etype2 = Etype#eflex_activity_type{frame = undefined,
				       panel = undefined},
    Etypes2 = lists:keystore(ActName,
			     #eflex_activity_type.name,
			     Etypes,
			     Etype2),
    Econfig2 = Econfig#eflex_config{activity_types = Etypes2},
    Eyear2 = Eyear#eflex_year{config = Econfig2},
    S#state{year = Eyear2}.

refresh_sizer(Frame, Panel, Sizer) ->
    wxSizer:layout(Sizer),
    wxPanel:setSizer(Panel, Sizer),
    wxSizer:fit(Sizer, Frame),
    wxSizer:setSizeHints(Sizer, Frame),
    wxWindow:refresh(Frame),
    wxWindow:update(Frame).

replace_type(S, Etype) ->
    Name = Etype#eflex_activity_type.name,
    Eyear = S#state.year,
    Econfig = Eyear#eflex_year.config,
    Etypes = Econfig#eflex_config.activity_types,
    Etypes2 = lists:keyreplace(Name, #eflex_activity_type.name, Etypes, Etype),
    Econfig2 = Econfig#eflex_config{activity_types = Etypes2},
    Eyear2 = Eyear#eflex_year{config = Econfig2},
    S#state{year = Eyear2}.

replace_user_data(ObjRef, CmdType, AD, Etype) ->
    wxEvtHandler:disconnect(ObjRef, CmdType),
    wxEvtHandler:connect(ObjRef, CmdType,
		       [{userData, AD#activity_data{type = Etype}}]).
    
replace_name(Eweek, Econfig, OldName, NewName) ->
    Replace =
	fun(#eflex_activity{name = N} = Eactivity) ->
		if
		    N =:= OldName ->
			Eactivity#eflex_activity{name = NewName};
		    true ->
			Eactivity
		end
	end,
    Eactivities = lists:map(Replace, Eweek#eflex_week.activities),
    Eactivities2 = eflex_lib:sort_activities(Eactivities, Econfig),
    Eweek#eflex_week{activities = Eactivities2}.
    
handle_config(S, Event) ->
    %% io:format("Received config event: ~p\n", [Event]),
    case Event of
   	#wx{id = ?CONFIG_FRAME_ID,
	    event = #wxClose{type = close_window}} ->
	    wxFrame:destroy(S#state.config_frame),
	    S#state{config_frame = undefined, config_panel = undefined};
	#wx{userData = #config_data{text = ?UNIT_CONFIG_ITEM},
	    event = #wxCommand{type = command_choice_selected,
			       cmdString = ValStr0}} ->
	    Unit =
		case ValStr0 of
		    "Minutes" -> minutes;
		    "Decimal" -> decimal
		end,
	    Eyear = S#state.year,
	    Econfig = Eyear#eflex_year.config,
	    Econfig2 = Econfig#eflex_config{unit = Unit},
	    Eyear2 = Eyear#eflex_year{config = Econfig2},
	    S2 = S#state{year = Eyear2},
	    recreate_config_window(S2);
	#wx{userData = #config_data{text = ?ABSENCE_CONFIG_ITEM},
	    event = #wxCommand{type = command_choice_selected,
			       cmdString = ValStr0}} ->
	    Absence =
		case ValStr0 of
		    ?FLEX_ACT -> undefined;
		    _ -> ValStr0
		end,
	    Eyear = S#state.year,
	    Econfig = Eyear#eflex_year.config,
	    Econfig2 = Econfig#eflex_config{absence_all_day = Absence},
	    Eyear2 = Eyear#eflex_year{config = Econfig2},
	    S#state{year = Eyear2};
	#wx{userData = #config_data{text = ?UNSPEC_CONFIG_ITEM},
	    event = #wxCommand{type = command_choice_selected,
			       cmdString = ValStr0}} ->
	    {UnspecName, Unspec, Share} =
		case ValStr0 of
		    ?UNSPEC_ACT -> {undefined, make_ref(), 0};
		    _           -> {ValStr0, undefined, 1}
		end,
	    Eyear = S#state.year,
	    Econfig = Eyear#eflex_year.config,
	    Econfig2 = Econfig#eflex_config{unspecified_work = UnspecName},
	    Eyear2 = Eyear#eflex_year{config = Econfig2},
	    Eyear3 = eflex_lib:unify_shares(Eyear2, Unspec, Share),	    
	    recreate_activity_type_window(S#state{year = Eyear3});
	#wx{obj = _ObjRef,
	    userData = #config_data{label = ?MINIBREAK, text = _Text, initial = Initial},
	    event = #wxCommand{type = command_text_enter, cmdString = ValStr}} ->
	    Val = eflex_lib:convert_time(ValStr, integer, Initial),
	    Eyear = S#state.year,
	    Econfig = Eyear#eflex_year.config,
	    Econfig2 = Econfig#eflex_config{mini_break = Val},
	    Eyear2 = Eyear#eflex_year{config = Econfig2},	    
	    eflex_break:start(Val),
	    recreate_activity_type_window(S#state{year = Eyear2});
	#wx{obj = ObjRef,
	    userData = #config_data{label = Label, text = Text, initial = Initial},
	    event = #wxCommand{type = command_text_enter,
			       cmdString = ValStr0}} ->
	    Eyear = S#state.year,
	    Econfig = Eyear#eflex_year.config,
	    #eflex_config{unit = Unit,
			  working_time = Work,
			  workday_break = Wbreak,
			  freeday_break = Fbreak} =
		Econfig,
	    ValStr = eflex_lib:convert_time(ValStr0, Unit, Initial),
	    Val = eflex_lib:convert_time(ValStr, integer, 0),
	    wxTextCtrl:setValue(ObjRef, ValStr),
	    Econfig2 = 
		case Label of
		    ?WORK_TIME_PER_DAY ->
			Work2 =
			    case Text of
				?MON_CONFIG_ITEM ->
				    Work#eflex_working_time{monday = Val};
				?TUE_CONFIG_ITEM ->
				    Work#eflex_working_time{tuesday = Val};
				?WED_CONFIG_ITEM ->
				    Work#eflex_working_time{wednesday = Val};
				?THU_CONFIG_ITEM ->
				    Work#eflex_working_time{thursday = Val};
				?FRI_CONFIG_ITEM ->
				    Work#eflex_working_time{friday = Val};
				?SAT_CONFIG_ITEM ->
				    Work#eflex_working_time{saturday = Val};
				?SUN_CONFIG_ITEM ->
				    Work#eflex_working_time{sunday = Val}
			    end,
			Econfig#eflex_config{working_time = Work2};
		    ?WORKDAY ->
			case Text of
			    ?LUNCH_CONFIG_ITEM ->
				Econfig#eflex_config{lunch_duration = Val};
			    ?BREAK_CONFIG_ITEM ->
				Wbreak2 = Wbreak#eflex_break{duration = Val},
				Econfig#eflex_config{workday_break = Wbreak2};
			    ?BEFORE_CONFIG_ITEM -> 
				Wbreak2 = Wbreak#eflex_break{time_before_break = Val},
				Econfig#eflex_config{workday_break = Wbreak2}
			end;
		    ?FREEDAY ->
			case Text of
			    ?BREAK_CONFIG_ITEM ->
				Fbreak2 = Fbreak#eflex_break{duration = Val},
				Econfig#eflex_config{workday_break = Fbreak2};
			    ?BEFORE_CONFIG_ITEM -> 
				Fbreak2 = Fbreak#eflex_break{time_before_break = Val},
				Econfig#eflex_config{workday_break = Fbreak2}
			end
		end,
	    Eyear2 = Eyear#eflex_year{config = Econfig2},
	    S#state{year = Eyear2};
	_ ->
	    io:format("Unknown eflex config event: ~p\n", [Event]),
	    S
    end.

handle_holiday(S, Event) ->
    %% io:format("Received holiday event: ~p\n", [Event]),
    case Event of
   	#wx{id = ?HOLIDAY_FRAME_ID,
	    event = #wxClose{type = close_window}} ->
	    wxFrame:destroy(S#state.holiday_frame),
	    S#state{holiday_frame = undefined,
		    holiday_panel = undefined,
		    holiday_cals = []};
	#wx{obj = Cal, userData = #calendar_data{},
	    event = #wxCalendar{type = calendar_sel_changed}} ->
	    wx:batch(fun() ->
			     {Date, _Time} = wxCalendarCtrl:getDate(Cal),
			     holiday_popup(S, Date) 
		     end);
	_ ->
	    io:format("Unknown eflex holiday event: ~p\n", [Event]),
	    S
    end.

handle_main_button(S, Event) ->
    case Event of
	#wx{userData = #main_button_data{action = auto_code}} ->
	    auto_code(S);
	#wx{userData = #main_button_data{action = away_a_while}} ->
	    S2 = check_mouse_pos(S),
	    wxStatusBar:setStatusText(S2#state.main_status_bar,
				      "Back again - adjust cancelled"),
	    away_a_while(S2);
	#wx{userData = #main_button_data{action = Action}} ->
	    week_selected(S, Action);
	_ ->
	    io:format("Unknown eflex main button event: ~p\n", [Event]),
	    S
    end.
    
close_window(Options) ->
    %% io:format("Window closed\n", []),
    wx:destroy(),
    eflex_lib:unlock(Options),
    exit(shutdown).

update_year(S, undefined) ->
    wx:batch(fun() -> update_week(S) end);
update_year(S, OldS) ->
    NewS = check_mouse_pos(S),
    #state{options = #options{date = NewDate}, year = NewYear} = NewS,
    #state{options = #options{date = OldDate}, year = OldYear} = OldS,
    {NewYearNo, NewWeekNo} = eflex_lib:week_of_the_year(NewDate),
    {OldYearNo, OldWeekNo} = eflex_lib:week_of_the_year(OldDate),
    NewWeek = element(NewWeekNo, NewYear#eflex_year.weeks),
    OldWeek = element(OldWeekNo, OldYear#eflex_year.weeks),
    NewConfig = NewYear#eflex_year.config,
    OldConfig = OldYear#eflex_year.config,
    if
	NewYearNo =:= OldYearNo,
	NewWeekNo =:= OldWeekNo,
	NewWeek   =:= OldWeek,
	NewConfig =:= OldConfig ->
	    NewS;
	NewYearNo =:= OldYearNo ->
	    %% The week data or the config has been changed
	    wx:batch(fun() -> update_week(NewS) end);
	true ->
	    %% The week data or the config has been changed
	    wx:batch(fun() ->
			     NewS2 = recreate_activity_type_window(NewS),
			     NewS3 = recreate_config_window(NewS2),
			     NewS4 = recreate_holiday_window(NewS3),
			     update_week(NewS4) 
		     end)
    end.

create_main_window(#state{options = Options, wx = Wx} = S) ->
    %% Frame
    Frame = wxFrame:new(Wx, ?MAIN_FRAME_ID, "Eflex", []),
    Panel = wxPanel:new(Frame, []),
    StatusBar = wxFrame:createStatusBar(Frame,[]),
    wxFrame:connect(Frame, close_window),
    wxFrame:connect(Frame, iconize),

    %% Create menubar
    MenuBar = wxMenuBar:new(),
    File    = wxMenu:new([]),
    Help    = wxMenu:new([]),
    wxMenuBar:append(MenuBar, File, "File" ),
    wxMenu:append(File, ?ACT_TYPE_EDIT_ITEM, "Edit activity types" ),
    wxMenu:append(File, ?CONFIG_EDIT_ITEM, "Edit configuration" ),
    wxMenu:append(File, ?HOLIDAYS_EDIT_ITEM, "Edit holidays" ),
    wxMenu:append(File, ?CLOSE_ITEM, "Close" ),
    wxMenuBar:append(MenuBar, Help, "Help" ),
    wxMenu:append(Help, ?CONTENTS_ITEM, "Contents" ),
    wxMenu:append(Help, ?ABOUT_ITEM, "About" ),
    wxFrame:setMenuBar(Frame, MenuBar),
    wxEvtHandler:connect(Frame,
			 command_menu_selected,
			 [{userData, main_window}]),
    wxEvtHandler:connect(File, menu_open),
    wxEvtHandler:connect(Help, menu_open),

    %% Split the window in three major parts
    MainSz = wxBoxSizer:new(?wxVERTICAL),
    TopSz = wxBoxSizer:new(?wxHORIZONTAL),
    AwayButton = create_main_buttons(TopSz, Panel),
    wxSizer:add(MainSz, TopSz, [{flag, ?wxEXPAND}]),
    
    {MainGrid, ActColWidth} = create_main_grid(Panel, Options),
    wxSizer:add(MainSz, MainGrid, [{border, 2},
				   {flag, ?wxALL bor ?wxEXPAND}]),
    refresh_sizer(Frame, Panel, MainSz),
    {ok,IconBinary,_} = erl_prim_loader:get_file(filename:join(code:priv_dir(eflex),"plan.png")),
    %% Apparently wx can't handle binaries ..
    ok = file:write_file("/tmp/plan.png",IconBinary),
    Icon = wxIcon:new("/tmp/plan.png", [{type,?wxBITMAP_TYPE_PNG}]),
    file:delete("/tmp/plan.png"),
    wxFrame:setIcon(Frame, Icon),
    wxFrame:show(Frame),

    S#state{main_frame         = Frame,
	    main_panel         = Panel,
	    main_status_bar    = StatusBar,
            main_grid          = MainGrid,
            main_grid_size     = wxWindow:getSize(MainGrid),
            activity_col_width = ActColWidth,
	    away_button        = AwayButton}.

create_main_buttons(TopSz, Panel) ->
    create_button(TopSz, Panel, "Auto code", auto_code, 1),
    create_button(TopSz, Panel, "<<<",   {year,  previous}, 1),
    create_button(TopSz, Panel, "<<",    {month, previous}, 1),
    create_button(TopSz, Panel, "<",     {week,  previous}, 1),
    create_button(TopSz, Panel, "Today", {week,  current}, 1),
    create_button(TopSz, Panel, ">",     {week,  next}, 1),
    create_button(TopSz, Panel, ">>",    {month, next}, 1),
    create_button(TopSz, Panel, ">>>",   {year,  next}, 1),
    create_button(TopSz, Panel, ?AWAY,   away_a_while, 1).
    
create_button(TopSz, Panel, Label, Action, Prop) ->
    Button = wxButton:new(Panel, ?wxID_ANY, [{label, Label}]),
    wxButton:connect(Button,
		     command_button_clicked, 
		     [{userData, #main_button_data{action = Action}}]),
    wxSizer:add(TopSz, Button, [{flag, ?wxEXPAND bor ?wxALL},
				{border, 0},
				{proportion, Prop}]),
    Button.


create_main_grid(Panel, #options{n_rows = Nrows}) ->
    %% Create grid
    ActColWidth = ?MIN_ACT_NAME_LEN,
    Grid = wxGrid:new(Panel, ?MAIN_FLEX_ID),
    wxGrid:createGrid(Grid, Nrows, ?SUNDAY_COL + 1,
                      [{selmode, ?wxGrid_wxGridSelectCells}]),
    wxGrid:setDefaultCellAlignment(Grid, ?wxALIGN_RIGHT, ?wxALIGN_CENTER),
    wxGrid:setRowLabelSize(Grid, 0),
    wxGrid:disableDragColSize(Grid),
    wxGrid:disableDragRowSize(Grid),

    %% Populate grid
    InitCol =
        fun(Label, {Col, Width}) ->
                wxGrid:setColMinimalWidth(Grid, Col, Width),
                wxGrid:setColSize(Grid, Col, Width),
                wxGrid:setColLabelValue(Grid, Col, Label),
                {Col + 1, Width}
        end,
    InitCol("Activity", {?ACTIVITY_COL, ActColWidth}),
    [begin
	 [cell_set_read_only(Grid, Row, Col) ||
	     Col <- lists:seq(?ACTIVITY_COL, ?SUNDAY_COL),
	     Row =/= 1, Row =/= 2, Row =/= 4],
	 wxGrid:setCellAlignment(Grid, Row, ?ACTIVITY_COL,
				 ?wxALIGN_LEFT, ?wxALIGN_CENTER)
     end
     || Row <- lists:seq(0, Nrows)],



    lists:foldl(InitCol, {?YEAR_SUM_COL, 65}, 
                ["Year", "Week", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]),
    UnspecRow = Nrows - 1,

    %% [wxGrid:setCellTextColour(Grid, Row, ?ACTIVITY_COL, ?wxRED)
    %%  || Row <- [?DATE_ROW, ?BREAK_ROW, ?WORK_ROW, ?FLEX_ROW, UnspecRow]],
    [wxGrid:setCellTextColour(Grid, UnspecRow, Col, ?wxRED)
     || Col <- lists:seq(?MONDAY_COL, ?SUNDAY_COL)],
    wxGrid:enableEditing(Grid, true),

    %% Connect to grid
    WinPid = self(),
    Left =
	fun(Wx, EventRef) ->
		grid_cell_left_click(WinPid, UnspecRow, Wx, EventRef)
	end,
    Middle =
	fun(Wx, EventRef) ->
		grid_cell_middle_click(WinPid, Grid, UnspecRow, Wx, EventRef)
	end,
    Right =
	fun(Wx, EventRef) ->
		grid_cell_right_click(WinPid, UnspecRow, Wx, EventRef)
	end,
    wxGrid:connect(Grid, grid_cell_left_click, [{callback, Left}]),
    wxGrid:connect(Grid, grid_cell_left_dclick, [{callback, Left}]),
    wxGrid:connect(Grid, grid_cell_right_click, [{callback, Right}]),
    wxGrid:connect(Grid, grid_cell_right_dclick, [{callback, Right}]),
    wxGrid:connect(Grid, grid_cell_change),
    wxGrid:connect(Grid, size),
    KeyPress = fun(Wx, EventRef) -> key_press(WinPid, Wx, EventRef) end,
    wxGrid:connect(Grid, key_down, [{callback, KeyPress}]),
    Ignore = fun(_Wx, _EventRef) -> ignore end,
    wxGrid:connect(Grid, grid_label_left_click, [{callback, Ignore}]),
    wxGrid:connect(Grid, grid_label_left_dclick, [{callback, Ignore}]),
    GridWindow = wxGrid:getGridWindow(Grid),
    wxWindow:connect(GridWindow, middle_down, [{callback, Middle}]),
    
    wxGrid:setFocus(Grid), % Get keyboard focus
    {Grid, ActColWidth}.

cell_set_read_only(Grid, Row, Col) ->
    wxGrid:setReadOnly(Grid, Row, Col, [{isReadOnly, true}]),
    Color = wxGrid:getLabelBackgroundColour(Grid),
    wxGrid:setCellBackgroundColour(Grid, Row, Col, Color).

cell_set_read_write(Grid, Row, Col) ->
    wxGrid:setReadOnly(Grid, Row, Col, [{isReadOnly, false}]),
    wxGrid:setCellBackgroundColour(Grid, Row, Col, ?wxWHITE).

recreate_activity_type_window(#state{activity_frame = undefined} = S) ->
    S;
recreate_activity_type_window(#state{activity_frame = Frame} = S) ->
    S2 = S#state{activity_frame = undefined,
		 activity_panel = undefined},
    wx:batch(fun() ->
		     wxFrame:destroy(Frame),
		     create_activity_type_window(S2) 
	     end).

create_activity_type_window(#state{activity_frame = Frame} = S)
  when Frame =/= undefined  ->
    S;
create_activity_type_window(#state{wx = Wx, year = Eyear} = S) ->
    %% Create frame
    Title = "Eflex "++ integer_to_list(Eyear#eflex_year.no) ++
	" - activity type editor " ,
    Frame = wxFrame:new(Wx, ?ACT_FRAME_ID, Title, []),
    Panel = wxPanel:new(Frame, []),

    %% Create menubar
    MenuBar = wxMenuBar:new(),
    File    = wxMenu:new([]),
    wxMenuBar:append(MenuBar, File, "File" ),
    wxMenu:append(File,
		  ?ACT_COPY_INITIAL_ITEM,
		  "Copy initial values from end of previous year." ),
    wxMenu:append(File, ?CLOSE_ITEM, "Close" ),
    wxFrame:setMenuBar(Frame, MenuBar),
    wxEvtHandler:connect(Frame,
			 command_menu_selected,
			 [{userData, activity_type_editor}]),
    wxEvtHandler:connect(File, menu_open),

    %% Populate
    WrapperSz = wxBoxSizer:new(?wxVERTICAL),
    MainSz = wxBoxSizer:new(?wxVERTICAL),
    wxSizer:add(WrapperSz, MainSz, [{border, 2}, {flag, ?wxALL}]),
    create_activity_type_header(Panel, MainSz),
    #eflex_year{config = #eflex_config{activity_types = Etypes},
	       weeks = Eweeks} = Eyear,
    EweekList = tuple_to_list(Eweeks),
    {[Unspec | Mandatory], Attendance, Project} =
        eflex_lib:multi_split_activity_types(Etypes),
    [create_activity_type_row(Panel, MainSz, Etype, EweekList, true)
     || Etype <- Mandatory,
        Etype#eflex_activity_type.name =/= ?DATE_ACT],
    create_activity_type_row(Panel, MainSz, Unspec, EweekList, true),
    [create_activity_type_row(Panel, MainSz, Etype, EweekList, false)
     || Etype <- Attendance],
    [create_activity_type_row(Panel, MainSz, Etype, EweekList, false)
     || Etype <- Project],

    %% Hints
    refresh_sizer(Frame, Panel, WrapperSz),
    wxFrame:connect(Frame, close_window),
    wxFrame:show(Frame),
    S#state{activity_frame = Frame, activity_panel = Panel}.

create_activity_type_header(Panel, OuterSizer) ->
    Sizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(OuterSizer, Sizer,
                [{flag, ?wxEXPAND bor ?wxALL bor ?wxRIGHT}, {border, 2}]),  
    Height = 25,
    NameSize = {?MIN_ACT_NAME_LEN, Height},
    Name = wxStaticText:new(Panel, ?wxID_ANY,  "Activity type",
			    [{size, NameSize},
			     {style, ?wxTE_READONLY}]),
    IntSize = {80, Height},
    Init = wxStaticText:new(Panel, ?wxID_ANY, "Initial",
			    [{size, IntSize},
			     {style, ?wxTE_READONLY}]),
    Share = wxStaticText:new(Panel, ?wxID_ANY, "Share",
			     [{size, IntSize},
			      {style, ?wxTE_READONLY}]),
    wxSizer:add(Sizer, Name, [{flag, ?wxALIGN_CENTER_VERTICAL}]),
    wxSizer:add(Sizer, Init, [{flag, ?wxALIGN_CENTER_VERTICAL}]),
    wxSizer:add(Sizer, Share, [{flag, ?wxALIGN_CENTER_VERTICAL}]).

create_activity_type_row(Panel, OuterSizer, Etype, EweekList, IsMandatory) ->
    NameStr = activity_value(Etype, name),
    MatchingActs =
	[{No, EA} || #eflex_week{no = No, activities = Acts} <- EweekList,
		     EA <- Acts,
		     EA#eflex_activity.name =:= NameStr],
    Height = -1,
    NameSize = {?MIN_ACT_NAME_LEN, Height},
    IntSize = {80, Height},
    ChoiceSize = {-1, Height},
    ButtonSize = {-1, Height},
    InnerSizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(OuterSizer, InnerSizer,
                [{flag, ?wxEXPAND bor ?wxRIGHT}]),  
    AD = #activity_data{type = Etype, inner = InnerSizer, outer = OuterSizer},

    %% Name
    Name = 
	if
	    IsMandatory =:= false ->
		wxTextCtrl:new(Panel, ?wxID_ANY, [{size, NameSize},
						  {style, ?wxTE_PROCESS_ENTER}]);
	    true ->
		TextCtrl = wxTextCtrl:new(Panel, ?wxID_ANY,
					  [{size, NameSize},
					   {style, ?wxTE_READONLY}]),
		wxTextCtrl:setForegroundColour(TextCtrl, ?wxRED),
		TextCtrl
	end,
    wxTextCtrl:setValue(Name, NameStr),
    wxTextCtrl:connect(Name, command_text_enter, 
		       [{userData, AD#activity_data{label = name}}]),    
    wxSizer:add(InnerSizer, Name, [{flag, ?wxEXPAND}]),

    %% Initial
    Initial = wxTextCtrl:new(Panel,
                             ?wxID_ANY,
                             [{size, IntSize},
			      {style, ?wxTE_PROCESS_ENTER bor ?wxALIGN_RIGHT}]),
    wxTextCtrl:setValue(Initial, activity_value(Etype, initial)),
    wxTextCtrl:connect(Initial, command_text_enter, 
		       [{userData, AD#activity_data{label = initial}}]),    
    wxSizer:add(InnerSizer, Initial, [{flag, ?wxEXPAND}]),

    %% Share
    ShareStyle0 = ?wxTE_PROCESS_ENTER bor ?wxALIGN_RIGHT,
    {ShareStyle, Color} =
	if
	    IsMandatory; Etype#eflex_activity_type.category =:= attendance ->
		{ShareStyle0 bor ?wxTE_READONLY, ?wxRED};
	    true -> {ShareStyle0, ?wxBLACK}
	end,
    Share = wxTextCtrl:new(Panel,
			   ?wxID_ANY,
			   [{size, IntSize},
			    {style, ShareStyle}]),
    wxTextCtrl:setForegroundColour(Share, Color),
    wxTextCtrl:setValue(Share, activity_value(Etype, share)),
    wxTextCtrl:connect(Share, command_text_enter, 
		       [{userData, AD#activity_data{label = share}}]),    
    wxSizer:add(InnerSizer, Share, [{flag, ?wxEXPAND}]),

    %% Unit
    Unit = wxChoice:new(Panel,
			?wxID_ANY,
			[{size, ChoiceSize},
			 {choices, ["Minutes", "Decimal"]}]),
    wxControlWithItems:setStringSelection(Unit, activity_value(Etype, unit)),
    wxEvtHandler:connect(Unit, command_choice_selected,
			 [{userData, AD#activity_data{label = unit}}]),
    wxSizer:add(InnerSizer, Unit, [{proportion, 1}]),

    case IsMandatory of
	true ->
	    Line = wxStaticLine:new(Panel, [{style, ?wxLI_VERTICAL}]),
	    wxSizer:add(InnerSizer, Line, [{proportion, 4}]);
	false ->
            %% Sign
            Sign = wxChoice:new(Panel,
				?wxID_ANY,
				[{size, ChoiceSize},
				 {choices,["Positive", "Negative"]}]),
            wxControlWithItems:setStringSelection(Sign,
						  activity_value(Etype, sign)),
	    wxEvtHandler:connect(Sign, command_choice_selected,
				 [{userData, AD#activity_data{label = sign}}]),
            wxSizer:add(InnerSizer, Sign, [{proportion, 1}]),

            %% Visibility
            Visibility = wxChoice:new(Panel,
				      ?wxID_ANY,
				      [{size, ChoiceSize},
				       {choices, ["Visible", "Hidden"]}]),
            wxControlWithItems:setStringSelection(Visibility,
						  activity_value(Etype,
								 visibility)),
	    wxEvtHandler:connect(Visibility, command_choice_selected, 
				 [{userData,
				   AD#activity_data{label = visibility}}]),
            wxSizer:add(InnerSizer, Visibility, [{proportion, 1}]),

            %% Category
            Category = wxChoice:new(Panel,
				    ?wxID_ANY,
				    [{size, ChoiceSize},
				     {choices,["Project", "Attendance"]}]),
            wxControlWithItems:setStringSelection(Category,
						  activity_value(Etype, 
								 category)),
	    wxEvtHandler:connect(Category, command_choice_selected,
				 [{userData,
				   AD#activity_data{label = category}}]),
            wxSizer:add(InnerSizer, Category, [{proportion, 1}]),

            %% Clone
            Copy = wxButton:new(Panel,
				?CLONE_BUTTON,
				[{label, "Clone activity"}, 
				 {size, ButtonSize}]),
	    wxEvtHandler:connect(Copy, command_button_clicked,
				 [{userData, 
				   AD#activity_data{label = clone}}]),
            wxSizer:add(InnerSizer, Copy, [{proportion, 1}])
    end,
    %% Delete/view
    case length(MatchingActs) of
	0 ->
	    %% Delete
	    Delete = wxButton:new(Panel, ?DELETE_BUTTON,
				  [{label, "Delete type"}, 
				   {size, ButtonSize}]),
	    wxEvtHandler:connect(Delete,
				 command_button_clicked,
				 [{userData, 
				   AD#activity_data{label = delete}}]),
	    wxSizer:add(InnerSizer, Delete, [{proportion, 1}]);
	N ->
	    %% View
	    Label = lists:concat(["View ", N, " activities"]),
	    View = wxButton:new(Panel, ?VIEW_BUTTON,
				[{label, Label}, {size, ButtonSize}]),
	    wxEvtHandler:connect(View, 
				 command_button_clicked,
				 [{userData,
				   AD#activity_data{
				     label = {view, NameStr}}}]),
	    wxSizer:add(InnerSizer, View, [{proportion, 1}])
    end.    

activity_value(Etype, Type) ->
    case Type of
        name ->
            Etype#eflex_activity_type.name;
        initial ->
	    eflex_lib:convert_time(Etype#eflex_activity_type.initial,
				   Etype#eflex_activity_type.unit);
        share ->
	    integer_to_list(Etype#eflex_activity_type.share);
        unit ->
	    unit_to_string(Etype#eflex_activity_type.unit);
        category ->
            case Etype#eflex_activity_type.category of
                project    -> "Project";
                attendance -> "Attendance"
            end;
        sign ->
            case Etype#eflex_activity_type.sign of
                negative -> "Negative";
                positive -> "Positive"
            end;
        visibility ->
            case Etype#eflex_activity_type.visibility of
                hidden  -> "Hidden";
                visible -> "Visible"
            end
    end.

unit_to_string(Unit) when is_atom(Unit) ->
    [H | T] = atom_to_list(Unit),
    [string:to_upper(H) | string:to_lower(T)].

recreate_config_window(#state{config_frame = undefined} = S) ->
    S;
recreate_config_window(#state{config_frame = Frame} = S) ->
    S2 = S#state{config_frame = undefined,
		 config_panel = undefined},
    wx:batch(fun() ->
		     wxFrame:destroy(Frame),
		     create_config_window(S2) 
	     end).

create_config_window(#state{config_frame = Frame} = S) 
  when Frame =/= undefined->
    S;
create_config_window(#state{wx = Wx, year = Eyear} = S) ->
    %% Create frame
    Title = "Eflex "++ integer_to_list(Eyear#eflex_year.no) ++
	" - config editor" ,
    Frame = wxFrame:new(Wx, ?CONFIG_FRAME_ID, Title, []),
    Panel = wxPanel:new(Frame, []),

    %% Create menubar
    MenuBar = wxMenuBar:new(),
    File    = wxMenu:new([]),
    wxMenuBar:append(MenuBar, File, "File" ),
    wxMenu:append(File, ?CONFIG_COPY_ITEM,
		  "Copy configuration from previous year." ),
    wxMenu:append(File, ?CLOSE_ITEM, "Close" ),
    wxFrame:setMenuBar(Frame, MenuBar),
    wxEvtHandler:connect(Frame,
			 command_menu_selected, 
			 [{userData, config_editor}]),
    wxEvtHandler:connect(File, menu_open),

    %% Populate
    Econfig = Eyear#eflex_year.config,
    Etypes = Econfig#eflex_config.activity_types,
    #eflex_config{unit = Unit,
		  working_time = Work,
		  lunch_duration = Lunch,
		  workday_break = Wbreak,
		  freeday_break = Fbreak,
		  absence_all_day = Abs,
		  unspecified_work = Unspec,
		  mini_break = MiniBreak} = Econfig,
    #eflex_working_time{monday = Mon,
			tuesday = Tue,
			wednesday = Wed,
			thursday = Thu,
			friday = Fri,
			saturday = Sat,
			sunday = Sun} = Work,
    #eflex_break{duration = Wdur,
		 time_before_break = Wbefore} = Wbreak,
    #eflex_break{duration = Fdur,
		 time_before_break = Fbefore} = Fbreak,

    MainSizer = wxBoxSizer:new(?wxVERTICAL),

    %% Unit
    GeneralBox = wxStaticBox:new(Panel, ?wxID_ANY, ?GENERAL),
    GeneralSizer = wxStaticBoxSizer:new(GeneralBox, ?wxVERTICAL),
    wxSizer:add(MainSizer, GeneralSizer,
		[{border,5}, {flag, ?wxALL bor ?wxEXPAND}]),
    create_config_choice(Panel, GeneralSizer, ?GENERAL, ?UNIT_CONFIG_ITEM,
			 unit_to_string(Unit), ["Minutes", "Decimal"]),

    %% Work time per day
    wxSizer:addStretchSpacer(MainSizer),
    WorkBox = wxStaticBox:new(Panel, ?wxID_ANY, ?WORK_TIME_PER_DAY),
    WorkTimeSizer = wxStaticBoxSizer:new(WorkBox, ?wxVERTICAL),
    wxSizer:add(MainSizer, WorkTimeSizer,
		[{border,5}, {flag, ?wxALL bor ?wxEXPAND}]),
    create_config_text(Panel, WorkTimeSizer, Unit,
		       ?WORK_TIME_PER_DAY, ?MON_CONFIG_ITEM, Mon),
    create_config_text(Panel, WorkTimeSizer, Unit,
		       ?WORK_TIME_PER_DAY, ?TUE_CONFIG_ITEM, Tue),
    create_config_text(Panel, WorkTimeSizer, Unit,
		       ?WORK_TIME_PER_DAY, ?WED_CONFIG_ITEM, Wed),
    create_config_text(Panel, WorkTimeSizer, Unit,
		       ?WORK_TIME_PER_DAY, ?THU_CONFIG_ITEM, Thu),
    create_config_text(Panel, WorkTimeSizer, Unit,
		       ?WORK_TIME_PER_DAY, ?FRI_CONFIG_ITEM, Fri),
    create_config_text(Panel, WorkTimeSizer, Unit,
		       ?WORK_TIME_PER_DAY, ?SAT_CONFIG_ITEM, Sat),
    create_config_text(Panel, WorkTimeSizer, Unit,
		       ?WORK_TIME_PER_DAY, ?SUN_CONFIG_ITEM, Sun),

    %% Workday
    wxSizer:addStretchSpacer(MainSizer),
    WorkdayBox = wxStaticBox:new(Panel, ?wxID_ANY, ?WORKDAY),
    WorkdaySizer = wxStaticBoxSizer:new(WorkdayBox, ?wxVERTICAL),
    wxSizer:add(MainSizer, WorkdaySizer,
		[{border,5}, {flag, ?wxALL bor  ?wxEXPAND}]),
    create_config_text(Panel, WorkdaySizer, Unit,
		       ?WORKDAY, ?LUNCH_CONFIG_ITEM, Lunch),
    create_config_text(Panel, WorkdaySizer, Unit,
		       ?WORKDAY, ?BREAK_CONFIG_ITEM, Wdur),
    create_config_text(Panel, WorkdaySizer, Unit,
		       ?WORKDAY, ?BEFORE_CONFIG_ITEM, Wbefore),

    %% Freeday
    wxSizer:addStretchSpacer(MainSizer),
    FreedayBox = wxStaticBox:new(Panel, ?wxID_ANY, ?FREEDAY),
    FreedaySizer = wxStaticBoxSizer:new(FreedayBox, ?wxVERTICAL),
    wxSizer:add(MainSizer, FreedaySizer,
		[{border,5}, {flag, ?wxALL bor ?wxEXPAND}]),
    create_config_text(Panel, FreedaySizer, Unit,
		       ?FREEDAY, ?BREAK_CONFIG_ITEM, Fdur),
    create_config_text(Panel, FreedaySizer, Unit,
		       ?FREEDAY, ?BEFORE_CONFIG_ITEM, Fbefore),

    %% Auto code
    wxSizer:addStretchSpacer(MainSizer),
    AutoBox = wxStaticBox:new(Panel, ?wxID_ANY, ?AUTO_CODE),
    AutoSizer = wxStaticBoxSizer:new(AutoBox, ?wxVERTICAL),
    wxSizer:add(MainSizer, AutoSizer,
		[{border,5}, {flag, ?wxALL bor ?wxEXPAND}]),
    {_Mandatory, Attendance, Project} =
	eflex_lib:multi_split_activity_types(Etypes),
    Achoices = [activity_value(Etype, name)||
		   Etype <- Attendance,
		   Etype#eflex_activity_type.sign =:= positive],
    Uchoices = [activity_value(Etype, name)||
		   Etype <- Project,
		   Etype#eflex_activity_type.sign =:= positive],
    Abs2 =
	case Abs of
	    undefined -> ?FLEX_ACT;
	    _ -> Abs
	end,		
    create_config_choice(Panel, AutoSizer, ?AUTO_CODE, ?ABSENCE_CONFIG_ITEM,
			 Abs2, [?FLEX_ACT | Achoices]),
    Unspec2 =
	case Unspec of
	    undefined -> ?UNSPEC_ACT;
	    _ -> Unspec
	end,		
    create_config_choice(Panel, AutoSizer, ?AUTO_CODE, ?UNSPEC_CONFIG_ITEM,
			 Unspec2, [?UNSPEC_ACT | Uchoices]),

    create_config_text(Panel, MainSizer, Unit,
		       ?MINIBREAK, ?BEFORE_CONFIG_ITEM, MiniBreak),
    %% Hints
    refresh_sizer(Frame, Panel, MainSizer),
    wxFrame:connect(Frame, close_window),
    wxFrame:show(Frame),
    S#state{config_frame = Frame, config_panel = Panel}.

create_config_text(Panel, OuterSizer, Unit, BoxLabel, Label, Val) ->
    InnerSizer = create_config_header(Panel, OuterSizer, Label),
    Input = wxTextCtrl:new(Panel, ?wxID_ANY,
			   [{size, {-1, -1}},
			    {style, ?wxTE_PROCESS_ENTER bor ?wxALIGN_RIGHT}]),
    Initial =
	if
	    is_integer(Val) ->
                eflex_lib:convert_time(Val, Unit);
	    is_list(Val) ->
		Val;
	    Val =:= undefined ->
		""
	end,
    wxTextCtrl:setValue(Input, Initial),
    CD = #config_data{label = BoxLabel, text = Label, initial = Val},
    wxTextCtrl:connect(Input, command_text_enter, [{userData, CD}]),
    wxSizer:add(InnerSizer, Input, [{flag, ?wxEXPAND}]).

create_config_header(Panel, OuterSizer, Label) ->
    InnerSizer = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(OuterSizer, InnerSizer, [{flag, ?wxEXPAND}, {border, 2}]),
    Text = wxStaticText:new(Panel, ?wxID_ANY,  Label, []),
    wxSizer:add(InnerSizer, Text, [{flag, ?wxALIGN_CENTER_VERTICAL}]),
    wxSizer:addSpacer(InnerSizer, 10),
    wxSizer:addStretchSpacer(InnerSizer),
    InnerSizer.

create_config_choice(Panel, OuterSizer, BoxLabel, Label, Val, Choices) ->
    InnerSizer = create_config_header(Panel, OuterSizer, Label),
    Choice = wxChoice:new(Panel,
			  ?wxID_ANY,
			  [{size, {-1, -1}},
			   {choices, Choices}]),
    CD = #config_data{label = BoxLabel, text = Label, initial = Val},
    wxControlWithItems:setStringSelection(Choice, Val),
    wxEvtHandler:connect(Choice, command_choice_selected, [{userData, CD}]),
    wxSizer:add(InnerSizer, Choice, [{flag, ?wxEXPAND}]).

recreate_holiday_window(#state{holiday_frame = undefined} = S) ->
    S;
recreate_holiday_window(#state{holiday_frame = Frame} = S) ->
    S2 = S#state{holiday_frame = undefined,
		 holiday_panel = undefined,
		 holiday_cals  = []},
    wx:batch(fun() ->
		     wxFrame:destroy(Frame),
		     create_holiday_window(S2) 
	     end).

create_holiday_window(#state{holiday_frame = Frame} = S)
  when Frame =/= undefined->
    S;
create_holiday_window(#state{wx = Wx, year = Eyear} = S) ->
    %% Create frame
    YearNo = Eyear#eflex_year.no,
    Title = "Eflex "++ integer_to_list(YearNo) ++ 
	" - holiday editor" ,
    Frame = wxFrame:new(Wx, ?HOLIDAY_FRAME_ID, Title, []),
    Panel = wxPanel:new(Frame, []),

    %% Create menubar
    MenuBar = wxMenuBar:new(),
    File    = wxMenu:new([]),
    wxMenuBar:append(MenuBar, File, "File" ),
    wxMenu:append(File, ?HOLIDAY_COPY_ITEM,
		  "Copy holidays from previous year" ),
    wxMenu:append(File, ?CLOSE_ITEM, "Close" ),
    wxFrame:setMenuBar(Frame, MenuBar),
    wxEvtHandler:connect(Frame,
			 command_menu_selected,
			 [{userData, holiday_editor}]),
    wxEvtHandler:connect(File, menu_open),

    %% Populate
    Econfig = Eyear#eflex_year.config,
    Holidays = Econfig#eflex_config.holidays,
    CalFun =
	fun(MonthNo, Sz) ->
		Opts =
		    [{date, {{YearNo, MonthNo, 1} ,{1,1,1}}},
		     {style, ?wxCAL_MONDAY_FIRST bor 
		      ?wxCAL_SHOW_HOLIDAYS bor
		      ?wxCAL_NO_YEAR_CHANGE bor
		      ?wxCAL_NO_MONTH_CHANGE bor
		      %%?wxCAL_SHOW_SURROUNDING_WEEKS bor
		      ?wxCAL_SEQUENTIAL_MONTH_SELECTION}],
		Cal = wxCalendarCtrl:new(Panel, ?wxID_ANY, Opts),
		SetHoliday =
		    fun(DayNo) ->
			    Attr = wxCalendarDateAttr:new(),
			    H = #eflex_holiday{month = MonthNo, day = DayNo},
			    IsHoliday = lists:member(H, Holidays),
			    wxCalendarDateAttr:setHoliday(Attr, IsHoliday),
			    wxCalendarCtrl:setAttr(Cal, DayNo, Attr)
		    end,
		LastDayNo = calendar:last_day_of_the_month(YearNo, MonthNo),
		lists:foreach(SetHoliday, lists:seq(1, LastDayNo)),
		select_cal_date(Cal),
		wxEvtHandler:connect(Cal, calendar_sel_changed,
				     [{userData, #calendar_data{month = MonthNo}}]),
		wxSizer:add(Sz, Cal, [{flag, ?wxEXPAND}]),
		{MonthNo, Cal}
	end,

    MainSizer = wxBoxSizer:new(?wxVERTICAL),
    Q1 = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(MainSizer, Q1,
		[{border, 5}, {flag, ?wxALL bor ?wxEXPAND}]),
    C1 = CalFun(1, Q1),
    wxSizer:addStretchSpacer(Q1),
    wxSizer:add(Q1, wxStaticLine:new(Panel, [{style, ?wxLI_VERTICAL}])),
    C2 = CalFun(2, Q1),
    wxSizer:addStretchSpacer(Q1),
    wxSizer:add(Q1, wxStaticLine:new(Panel, [{style, ?wxLI_VERTICAL}])),
    C3 = CalFun(3, Q1),

    Q2 = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(MainSizer, Q2,
		[{border, 5}, {flag, ?wxALL bor ?wxEXPAND}]),
    C4 = CalFun(4, Q2),
    wxSizer:addStretchSpacer(Q2),
    wxSizer:add(Q2, wxStaticLine:new(Panel, [{style, ?wxLI_VERTICAL}])),
    C5 = CalFun(5, Q2),
    wxSizer:addStretchSpacer(Q2),
    wxSizer:add(Q2, wxStaticLine:new(Panel, [{style, ?wxLI_VERTICAL}])),
    C6 = CalFun(6, Q2),

    Q3 = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(MainSizer, Q3,
		[{border, 5}, {flag, ?wxALL bor ?wxEXPAND}]),
    C7 = CalFun(7, Q3),
    wxSizer:addStretchSpacer(Q3),
    wxSizer:add(Q3, wxStaticLine:new(Panel, [{style, ?wxLI_VERTICAL}])),
    C8 = CalFun(8, Q3),
    wxSizer:addStretchSpacer(Q3),
    wxSizer:add(Q3, wxStaticLine:new(Panel, [{style, ?wxLI_VERTICAL}])),
    C9 = CalFun(9, Q3),

    Q4 = wxBoxSizer:new(?wxHORIZONTAL),
    wxSizer:add(MainSizer, Q4,
		[{border, 5}, {flag, ?wxALL bor ?wxEXPAND}]),
    C10 = CalFun(10, Q4),
    wxSizer:addStretchSpacer(Q4),
    wxSizer:add(Q4, wxStaticLine:new(Panel, [{style, ?wxLI_VERTICAL}])),
    C11 = CalFun(11, Q4),
    wxSizer:addStretchSpacer(Q4),
    wxSizer:add(Q4, wxStaticLine:new(Panel, [{style, ?wxLI_VERTICAL}])),
    C12 = CalFun(12, Q4),

    %% Hints
    refresh_sizer(Frame, Panel, MainSizer),
    wxFrame:connect(Frame, close_window),
    wxFrame:show(Frame),
    Cals = [C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12],
    S#state{holiday_frame = Frame,
	    holiday_panel = Panel,
	    holiday_cals = Cals}.

select_cal_date(Cal) ->
    {{Y, M, _D}, Time} = wxCalendarCtrl:getDate(Cal),
    Fun = fun(DayNo) ->
		  Attr = wxCalendarCtrl:getAttr(Cal, DayNo),
		  case wxCalendarDateAttr:isHoliday(Attr) of
		      false ->
			  wxCalendarCtrl:setDate(Cal, {{Y, M, DayNo}, Time}),
			  throw(done);
		      true ->
			  try_next
		  end
	  end,
    LastDayNo = calendar:last_day_of_the_month(Y, M),
    catch lists:foreach(Fun, lists:seq(1, LastDayNo)).
    
recreate_activity_list_windows(#state{year = Eyear} = S0) ->
    Econfig = Eyear#eflex_year.config,
    Etypes = Econfig#eflex_config.activity_types,
    Fun =
	fun(#eflex_activity_type{frame = undefined}, S) ->
		S;
	   (#eflex_activity_type{frame = Frame,
				 name = ActName} = Etype, S) ->
		wxFrame:destroy(Frame),
		do_create_activity_list_window(S, Etype, ActName) 		
	end,
    wx:batch(fun() ->
		     lists:foldl(Fun, S0, Etypes)
	     end).

create_activity_list_window(#state{year = Eyear} = S, ActName) ->
    Econfig = Eyear#eflex_year.config,
    Etypes = Econfig#eflex_config.activity_types,
    Etype = ?KEYSEARCH(ActName,
		       #eflex_activity_type.name,
		       Etypes),
    case Etype#eflex_activity_type.frame of
	undefined ->
	    do_create_activity_list_window(S, Etype, ActName);
	_ ->
	    S
    end.

do_create_activity_list_window(#state{wx = Wx, year = Eyear} = S, Etype, ActName) ->
    %% Create frame
    Title = "Eflex "++ integer_to_list(Eyear#eflex_year.no) ++ " - " ++ ActName,
    Frame = wxFrame:new(Wx, ?LIST_FRAME_ID, Title, []),
    Panel = wxPanel:new(Frame, []),

    %% Create menubar
    MenuBar = wxMenuBar:new(),
    File    = wxMenu:new([]),
    wxMenuBar:append(MenuBar, File, "File" ),
    wxMenu:append(File, ?CLOSE_ITEM, "Close" ),
    wxFrame:setMenuBar(Frame, MenuBar),
    wxEvtHandler:connect(Frame,
			 command_menu_selected,
			 [{userData, #activity_list{name = ActName}}]),
    wxEvtHandler:connect(File, menu_open),

    %% Create grid
    Initial = Etype#eflex_activity_type.initial,
    Unit = Etype#eflex_activity_type.unit,
    MainSz = wxBoxSizer:new(?wxVERTICAL),
    Grid = create_list_grid(Panel, Eyear, ActName, Initial, Unit),
    wxSizer:add(MainSz, Grid, [{border, 2},
			       {flag, ?wxALL bor ?wxEXPAND},
			       {proportion, 1}]),
    wxGrid:connect(Grid, grid_cell_left_click, [{userData, week_selected}]),
    wxGrid:connect(Grid, grid_cell_right_click, [{userData, week_selected}]),
    
    %% Hints
    refresh_sizer(Frame, Panel, MainSz),
    wxFrame:connect(Frame, close_window,
		    [{userData, #activity_list{name = ActName}}]),
    wxFrame:show(Frame),

    Econfig = Eyear#eflex_year.config,
    Etypes = Econfig#eflex_config.activity_types,
    Etype2 = Etype#eflex_activity_type{frame = Frame, panel = Panel},
    Etypes2 = lists:keyreplace(ActName, #eflex_activity_type.name, Etypes, Etype2),
    Econfig2 = Econfig#eflex_config{activity_types = Etypes2}, 
    Eyear2 = Eyear#eflex_year{config = Econfig2},
    S#state{year = Eyear2}.

create_list_grid(Panel, Eyear, ActName, Initial, Unit) ->
    EweekList = tuple_to_list(Eyear#eflex_year.weeks),
    MatchingActs =
	[{No, EA} || #eflex_week{no = No, activities = Acts} <- EweekList,
		     EA <- Acts,
		     EA#eflex_activity.name =:= ActName],

    Grid = wxGrid:new(Panel, ?wxID_ANY, [{size, {-1, 400}}]),
    %% Grid = wxGrid:new(Panel, ?wxID_ANY, []),
    Nrows = length(MatchingActs) + 1,
    wxGrid:createGrid(Grid, Nrows, ?SUNDAY_COL + 1,
                      [{selmode, ?wxGrid_wxGridSelectCells}]),
    wxGrid:setRowLabelSize(Grid, 0),
    wxGrid:disableDragColSize(Grid),
    wxGrid:disableDragRowSize(Grid),

    %% Populate grid
    ColWidth = 60,
    InitCol =
        fun(Label, Col) ->
                wxGrid:setColMinimalWidth(Grid, Col, ColWidth),
                wxGrid:setColSize(Grid, Col, ColWidth),
                wxGrid:setColLabelValue(Grid, Col, Label),
                Col + 1
        end,
    lists:foldl(InitCol, 0,
		["Week no", "Year", "Week", 
		 "Mon", "Tue", "Wed", "Thu",
		 "Fri", "Sat", "Sun"]),
    SetCell =
	fun(_Row, _Col, undefined) ->
		ignore;
	   (_Row, _Col, 0) ->
		ignore;
	   (Row, Col, Int) ->
		Val = eflex_lib:convert_time(Int, Unit),
		wxGrid:setCellValue(Grid, Val, Row, Col)
	end,
    InitWeek =
	fun({WeekNo, EA}, {Row, YearSum}) ->
		WeekSum = eflex_lib:calc_week_sum(EA),
		YearSum2 = YearSum + WeekSum,
		WeekStr = integer_to_list(WeekNo),
		wxGrid:setCellValue(Grid, WeekStr, Row, ?WEEK_NO_COL),
		SetCell(Row, ?YEAR_SUM_COL, YearSum2),
		SetCell(Row, ?WEEK_SUM_COL, WeekSum),
		SetCell(Row, ?MONDAY_COL, EA#eflex_activity.monday),
		SetCell(Row, ?MONDAY_COL + 1, EA#eflex_activity.tuesday),
		SetCell(Row, ?MONDAY_COL + 2, EA#eflex_activity.wednesday),
		SetCell(Row, ?MONDAY_COL + 3, EA#eflex_activity.thursday),
		SetCell(Row, ?MONDAY_COL + 4, EA#eflex_activity.friday),
		SetCell(Row, ?MONDAY_COL + 5, EA#eflex_activity.saturday),
		SetCell(Row, ?MONDAY_COL + 6, EA#eflex_activity.sunday),
		{Row + 1, YearSum2}
	end,
    wxGrid:setCellValue(Grid, "Initial", 0, 0),
    wxGrid:setCellValue(Grid, eflex_lib:convert_time(Initial, Unit), 0, 1),
    lists:foldl(InitWeek, {1, Initial}, MatchingActs),

    wxGrid:enableEditing(Grid, false),

    Grid.

grid_cell_left_click(WinPid,
		     UnspecRow,
		     #wx{event = #wxGrid{type = _Type,
					 row = Row,
					 col = Col}},
		     EventRef) ->
    if
	Col =:= ?ACTIVITY_COL ->
	    WinPid ! {select_activity, Row, Col};
	Col =:= ?YEAR_SUM_COL -> % Row =:= ?DATE_ROW
	    WinPid ! {select_year, Row, Col};
	Col =:= ?WEEK_SUM_COL -> % Row =:= ?DATE_ROW
	    WinPid ! {select_week, Row, Col};
	Row =:= ?DATE_ROW ->
	    WinPid ! {toggle_holiday, Row, Col};
	Row =:= ?BREAK_ROW ->
	    WinPid ! {copy_cell, Row, Col};
	Row =:= ?WORK_ROW ->
	    WinPid ! {copy_cell, Row, Col};
	Row =:= ?FLEX_ROW ->
	    WinPid ! {copy_cell, Row, Col};
	Row =:= UnspecRow ->
	    WinPid ! {copy_cell, Row, Col};
	true ->
	    WinPid ! {copy_cell, Row, Col},
	    wxEvent:skip(EventRef)
    end.

grid_cell_middle_click(WinPid,
		       Grid,
		       UnspecRow,
		       #wx{event = #wxMouse{type = middle_down,
					    x = X,
					    y = Y}} = Wx,
		       EventRef) ->
    Col = wxGrid:xToCol(Grid, X),
    Row = wxGrid:yToRow(Grid, Y),
    Fake = #wxGrid{type = grid_cell_right_click,
		   row = Row,
		   col = Col},
    grid_cell_right_click(WinPid,
			  UnspecRow,
			  Wx#wx{event = Fake},
			  EventRef).

grid_cell_right_click(WinPid,
		      UnspecRow,
		      #wx{event = #wxGrid{type = _Type,
					  row = Row,
					  col = Col}} = Wx,
		      EventRef) ->
    if
        Row =/= ?DATE_ROW,
        Row =/= ?BREAK_ROW,
        Row =/= ?WORK_ROW,
        Row =/= ?FLEX_ROW,
        Row =/= UnspecRow,
        Col =/= ?ACTIVITY_COL,
        Col =/= ?YEAR_SUM_COL,
        Col =/= ?WEEK_SUM_COL ->
	    WinPid ! {paste_cell, Row, Col};
	true ->
	    grid_cell_left_click(WinPid,
				 UnspecRow,
				 Wx,
				 EventRef)
    end.

key_press(WinPid,
	  #wx{event = #wxKey{type        = key_down,
			     keyCode     = Key,
			     controlDown = Control,
			     shiftDown   = Shift}},
	  EventRef) ->
    case Key of
	?WXK_HOME ->
	    WinPid ! {week_selected, eflex_lib:week_of_the_year(date())};
	?WXK_END ->
	    WinPid ! auto_code;
	?WXK_PAGEUP when Control, Shift ->
	    WinPid ! {week_selected, {year,  previous}};
	?WXK_PAGEUP when Control ->
	    WinPid ! {week_selected, {month, previous}};
	?WXK_PAGEUP when Shift ->
	    WinPid ! {week_selected, {month, previous}};
	?WXK_PAGEUP ->
	    WinPid ! {week_selected, {week,  previous}};
	?WXK_PAGEDOWN when Control, Shift ->
	    WinPid ! {week_selected, {year,  next}};
	?WXK_PAGEDOWN when Control ->
	    WinPid ! {week_selected, {month, next}};
	?WXK_PAGEDOWN when Shift ->
	    WinPid ! {week_selected, {month, next}};
	?WXK_PAGEDOWN ->
	    WinPid ! {week_selected, {week,  next}};
	_ ->
	    wxEvent:skip(EventRef)
    end.
    
holiday_popup(S, Date) ->
    {YearNo, WeekNo} = eflex_lib:week_of_the_year(Date),
    MondayDate = eflex_lib:monday_of_the_week({YearNo, WeekNo}),
    MondayDayNo = calendar:date_to_gregorian_days(MondayDate),
    DayNo = calendar:date_to_gregorian_days(Date),
    WeekDayNo = DayNo - MondayDayNo + 1,
    holiday_popup2(S, MondayDayNo, WeekDayNo).
    
holiday_popup(#state{options = Options} = S, _Row, Col) ->
    {YearNo, WeekNo} = eflex_lib:week_of_the_year(Options#options.date),
    MondayDate = eflex_lib:monday_of_the_week({YearNo, WeekNo}),
    MondayDayNo = calendar:date_to_gregorian_days(MondayDate),
    WeekDayNo = Col - 2,
    holiday_popup2(S, MondayDayNo, WeekDayNo).

holiday_popup2(#state{main_frame = Frame, year = Eyear} = S,
	       MondayDayNo, WeekDayNo) ->
    wxStatusBar:setStatusText(S#state.main_status_bar, ""),
    SelectedDayNo = MondayDayNo + WeekDayNo - 1,
    SelectedDate = calendar:gregorian_days_to_date(SelectedDayNo),
    Econfig = Eyear#eflex_year.config,
    Ignore = 0,
    Toggle = 1,
    PopupMenu = wxMenu:new(),
    Label =
	case eflex_lib:lookup_worktime(WeekDayNo, Econfig) of
	    0 -> "Free day";
	    _ -> "Work day"
	end,
    Items =
	case eflex_lib:is_holiday(MondayDayNo, WeekDayNo, Econfig) of
	    true  ->
		wxMenu:append(PopupMenu, Ignore, "Holiday"),
		wxMenu:appendSeparator(PopupMenu),
		wxMenu:append(PopupMenu, Ignore, "Keep as holiday"),
		wxMenu:append(PopupMenu, Toggle,
			      "Make " ++ string:to_lower(Label)),
		[make_normal];
	    false ->
		wxMenu:append(PopupMenu, Ignore, Label),
		wxMenu:appendSeparator(PopupMenu),
		wxMenu:append(PopupMenu, Ignore,
			      "Keep as " ++ string:to_lower(Label)),
		wxMenu:append(PopupMenu, Toggle, "Make holiday"),		
		[make_holiday]
	end,

    wxEvtHandler:connect(PopupMenu, command_menu_selected),
    %% wxWindow:popupMenu(Frame, PopupMenu, X, Y),
    wxWindow:popupMenu(Frame, PopupMenu),
    S#state{popup = #holiday_toggled{items = Items, date = SelectedDate}}.

holiday_toggled(#state{year = #eflex_year{config = Econfig} = Eyear,
		       holiday_cals = Cals} = S, 
		{_YearNo, MonthNo, DayNo}, Action) ->
    %% io:format("Toggle holiday: ~p\n", [{{_YearNo, MonthNo, DayNo}, Action}]),
    Eholiday = #eflex_holiday{day = DayNo, month = MonthNo},
    Eholidays = Econfig#eflex_config.holidays,
    Eholidays2 =
	case Action of
	    make_normal  -> Eholidays -- [Eholiday];
	    make_holiday -> lists:usort([Eholiday | Eholidays])
	end,
    Econfig2 = Econfig#eflex_config{holidays = Eholidays2},
    Eyear2 = Eyear#eflex_year{config = Econfig2},
    S2 = S#state{year = Eyear2},
    case lists:keysearch(MonthNo, 1, Cals) of
	{value, {_, Cal}} ->
	    Attr = wxCalendarCtrl:getAttr(Cal, DayNo),
	    IsHoliday = lists:member(Eholiday, Eholidays2),
	    wxCalendarDateAttr:setHoliday(Attr, IsHoliday),
	    select_cal_date(Cal),
	    S2;
	false ->
	    S2
    end.

year_popup(#state{main_frame = Frame, options = Options} = S) ->
    wxStatusBar:setStatusText(S#state.main_status_bar, ""),
    Date = Options#options.date,
    {YearNo, WeekNo} = eflex_lib:week_of_the_year(Date),
    CurrentDate  = date(),
    PopupMenu = wxMenu:new(),
    wxMenu:append(PopupMenu, 0, "Choose year"),
    wxMenu:appendSeparator(PopupMenu),

    Id1 = 1,
    Items1 = [],
    {Id2, Items2} =
        if
            Date =:= CurrentDate ->
                {Id1, Items1};
            true ->
                wxMenu:append(PopupMenu, Id1, "Curr year"),
		{CurrYearNo, _CurrWeekNo} =
		    eflex_lib:week_of_the_year(CurrentDate),
                {Id1 + 1, [{CurrYearNo, WeekNo} | Items1]}
        end,

    Years = eflex_lib:list_years(Options),
    Pred = fun(N) -> N =/= YearNo end,
    {PrevYears, NextYears} = lists:splitwith(Pred, Years),
    PrevYearsLen = length(PrevYears),

    {Id3, Items3} =
        case PrevYearsLen of
            0 ->
                {Id2, Items2};
            _ -> 
                wxMenu:append(PopupMenu, Id2, "Prev year"),
                {Id2 + 1, [{YearNo - 1, WeekNo} | Items2]}
        end,

    {Id4, Items4} =
        case length(NextYears) of
            1 -> 
                {Id3, Items3};
            _ ->
                wxMenu:append(PopupMenu, Id3, "Next year"),
                {Id3 + 1, [{YearNo + 1, WeekNo} | Items3]}
        end,

    wxMenu:appendSeparator(PopupMenu),

    Append =
        fun(N, {Id, Items}) -> 
                wxMenu:append(PopupMenu, Id, integer_to_list(N)),
                {Id + 1, [{N, WeekNo} | Items]}
        end,
    {_, Items5} = lists:foldl(Append, {Id4, Items4}, Years),

    wxEvtHandler:connect(PopupMenu, command_menu_selected),
    %% wxWindow:popupMenu(Frame, PopupMenu, X, Y),
    wxWindow:popupMenu(Frame, PopupMenu),
    S#state{popup = #week_selected{items = lists:reverse(Items5)}}.

week_popup(#state{main_frame = Frame,
                  year = #eflex_year{weeks = Eweeks},
                  options = #options{date = Date}} = S) ->
    wxStatusBar:setStatusText(S#state.main_status_bar, ""),
    CurrentDate  = date(),
    {YearNo, WeekNo} = eflex_lib:week_of_the_year(Date),
    PopupMenu = wxMenu:new(),
    wxMenu:append(PopupMenu, ?wxID_ANY, "Choose week"),
    wxMenu:appendSeparator(PopupMenu),

    Id1 = 1,
    Items1 = [],    
    {Id2, Items2} =
        if
            Date =:= CurrentDate ->
                {Id1, Items1};
            true ->
                wxMenu:append(PopupMenu, Id1, "Curr week"),
		{_CurrYearNo, CurrWeekNo} = 
		    eflex_lib:week_of_the_year(CurrentDate),

                {Id1 + 1, [{YearNo, CurrWeekNo} | Items1]}
        end,

    {Id3, Items3} =
        case WeekNo =:= 1 of
            true  ->
                {Id2, Items2};
            false ->
                wxMenu:append(PopupMenu, Id2, "Prev week"),
                {Id2 + 1, [{YearNo, WeekNo - 1} | Items2]}
        end,

    {Id4, Items4} =
        case WeekNo =:= size(Eweeks) of
            true ->
                {Id3, Items3};
            false ->
                wxMenu:append(PopupMenu, Id3, "Next week"),
                {Id3 + 1, [{YearNo, WeekNo + 1} | Items3]}
        end,

    wxMenu:appendSeparator(PopupMenu),
    
    MakeMonday = 
        fun(W) ->
                Eweek = element(W, Eweeks),
                DateAct = ?KEYSEARCH(?DATE_ACT,
                                     #eflex_activity.name,
                                     Eweek#eflex_week.activities),
                Monday = DateAct#eflex_activity.monday,
                WeekDate = eflex_lib:convert_time(Monday, date),
                Month = week_date_to_month(WeekDate),
                {W, WeekDate, Month}
        end,

    [FirstMonday | Mondays] = 
	lists:map(MakeMonday, lists:seq(1, size(Eweeks))),
    January = week_date_to_month("1/1"),
    FirstMonday2 = setelement(3, FirstMonday, January),
    Mondays2 = [FirstMonday2 | Mondays],

    OptMakeSubMenu =
        fun(Month, Month, SubMenu) ->
                SubMenu;
           (Month, _Month, _SubMenu) ->
                SubMenu = wxMenu:new(),
                wxMenu:append(PopupMenu, ?wxID_ANY, Month, SubMenu),
                wxEvtHandler:connect(SubMenu, command_menu_selected),
                SubMenu
        end,
                
    Append =
        fun({W, WeekDate, Month}, {Id, Items, PrevMonth, PrevSubMenu}) ->
                SubMenu = OptMakeSubMenu(Month, PrevMonth, PrevSubMenu),
                Label = "w" ++ integer_to_list(W) ++ " - " ++ WeekDate,
                wxMenu:append(SubMenu, Id, Label),
                {Id + 1, [{YearNo, W} | Items], Month, SubMenu}
        end,
    {_, Items5, _, _} =
	lists:foldl(Append, {Id4, Items4, undefined, undefined}, Mondays2),

    wxEvtHandler:connect(PopupMenu, command_menu_selected),
    %% wxWindow:popupMenu(Frame, PopupMenu, X, Y),
    wxWindow:popupMenu(Frame, PopupMenu),
    S#state{popup = #week_selected{items = lists:reverse(Items5)}}.

week_selected(#state{options = #options{date = Date}} = S, {week, What}) ->
    {YearNo, WeekNo} = eflex_lib:week_of_the_year(Date),
    {YearNo2, WeekNo2} =
	case What of
	    current ->
		eflex_lib:week_of_the_year(date());
	    previous ->
		case WeekNo - 1 of
		    0 ->
			PrevYearNo = YearNo - 1,
			LastWeekNo =
			    eflex_lib:last_week_no_of_the_year(PrevYearNo),
			{PrevYearNo, LastWeekNo};
		    PrevWeekNo ->
			{YearNo, PrevWeekNo}
		end;
	    next ->
		LastWeekNo = eflex_lib:last_week_no_of_the_year(YearNo),
		NextWeekNo = WeekNo + 1,
		if
		    NextWeekNo > LastWeekNo ->
			NextYearNo = YearNo + 1,
			FirstDayNo =
			    eflex_lib:monday_of_the_first_week(NextYearNo),
			FirstDate = calendar:gregorian_days_to_date(FirstDayNo),
			eflex_lib:week_of_the_year(FirstDate);
		    true ->
			{YearNo, NextWeekNo}
		end
	end,
    week_selected(S, {YearNo2, WeekNo2});
week_selected(#state{options = #options{date = Date} = O} = S, {month, What}) ->
    {YearNo, MonthNo, DayNo} = Date,
    NewDate =
	case What of
	    current ->
		{_, CurrentMonthNo, _} = date(),
		{YearNo, CurrentMonthNo, DayNo};
	    previous ->
		case MonthNo - 1 of
		    0    -> {YearNo - 1,  12, DayNo};
		    Prev -> {YearNo, Prev, DayNo}
		end;
	    next ->
		case MonthNo + 1 of
		    13    -> {YearNo + 1,  1, DayNo};
		    Next -> {YearNo, Next, DayNo}
		end
	end,
    case catch eflex_lib:week_of_the_year(NewDate) of
	{'EXIT', _} ->
	    {NewYearNo, NewMonthNo, _NewDayNo} = NewDate,
	    case calendar:last_day_of_the_month(NewYearNo, NewMonthNo) of
		LastDay when NewDate > LastDay ->
		    NewDate2 = {NewYearNo, NewMonthNo, LastDay},
		    Selected = eflex_lib:week_of_the_year(NewDate2),
		    week_selected(S, Selected);
		_ ->
		    Q = "Do you really want to create a file for year " ++
			integer_to_list(NewYearNo) ++ "?",
		    case ask_question(Q) of
			false ->
			    S;
			true ->
			    %% Create new year
			    MondayDate = eflex_lib:monday_of_the_week(NewDate),
			    O2 = O#options{date = MondayDate},
			    {O3, NewEyear} = eflex_lib:init_files(O2),
			    S#state{options = O3, year = NewEyear}    
		    end
	    end;
	Selected ->
	    week_selected(S, Selected)
    end;
week_selected(#state{options = #options{date = Date}} = S, {year, What}) ->
    {YearNo, WeekNo} = eflex_lib:week_of_the_year(Date),
    YearNo2 =
	case What of
	    current ->
		{CurrYearNo, _} = eflex_lib:week_of_the_year(date()),
		CurrYearNo;
	    previous ->
		YearNo - 1;
	    next ->
		YearNo + 1
	end,
    week_selected(S, {YearNo2, WeekNo});
week_selected(#state{options = #options{date = Date} = O} = S,
	      {YearNo, WeekNo}) ->
    %% io:format("Week selected: ~p date: ~p\n", [{YearNo, WeekNo}, Date]),
    case eflex_lib:is_week_within_range({YearNo, WeekNo}, O) of
	true ->
	    {PrevYearNo, PrevWeekNo} = eflex_lib:week_of_the_year(Date),
	    if
		YearNo =:= PrevYearNo,
		WeekNo =:= PrevWeekNo ->
		    %% Same week
		    S;
		YearNo =:= PrevYearNo ->
		    %% Same year
		    MondayDate = eflex_lib:monday_of_the_week({YearNo, WeekNo}),
		    S#state{options = O#options{date = MondayDate}};
		true ->
		    %% Other year
		    MondayDate = eflex_lib:monday_of_the_week({YearNo, WeekNo}),
		    O2 = O#options{date = MondayDate},
		    {O3, NewEyear} = eflex_lib:init_files(O2),
		    S#state{options = O3, year = NewEyear}
	    end;
	false when WeekNo < 1 ->
	    week_selected(S, {YearNo, 1});
	false ->
	    case eflex_lib:last_week_no_of_the_year(YearNo) of
		LastWeekNo when WeekNo > LastWeekNo ->
		    week_selected(S, {YearNo, LastWeekNo});
		_ ->
		    Q = "Do you really want to create a file for year " ++
			integer_to_list(YearNo) ++ "?",
		    case ask_question(Q) of
			false ->
			    S;
			true ->
			    %% Create new year
			    MondayDate = eflex_lib:monday_of_the_week({YearNo, WeekNo}),
			    O2 = O#options{date = MondayDate},
			    {O3, NewEyear} = eflex_lib:init_files(O2),
			    S#state{options = O3, year = NewEyear}    
		    end
	    end
    end.

week_date_to_month(Date) when is_list(Date) ->
    [_Day, Month] = string:tokens(Date, "/"),
    case Month of
        "1"  -> "January";
        "2"  -> "February";
        "3"  -> "March";
        "4"  -> "April";
        "5"  -> "May";
        "6"  -> "June";
        "7"  -> "July";
        "8"  -> "August";
        "9"  -> "September";
        "10" -> "October";
        "11" -> "November";
        "12" -> "December"
    end.
    
auto_code(#state{options = #options{date = Date} = O,
                      year = Eyear} = S) ->
    #eflex_year{weeks = Eweeks,
		config = Econfig} = Eyear,
    {_, WeekNo} = eflex_lib:week_of_the_year(Date),
    Eweek = element(WeekNo, Eweeks),
    {Eactivities2, Econfig2} =
	absence_all_day(Econfig#eflex_config.absence_all_day,
			Eweek#eflex_week.activities,
			Econfig,
			O),
    Eactivities3 = unspecified_work(Eactivities2, Econfig2),
    Eweek2 = Eweek#eflex_week{activities = Eactivities3},
    Eweeks2 = setelement(WeekNo, Eweeks, Eweek2),
    Eyear2 = Eyear#eflex_year{weeks = Eweeks2, config = Econfig2},
    S#state{year = Eyear2}.
    
activity_popup(#state{options = #options{date = Date, n_rows = Nrows},
                      main_grid = MainGrid,
                      year = #eflex_year{weeks = Eweeks,
                                         config = Econfig}} = S,
               Row) ->
    wxStatusBar:setStatusText(S#state.main_status_bar, ""),
    #eflex_config{activity_types = Etypes} = Econfig,
    wxGrid:clearSelection(MainGrid),
    {_, WeekNo} = eflex_lib:week_of_the_year(Date),
    Eweek = element(WeekNo, Eweeks),
    Eactivities = Eweek#eflex_week.activities,
    Tuple = eflex_lib:make_activity_tuple(Nrows, Eactivities),
    EactivityNo = Row + 1,
    {OldName, Visibility, OptCategory} = 
        case element(EactivityNo, Tuple) of
            undefined ->
                {undefined, visible, undefined};
            #eflex_activity{name = Name0} ->
                #eflex_activity_type{visibility = Visibility0,
				     category = Category0} =
                    ?KEYSEARCH(Name0, #eflex_activity.name, Etypes),
                {Name0, Visibility0, Category0}
        end,
    EweekList = tuple_to_list(Eweeks),
    PopupMenu = wxMenu:new(),
    ViewUsage =
	fun(Id, Items) ->
		MatchingActs =
		    [{No, EA} || #eflex_week{no = No, activities = Acts} <- EweekList,
				 EA <- Acts,
				 EA#eflex_activity.name =:= OldName],
		N = integer_to_list(length(MatchingActs)),
		wxMenu:append(PopupMenu, Id, "View usage (" ++ N ++ "weeks)"),
		{Id + 1, [{view, OldName} | Items]}
	end,
    {Id5, Items5} =
	case Visibility of
	    hidden ->
		ViewUsage(1, []);
	    visible ->
		NamePos = #eflex_activity.name,
		MenuItem =
		    fun(#eflex_activity_type{name = Name,
					     visibility = V,
					     category = Category},
			{Id, Items}) ->
			    Name2 =
				if
				    Name =:= Econfig#eflex_config.absence_all_day ->
					Name ++ "*";
				    Name =:= Econfig#eflex_config.unspecified_work ->
					Name ++ "*";
				    true ->
					Name
				end,
			    case lists:keymember(Name, NamePos, Eactivities) of
				false when V =:= visible, OptCategory =:= undefined ->
				    wxMenu:append(PopupMenu, Id, Name2),
				    {Id + 1, [{create, Name} | Items]};
				false when V =:= visible, OptCategory =:= Category ->
				    wxMenu:append(PopupMenu, Id, Name2),
				    {Id + 1, [{rename, OldName, Name} | Items]};
				_ ->
				    {Id, Items}
			    end
		    end,
		{_Mandatory, Attendance, Project} =
		    eflex_lib:multi_split_activity_types(Etypes),

		wxMenu:append(PopupMenu, 0, "Choose activity"),

		wxMenu:appendSeparator(PopupMenu),
		{Id, Items} = lists:foldl(MenuItem, {1, []}, Attendance),

		wxMenu:appendSeparator(PopupMenu),
		{Id2, Items2} = lists:foldl(MenuItem, {Id, Items}, Project),

		wxMenu:appendSeparator(PopupMenu),
		if
		    OldName =:= undefined ->
			{Id2, Items2};
		    true ->
			{Id3, Items3} = ViewUsage(Id2, Items2),
			wxMenu:append(PopupMenu, Id3, "Delete activity"),
			Id4 = Id3 + 1,
			Items4 = [{delete, OldName} | Items3],
			case lists:keymember(OldName, NamePos, Attendance) of
			    true ->
				wxMenu:append(PopupMenu, Id4, "Absence all day"),
				{Id4 + 1, [{absence_all_day, OldName} | Items4]};
			    false ->
				wxMenu:append(PopupMenu, Id4, "Unspecified work"),
				{Id4 + 1, [{unspecified_work, OldName} | Items4]}
			end
		end
	end,
    wxMenu:append(PopupMenu, Id5, "Edit activity types"),
    Id6 = Id5 + 1,
    wxMenu:append(PopupMenu, Id6, "Edit configuration"),
    Id7 = Id6 + 1,
    wxMenu:append(PopupMenu, Id7, "Edit holidays"),
    Items7 = [edit_holidays, edit_config, edit_types | Items5],
    wxEvtHandler:connect(PopupMenu, command_menu_selected),
    %% wxWindow:popupMenu(S#state.main_frame, PopupMenu, X, Y),
    wxWindow:popupMenu(S#state.main_frame, PopupMenu),
    S#state{popup = #activity_selected{items = lists:reverse(Items7)}}.
    

activity_selected(#state{options = #options{date = Date} = O,
                         year = #eflex_year{weeks = Eweeks,
                                            config = Econfig} = Eyear} = S,
                  Action) ->
    {_, WeekNo} = eflex_lib:week_of_the_year(Date),
    Eweek = element(WeekNo, Eweeks),
    Eactivities = Eweek#eflex_week.activities,
    NamePos = #eflex_activity.name,
    {Eactivities3, S3, Econfig3} =
        case Action of
	    edit_types ->
		{Eactivities,
		 wx:batch(fun() -> create_activity_type_window(S) end), Econfig};
	    edit_config ->
		S2 = wx:batch(fun() -> create_config_window(S) end),
		{Eactivities, S2, Econfig};
	    edit_holidays ->
		S2 = wx:batch(fun() -> create_holiday_window(S) end),
		{Eactivities, S2, Econfig};
	    {delete, OldName} ->
                %% Delete an activity
		{lists:keydelete(OldName, NamePos, Eactivities), S, Econfig};
	    {absence_all_day, OldName} ->
		{Eactivities2, Econfig2} =
		    absence_all_day(OldName, Eactivities, Econfig, O),
		{Eactivities2, S, Econfig2};
	    {unspecified_work, _OldName} ->
		{Eactivities2, Econfig2} = unspecified_work(Eactivities, Econfig),
		{Eactivities2, S, Econfig2};
            {create, Name} ->
                %% Add a new activity
                DefaultSum = 0,
                DefaultDay = undefined,
                Eactivity =
                    #eflex_activity{name      = Name,
                                    year_sum  = DefaultSum,
                                    week_sum  = DefaultSum,
                                    monday    = DefaultDay,
                                    tuesday   = DefaultDay,
                                    wednesday = DefaultDay,
                                    thursday  = DefaultDay,
                                    friday    = DefaultDay,
                                    saturday  = DefaultDay,
                                    sunday    = DefaultDay},
		Eactivities2 =
		    eflex_lib:sort_activities([Eactivity | Eactivities], Econfig),
		{Eactivities2, S, Econfig};
            {rename, OldName, Name} ->
                %% Rename an activity.
                Eactivity = ?KEYSEARCH(OldName, NamePos, Eactivities),
                Eactivity2 = Eactivity#eflex_activity{name = Name},
		Eactivities2 =
		    lists:keyreplace(OldName, NamePos, Eactivities, Eactivity2),
		{eflex_lib:sort_activities(Eactivities2, Econfig), S, Econfig};
	    {view, Name} ->
		wx:batch(fun() ->
				 S2 = create_activity_list_window(S, Name),
				 Eyear2 = S2#state.year,
				 {Eactivities, S2, Eyear2#eflex_year.config}
			 end)
        end,
    Eweek2 = Eweek#eflex_week{activities = Eactivities3},
    Eweeks2 = setelement(WeekNo, Eweeks, Eweek2),
    S4 = S3#state{year = Eyear#eflex_year{weeks = Eweeks2, config = Econfig3}},
    if
	Econfig3 =:= Econfig, Eactivities3 =:= Eactivities ->
	    S4;
	true ->
	    recreate_activity_type_window(S4)
    end.

absence_all_day(undefined, Eactivities, Econfig, _Options) ->
    {Eactivities, Econfig#eflex_config{absence_all_day = undefined}};
absence_all_day(Name, Eactivities, Econfig, Options) ->
    NamePos = #eflex_activity.name,
    %% Re-code absence all day
    Eactivity = eflex_lib:ensure_action(Name, Eactivities, Econfig),
    Arr  = ?KEYSEARCH(?ARRIVED_ACT, NamePos, Eactivities),
    Left = ?KEYSEARCH(?LEFT_ACT, NamePos, Eactivities),
    Flex = ?KEYSEARCH(?FLEX_ACT, NamePos, Eactivities),
    Today = calendar:date_to_gregorian_days(date()),
    Fun =
	fun(Pos, Act) ->
		if
		    element(Pos, Arr) =:= undefined,
		    element(Pos, Left) =:= undefined ->
			%% Absence all day
			{YearNo, WeekNo} =
			    eflex_lib:week_of_the_year(Options#options.date),
			MondayDate = 
			    eflex_lib:monday_of_the_week({YearNo, WeekNo}),
			MondayDayNo =
			    calendar:date_to_gregorian_days(MondayDate),
			WeekDayNo = Pos - #eflex_activity.monday + 1,
			case Today >= (MondayDayNo + WeekDayNo - 1) of
			    true ->
				case eflex_lib:expected_worktime(MondayDayNo,
								 WeekDayNo,
								 Econfig) of
				    0 ->
					Act;
				    ExpectedWorkTime 
				    when -ExpectedWorkTime =:= element(Pos, Flex) ->
					setelement(Pos, Act, ExpectedWorkTime);
				    _ ->
					Act
				end;
			    false ->
				Act
			end;
		    true ->
			%% Some work
			Act
		end
	end,
    Eactivity2 = 
	lists:foldl(Fun,
		    Eactivity,
		    lists:seq(#eflex_activity.monday,
			      #eflex_activity.sunday)),
    case eflex_lib:calc_week_sum(Eactivity2) of
	Sum when Sum =/= 0 ->
	    Eactivities2 = 
		lists:keystore(Name, NamePos, Eactivities, Eactivity2),
	    Eactivities3 = eflex_lib:sort_activities(Eactivities2, Econfig),
	    %% {Eactivities3, Econfig#eflex_config{absence_all_day = Name}};
	    {Eactivities3, Econfig};
	_ ->
	    {Eactivities, Econfig}
    end.

%% unspecified_work(undefined, Eactivities, Econfig) ->
%%     {Eactivities, Econfig#eflex_config{unspecified_work = undefined}};
%% unspecified_work(Name, Eactivities, Econfig) ->
%%     %%  Re-code unspecified work
%%     NamePos = #eflex_activity.name,
%%     Eactivity = eflex_lib:ensure_action(Name, Eactivities, Econfig),
%%     Unspec = ?KEYSEARCH(?UNSPEC_ACT, NamePos, Eactivities),
%%     Fun =
%% 	fun(Pos, Act) ->
%% 		New = element(Pos, Unspec),
%% 		Old = element(Pos, Act),
%% 		if
%% 		    New =:= undefined -> Act;
%% 		    Old =:= undefined -> setelement(Pos, Act, New);
%% 		    true -> setelement(Pos, Act, Old + New)
%% 		end
%% 	end,
%%     Eactivity2 = 
%% 	lists:foldl(Fun,
%% 		    Eactivity,
%% 		    lists:seq(#eflex_activity.monday,
%% 			      #eflex_activity.sunday)),
%%     case eflex_lib:calc_week_sum(Eactivity2) of
%% 	Sum when Sum =/= 0 ->
%% 	    Eactivities2 =
%% 		lists:keystore(Name, NamePos, Eactivities, Eactivity2),
%% 	    Eactivities3 = eflex_lib:sort_activities(Eactivities2, Econfig),
%% 	    %% {Eactivities3, Econfig#eflex_config{unspecified_work = Name}};
%% 	    {Eactivities3, Econfig};
%% 	_ ->
%% 	    {Eactivities, Econfig}
%%     end.

unspecified_work(Eactivities, Econfig) ->
    %%  Re-code unspecified work
    Etypes = Econfig#eflex_config.activity_types,
    Count = fun(A, Sum) -> Sum + A#eflex_activity_type.share end,
    AllShares = lists:foldl(Count, 0, Etypes),
    NamePos = #eflex_activity.name,
    Unspec = ?KEYSEARCH(?UNSPEC_ACT, NamePos, Eactivities),
    Fun =
	fun(Etype, Acts) ->
		Name = Etype#eflex_activity_type.name,
		case Etype#eflex_activity_type.share of
		    0 ->
			Acts;
		    Share ->
			Percent = Share / AllShares,
			Eactivity = eflex_lib:ensure_action(Name, Eactivities, Econfig),
			case do_unspecified_work(Eactivity, Unspec, Percent) of
			    Eactivity2 when Eactivity2 =/= Eactivity ->
				lists:keystore(Name, NamePos, Acts, Eactivity2);
			    _ ->
				Acts
			end
		end
	end,
    Eactivities2 = 
	lists:foldl(Fun, Eactivities, Econfig#eflex_config.activity_types),
    eflex_lib:sort_activities(Eactivities2, Econfig).

do_unspecified_work(Eactivity, Unspec, Percent) ->
    %%  Re-code unspecified project work
    Fun =
	fun(Pos, Act) ->
		OldVal = element(Pos, Act),
		UnspecVal = element(Pos, Unspec),
		NewVal = 
		    if
			UnspecVal =:= undefined ->
			    OldVal;
			OldVal =:= undefined ->
			    round(UnspecVal * Percent);
			true ->
			    OldVal + round(UnspecVal * Percent)
		    end,
		setelement(Pos, Act, NewVal)	
	end,
    PosList = lists:seq(#eflex_activity.monday, #eflex_activity.sunday),
    lists:foldl(Fun, Eactivity, PosList).

opt_update_activity(#state{options = #options{n_rows = Nrows} = Options,
                           year = Eyear} = S, MainGrid, Row, Col) ->
    Val = wxGrid:getCellValue(MainGrid, Row, Col),
    ActivityPos = Row + 1,
    DayNo = Col - 2,
    {Options2, Eyear2} =
        eflex_lib:opt_update_activity(Val, ActivityPos, DayNo, Options, Eyear, Nrows),
    S#state{options = Options2, year = Eyear2}.

resize(#state{main_grid_size = {OldWidth, _OldHeight},
              activity_col_width = OldActColWidth} = S,
       MainGrid,
       #wxSize{type = size,
               size = {NewWidth, _NewHeight} = NewSize}) ->
    %% io:format("Resize\n",  []),
    Diff = OldWidth - NewWidth,
    NewActColWidth = OldActColWidth - Diff,
    NewActColWidth > 0 andalso wxGrid:setColSize(MainGrid, ?ACTIVITY_COL, NewActColWidth),
    wxGrid:forceRefresh(MainGrid),
    S#state{main_grid_size     = NewSize,
            activity_col_width = NewActColWidth}.

update_week(#state{options = #options{date = Date, n_rows = Nrows},
		   main_grid = MainGrid,
		   year = Eyear} = S) ->
    {YearNo, WeekNo} = eflex_lib:week_of_the_year(Date),
    MondayDate = eflex_lib:monday_of_the_week({YearNo, WeekNo}),
    MondayDayNo = calendar:date_to_gregorian_days(MondayDate),
    #eflex_year{config = #eflex_config{activity_types = AccEtypes} = Econfig} =
        eflex_lib:sum_activities(Eyear, WeekNo - 1),
    InitRow =
        fun(#eflex_activity{name      = Name,
                            year_sum  = OldYearSum,
                            week_sum  = OldWeekSum,
                            monday    = Mon,
                            tuesday   = Tue,
                            wednesday = Wed,
                            thursday  = Thu,
                            friday    = Fri,
                            saturday  = Sat,
                            sunday    = Sun} = Eactivity,
            {AttendanceRow, ProjectRow}) ->
                InitCol = 
                    fun(Val, {Row, Col}) ->
                            if
                                Col =:= ?ACTIVITY_COL ->
                                    wxGrid:setCellValue(MainGrid, Val, Row, Col);
                                Col =:= ?YEAR_SUM_COL, Name =:= ?ARRIVED_ACT->
                                    ignore;
                                Col =:= ?WEEK_SUM_COL, Name =:= ?ARRIVED_ACT ->
                                    ignore;
                                Col =:= ?WEEK_SUM_COL, Name =:= ?LEFT_ACT ->
                                    ignore;
                                Col =:= ?YEAR_SUM_COL, Name =:= ?LEFT_ACT ->
                                    ignore;
                                Col =:= ?YEAR_SUM_COL, Name =:= ?DATE_ACT ->
				    Int = eflex_lib:convert_time(OldYearSum,
								 integer),
                                    Val2 = integer_to_list(Int),
                                    wxGrid:setCellValue(MainGrid, Val2, Row, Col);
                                Col =:= ?WEEK_SUM_COL, Name =:= ?DATE_ACT ->
				    Int = eflex_lib:convert_time(OldWeekSum,
								 integer),
                                    Val2 = integer_to_list(Int),
                                    wxGrid:setCellValue(MainGrid, Val2, Row, Col);
                                Name =:= ?DATE_ACT ->
				    WeekDayNo = Col - 2,
				    IsHoliday =
					eflex_lib:is_holiday(MondayDayNo, WeekDayNo, Econfig),
				    Color =
					case IsHoliday of
					    true  -> ?wxRED;
					    false -> ?wxBLACK
					end,
				    wxGrid:setCellTextColour(MainGrid,
							     Row,
							     Col,
							     Color),
                                    Val2 = eflex_lib:convert_time(Val, date),
				    Val3 =
					case eflex_lib:lookup_worktime(WeekDayNo, Econfig) of
					    0 -> "(" ++ Val2 ++ ")";
					    _ -> Val2
					end,
                                    wxGrid:setCellValue(MainGrid, Val3, Row, Col);
                                true ->
                                    Val2 =
                                        case Val of
                                            undefined ->
						"";
                                            _ -> 
						eflex_lib:convert_activity_time(Val, 
										Eyear,
										Name)
                                        end,
                                    wxGrid:setCellValue(MainGrid, Val2, Row, Col),
                                    if
                                        Name =:= ?WORK_ACT, Val < 0 ->
                                            wxGrid:setCellTextColour(MainGrid, 
								     Row,
								     Col,
								     ?wxRED);
                                        Name =:= ?WORK_ACT ->
                                            wxGrid:setCellTextColour(MainGrid,
								     Row,
								     Col,
								     ?wxBLACK);
                                        Name =:= ?FLEX_ACT,
					Col =/= ?YEAR_SUM_COL,
					Col =/= ?WEEK_SUM_COL ->
					    WeekDayNo = Col - 2,
					    ExpectedWorkTime =
						eflex_lib:expected_worktime(MondayDayNo,
									    WeekDayNo,
									    Econfig),
					    if
						Val =:= -ExpectedWorkTime ->
						    %% Absence all day
						    wxGrid:setCellTextColour(MainGrid,
									     Row, 
									     Col,
									     ?wxRED);
						true ->
						    wxGrid:setCellTextColour(MainGrid,
									     Row,
									     Col,
									     ?wxBLACK)
					    end;
					Col =/= ?YEAR_SUM_COL,
					Col =/= ?WEEK_SUM_COL ->
					    case Name of
						?DATE_ACT ->
						    ignore;
						?BREAK_ACT ->
						    ignore;
						?WORK_ACT ->
						    ignore;
						?UNSPEC_ACT ->
						    ignore;
						_ ->
						    cell_set_read_write(MainGrid, Row, Col)
					    end;
                                        true ->
                                            ignore
                                    end
                            end,
                            {Row, Col + 1}
                    end,
                WeekSum = eflex_lib:calc_week_sum(Eactivity),
                #eflex_activity_type{initial = PrevWeekSum, 
				     category = Category} =
                    ?KEYSEARCH(Name, #eflex_activity_type.name, AccEtypes),
                YearSum = PrevWeekSum + WeekSum,
                Vals = [Name, YearSum, WeekSum,
			Mon, Tue, Wed, Thu, Fri, Sat, Sun],
                {TheRow, AttendanceRow2, ProjectRow2} =
                    case Category of
                        attendance -> 
			    {AttendanceRow, AttendanceRow + 1, ProjectRow};
                        project ->
			    {ProjectRow,  AttendanceRow, ProjectRow - 1}
                    end,
                lists:foldl(InitCol, {TheRow, 0}, Vals),
                {AttendanceRow2, ProjectRow2}
        end,
    Eweeks = Eyear#eflex_year.weeks,
    Eweek = element(WeekNo, Eweeks),
    Eactivities = Eweek#eflex_week.activities,
    Eactivities2 = 
	eflex_lib:update_derived_activities(MondayDayNo, Eactivities, Econfig),    
    UnspecifiedRow = Nrows - 1,
    {Y, M, D} = date(),
    Title = lists:concat(["Eflex ", D, "/", M, "-", Y]),
    wxFrame:setTitle(S#state.main_frame, Title),
    [
     begin
	 wxGrid:setCellValue(MainGrid, Row, Col, ""),
	 cell_set_read_only(MainGrid, Row, Col)
     end ||
	Row <- lists:seq(0, wxGrid:getNumberRows(MainGrid)-1),
	Col <- lists:seq(0, wxGrid:getNumberCols(MainGrid)-1)],
    %% wxGrid:clearGrid(MainGrid),
    lists:foldl(InitRow, {?DATE_ROW, UnspecifiedRow}, Eactivities2),
    Eweek2 = Eweek#eflex_week{activities = Eactivities2},
    Eweeks2 = setelement(WeekNo, Eweeks, Eweek2),
    S#state{year = Eyear#eflex_year{weeks = Eweeks2}}.

clipboard_copy(Text) ->
    Clipboard = wxClipboard:'get'(),
    ok = wxClipboard:usePrimarySelection(Clipboard, [{primary, true}]),
    case wxClipboard:open(Clipboard) of
	true ->
	    TextObj = wxTextDataObject:new([{text, Text}]),
	    true = wxClipboard:setData(Clipboard, wx:typeCast(TextObj, wxDataObject)),
	    ok = wxClipboard:close(Clipboard),
	    ok;
	false ->
	    ok = wxClipboard:close(Clipboard),
	    {error, "Failed to open clipboard"}
    end.

clipboard_paste() ->
    Clipboard = wxClipboard:'get'(),
    ok = wxClipboard:usePrimarySelection(Clipboard, [{primary, true}]),
    case wxClipboard:open(Clipboard) of
	true ->
	    TextObj = wxTextDataObject:new(),
	    wxClipboard:getData(Clipboard, wx:typeCast(TextObj, wxDataObject)),
	    Text = wxTextDataObject:getText(TextObj),
	    ok = wxClipboard:close(Clipboard),
	    {ok, Text};
	false ->
	    ok = wxClipboard:close(Clipboard),
	    {error, "Failed to open clipboard"}
    end.
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% System upgrade
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

system_continue(_Parent, _Debug, #state{options = Options} = S) ->
    case Options#options.window of
        false -> ?MODULE:no_window_loop(S);
        true  -> ?MODULE:window_loop(S)
    end.

system_terminate(_Reason, _Parent, _Debug, _State) ->
    ok.

system_code_change(State, _Module, _OldVsn, _Extra) ->
    {ok, State}.
