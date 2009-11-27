%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2009 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(APPLICATION, eflex).

-define(DATE_ACT,      	 "Date").
-define(ARRIVED_ACT,   	 "Arrived").
-define(LEFT_ACT,      	 "Left").
-define(BREAK_ACT,     	 "Break").
-define(ADJUST_ACT,    	 "Adjust").
-define(WORK_ACT,      	 "Work").
-define(FLEX_ACT,      	 "Flex").
-define(OVERTIME_ACT,  	 "Overtime").
-define(PARENTAL_ACT,  	 "Parental leave").
-define(ABSENCE_ACT,  	 "Absence").
-define(VACATION_ACT,  	 "Vacation").
-define(UNSPEC_ACT,      "Unspecified projects").
-define(MOVE_UP_ACT,   	 "Move up").
-define(MOVE_DOWN_ACT, 	 "Move down").

-define(KEYSEARCH(Key, Pos, List),
	eflex_lib:safe_keysearch(Key, Pos, List, ?MODULE, ?LINE)).

-record(options,
        {window,
         dir,
         debug,
         date,
	 read_only,
	 lock_file,
	 lock_cookie,
         arrived_file,
         arrived_mtime,
         left_file,
         left_mtime,
         mouse_pos,
         auto_increment,
	 n_rows,
	 absence_all_day,
	 unspecified_work}).

-record(eflex_year,
	{no,
	 weeks,
	 config}).

-record(eflex_week,
	{no,
	 activities}). % Sorted in significant order

-record(eflex_activity,
	{name,
	 year_sum,
	 week_sum,
	 monday,
	 tuesday,
	 wednesday,
	 thursday,
	 friday,
	 saturday,
	 sunday,
	 sign}).

-record(eflex_config,
        {unit,
	 working_time,
         lunch_duration,
         workday_break,
         freeday_break,
         activity_types,
         holidays,
	 absence_all_day,
	 unspecified_work,
	 mini_break
	}).

-record(eflex_working_time,
        {monday,
         tuesday,
         wednesday,
         thursday,
         friday,
         saturday,
         sunday}).
-record(eflex_break,
	{duration, 
	 time_before_break}).

-record(eflex_activity_type,
	{name,
	 initial,
	 unit,
	 category,
	 sign,
	 visibility,
	 share,
	 frame,
	 panel}).

-record(eflex_holiday,
	{month,
	 day}).

-define(SAFE(Expr, Extra),
        fun() ->
                case Expr of
                    ok ->
                        ok;
                    {ok, Res} ->
                        {ok, Res};
                    {error, Reason} ->
                        exit({?MODULE, ?LINE, Reason, Extra})
                end
        end()).
