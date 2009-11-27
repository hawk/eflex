%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Copyright (c) 2009 Hakan Mattsson
%%
%% See the file "LICENSE" for information on usage and redistribution
%% of this file, and for a DISCLAIMER OF ALL WARRANTIES.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(eflex_xml).
-compile([export_all]).

-include("eflex.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-define(INDENT(Indent), [$\t | Indent]).

test() ->
    O = eflex_lib:parse_options([]),
    encode(eflex_lib:make_initial_year(2008, O)).

test2() ->
    External = lists:flatten(test()),
    {Internal, []} = xmerl_scan:string(External),
    to_simple(Internal).

test3() ->
    External = lists:flatten(test()),
    decode(External).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% XML encode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

encode(Eflex) ->
    encode(Eflex, "").

encode(#eflex_year{no     = YearNo,
                   weeks  = Eweeks,
                   config = Econfig},
       Indent) when is_tuple(Eweeks) ->
    encode({year,
            [
             {no,    {integer, YearNo}},
             {weeks, tuple_to_list(Eweeks)},
             Econfig
            ]},
           Indent);
encode(#eflex_week{no         = WeekNo,
                   activities = Eactivities},
       Indent) ->
    encode({week,
            [
             {no, {integer, WeekNo}},
             Eactivities
            ]},
           Indent);
encode(#eflex_activity{name = Name,
                       year_sum  = YearSum,
                       week_sum  = WeekSum,
                       monday    = Monday,
                       tuesday   = Tuesday,
                       wednesday = Wednesday,
                       thursday  = Thursday,
                       friday    = Friday,
                       saturday  = Saturday,
                       sunday    = Sunday},
       Indent) ->
    encode({activity,
            [
             {name,      {string,  Name}},
             {year_sum,  {optional, {integer, YearSum}}},
             {week_sum,  {optional, {integer, WeekSum}}},
             {monday,    {optional, {integer, Monday}}},
             {tuesday,   {optional, {integer, Tuesday}}},
             {wednesday, {optional, {integer, Wednesday}}},
             {thursday,  {optional, {integer, Thursday}}},
             {friday,    {optional, {integer, Friday}}},
             {saturday,  {optional, {integer, Saturday}}},
             {sunday,    {optional, {integer, Sunday}}}
            ]},
           Indent);
encode(#eflex_config{unit             = Unit,
		     working_time     = WorkingTime,
                     lunch_duration   = Lunch, 
                     workday_break    = WorkdayBreak,
                     freeday_break    = FreedayBreak,
                     activity_types   = ActivityTypes,
                     holidays         = Holidays,
		     absence_all_day  = AbsenceAllDay,
		     unspecified_work = UnspecifiedWork,
		     mini_break       = MiniBreak},
       Indent) ->
    Mand =
	[
	 {unit, {atom, Unit}},
	 WorkingTime,
	 {lunch, {integer, Lunch}},
	 {workday, WorkdayBreak},
	 {freeday, FreedayBreak},
	 {activity_types, ActivityTypes},
	 {holidays, Holidays},
	 {mini_break, {integer, MiniBreak}}
	],
    Opt =
	case {AbsenceAllDay, UnspecifiedWork} of
	    {undefined, undefined} -> [];
	    {A, undefined} ->
		[{absence_all_day, {string, A}}];
	    {undefined, U} ->
		[ {unspecified_work, {string, U}}];
	    {A, U} ->
		[{absence_all_day, {string, A}},
		 {unspecified_work, {string, U}}]
	end,
    encode({configuration, Mand ++ Opt}, Indent);
encode(#eflex_working_time{monday    = Monday,
                           tuesday   = Tuesday,
                           wednesday = Wednesday,
                           thursday  = Thursday,
                           friday    = Friday,
                           saturday  = Saturday,
                           sunday    = Sunday},
       Indent) ->
    encode({working_time,
            [
             {monday,    {integer, Monday}},
             {tuesday,   {integer, Tuesday}},
             {wednesday, {integer, Wednesday}},
             {thursday,  {integer, Thursday}},
             {friday,    {integer, Friday}},
             {saturday,  {integer, Saturday}},
             {sunday,    {integer, Sunday}}
            ]},
           Indent);
encode(#eflex_break{duration          = Duration,
                    time_before_break = TimeBeforeBreak},
       Indent) ->
    encode({break,
            [
             {duration,          {integer, Duration}},
             {time_before_break, {integer, TimeBeforeBreak}}
            ]},
           Indent);

encode(#eflex_activity_type{name       = Name,
                            initial    = Initial,
                            unit       = Unit,
			    category   = Category,
                            sign       = Sign,
                            visibility = Visibility,
			    share      = Share},
       Indent) ->
    encode({activity_type,
            [
             {name,    	  {string,  Name}},
             {initial, 	  {integer, Initial}},
             {unit,    	  {atom,    Unit}},
	     {category,	  {atom,    Category}},
             {sign,    	  {atom,    Sign}},
             {visibility, {atom,    Visibility}},
             {share,      {integer, Share}}
            ]},
           Indent);
encode(#eflex_holiday{day   = Day,
                      month = Month},
       Indent) ->
    encode({holiday,
            [
             {day,   {integer, Day}},
             {month, {integer, Month}}
            ]},
           Indent);
encode(Char, _Indent) when is_integer(Char) ->
    case Char of
        $< -> "&lt;";
        $> -> "&gt;";
        $& -> "&amp;";
        34 -> "&quot;"; % $"
        39 -> "&apos;"; % $'
        Int when is_integer(Int) -> Int
    end;
encode({Tag, {Type, Val}}, Indent) ->
    TagStr = atom_to_list(Tag),
    case Type of
        optional ->
            {_SubType, OptVal} = Val,
            case OptVal of
                undefined  -> encode({Tag,  {boolean, true}}, Indent);
                _ -> encode({Tag, Val}, Indent)
            end;
        string ->
            [Indent, "<", TagStr, ">", Val,"</", TagStr, ">\n"];
        integer ->
            [Indent, "<", TagStr, ">", integer_to_list(Val), "</", TagStr, ">\n"];
        atom ->
            [Indent, "<", TagStr, ">", atom_to_list(Val), "</", TagStr, ">\n"];
        boolean when Val =:= false ->
            [];
        boolean when Val =:= true ->
            [Indent, "<", TagStr, "/>\n"]
    end;
%% encode({_Tag, undefined}, _Indent) ->
%%     [];
encode(List, Indent) when is_list(List) ->
    [encode(Elem, Indent) || Elem <- List];
encode({Tag, List}, Indent) when is_atom(Tag), is_list(List) ->
    TagStr = atom_to_list(Tag),
    [
     Indent, "<", TagStr, ">\n",
     [encode(Sub, ?INDENT(Indent)) || Sub <- List],
     Indent, "</", TagStr, ">\n"
    ];
encode({Tag, Val}, Indent) when is_atom(Tag) ->
    TagStr = atom_to_list(Tag),
    [
     Indent, "<", TagStr, ">\n",
     encode(Val, ?INDENT(Indent)),
     Indent, "</", TagStr, ">\n"
    ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% XML decode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

decode(External) ->
    {Internal, []} = xmerl_scan:string(External),
    Simple = to_simple(Internal),
    %% io:format("Simple: ~p\n", [Simple]),
    simple_decode(Simple).

to_simple(List) when is_list(List) ->
    Simple = fun(Elem, Acc) ->
                     case to_simple(Elem) of
                         []       -> Acc;
                         Stripped -> [Stripped | Acc]
                     end
             end,
    lists:reverse(lists:foldl(Simple, [], List));
to_simple(#xmlElement{name    = Tag,
                      content = Content}) ->
    {Tag, lists:flatten(to_simple(Content))};
to_simple(#xmlText{value = Text}) ->
    strip(Text).

%% Strips off leading and trailing white spaces
strip([]) ->
    [];
strip([Char | Text]) when Char==$\s; Char==$\n; Char==$\t ->
    strip(Text);
strip(Text) ->
    strip(Text,[],[]).
strip([Char | Text], TAcc, SAcc) when Char==$\s; Char==$\n; Char==$\t ->
    strip(Text, TAcc, [Char | SAcc]);
strip([Char |Text], TAcc, SAcc) ->
    strip(Text, [Char | SAcc ++ TAcc], []);
strip([], TAcc, _SAcc) ->
    lists:reverse(TAcc).

simple_decode({year, [{no, YearNo},
                      {weeks, Weeks},
                      {configuration, _} = Config]}) when is_list(Weeks) ->
    Eyear = #eflex_year{no     = decode_integer(YearNo),
			weeks  = list_to_tuple(simple_decode(Weeks)),
			config = simple_decode(Config)},
    eflex_lib:unify_shares(Eyear, undefined, 1);
simple_decode({week, [{no, WeekNo} | Activities]}) ->
    #eflex_week{no         = decode_integer(WeekNo),
                activities = simple_decode(Activities)};
simple_decode({activity, [{name, Name0},
                          {year_sum, YearSum},
                          {week_sum, WeekSum},
                          {monday, Monday},
                          {tuesday, Tuesday},
                          {wednesday, Wednesday},
                          {thursday, Thursday},
                          {friday, Friday},
                          {saturday, Saturday},
                          {sunday, Sunday}]}) ->
    Name =
	case Name0 of
	    "Unspecified" -> ?UNSPEC_ACT;
	    _             -> Name0
	end,
    #eflex_activity{name = Name,
                    year_sum  = opt_decode_integer(YearSum),
                    week_sum  = opt_decode_integer(WeekSum),
                    monday    = opt_decode_integer(Monday),
                    tuesday   = opt_decode_integer(Tuesday),
                    wednesday = opt_decode_integer(Wednesday),
                    thursday  = opt_decode_integer(Thursday),
                    friday    = opt_decode_integer(Friday),
                    saturday  = opt_decode_integer(Saturday),
                    sunday    = opt_decode_integer(Sunday)};
simple_decode({configuration, [WorkingTime,
                               {lunch, Lunch} | Rest]}) ->
    io:format("Add configuration unit: minutes\n", []),
    simple_decode({configuration, [{unit, "minutes"},
				   WorkingTime,
				   {lunch, Lunch} | Rest]});
simple_decode({configuration, [{unit, Unit0},
			       WorkingTime,
                               {lunch, Lunch},
                               {workday, [WorkdayBreak]},
                               {freeday, [FreedayBreak]},
                               {activity_types, ActivityTypes},
                               {holidays, Holidays} | Rest0]}) ->
    Unit =
	case Unit0 of
	    "date"    -> date;
	    "minutes" -> minutes;
	    "decimal" -> decimal
	end,
    case Rest0 of 
	[{mini_break, MiniBreak} | Rest] -> ok;
	Rest -> MiniBreak = "0"
    end,

    {AbsenceAllDay, UnspecifiedWork} =
	case Rest of
	    [{absence_all_day, A}, {unspecified_work, U}] ->
		{A, U};
	    [{absence_all_day, A}] ->
		{A, undefined};
	    [{unspecified_work, U}] ->
		{undefined, U};
	    [] ->
		{undefined, undefined}
	end,

    #eflex_config{unit             = Unit,
		  working_time     = simple_decode(WorkingTime),
                  lunch_duration   = decode_integer(Lunch), 
                  workday_break    = simple_decode(WorkdayBreak),
                  freeday_break    = simple_decode(FreedayBreak),
                  activity_types   = simple_decode(ActivityTypes),
                  holidays         = simple_decode(Holidays),
		  absence_all_day  = AbsenceAllDay,
		  unspecified_work = UnspecifiedWork,
		  mini_break       = decode_integer(MiniBreak)};
simple_decode({working_time, [{monday, Monday},
                              {tuesday, Tuesday},
                              {wednesday, Wednesday},
                              {thursday, Thursday},
                              {friday, Friday},
                              {saturday, Saturday},
                              {sunday, Sunday}]}) ->
    #eflex_working_time{monday        = decode_integer(Monday),
                        tuesday       = decode_integer(Tuesday),
                        wednesday     = decode_integer(Wednesday),
                        thursday      = decode_integer(Thursday),
                        friday        = decode_integer(Friday),
                        saturday      = decode_integer(Saturday),
                        sunday        = decode_integer(Sunday)};
simple_decode({break, [{duration, Duration},
                       {time_before_break, TimeBeforeBreak}]}) ->
    #eflex_break{duration          = decode_integer(Duration),
                 time_before_break = decode_integer(TimeBeforeBreak)};
simple_decode({activity_type, [{name,       Name0},
                               {initial,    Initial},
                               {unit,       Unit0},
			       {category,   Category0},
			       {sign,       Sign0},
			       {visibility, Visibility0},
			       {share,      Share}]}) ->
    %% Backwards compatibility. Convert from old format
    Name =
	case Name0 of
	    "Unspecified" -> ?UNSPEC_ACT;
	    _             -> Name0
	end,
    Unit =
	case Unit0 of
	    "date"    -> date;
	    "minutes" -> minutes;
	    "decimal" -> decimal
	end,
    Category =
	case Category0 of
	    "attendance" -> attendance;
	    "project"    -> project
	end,
    Sign =
	case Sign0 of
	    "positive" -> positive;
	    "negative" -> negative
	end,
    Visibility =
	case Visibility0 of
	    "visible" -> visible;
	    "hidden"  -> hidden
	end,
    #eflex_activity_type{name       = Name,
			 initial    = decode_integer(Initial),
			 unit       = Unit,
			 category   = Category,
			 sign       = Sign,
			 visibility = Visibility,
			 share      = decode_integer(Share)};
simple_decode({activity_type, [{name,       Name},
                               {initial,    Initial},
                               {unit,       Unit},
			       {category,   Category},
			       {sign,       Sign},
			       {visibility, Visibility}]}) ->
    %% Backwards compatibility. Convert from old format
    io:format("Convert activity type: ~s\n", [Name]),
    simple_decode({activity_type, [{name,       Name},
				   {initial,    Initial},
				   {unit,       Unit},
				   {category,   Category},
				   {sign,       Sign},
				   {visibility, Visibility},
				   {share, "0"}]});
simple_decode({activity_type, [{name,    Name0},
			       {initial, Initial},
			       {unit,    Unit0} | Tail]}) ->
    %% Backwards compatibility. Convert from old format
    Name =
	case Name0 of
	    "Unspecified" -> ?UNSPEC_ACT;
	    _             -> Name0
	end,
    io:format("Convert activity type: ~s\n", [Name]),
    Unit =
	case Unit0 of
	    "date"    -> date;
	    "minutes" -> minutes;
	    "decimal" -> decimal
	end,
    Pick =
	fun(Expected, [{Key, _Val} | Rest]) when Key =:= Expected ->
		{true, Rest};
	   (_Expected, Rest) ->
		{false, Rest}
	end,
    {Negated, Tail2} = Pick(negated, Tail),
    Sign =
	case Negated of
	    true  -> negative;
	    false -> positive
	end,
    {Hidden, Tail3} = Pick(hidden, Tail2),
    Visibility =
	case Hidden of
	    true  -> hidden;
	    false -> visible
	end,
    {Duty, []} = Pick(duty, Tail3),
    Category =
	case Duty of
	    true  -> project;
	    false -> attendance
	end,
    #eflex_activity_type{name       = Name,
			 initial    = decode_integer(Initial),
			 unit       = Unit,
			 category   = Category,
			 sign       = Sign,
			 visibility = Visibility};
simple_decode({holiday, [{day,    Day},
			 {month,  Month}]}) ->
    #eflex_holiday{day   = decode_integer(Day),
		   month = decode_integer(Month)};
simple_decode(List) when is_list(List) ->
    [simple_decode(Elem) || Elem <- List].

decode_integer("") ->
    0;
decode_integer(IntStr) ->
    list_to_integer(IntStr).

opt_decode_integer("") ->
    undefined;
opt_decode_integer(IntStr) ->
    list_to_integer(IntStr).
