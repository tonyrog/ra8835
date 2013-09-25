%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    Display routines using ra8835 controller
%%% @end
%%% Created : 26 Apr 2013 by Tony Rogvall <tony@rogvall.se>

-module(ra8835_display).

-include("ra8835.hrl").

-define(base_addr,      ?PARAM_GRAPHICSTART).
-define(pixel_addr(X,Y), (?base_addr+((?DISPLAY_WIDTH bsr 3)*(Y))+
			      ((X) bsr 3))).

-export([memset/3,
	 set_pixel/3, 
	 get_pixel/2, 
	 horizontal_line/4, 
	 vertical_line/4,
	 goto_xy/2,
	 clear/0,
	 write_char/1,
	 write_string/1,
	 on/0, 
	 off/0,
	 create/0,
	 init/0]).

-import(lists, [foreach/2]).


memset(Addr, Data, Len) ->
    ra8835:set_cursor(Addr),
    ra8835:mfill(Data, Len).

get_pixel(X, Y) ->
    Addr = ?pixel_addr(X,Y),
    ra8835:set_cursor(Addr),
    [Data] = ra8835:mread(1),
    Bit = (1 bsl (7 - (X band 16#7))),
    (Data band Bit) =/= 0.

set_pixel(X, Y, Color) ->
    Addr = ?pixel_addr(X,Y),
    ra8835:set_cursor(Addr),
    [Data] = ra8835:mread(1),
    Bit = (1 bsl (7 - (X band 16#7))),
    Data1 = if Color =/= 0 ->
		    Data bor Bit;
	       true ->
		    Data band (bnot Bit)
	    end,
    ra8835:set_cursor(Addr),
    ra8835:mwrite([Data1]).

horizontal_line(X1, X2, Y, Color) ->
    foreach(fun(X) -> set_pixel(X, Y, Color) end, lists:seq(X1, X2)).

vertical_line(X, Y1, Y2, Color) ->
    foreach(fun(Y) -> set_pixel(X, Y, Color) end, lists:seq(Y1, Y2)).

goto_xy(X, Y) ->
    ra8835:set_cursor((?DISPLAY_WIDTH div 8)*Y + X).

clear() ->
    Addr = ra8835:get_cursor(),
    memset(16#0000, $\s, ?PARAM_TEXTSIZE),
    memset(?PARAM_GRAPHICSTART, 0, ?PARAM_GRAPHICSIZE),
    ra8835:set_cursor(Addr).

write_char(Char) ->
    ra8835:mfill(Char, 1).

write_string(String) ->
    list:foreach(fun write_char/1, String).

on() ->
    ra8835:display_on(0).

off() ->
    ra8835:display_off(0).


create() ->
    ra8835:system_set(?PARAM_SYS_P1, ?PARAM_SYS_P2,
		      ?PARAM_FY, ?PARAM_CR, ?PARAM_TCR, ?PARAM_LF,
		      ?PARAM_AP),
    ra8835:scroll(?PARAM_SAD1, ?PARAM_SL1,
		  ?PARAM_SAD2, ?PARAM_SL2,
		  ?PARAM_SAD3, ?PARAM_SAD4),
    ra8835:cursor_form(?PARAM_CRX, ?PARAM_CSRF_P2),
    ra8835:cgram_addr(?PARAM_SAG),
    ra8835:cursor_right(),
    ra8835:hdot_scroll(0),   %% Reset dot scroll to 0 [0-7]
    ra8835:overlay(?PARAM_OVLAY_P1),
    ra8835:display_on(?PARAM_FLASH).

init() ->
    ra8835:init().

