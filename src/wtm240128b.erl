%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    WTM240128B LCD display global parameters
%%% @end
%%% Created : 26 Apr 2013 by Tony Rogvall <tony@rogvall.se>

-module(wtm240128b).

-export([lcd_controller/0,
	 lcd_width/0,
	 lcd_height/0,
	 lcd_ram_size/0]).

%% controller module
lcd_controller() ->
    gpio_ra8835.

%% Number of horizontal pixels
lcd_width() ->
    240.

%% Number of vertical pixels
lcd_height() ->
    128.

%% Size of ram
lcd_ram_size() ->
    16#8000.
