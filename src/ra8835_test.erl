%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    Small test program to drive RA8835
%%% @end
%%% Created : 25 Sep 2013 by Tony Rogvall <tony@rogvall.se>

-module(ra8835_test).

-compile(export_all).

run() ->
    init(),
    loop(10000).

init() ->
    %% application:start(gpio),
    gpio_sup:start_link([{chipset,bcm2835}]),

    ra8835_display:init(),
    ra8835_display:create().

loop(0) ->
    ok;
loop(I) ->
    X = random:uniform(240)-1,
    Y = random:uniform(128)-1,
    C = random:uniform(2)-1,
    ra8835_display:set_pixel(X, Y, C),
    %% timer:sleep(100),
    loop(I-1).
