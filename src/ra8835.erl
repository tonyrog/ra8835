%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2013, Tony Rogvall
%%% @doc
%%%    RA8835 display controller api
%%% @end
%%% Created : 26 Apr 2013 by Tony Rogvall <tony@rogvall.se>

-module(ra8835).

-export([init/0,
	 wait_busy/0,
	 write_addr/1, 
	 system_set/7,
	 scroll/6,
	 cursor_form/2,
	 sleepin/0,
	 display_on/1,
	 display_off/1,
	 cursor_right/0,
	 cursor_left/0,
	 cursor_up/0,
	 cursor_down/0,
	 overlay/1,
	 cgram_addr/1,
	 hdot_scroll/1,
	 set_cursor/1,
	 get_cursor/0,
	 mwrite/1,
	 mread/1,
	 mfill/2]).

-export([make_byte_to_db/0]).
-export([byte_to_db/1]).
-export([db_to_byte/1]).

-import(lists, [foreach/2]).

-define(RA8835_CMD_SYSTEM_SET,  16#40).  %% Initialize Device and display
-define(RA8835_CMD_SLEEP_IN,    16#53).  %% Enter standby mode
-define(RA8835_CMD_DISP_OFF,    16#58).  %% Turn display OFF
-define(RA8835_CMD_DISP_ON,     16#59).  %% Turn display ON 
-define(RA8835_CMD_SCROLL,      16#44).  %% Set dislpay start address and region
-define(RA8835_CMD_CSRFORM,     16#5D).  %% Set cursor byte 
-define(RA8835_CMD_CGRAM_ADDR,  16#5C).  %% Set start address of char generator
-define(RA8835_CMD_CUR_RIGHT,   16#4C).  %% Cursor direction
-define(RA8835_CMD_CUR_LEFT,    16#4D).  %% Cursor direction
-define(RA8835_CMD_CUR_UP,      16#4E).  %% Cursor direction
-define(RA8835_CMD_CUR_DOWN,    16#4F).  %% Cursor direction
-define(RA8835_CMD_HDOT_SCROLL, 16#5A).  %% Set horizontal scroll position 
-define(RA8835_CMD_OVLAY,       16#5B).  %% Set display overlay format 
-define(RA8835_CMD_CSRW,        16#46).  %% Set cursor address 
-define(RA8835_CMD_CSRR,        16#47).  %% Read cursor address (2 bytes)
-define(RA8835_CMD_MWRITE,      16#42).  %% Write to display memory -
-define(RA8835_CMD_MREAD,       16#43).  %% Read from display memory - 

%% -define(LCD_RW_PIN,  5).
%% -define(LCD_E_PIN,   6).

-define(LCD_A0_PIN,  4).   %% LCD_RS_PIN
%% -define(LCD_RS_PIN,  4).
-define(LCD_WR_PIN,  5).
-define(LCD_RD_PIN,  6).

-define(LCD_CS_PIN,     19).
%% LCD_68_80_PIN should/could be connected to VDD or VSS!
-define(LCD_68_80_PIN,  20).  %% may be constant!
-define(LCD_RST_PIN,    22).

-define(DB0_PIN, 9).
-define(DB1_PIN, 10).
-define(DB2_PIN, 11).
-define(DB3_PIN, 12).
-define(DB4_PIN, 13).
-define(DB5_PIN, 15).
-define(DB6_PIN, 17).
-define(DB7_PIN, 18).

-define(P0(I), ((1 bsl (I)))).

-define(LCD_A0,    ?P0(?LCD_A0_PIN)).       %%/ Command/Data select
%% -define(LCD_RW,    ?P0(?LCD_RW_PIN)).       %% Read/Write select (6800 style)
%% -define(LCD_E,     ?P0(?LCD_E_PIN)).       %% Enable  ( 6800 style)

%% -define(LCD_RS,   ?P0(?LCD_RS_PIN)).        %%  C/D | D/I | A0... 
-define(LCD_WR,   ?P0(?LCD_WR_PIN)).        %%  WR = RW (8080 style)
-define(LCD_RD,   ?P0(?LCD_RD_PIN)).        %% RD = E  (8080 style)

-define(LCD_CS,     ?P0(?LCD_CS_PIN)).      %% Chip select
-define(LCD_68_80,  ?P0(?LCD_68_80_PIN)).   %% signaling style
-define(LCD_RST,    ?P0(?LCD_RST_PIB)).     %% Reset pin

-define(DB0,  ?P0(?DB0_PIN)).
-define(DB1,  ?P0(?DB1_PIN)).
-define(DB2,  ?P0(?DB2_PIN)).
-define(DB3,  ?P0(?DB3_PIN)).
-define(DB4,  ?P0(?DB4_PIN)).
-define(DB5,  ?P0(?DB5_PIN)).
-define(DB6,  ?P0(?DB6_PIN)).
-define(DB7,  ?P0(?DB7_PIN)).

-define(DB_MASK, (?DB0 bor ?DB1 bor ?DB2 bor ?DB3 bor
		  ?DB4 bor ?DB5 bor ?DB6 bor ?DB7)).

-define(TCC_DELAY(),     ok).
-define(COMMAND_DELAY(), ok).
-define(WRITE_DELAY(),   ok).
-define(READ_DELAY(),    ok).

%% byte_to_db() must be generated
%% 
make_byte_to_db() ->
    Db = [ 
	   (if I band 16#01 =:= 0 -> 0; true -> ?DB0 end) +
	   (if I band 16#02 =:= 0 -> 0; true -> ?DB1 end) +
	   (if I band 16#04 =:= 0 -> 0; true -> ?DB2 end) +
	   (if I band 16#08 =:= 0 -> 0; true -> ?DB3 end) +
	   (if I band 16#10 =:= 0 -> 0; true -> ?DB4 end) +
	   (if I band 16#20 =:= 0 -> 0; true -> ?DB5 end) +
	   (if I band 16#40 =:= 0 -> 0; true -> ?DB6 end) +
	   (if I band 16#80 =:= 0 -> 0; true -> ?DB7 end) 
	   || I <- lists:seq(0, 255)],
    io:format("byte_to_db() -> \n{ 16#~8.16.0B", [hd(Db)]),
    foreach(fun(D) -> io:format(",16#~8.16.0B", [D]) end, tl(Db)),
    io:format("}.\n").

%% make this faster...
db_to_byte(X) -> 
    (if X band ?DB0 =:= 0 -> 0; true -> 16#01 end) + 
    (if X band ?DB1 =:= 0 -> 0; true -> 16#02 end) + 
    (if X band ?DB2 =:= 0 -> 0; true -> 16#04 end) + 
    (if X band ?DB3 =:= 0 -> 0; true -> 16#08 end) + 
    (if X band ?DB4 =:= 0 -> 0; true -> 16#10 end) + 
    (if X band ?DB5 =:= 0 -> 0; true -> 16#20 end) + 
    (if X band ?DB6 =:= 0 -> 0; true -> 16#40 end) + 
    (if X band ?DB7 =:= 0 -> 0; true -> 16#80 end).

set_dbx_input() ->
    gpio:set_input_mask(0, ?DB_MASK).

set_dbx_output() ->
    gpio:set_output_mask(0, ?DB_MASK).

write_dbx(Db) ->
    gpio:clr_mask(0, ?DB_MASK),
    gpio:set_mask(0, byte_to_db(Db)).

read_dbx() ->
    db_to_byte(gpio:get_mask(0)).

status() ->
    gpio:set_input_mask(0, ?DB6), 
    gpio:clr_mask(0, ?LCD_A0),
    gpio:clr_mask(0, ?LCD_CS),
    %% T16ns;
    gpio:clr_mask(0, ?LCD_RD), %% At least later than CS&A0
    ?TCC_DELAY(), %% Strobe pulse delay
    Res = (gpio:get_mask(0) band ?DB6) =/= 0,
    gpio:set_mask(0, ?LCD_RD),
    gpio:set_mask(0, ?LCD_CS),
    gpio:set_mask(0, ?LCD_A0),
    gpio:set_output_mask(0, ?DB6), 
    Res.

sync_data() ->
    gpio:clr_mask(0, ?LCD_A0),
    gpio:clr_mask(0, ?LCD_CS),
    %% T16ns;
    gpio:clr_mask(0, ?LCD_WR),   %% At least later than CS&A0
    ?TCC_DELAY(),  %% Strobe pulse delay
    gpio:set_mask(0, ?LCD_WR),
    gpio:set_mask(0, ?LCD_CS),
    gpio:set_mask(0, ?LCD_A0),
    ?WRITE_DELAY().

write_byte(Data) ->
    write_dbx(Data),
    sync_data().

instruction(I) ->
    write_dbx(I),
    gpio:clr_mask(0, ?LCD_CS),
    %% T16ns;
    gpio:clr_mask(0, ?LCD_WR),  %% At least later than CS&A0
    ?TCC_DELAY(), %% T16ns;  // Strobe pulse delay
    gpio:set_mask(0, ?LCD_WR),
    gpio:set_mask(0, ?LCD_CS),
    ?COMMAND_DELAY().

read_byte() ->
    gpio:clr_mask(0, ?LCD_CS),
    %% T16ns;
    gpio:clr_mask(0, ?LCD_RD), %% At least later than CS&A0
    ?TCC_DELAY(), %% Strobe pulse delay
    Data = read_dbx(),
    gpio:set_mask(0, ?LCD_RD),
    %% T16ns;
    gpio:set_mask(0, ?LCD_CS),
    ?READ_DELAY(), %% Output disable time
    Data.

init() ->
    gpio:clr_mask(?DB_MASK),  %% Clear data pins
    set_dbx_output(),     %% Default to output on data pins

    %% LCD_68_80=0, (=8080) Then set output direction
    gpio:set_direction(?LCD_68_80_PIN, low),  

    %% Set default output values
    gpio:set_direction(?LCD_RD_PIN,  high),
    gpio:set_direction(?LCD_WR_PIN,  high),
    gpio:set_direction(?LCD_A0_PIN,  high),
    gpio:set_direction(?LCD_CS_PIN,  high),

    gpio:set_direction(?LCD_RST_PIN, high),

    %% Reset the 8835
    gpio:clr(?LCD_RST_PIN),
    timer:sleep(2),
    gpio:set(?LCD_RST_PIN),
    timer:sleep(4).


wait_busy() ->
    case status() of
	false -> wait_busy();
	true -> ok
    end.

write_addr(Addr) ->
    write_byte(Addr band 16#ff),
    write_byte(Addr bsr  8).

system_set(P1, P2, Fy, Cr, Tcr, Lf, Ap) ->
    instruction(?RA8835_CMD_SYSTEM_SET),
    %% TIME_WAIT_1US();
    write_byte(P1),
    write_byte(P2),
    write_byte(Fy),
    write_byte(Cr),
    write_byte(Tcr),
    write_byte(Lf),
    write_addr(Ap).

%%
%%  Set up screen render registers (may be used to scroll)
%%
scroll(SDA1, SL1, SDA2, SL2, SDA3, SDA4) ->
    instruction(?RA8835_CMD_SCROLL),
    write_addr(SDA1),
    write_byte(SL1),
    write_addr(SDA2),
    write_byte(SL2),
    write_addr(SDA3),
    write_addr(SDA4).

cursor_form(Crx, Cry) ->
    instruction(?RA8835_CMD_CSRFORM),
    write_byte(Crx),
    write_byte(Cry).

sleepin() ->
    instruction(?RA8835_CMD_SLEEP_IN).

display_on(Flash) ->
    instruction(?RA8835_CMD_DISP_ON),
    write_byte(Flash).

display_off(Flash) ->
    instruction(?RA8835_CMD_DISP_OFF),
    write_byte(Flash).

cursor_right() ->
    instruction(?RA8835_CMD_CUR_RIGHT).

cursor_left() ->
    instruction(?RA8835_CMD_CUR_LEFT).

cursor_up() ->
    instruction(?RA8835_CMD_CUR_UP).

cursor_down() ->
    instruction(?RA8835_CMD_CUR_DOWN).

overlay(Param) ->
    instruction(?RA8835_CMD_OVLAY),
    write_byte(Param).

cgram_addr(Addr) ->
    instruction(?RA8835_CMD_CGRAM_ADDR),
    write_addr(Addr).

hdot_scroll(Param) ->
    instruction(?RA8835_CMD_HDOT_SCROLL),
    write_byte(Param band 16#7).

set_cursor(Addr) ->
    instruction(?RA8835_CMD_CSRW),
    write_addr(Addr).

get_cursor() ->
    instruction(?RA8835_CMD_CSRR),
    set_dbx_input(),
    Addr0 = read_byte(),
    Addr = (read_byte() bsl 8) bor Addr0,
    set_dbx_output(),
    Addr.

mwrite(Src) ->
    instruction(?RA8835_CMD_MWRITE),
    foreach(fun(C) -> write_byte(C) end, Src).

mread(Len) ->
    instruction(?RA8835_CMD_MREAD),
    set_dbx_input(),
    Data = [ read_byte() || _ <- lists:seq(1, Len)],
    set_dbx_output(),
    Data.

mfill(Data, Len) ->
    instruction(?RA8835_CMD_MWRITE),
    if Len > 0 ->
	    write_byte(Data),
	    %% Sync dame data over agin
	    foreach(fun(_) -> sync_data() end, list:seq(1, Len-1));
       true ->
	    ok
    end.


%%
%% generate when needed (with make_byte_to_db() )
%%
byte_to_db(X) ->
    element((X band 16#ff)+1,
{ 16#00000000,16#00000200,16#00000400,16#00000600,16#00000800,16#00000A00,16#00000C00,16#00000E00,16#00001000,16#00001200,16#00001400,16#00001600,16#00001800,16#00001A00,16#00001C00,16#00001E00,16#00002000,16#00002200,16#00002400,16#00002600,16#00002800,16#00002A00,16#00002C00,16#00002E00,16#00003000,16#00003200,16#00003400,16#00003600,16#00003800,16#00003A00,16#00003C00,16#00003E00,16#00008000,16#00008200,16#00008400,16#00008600,16#00008800,16#00008A00,16#00008C00,16#00008E00,16#00009000,16#00009200,16#00009400,16#00009600,16#00009800,16#00009A00,16#00009C00,16#00009E00,16#0000A000,16#0000A200,16#0000A400,16#0000A600,16#0000A800,16#0000AA00,16#0000AC00,16#0000AE00,16#0000B000,16#0000B200,16#0000B400,16#0000B600,16#0000B800,16#0000BA00,16#0000BC00,16#0000BE00,16#00020000,16#00020200,16#00020400,16#00020600,16#00020800,16#00020A00,16#00020C00,16#00020E00,16#00021000,16#00021200,16#00021400,16#00021600,16#00021800,16#00021A00,16#00021C00,16#00021E00,16#00022000,16#00022200,16#00022400,16#00022600,16#00022800,16#00022A00,16#00022C00,16#00022E00,16#00023000,16#00023200,16#00023400,16#00023600,16#00023800,16#00023A00,16#00023C00,16#00023E00,16#00028000,16#00028200,16#00028400,16#00028600,16#00028800,16#00028A00,16#00028C00,16#00028E00,16#00029000,16#00029200,16#00029400,16#00029600,16#00029800,16#00029A00,16#00029C00,16#00029E00,16#0002A000,16#0002A200,16#0002A400,16#0002A600,16#0002A800,16#0002AA00,16#0002AC00,16#0002AE00,16#0002B000,16#0002B200,16#0002B400,16#0002B600,16#0002B800,16#0002BA00,16#0002BC00,16#0002BE00,16#00040000,16#00040200,16#00040400,16#00040600,16#00040800,16#00040A00,16#00040C00,16#00040E00,16#00041000,16#00041200,16#00041400,16#00041600,16#00041800,16#00041A00,16#00041C00,16#00041E00,16#00042000,16#00042200,16#00042400,16#00042600,16#00042800,16#00042A00,16#00042C00,16#00042E00,16#00043000,16#00043200,16#00043400,16#00043600,16#00043800,16#00043A00,16#00043C00,16#00043E00,16#00048000,16#00048200,16#00048400,16#00048600,16#00048800,16#00048A00,16#00048C00,16#00048E00,16#00049000,16#00049200,16#00049400,16#00049600,16#00049800,16#00049A00,16#00049C00,16#00049E00,16#0004A000,16#0004A200,16#0004A400,16#0004A600,16#0004A800,16#0004AA00,16#0004AC00,16#0004AE00,16#0004B000,16#0004B200,16#0004B400,16#0004B600,16#0004B800,16#0004BA00,16#0004BC00,16#0004BE00,16#00060000,16#00060200,16#00060400,16#00060600,16#00060800,16#00060A00,16#00060C00,16#00060E00,16#00061000,16#00061200,16#00061400,16#00061600,16#00061800,16#00061A00,16#00061C00,16#00061E00,16#00062000,16#00062200,16#00062400,16#00062600,16#00062800,16#00062A00,16#00062C00,16#00062E00,16#00063000,16#00063200,16#00063400,16#00063600,16#00063800,16#00063A00,16#00063C00,16#00063E00,16#00068000,16#00068200,16#00068400,16#00068600,16#00068800,16#00068A00,16#00068C00,16#00068E00,16#00069000,16#00069200,16#00069400,16#00069600,16#00069800,16#00069A00,16#00069C00,16#00069E00,16#0006A000,16#0006A200,16#0006A400,16#0006A600,16#0006A800,16#0006AA00,16#0006AC00,16#0006AE00,16#0006B000,16#0006B200,16#0006B400,16#0006B600,16#0006B800,16#0006BA00,16#0006BC00,16#0006BE00}).

