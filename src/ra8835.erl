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
%% debug
-export([dbx_direction/1, write_dbx/1, read_dbx/0]).

-import(lists, [foreach/2]).

-include("../include/ra8835.hrl").
-include("../include/ra8835_cfg.hrl").

-define(P0(I), ((1 bsl (I)))).

-define(LCD_A0,   ?P0(?LCD_A0_PIN)).       %%/ Command/Data select
-define(LCD_WR,   ?P0(?LCD_WR_PIN)).        %%  WR = RW (8080 style)
-define(LCD_RD,   ?P0(?LCD_RD_PIN)).        %% RD = E  (8080 style)

-define(LCD_CS,   ?P0(?LCD_CS_PIN)).      %% Chip select
-define(LCD_RST,  ?P0(?LCD_RST_PIB)).     %% Reset pin

-define(DB0, ?P0(?DB0_PIN)).
-define(DB1, ?P0(?DB1_PIN)).
-define(DB2, ?P0(?DB2_PIN)).
-define(DB3, ?P0(?DB3_PIN)).
-define(DB4, ?P0(?DB4_PIN)).
-define(DB5, ?P0(?DB5_PIN)).
-define(DB6, ?P0(?DB6_PIN)).
-define(DB7, ?P0(?DB7_PIN)).

-define(DB_MASK, (?DB0 bor ?DB1 bor ?DB2 bor ?DB3 bor
		  ?DB4 bor ?DB5 bor ?DB6 bor ?DB7)).

-define(TCC_DELAY(),     ok).
-define(COMMAND_DELAY(), ok).
-define(WRITE_DELAY(),   ok).
-define(READ_DELAY(),    ok).

dbx_direction(D) ->
    gpio:set_direction(?DB0_PIN, D),
    gpio:set_direction(?DB1_PIN, D),
    gpio:set_direction(?DB2_PIN, D),
    gpio:set_direction(?DB3_PIN, D),
    gpio:set_direction(?DB4_PIN, D),
    gpio:set_direction(?DB5_PIN, D),
    gpio:set_direction(?DB6_PIN, D),
    gpio:set_direction(?DB7_PIN, D).

write_dbx(Db) ->
    gpio:clr_mask(?DB_MASK),
    gpio:set_mask(byte_to_db(Db)).

read_dbx() ->
    {ok,Db} = gpio:get_mask(?DB_MASK),
    db_to_byte(Db).

status() ->
    gpio:set_direction(?DB6_PIN, in),
    gpio:clr(?LCD_A0_PIN),
    gpio:clr(?LCD_CS_PIN),
    %% T16ns;
    gpio:clr(?LCD_RD_PIN), %% At least later than CS&A0
    ?TCC_DELAY(), %% Strobe pulse delay
    Res = gpio:get(?DB6_PIN) =/= 0,
    gpio:set(?LCD_RD_PIN),
    gpio:set(?LCD_CS_PIN),
    gpio:set(?LCD_A0_PIN),
    gpio:set_direction(?DB6_PIN, out), 
    Res.

sync_data() ->
    gpio:clr(?LCD_A0_PIN),
    gpio:clr(?LCD_CS_PIN),
    %% T16ns;
    gpio:clr(?LCD_WR_PIN),   %% At least later than CS&A0
    ?TCC_DELAY(),  %% Strobe pulse delay
    gpio:set(?LCD_WR_PIN),
    gpio:set(?LCD_CS_PIN),
    gpio:set(?LCD_A0_PIN),
    ?WRITE_DELAY().

write_byte(Data) ->
    write_dbx(Data),
    sync_data().

instruction(I) ->
    write_dbx(I),
    gpio:clr(?LCD_CS_PIN),
    %% T16ns;
    gpio:clr(?LCD_WR_PIN),  %% At least later than CS&A0
    ?TCC_DELAY(), %% T16ns;  // Strobe pulse delay
    gpio:set(?LCD_WR_PIN),
    gpio:set(?LCD_CS_PIN),
    ?COMMAND_DELAY().

read_byte() ->
    gpio:clr(?LCD_CS_PIN),
    %% T16ns;
    gpio:clr(?LCD_RD_PIN), %% At least later than CS&A0
    ?TCC_DELAY(), %% Strobe pulse delay
    Data = read_dbx(),
    gpio:set(?LCD_RD_PIN),
    %% T16ns;
    gpio:set(?LCD_CS_PIN),
    ?READ_DELAY(), %% Output disable time
    Data.

%% LCD_68_80=0, (=8080) Then set output direction
%% if pin is not defined then the pin should have been wired to ground!
-ifdef(LCD_68_80_PIN).
init_68_80_pin() ->
    gpio:init(?LCD_CS_PIN).
set_8080_mode() ->
    gpio:set_direction(?LCD_68_80_PIN, low).
-else.
init_68_80_pin() ->
    ok.
set_8080_mode() ->
    ok.
-endif.

init() ->
    application:start(gpio),
    gpio:init(?LCD_A0_PIN),
    gpio:init(?LCD_WR_PIN),
    gpio:init(?LCD_RD_PIN),

    init_68_80_pin(),
			 
    gpio:init(?LCD_CS_PIN),
    gpio:init(?LCD_RST_PIN),

    gpio:init(?DB0_PIN),
    gpio:init(?DB1_PIN),
    gpio:init(?DB2_PIN),
    gpio:init(?DB3_PIN),
    gpio:init(?DB4_PIN),
    gpio:init(?DB5_PIN),
    gpio:init(?DB6_PIN),
    gpio:init(?DB7_PIN),

    dbx_direction(low),    %% init low output

    set_8080_mode(),  %% 8080 signaling mode only 

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
    dbx_direction(in),
    Addr0 = read_byte(),
    Addr = (read_byte() bsl 8) bor Addr0,
    dbx_direction(out),
    Addr.

mwrite(Src) ->
    instruction(?RA8835_CMD_MWRITE),
    write_loop(Src).

mread(Len) ->
    instruction(?RA8835_CMD_MREAD),
    dbx_direction(in),
    Data = read_loop(Len),
    dbx_direction(out),
    Data.

mfill(Data, Len) ->
    instruction(?RA8835_CMD_MWRITE),
    if Len > 0 ->
	    write_byte(Data),
	    sync_loop(Len-1);
       true ->
	    ok
    end.

write_loop([C|Cs]) ->
    write_byte(C),
    write_loop(Cs);
write_loop([]) ->
    ok.

read_loop(0) ->
    [];
read_loop(I) ->
    [read_byte() | read_loop(I-1)].

sync_loop(0) ->
    ok;
sync_loop(I) ->
    sync_data(),
    sync_loop(I-1).

%% db_to_byte pick DB0-DB7 from 32 bit register
%% DB0..DB4 -> 7..11  DB5..DB7 -> 22..24
db_to_byte(X) ->
    (((X) bsr 7) band 16#1f) bor (((X) bsr (22-5)) band 16#e0).

byte_to_db(B) ->
    ((B band 16#e0) bsl (22-5)) bor ((B band 16#1f) bsl 7).


%% byte_to_db() must be generated
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
    io:format("byte_to_db_tab() -> \n{ 16#~8.16.0B", [hd(Db)]),
    foreach(fun(D) -> io:format(",16#~8.16.0B", [D]) end, tl(Db)),
    io:format("}.\n").

%% make this faster...
%% db_to_byte(X) -> 
%%     (if X band ?DB0 =:= 0 -> 0; true -> 16#01 end) + 
%%     (if X band ?DB1 =:= 0 -> 0; true -> 16#02 end) + 
%%     (if X band ?DB2 =:= 0 -> 0; true -> 16#04 end) + 
%%     (if X band ?DB3 =:= 0 -> 0; true -> 16#08 end) + 
%%     (if X band ?DB4 =:= 0 -> 0; true -> 16#10 end) + 
%%     (if X band ?DB5 =:= 0 -> 0; true -> 16#20 end) + 
%%     (if X band ?DB6 =:= 0 -> 0; true -> 16#40 end) + 
%%     (if X band ?DB7 =:= 0 -> 0; true -> 16#80 end).

%%
%% generate when needed (with make_byte_to_db() )
%%

%% byte_to_db(X) ->
%%     element((X band 16#ff)+1, byte_to_db_tab()).

%% byte_to_db_tab() -> 
%% { 16#00000000,16#00000080,16#00000100,16#00000180,16#00000200,16#00000280,16#00000300,16#00000380,16#00000400,16#00000480,16#00000500,16#00000580,16#00000600,16#00000680,16#00000700,16#00000780,16#00000800,16#00000880,16#00000900,16#00000980,16#00000A00,16#00000A80,16#00000B00,16#00000B80,16#00000C00,16#00000C80,16#00000D00,16#00000D80,16#00000E00,16#00000E80,16#00000F00,16#00000F80,16#00400000,16#00400080,16#00400100,16#00400180,16#00400200,16#00400280,16#00400300,16#00400380,16#00400400,16#00400480,16#00400500,16#00400580,16#00400600,16#00400680,16#00400700,16#00400780,16#00400800,16#00400880,16#00400900,16#00400980,16#00400A00,16#00400A80,16#00400B00,16#00400B80,16#00400C00,16#00400C80,16#00400D00,16#00400D80,16#00400E00,16#00400E80,16#00400F00,16#00400F80,16#00800000,16#00800080,16#00800100,16#00800180,16#00800200,16#00800280,16#00800300,16#00800380,16#00800400,16#00800480,16#00800500,16#00800580,16#00800600,16#00800680,16#00800700,16#00800780,16#00800800,16#00800880,16#00800900,16#00800980,16#00800A00,16#00800A80,16#00800B00,16#00800B80,16#00800C00,16#00800C80,16#00800D00,16#00800D80,16#00800E00,16#00800E80,16#00800F00,16#00800F80,16#00C00000,16#00C00080,16#00C00100,16#00C00180,16#00C00200,16#00C00280,16#00C00300,16#00C00380,16#00C00400,16#00C00480,16#00C00500,16#00C00580,16#00C00600,16#00C00680,16#00C00700,16#00C00780,16#00C00800,16#00C00880,16#00C00900,16#00C00980,16#00C00A00,16#00C00A80,16#00C00B00,16#00C00B80,16#00C00C00,16#00C00C80,16#00C00D00,16#00C00D80,16#00C00E00,16#00C00E80,16#00C00F00,16#00C00F80,16#01000000,16#01000080,16#01000100,16#01000180,16#01000200,16#01000280,16#01000300,16#01000380,16#01000400,16#01000480,16#01000500,16#01000580,16#01000600,16#01000680,16#01000700,16#01000780,16#01000800,16#01000880,16#01000900,16#01000980,16#01000A00,16#01000A80,16#01000B00,16#01000B80,16#01000C00,16#01000C80,16#01000D00,16#01000D80,16#01000E00,16#01000E80,16#01000F00,16#01000F80,16#01400000,16#01400080,16#01400100,16#01400180,16#01400200,16#01400280,16#01400300,16#01400380,16#01400400,16#01400480,16#01400500,16#01400580,16#01400600,16#01400680,16#01400700,16#01400780,16#01400800,16#01400880,16#01400900,16#01400980,16#01400A00,16#01400A80,16#01400B00,16#01400B80,16#01400C00,16#01400C80,16#01400D00,16#01400D80,16#01400E00,16#01400E80,16#01400F00,16#01400F80,16#01800000,16#01800080,16#01800100,16#01800180,16#01800200,16#01800280,16#01800300,16#01800380,16#01800400,16#01800480,16#01800500,16#01800580,16#01800600,16#01800680,16#01800700,16#01800780,16#01800800,16#01800880,16#01800900,16#01800980,16#01800A00,16#01800A80,16#01800B00,16#01800B80,16#01800C00,16#01800C80,16#01800D00,16#01800D80,16#01800E00,16#01800E80,16#01800F00,16#01800F80,16#01C00000,16#01C00080,16#01C00100,16#01C00180,16#01C00200,16#01C00280,16#01C00300,16#01C00380,16#01C00400,16#01C00480,16#01C00500,16#01C00580,16#01C00600,16#01C00680,16#01C00700,16#01C00780,16#01C00800,16#01C00880,16#01C00900,16#01C00980,16#01C00A00,16#01C00A80,16#01C00B00,16#01C00B80,16#01C00C00,16#01C00C80,16#01C00D00,16#01C00D80,16#01C00E00,16#01C00E80,16#01C00F00,16#01C00F80}.
