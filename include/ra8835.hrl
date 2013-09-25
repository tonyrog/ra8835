-ifndef(__RA8835_HRL__).
-define(__RA8835_HRL__, true).

-define(DISPLAY_WIDTH,  240).
-define(DISPLAY_HEIGHT, 128).

%% Parameters needed for the RA8835 controler setup 
-define(PARAM_M0, 0).       %% 0:Internal CG ROM, 1:External CG ROM
-define(PARAM_M1, 0).       %% 0:RAM1 RAM not contiguous, 1: contiguous
-define(PARAM_M2, 0).       %% 0: 8 pixel char,  1: 16 pixel char
-define(PARAM_WS, 0).       %% 0: Single panel, 1: Dual panel
-define(PARAM_IV, 1).       %% 0: Screen top-line correction, 1:No 
-define(PARAM_FX, 7).       %% Horizontal char size FX=[FX]-1
-define(PARAM_FY, 7).       %% Vertical char size FY=[FY]-1
-define(PARAM_WF, 1).       %% 0: 16-line AC drive, 1: two frame AC drive
 %% Bytes per display line
-define(PARAM_CR, ((?DISPLAY_WIDTH div (?PARAM_FX+1))-1)).
%% Length of line (+4)
-define(PARAM_TCR, (?PARAM_CR + 4)).
-define(PARAM_LF,  (?DISPLAY_HEIGHT-1)).
%% No virtual screen AP=CR+1
-define(PARAM_AP,  (?PARAM_CR+1)).
-define(PARAM_LINES, (?DISPLAY_HEIGHT div (?PARAM_FY+1))).

-define(PARAM_SAD1, 16#0000).
-define(PARAM_SL1,  ?PARAM_LF).          %% IV=1 => 0..LF,  IV=0 => 0..LF+1
-define(PARAM_SAD2, 16#01E0).
-define(PARAM_SL2,  ?PARAM_LF).           %% IV=1 => 0..LF,  IV=0 => 0..LF+1
-define(PARAM_SAD3, 16#0000).
-define(PARAM_SAD4, 16#0000).

-define(PARAM_CRX, 16#04).
-define(PARAM_CRY, 16#07).
-define(PARAM_CM,  0).
-define(PARAM_MX0, 1).
-define(PARAM_MX1, 0).
-define(PARAM_DM1, 0).
-define(PARAM_DM2, 0).
-define(PARAM_OV,  0).      

-define(PARAM_SAG,          16#7000).
-define(PARAM_FLASH,        16#16).
-define(PARAM_TEXTSIZE,     ?PARAM_SAD2).
-define(PARAM_GRAPHICSTART, ?PARAM_SAD2).
-define(PARAM_GRAPHICSIZE,  ((?DISPLAY_WIDTH bsr 3) * ?DISPLAY_HEIGHT)).
-define(PARAM_MEM_END,      16#8000).

-define(PARAM_SYS_P1,
	(16#10 bor (?PARAM_IV bsl 5) bor
	     (?PARAM_WS bsl 3) bor (?PARAM_M2 bsl 2) bor
	     (?PARAM_M1 bsl 1) bor (?PARAM_M0))).

-define(PARAM_SYS_P2, ((?PARAM_WF bsl 7) bor (?PARAM_FX))).

-define(PARAM_CSRF_P2, ((?PARAM_CM bsl 7) bor ?PARAM_CRY)).

-define(PARAM_OVLAY_P1, 
	((?PARAM_OV bsl 4) bor (?PARAM_DM2 bsl 3) bor
	     (?PARAM_DM1 bsl 2) bor (?PARAM_MX1 bsl 1) bor (?PARAM_MX0))).
-endif.
