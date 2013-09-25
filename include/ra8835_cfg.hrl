%%
%% Pin configuration for RA8835
%%

%% only using 8088 signalling

%% (blacklist i2c)
-define(LCD_A0_PIN,  2).   
-define(LCD_WR_PIN,  3).   %% or R/W
-define(LCD_RD_PIN,  4).   %% or E

%% LCD_68_80_PIN should/could be connected to VDD or set low if defined!
-define(LCD_CS_PIN,     18).  %% Chip select
%% -define(LCD_68_80_PIN,  nn).  %% SEL1 8080=low, 6800=high
-define(LCD_RST_PIN,    17).  %% Reset pin

%% raspberry pin selection 
%% (blacklist spi)
-define(DB0_PIN, 7).
-define(DB1_PIN, 8).
-define(DB2_PIN, 9).
-define(DB3_PIN, 10).
-define(DB4_PIN, 11).
-define(DB5_PIN, 22).
-define(DB6_PIN, 23).
-define(DB7_PIN, 24).
