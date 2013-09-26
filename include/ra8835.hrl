-ifndef(__RA8835_HRL__).
-define(__RA8835_HRL__, true).



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


-endif.
