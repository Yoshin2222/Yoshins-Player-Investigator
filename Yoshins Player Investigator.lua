------------------------------MACROS------------------------------
rb = memory.readbyte  rw = memory.readword  rd = memory.readdword wb = memory.writebyte ww = memory.writeword wd = memory.writedword
--Based on 6502 Machine code cause i like it :p
txt = gui.text asl = bit.lshift lsr = bit.rshift
------------------------------VARS------------------------------
local games = {
--Name, Timer, charselct timer, P1 UID, P2 UID,Number of Players, MaxTimer, Maxcharselect timer, Animelem Length
	["Invalid"] = {}, -- null case
	--GAME NAME			STRING
					   --P1 UID	  P2 UID	   
					   --FIGHT TIMER ADR/VAL  TIMER SIZE 
					   --NUMBER OF PLAYERS
					   --SELECT TIMER ADR/VAL  TIMER SIZE
	["asurabus"]    = {"asurabus",	
						0x4034EB,0x4041A5, 
						0x40000A,  0x99,   wb, 
						2,},
    ["dstlk"]    	= {"dstlk",
						0xFF8388, 0xFF8788,
						0xFF9409, 0x9900  , ww,
						2,
						0xFFF418, 0x0384  , ww,},
    ["garou"]    	= {"garou",
						0x100400, 0x100500,
						0x107490, 0x60    ,  wb, 
						2, 
						0x102B16, 0x09    ,  wb,}
}
LOOK_VALUE = 0x03 --Originally meant for CPS3 games, oftentimes Pointers begin with 0x06. Edit this to suit your needs
FORCE_VALUE = 0xFFFFFFFF -- Value to Assert in to RAM by default
ENABLED = 1
-- HERE YOU CAN CONFIGURE WHICH KEYS ON THE KEYBOARD CONTROL WHAT ACTION. UPPER CASE ONLY FOR KEYS!
INC_1 = "W" DEC_1 = "D" INC_4 = "S" DEC_4 = "A" RES = "X" TMR = "C" PLR = "P" DAT = "Q" FRZ = "F" NEG = "G" LOK = "V" CPY = "Z" ON = "space"
valid_buttons = {[RES] = {}, [TMR] = {}, [INC_1] = {}, [INC_4] = {}, [PLR] = {}, [DAT] = {}, [DEC_1] = {}, [DEC_4] = {}, [FRZ] = {}, [NEG] = {},[LOK] = {},[CPY] = {}, [ON] = {},}
Button_X = valid_buttons[RES] -- RESET SEARCH OFFSET
Button_C = valid_buttons[TMR] -- TOGGLE TIMERS
Button_W = valid_buttons[INC_1] -- INCREMENT SEARCH OFFSET
Button_S = valid_buttons[INC_4] -- DECREMENT SEARCH OFFSET
Button_P = valid_buttons[PLR] -- TOGGLE PLAYERS
Button_Q = valid_buttons[DAT] -- TOGGLE DATA TYPE
Button_D = valid_buttons[DEC_1] -- INCREMENT SEARCH OFFSET
Button_A = valid_buttons[DEC_4] -- DECREMENT SEARCH OFFSET
Button_F = valid_buttons[FRZ] -- SET ADDRESS TO 0X00
Button_G = valid_buttons[NEG] -- SET ADDRESS TO FORCE VALUE
Button_V = valid_buttons[LOK] -- SEARCH RAM TILL YOU BUMP IN TO LOOK_VALUE
Button_Z = valid_buttons[CPY] -- COPY CURRENT VALUE IN RAM FOR ASSERTION
Button_SP = valid_buttons[ON] -- TOGGLE THE SCRIPT FUNCTIONS

-----------------------------FUNCTIONS------------------------------
Freeze = 0 freeze_temp = 0 Infinite_time = 1 current_player = 1 dat_size = 1 length = 0 looking = 0
dat_types = {rb,rw,rd,} dat_writes = {wb,ww,wd,} dat_names = {"Byte","WORD","DWORD",}
Search = {Adr = 0x0, Base = 0,Offset = 0x0,} button_states = {pressed = 1, held = 2, released = 3,}
--Update each Button table with Booleans for the Button States
for b in pairs(button_states) do length = length + 1 end
	for k in pairs(valid_buttons) do for b = 0, length, 1 do table.insert(valid_buttons[k], b, 0) end
end

function display_RAM(adr)
	for i = 0, 21, 1 do x = adr + (asl(i,dat_size-1)) t = dat_types[dat_size](x)
		txt(10,10+(10*i), string.format("0x%x::0x%x",x,t)) end
end

function button_status(status, x, l)
	local tst = 0 local input = valid_buttons[x] local pressed = input[button_states.pressed] local held = input[button_states.held] local released = input[button_states.released]
	--Handle No input
	if valid_buttons[x] == nil then print("INVALID BUTTON!") return end
--Update Vars based on Button Status
	if status then if pressed == 1 then pressed = 0 end if held == 0 then held = 1 pressed = 1 released = 0 end
	else if released == 1 then released = 0 end if held == 1 then released = 1 end held = 0 
	end
--Shoot the values back
	input[button_states.pressed] = pressed input[button_states.held] = held input[button_states.released] = released
	if tst == 1 then base_x = 250 base_y = 20 txt(base_x,base_y+(20*l), "("..x..")")
		txt(base_x+20,base_y+(20*l), "P") txt(base_x+30,base_y+(20*l), "H") txt(base_x+40,base_y+(20*l), "R")
		txt(base_x+20,base_y+10+(20*l), pressed) txt(base_x+30,base_y+10+(20*l), held) txt(base_x+40,base_y+10+(20*l), released)
	end
	return status
end

function assert_gamevars()
	current_game = "" current_game = games[emu.romname()]
--Break if invalid game, otherwise, grab vars from games table at the top of the file
	if current_game == nil then txt(0,0, "Invalid game! Look at the top of the LUA for valid games") return 0 end
	txt(0,0, current_game[1].." || - Player ("..PLR..") "..current_player.." || - MODE ("..DAT..") ".." =  "..dat_names[dat_size])
	UID = {P1 = current_game[2], P2 = current_game[3],}
	Timer = {Adr =  current_game[4], Val =  current_game[5],}
	TMR_SIZE = current_game[6] Numplayers = current_game[7]
	charselect_timer = {Adr =  current_game[8], Val =  current_game[9], SIZE = current_game[10],}
	Space = UID.P2 - UID.P1 Search.Base = UID.P1
	Search.Adr = Search.Base + Search.Offset + (Space * (current_player-1))
	return 1
end
function freezeadr(cond, x, y,z) if cond == nil or x == nil or y == nil or z == nil then return end if cond == 1 then z(x, y) end end
function toggleset(x, bot, limit) if x == nil or bot == nil or limit == nil then return end if x+1 > limit then return bot else return x + 1 end end
function display_key_data()
	x =  math.floor(sw/2)-60 y = 10 y_off = 10  low = 15
	txt(x,y+(y_off*0), string.format("PLAYER LENGTH = 0x%x", Space))
	txt(x,y+(y_off*1), "Address = Base + (Space X (current_player-1)) + Offset")
	txt(x,y+(y_off*2), string.format("0x%x = 0x%x + (0x%x X (0x%x-1) + 0x%x", Search.Adr, Search.Base, Space, current_player, Search.Offset))
	txt(x,y+(y_off*(low+0)), "FREEZE TIMERS   ("..TMR..") = "..Infinite_time)
	txt(x,y+(y_off*(low+1)), "FREEZE ADDRESS  ("..FRZ..") = "..Freeze..string.format(" TEMP || 0x%x", freeze_temp))
	txt(x,y+(y_off*(low+2)), "FORCE  ADDRESS  ("..NEG..")".."COPY RAM ("..CPY..")"..string.format("VAL = 0x%x", FORCE_VALUE))
	txt(x,y+(y_off*(low+3)), "RESET  ADDRESS  ("..RES..")")
	txt(x,y+(y_off*(low+4)), "CHANGE ADDRESS  +1("..INC_1..")".."-1("..DEC_1..")".."+4("..INC_4..")".."-4("..DEC_4..")")	
	txt(x,y+(y_off*(low+5)), "SEARCH VALUE    ("..LOK..")"..string.format(" = 0x%x - 0x%x", LOOK_VALUE, looking))
end

------------------------------MAIN LOOP------------------------------
while true do local keys = input.get() gui.clearuncommitted() sw = emu.screenwidth() sh = emu.screenheight()
	-- READ THE VALID BUTTONS
	length = 0 for k in pairs(valid_buttons) do length = length + 1 button_status(keys[k], k, length) end
	--Toggle Script
	if Button_SP[button_states.pressed] == 1 then ENABLED = toggleset(ENABLED, 0, 1) end
	txt(sw - 90,10, "ENABLED ("..ON..") = "..ENABLED)
	if ENABLED == 1 then
		if assert_gamevars() == 1 then
			--Updated via input to display RAM
			display_RAM(Search.Adr) display_key_data()
			--Toggle Timer Freeze
			if Button_C[button_states.pressed] == 1 then Infinite_time = toggleset(Infinite_time, 0,1) end
			freezeadr(Infinite_time, Timer.Adr, Timer.Val, TMR_SIZE)
			freezeadr(Infinite_time, charselect_timer.Adr, charselect_timer.Val, charselect_timer.SIZE)
			--Toggle Data type
			if Button_Q[button_states.pressed] == 1 then dat_size = toggleset(dat_size, 1,3) 
				if Freeze == 1 then freeze_temp = dat_types[dat_size](Search.Adr) Freeze = 0 end				
			end
			-- Reset Search Offset
			if Button_X[button_states.pressed] == 1 then Search.Offset = 0 end
			-- Freeze current Address
			if Button_F[button_states.pressed] == 1 then Freeze = toggleset(Freeze, 0,1) 
				if Freeze == 1 then freeze_temp = dat_types[dat_size](Search.Adr)
				freezeadr(Freeze, Search.Adr, 0x00, dat_writes[dat_size])	
				else dat_writes[dat_size](Search.Adr,freeze_temp) end
			end
			-- Freeze current Address
			if Button_G[button_states.pressed] == 1 then Freeze = toggleset(Freeze, 0,1) 
				if Freeze == 1 then freeze_temp = dat_types[dat_size](Search.Adr)
				freezeadr(Freeze, Search.Adr, FORCE_VALUE, dat_writes[dat_size])	
				else dat_writes[dat_size](Search.Adr,freeze_temp) end
			end
			if Freeze == 0 then
				--Toggle Between Players
				if Button_Z[button_states.pressed] == 1 then FORCE_VALUE = dat_types[dat_size](Search.Adr) end
				--Toggle Between Players
				if Button_P[button_states.pressed] == 1 then current_player = toggleset(current_player, 1, Numplayers) end
				-- Increment Search Offset 1
				if Button_W[button_states.pressed] == 1 then Search.Offset = Search.Offset + 1 end
				-- Decrement Search Offset 1
				if Button_S[button_states.pressed] == 1 then Search.Offset = Search.Offset - 1 end
				-- Increment Search Offset 4
				if Button_D[button_states.pressed] == 1 then Search.Offset = Search.Offset + 4 end
				-- Decrement Search Offset 4
				if Button_A[button_states.pressed] == 1 then Search.Offset = Search.Offset - 4 end
			end
			-- Search for a vakue
			if Button_V[button_states.pressed] == 1 then looking = toggleset(looking, 0, 1) end
			if looking == 1 then Freeze = 1 end
			if Freeze == 1 and looking == 1 then
				if LOOK_VALUE == rb(Search.Adr + 1) then
					Freeze = 0 looking = 0 Search.Offset = Search.Offset + 1
				else Search.Offset = Search.Offset + 1
				end
			end
		end
	end emu.frameadvance() end