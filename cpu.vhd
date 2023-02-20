-- cpu.vhd: Simple 8-bit CPU (BrainFuck interpreter)
-- Copyright (C) 2022 Brno University of Technology,
--                    Faculty of Information Technology
-- Author(s): Filip Cvrkal <xcvrka02>
--
library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_arith.all;
use ieee.std_logic_unsigned.all;

-- ----------------------------------------------------------------------------
--                        Entity declaration
-- ----------------------------------------------------------------------------
entity cpu is
 port (
   CLK   : in std_logic;  -- hodinovy signal
   RESET : in std_logic;  -- asynchronni reset procesoru
   EN    : in std_logic;  -- povoleni cinnosti procesoru
 
   -- synchronni pamet RAM
   DATA_ADDR  : out std_logic_vector(12 downto 0); -- adresa do pameti
   DATA_WDATA : out std_logic_vector(7 downto 0); -- mem[DATA_ADDR] <- DATA_WDATA pokud DATA_EN='1'
   DATA_RDATA : in std_logic_vector(7 downto 0);  -- DATA_RDATA <- ram[DATA_ADDR] pokud DATA_EN='1'
   DATA_RDWR  : out std_logic;                    -- cteni (0) / zapis (1)
   DATA_EN    : out std_logic;                    -- povoleni cinnosti
   
   -- vstupni port
   IN_DATA   : in std_logic_vector(7 downto 0);   -- IN_DATA <- stav klavesnice pokud IN_VLD='1' a IN_REQ='1'
   IN_VLD    : in std_logic;                      -- data platna
   IN_REQ    : out std_logic;                     -- pozadavek na vstup data
   
   -- vystupni port
   OUT_DATA : out  std_logic_vector(7 downto 0);  -- zapisovana data
   OUT_BUSY : in std_logic;                       -- LCD je zaneprazdnen (1), nelze zapisovat
   OUT_WE   : out std_logic                       -- LCD <- OUT_DATA pokud OUT_WE='1' a OUT_BUSY='0'
 );
end cpu;


-- ----------------------------------------------------------------------------
--                      Architecture declaration
-- ----------------------------------------------------------------------------
architecture behavioral of cpu is

	type instr_type is (i_no_operation, i_add_increment, i_add_decrement, i_pointer_increment, i_poineter_decrement, i_starts_while, i_ends_while, i_prints, i_loads, i_halt);

	type fsm_state is (st_starts, st_loads_instr_1, st_loads_instr_2, st_pointer_increment, st_pointer_decrement, st_loads_value_1, st_loads_value_2, st_prints, st_no_operation, st_halt,
						st_swhile_start_1, st_swhile_start_2, st_while_miss_forw_1, st_while_miss_forw_2, swhile_end_2, swhile_end_3, st_while_miss_back_1, st_while_miss_back_2);

	signal ptr_instr_type : instr_type;
	signal present_state : fsm_state;
	signal next_state : fsm_state;

	signal cnt_state : std_logic;
	signal cnt_increment : std_logic;
	signal cnt_decrement : std_logic;
	signal cnt_registr : std_logic_vector (7 downto 0);

	signal pc_state : std_logic;
	signal pc_increment : std_logic;
	signal pc_decrement : std_logic;
	signal pc_registr : std_logic_vector (12 downto 0);

	signal ptr_state : std_logic;
	signal ptr_increment : std_logic;
	signal ptr_decrement : std_logic;
	signal ptr_registr : std_logic_vector (12 downto 0);



begin
	-------------------------------------------------------------------------------------
	--COUNTER FOR WHILES
	-------------------------------------------------------------------------------------
	reg_CNT : process (RESET, CLK)
	begin
		if(RESET = '1') then
			cnt_registr <= (others => '0');
		elsif(CLK'event) and (CLK = '1') then
			if(cnt_increment = '1') then
				cnt_registr <= cnt_registr + 1;
			elsif(cnt_decrement = '1') then
				cnt_registr <= cnt_registr - 1;
			elsif(cnt_state = '1') then
				cnt_registr <= "00000001";
			end if;
		end if;
	end process;

	-------------------------------------------------------------------------------------
	--PROGRAM COUNTER 
	-------------------------------------------------------------------------------------
	reg_PC: process (RESET, CLK)
	begin
		if(RESET = '1') then
			pc_registr <= (others => '0');
		elsif(CLK'event) and (CLK = '1') then
			if(pc_increment = '1') then
				pc_registr <= pc_registr + 1;
			elsif(pc_decrement = '1') then
				pc_registr <= pc_registr - 1;
			end if;
		end if;
	end process;

	-------------------------------------------------------------------------------------
	--POINTER COUNTER
	-------------------------------------------------------------------------------------
	reg_PTR: process (RESET, CLK)
	begin
		if(RESET = '1') then
			ptr_registr <= "1000000000000";
		elsif(CLK'event) and (CLK = '1') then
			if(ptr_increment = '1') then
				if(ptr_registr = "1111111111111") then
					ptr_registr <= "1000000000000";
				else
					ptr_registr <= ptr_registr + 1;
				end if;
			elsif(ptr_decrement = '1') then
				if(ptr_registr = "1000000000000") then
					ptr_registr <= "1111111111111";
				else
					ptr_registr <= ptr_registr - 1;
				end if;
			end if;
		end if;
	end process;

	-------------------------------------------------------------------------------------
	--DECODE INSTRUCTION
	-------------------------------------------------------------------------------------
	load_instruction : process (DATA_RDATA)
	begin
		case DATA_RDATA is
			when X"3E" => ptr_instr_type <= i_add_increment;		-- >
			when X"3C" => ptr_instr_type <= i_add_decrement; 	-- <
			when X"2B" => ptr_instr_type <= i_pointer_increment;		-- +
			when X"2D" => ptr_instr_type <= i_poineter_decrement; 		-- - 
			when X"2E" => ptr_instr_type <= i_prints; 		-- .
			when X"2C" => ptr_instr_type <= i_loads; 		-- ,
			when X"5B" => ptr_instr_type <= i_starts_while; 	-- [
			when X"5D" => ptr_instr_type <= i_ends_while; 	-- ]   
			when X"00" => ptr_instr_type <= i_halt; 		-- null
			when others => ptr_instr_type <= i_no_operation; 		-- others
		end case;
	end process;

	-------------------------------------------------------------------------------------
	--MX1
	-------------------------------------------------------------------------------------
	DATA_ADDR <= pc_registr when pc_state = '1' else (others => 'Z');
	DATA_ADDR <= ptr_registr when ptr_state = '1' else (others => 'Z');

	-------------------------------------------------------------------------------------
	--PRESENT STATE
	-------------------------------------------------------------------------------------
	present_state_registr : process (CLK, RESET, EN)
	begin
		if(RESET = '1') then
			present_state <= st_starts;
		elsif(CLK'event) and (CLK = '1') then
			if (EN = '1') then
				present_state <= next_state;
			end if;		
		end if;
	end process;

	-------------------------------------------------------------------------------------
	--NEXT STATE
	-------------------------------------------------------------------------------------
	next_state_registr : process (EN, present_state, IN_VLD, IN_DATA, DATA_RDATA, OUT_BUSY, ptr_instr_type)
	  begin

		  cnt_state <= '0';
		  pc_state <= '0';
		  ptr_state <= '0';

		  cnt_increment <= '0';
		  cnt_decrement <= '0';

		  pc_increment <= '0';
		  pc_decrement <= '0';

		  ptr_increment <= '0';
		  ptr_decrement <= '0';

		  IN_REQ <= '0';
		  OUT_WE <= '0';
		  DATA_EN <= '0';
		  DATA_WDATA <= X"00";
		  DATA_RDWR <= '0';

		  next_state <= st_starts;

		case present_state is

			  --FIRST STATE
			  when  st_starts =>
				      next_state <= st_loads_instr_1;

			  --LOAD INSTRUCTION
			  when  st_loads_instr_1 =>
				      pc_state <= '1';
				      DATA_EN <= '1';
				      next_state <= st_loads_instr_2;

			  --LOAD INSTRUCTION 
			  when   st_loads_instr_2 => 
				case   ptr_instr_type is

				-- >
				when  i_add_increment =>
					  	pc_increment <= '1';
						  ptr_increment <= '1';
						  next_state <= st_starts;

				-- <
				when  i_add_decrement =>
						  pc_increment <= '1';
						  ptr_decrement <= '1';
						  next_state <= st_starts;

				-- +
				when  i_pointer_increment =>
						  DATA_EN <= '1';
						  ptr_state <= '1';
						  DATA_RDWR <= '0';
						  next_state <= st_pointer_increment;

				-- -
				when  i_poineter_decrement =>
						  DATA_EN <= '1';
						  ptr_state <= '1';
						  DATA_RDWR <= '0';
						  next_state <= st_pointer_decrement;

				-- ,
				when  i_loads =>
						  next_state <= st_loads_value_1;

				-- .
				when  i_prints =>
						  DATA_EN <= '1';
						  ptr_state <= '1';
						  DATA_RDWR <= '0';
						  next_state <= st_prints;

				-- [
				when  i_starts_while =>
            	pc_increment <= '1';
            	ptr_state <= '1';
           		DATA_EN <= '1';
            	DATA_RDWR <= '0';
            	next_state <= st_swhile_start_1;

        -- ]
				when  i_ends_while =>
						  ptr_state <= '1';
            	DATA_EN <= '1';
            	DATA_RDWR <= '0';
            	next_state <= swhile_end_2;


        -- halt
				when  i_halt =>
						  next_state <= st_halt;

				when  i_no_operation =>
						  next_state <= st_no_operation;
				end case;
			

			  --PTR++
			  when  st_pointer_increment =>
				      DATA_EN <= '1';
				      ptr_state <= '1';
				      DATA_RDWR <= '1';
				      DATA_WDATA <= DATA_RDATA + 1;
				      pc_increment <= '1';
				      next_state <= st_starts;

			  --PTR--
			  when  st_pointer_decrement =>
				      DATA_EN <= '1';
				      DATA_RDWR <= '1';
				      ptr_state <= '1';
				      DATA_WDATA <= DATA_RDATA - 1;
				      pc_increment <= '1';
				      next_state <= st_starts;

			  --REQ LOAD VALUE
			  when  st_loads_value_1 =>
				      IN_REQ <= '1';
				      next_state <= st_loads_value_2;

			  --LOAD VALUE
			  when  st_loads_value_2 =>
				      IN_REQ <= '1';
				if(IN_VLD = '1') then
					    DATA_EN <= '1';
					    ptr_state <= '1';
					    DATA_RDWR <= '1';
					    DATA_WDATA <= IN_DATA;
				      pc_increment <= '1';
			       	next_state <= st_starts;
			 		    IN_REQ <= '0';
				else
					    next_state <= st_loads_value_1;
				end if;		

		    --PRINT
			  when st_prints =>
				if(OUT_BUSY = '0') then
					  OUT_WE <= '1';
					  OUT_DATA <= DATA_RDATA;
					  pc_increment <= '1';
					  next_state <= st_starts;
				else
					  next_state <= st_prints;
				end if;

			  --IF NULL -> HALT
			  when  st_halt =>
				      next_state <= st_halt;

			  --SKIP
			  when  st_no_operation =>
				      pc_increment <= '1';
				      next_state <= st_starts;


		    -----------WHILE-----------
		
    
        -- WHILE PTR second
		    when  st_swhile_start_1 =>
			  if ( DATA_RDATA = X"00" ) then
				      cnt_increment <= '1';
				      next_state <= st_swhile_start_2;        
			  else 
				      next_state <= st_starts;
			  end if;

		    --WHILE 0 
		    when  st_swhile_start_2 =>
			        next_state <= st_while_miss_forw_1;
			        pc_state <= '1';
			        DATA_EN <= '1';

		    --WHILE 0 
		    when st_while_miss_forw_1 =>
			      pc_increment <= '1';
			      next_state <= st_while_miss_forw_2;     
			  if ( ptr_instr_type = i_starts_while ) then
				    cnt_increment <= '1';
			  elsif ( ptr_instr_type = i_ends_while ) then
				    cnt_decrement <= '1';
			  end if;

		    when st_while_miss_forw_2 =>
			      next_state <= st_swhile_start_2;
			  if ( cnt_registr = X"00" )  then
			  	  next_state <= st_starts;
			  end if;

		    --END WHILE--second
		    when swhile_end_2 =>
			      next_state <= st_starts;
			  if ( DATA_RDATA = X"00" ) then
				    pc_increment <= '1';
		  	else 
				    pc_decrement <= '1';
				    cnt_increment <= '1';
				    next_state <= swhile_end_3;
			  end if;

		    --END WHILE-third
		    when swhile_end_3 =>
			      next_state <= st_while_miss_back_1;
			      pc_state <= '1';
			      DATA_EN <= '1';

		    when st_while_miss_back_1 =>
			next_state <= st_while_miss_back_2;
			  if ( ptr_instr_type = i_ends_while ) then
				cnt_increment <= '1';
			  elsif ( ptr_instr_type = i_starts_while ) then
				cnt_decrement <= '1';
			  end if;

		    when st_while_miss_back_2 =>
			next_state <= st_starts;
			  if ( cnt_registr = X"00" ) then
				pc_increment <= '1';      
			  else 
				next_state <= swhile_end_3;
				pc_decrement <= '1';
			  end if;


		    -----------END WHILE -----------


			  when others =>
				next_state <= st_starts;

		end case;
	end process;
end behavioral;
 
