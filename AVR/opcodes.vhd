-----------------------------------------------------------------------------
--
--  AVR opcode package
--
--  This package defines opcode constants for the complete AVR instruction
--  set.  Not all variants of the AVR implement all instructions.
--
--  Revision History
--      4/27/98   Glen George		initial revision
--      4/14/00   Glen George		updated comments
--      4/22/02   Glen George		added new instructions
--      4/22/02   Glen George		updated comments
--      5/16/02   Glen George		fixed LPM instruction constant
--
-----------------------------------------------------------------------------

library ieee;
use ieee.std_logic_1164.all;

library common;
use common.common.all;

package opcodes is

--  ALU opcodes

   constant OpADC    :  instruction_t := "000111----------";   -- ADC Rd, Rr
   constant OpADD    :  instruction_t := "000011----------";   -- ADD Rd, Rr
   constant OpADIW   :  instruction_t := "10010110--------";   -- ADIW Rdl, K
   constant OpAND    :  instruction_t := "001000----------";   -- AND Rd, Rr
   constant OpANDI   :  instruction_t := "0111------------";   -- ANDI Rd, K
   constant OpASR    :  instruction_t := "1001010-----0101";   -- ASR Rd
   constant OpBCLR   :  instruction_t := "100101001---1000";   -- BCLR s
   constant OpBLD    :  instruction_t := "1111100-----0---";   -- BLD Rd, b
   constant OpBSET   :  instruction_t := "100101000---1000";   -- BSET s
   constant OpBST    :  instruction_t := "1111101---------";   -- BST Rr, b
   constant OpCOM    :  instruction_t := "1001010-----0000";   -- COM Rd
   constant OpCP     :  instruction_t := "000101----------";   -- CP Rd, Rr
   constant OpCPC    :  instruction_t := "000001----------";   -- CPC Rd, Rr
   constant OpCPI    :  instruction_t := "0011------------";   -- CPI Rd, K
   constant OpDEC    :  instruction_t := "1001010-----1010";   -- DEC Rd
   constant OpEOR    :  instruction_t := "001001----------";   -- EOR Rd, Rr
   constant OpFMUL   :  instruction_t := "000000110---1---";   -- FMUL Rd, Rr
   constant OpFMULS  :  instruction_t := "000000111---0---";   -- FMULS Rd, Rr
   constant OpFMULSU :  instruction_t := "000000111---1---";   -- FMULSU Rd, Rr
   constant OpINC    :  instruction_t := "1001010-----0011";   -- INC Rd
   constant OpLSR    :  instruction_t := "1001010-----0110";   -- LSR Rd
   constant OpMUL    :  instruction_t := "100111----------";   -- MUL Rd, Rr
   constant OpMULS   :  instruction_t := "00000010--------";   -- MULS Rd, Rr
   constant OpMULSU  :  instruction_t := "000000110---0---";   -- MULSU Rd, Rr
   constant OpNEG    :  instruction_t := "1001010-----0001";   -- NEG Rd
   constant OpOR     :  instruction_t := "001010----------";   -- OR Rd, Rr
   constant OpORI    :  instruction_t := "0110------------";   -- ORI Rd, K
   constant OpROR    :  instruction_t := "1001010-----0111";   -- ROR Rd
   constant OpSBC    :  instruction_t := "000010----------";   -- SBC Rd, Rr
   constant OpSBCI   :  instruction_t := "0100------------";   -- SBCI Rd, K
   constant OpSBIW   :  instruction_t := "10010111--------";   -- SBIW Rdl, K
   constant OpSUB    :  instruction_t := "000110----------";   -- SUB Rd, Rr
   constant OpSUBI   :  instruction_t := "0101------------";   -- SUBI Rd, K
   constant OpSWAP   :  instruction_t := "1001010-----0010";   -- SWAP Rd

--  Load and Store Opcodes

   constant OpELPM   :  instruction_t := "1001010111011000";   -- ELPM
   constant OpELPMZ  :  instruction_t := "1001000-----0110";   -- ELPM Rd, Z
   constant OpELPMZI :  instruction_t := "1001000-----0111";   -- ELPM Rd, Z+
   constant OpLDX    :  instruction_t := "1001000-----1100";   -- LD Rd, X
   constant OpLDXI   :  instruction_t := "1001000-----1101";   -- LD Rd, X+
   constant OpLDXD   :  instruction_t := "1001000-----1110";   -- LD Rd, -X
   constant OpLDY    :  instruction_t := "1000000-----1000";   -- LD Rd, Y
   constant OpLDYI   :  instruction_t := "1001000-----1001";   -- LD Rd, Y+
   constant OpLDYD   :  instruction_t := "1001000-----1010";   -- LD Rd, -Y
   constant OpLDDY   :  instruction_t := "10-0--0-----1---";   -- LDD Rd, Y + q
   constant OpLDZ    :  instruction_t := "1000000-----0000";   -- LD Rd, Z
   constant OpLDZI   :  instruction_t := "1001000-----0001";   -- LD Rd, Z+
   constant OpLDZD   :  instruction_t := "1001000-----0010";   -- LD Rd, -Z
   constant OpLDDZ   :  instruction_t := "10-0--0-----0---";   -- LDD Rd, Z + q
   constant OpLDI    :  instruction_t := "1110------------";   -- LDI Rd, k
   constant OpLDS    :  instruction_t := "1001000-----0000";   -- LDS Rd, m
   constant OpLPM    :  instruction_t := "1001010111001000";   -- LPM
   constant OpLPMZ   :  instruction_t := "1001000-----0100";   -- LPM Rd, Z
   constant OpLPMZI  :  instruction_t := "1001000-----0101";   -- LPM Rd, Z+
   constant OpMOV    :  instruction_t := "001011----------";   -- MOV Rd, Rr
   constant OpMOVW   :  instruction_t := "00000001--------";   -- MOVW Rd, Rr
   constant OpSPM    :  instruction_t := "1001010111101000";   -- SPM
   constant OpSTX    :  instruction_t := "1001001-----1100";   -- ST X, Rr
   constant OpSTXI   :  instruction_t := "1001001-----1101";   -- ST X+, Rr
   constant OpSTXD   :  instruction_t := "1001001-----1110";   -- ST -X, Rr
   constant OpSTY    :  instruction_t := "1000001-----1000";   -- ST Y, Rr
   constant OpSTYI   :  instruction_t := "1001001-----1001";   -- ST Y+, Rr
   constant OpSTYD   :  instruction_t := "1001001-----1010";   -- ST -Y, Rr
   constant OpSTDY   :  instruction_t := "10-0--1-----1---";   -- STD Y + q, Rr
   constant OpSTZ    :  instruction_t := "1000001-----0000";   -- ST Z, Rr
   constant OpSTZI   :  instruction_t := "1001001-----0001";   -- ST Z+, Rr
   constant OpSTZD   :  instruction_t := "1001001-----0010";   -- ST -Z, Rr
   constant OpSTDZ   :  instruction_t := "10-0--1-----0---";   -- STD Z + q, Rr
   constant OpSTS    :  instruction_t := "1001001-----0000";   -- STS m, Rr

--  Push and Pop Opcodes

   constant OpPOP    :  instruction_t := "1001000-----1111";   -- POP Rd
   constant OpPUSH   :  instruction_t := "1001001-----1111";   -- PUSH Rd

--  Unconditional Branches

   constant OpEICALL :  instruction_t := "1001010100011001";   -- EICALL
   constant OpEIJMP  :  instruction_t := "1001010000011001";   -- EIJMP
   constant OpJMP    :  instruction_t := "1001010-----110-";   -- JMP a
   constant OpRJMP   :  instruction_t := "1100------------";   -- RJMP j
   constant OpIJMP   :  instruction_t := "10010100----1001";   -- IJMP
   constant OpCALL   :  instruction_t := "1001010-----111-";   -- CALL a
   constant OpRCALL  :  instruction_t := "1101------------";   -- RCALL j
   constant OpICALL  :  instruction_t := "10010101----1001";   -- ICALL
   constant OpRET    :  instruction_t := "100101010--01000";   -- RET
   constant OpRETI   :  instruction_t := "100101010--11000";   -- RETI

--  Conditional Branches

   constant OpBRBC   :  instruction_t := "111101----------";   -- BRBC s, r
   constant OpBRBS   :  instruction_t := "111100----------";   -- BRBS s, r

--  Skip Instructions

   constant OpCPSE   :  instruction_t := "000100----------";   -- CPSE Rd, Rr
   constant OpSBIC   :  instruction_t := "10011001--------";   -- SBIC p, b
   constant OpSBIS   :  instruction_t := "10011011--------";   -- SBIS p, b
   constant OpSBRC   :  instruction_t := "1111110---------";   -- SBRC Rr, b
   constant OpSBRS   :  instruction_t := "1111111---------";   -- SBRS Rr, b

--  I/O Instructions

   constant OpCBI    :  instruction_t := "10011000--------";   -- CBI p, b
   constant OpIN     :  instruction_t := "10110-----------";   -- IN Rd, p
   constant OpOUT    :  instruction_t := "10111-----------";   -- OUT p, Rr
   constant OpSBI    :  instruction_t := "10011010--------";   -- SBI p, b

--  Miscellaneous Instructions

   constant OpBREAK  :  instruction_t := "1001010110011000";   -- BREAK
   constant OpNOP    :  instruction_t := "0000000000000000";   -- NOP
   constant OpSLP    :  instruction_t := "10010101100-1000";   -- SLEEP
   constant OpWDR    :  instruction_t := "10010101101-1000";   -- WDR


end package;
