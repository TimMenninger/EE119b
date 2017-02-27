; This is the testcode for the Atmel AVR emulator.  There exist tests for functionality
; in ALU, registers and data memory.  This mainly serves to show that flow control
; and the instruction register work, although errors in the former three may cause
; tests generated from this file to fail.
;
;
; Revision History
;     5/11/00  Glen George      Initial revision (from 4/23/00 version of
;                               alutest.asm and 5/11/00 version of
;                               memtest.asm).
;     5/13/00  Glen George      Fixed mistakes in BST instructions (wrong
;                               registers).  Fixed some flag and register
;                               value problems.
;     7/27/00  Glen George      Added instructions for Homework #5 (jumps,
;                               calls, etc.).
;     5/16/04  Glen George      Added more testing and updated comments.
;     2/8/06   Glen George      Fixed some mistakes in the comments.
;     1/22/08  Glen George      Changed the code a litte to match up with the
;                               comments and vice-versa.
;     1/24/17  Tim Menninger    Added comments to demonstrate I know what's going on,
;                               and to meet HW3 specifications.  Also added some tests:
;                                   -Now read and write to every register at least once
;                                   -Added boundary cases for several operations
;                                   -Added dummy instructions to show jumps and branches
;                                    work
;                                   -Added more branch tests
;     2/26/17  Tim Menninger    Added PUSHes for arithmetic operations, but not a ton
;                               because that's what the ALU test is for



Start:                               ; start of the test code


                                     ; OpA OpB Res   Flags   Desc
                                     ;                       Test BCLR instruction.  We
                                     ;                       should see one bit clear per
                                     ;                       instruction, matching the
                                     ;                       index to the operand
        BCLR    0                    ; $00 XX  --  -------0  Testing clear flag bit 0
        BCLR    7                    ; $07 XX  --  0------0  Testing clear flag bit 7
        BCLR    4                    ; $04 XX  --  0--0---0  Testing clear flag bit 4
        BCLR    3                    ; $03 XX  --  0--00--0  Testing clear flag bit 3
        BCLR    1                    ; $01 XX  --  0--00-00  Testing clear flag bit 1
        BCLR    5                    ; $05 XX  --  0-000-00  Testing clear flag bit 5
        BCLR    2                    ; $02 XX  --  0-000000  Testing clear flag bit 2
        BCLR    6                    ; $06 XX  --  00000000  Testing clear flag bit 6

                                     ; OpA OpB Res   Flags   Desc
                                     ;                       Test BSET instruction.  We
                                     ;                       should see one bit set per
                                     ;                       instruction at the index
                                     ;                       that matches the operand.
        BSET    4                    ; $04 XX  --  00010000  Testing set flag bit 4
        BSET    1                    ; $01 XX  --  00010010  Testing set flag bit 1
        BSET    7                    ; $07 XX  --  10010010  Testing set flag bit 7
        BSET    0                    ; $00 XX  --  10010011  Testing set flag bit 0
        BSET    2                    ; $02 XX  --  10010111  Testing set flag bit 2
        BSET    6                    ; $06 XX  --  11010111  Testing set flag bit 6
        BSET    5                    ; $05 XX  --  11110111  Testing set flag bit 5
        BSET    3                    ; $03 XX  --  11111111  Testing set flag bit 3

                                     ; OpA OpB Res   Flags   Desc
                                     ;                       Test LDI instruction, which
                                     ;                       should set R16 to 0.
        LDI     R16, 0               ; 10   00  00 11111111  Testing load immediate
        MOV     R0, R16              ; 00   00  00 11111111  R0 should now be 0
        MOV     R1, R16              ; 01   00  00 11111111  R1 should now be 0
        MOV     R2, R16              ; 02   00  00 11111111  R2 should now be 0
        MOV     R3, R16              ; 03   00  00 11111111  R3 should now be 0
        MOV     R4, R16              ; 04   00  00 11111111  R4 should now be 0
        MOV     R5, R16              ; 05   00  00 11111111  R5 should now be 0
        MOV     R6, R16              ; 06   00  00 11111111  R6 should now be 0
        MOV     R7, R16              ; 07   00  00 11111111  R7 should now be 0

                                     ; OpA OpB Res   Flags   Desc
        BLD     R0, 7                ; 00  XX  80  11111111  Test bit load on bit 7 of R0
        BLD     R1, 3                ; 01  XX  08  11111111  Test bit load on bit 3 of R1
        BLD     R2, 1                ; 02  XX  02  11111111  Test bit load on bit 1 of R2
        BLD     R3, 6                ; 03  XX  40  11111111  Test bit load on bit 6 of R3
        BLD     R4, 0                ; 04  XX  01  11111111  Test bit load on bit 0 of R4
        BLD     R5, 5                ; 05  XX  20  11111111  Test bit load on bit 5 of R5
        BLD     R6, 4                ; 06  XX  10  11111111  Test bit load on bit 4 of R6
        BLD     R7, 2                ; 07  XX  04  11111111  Test bit load on bit 2 of R7

                                     ; OpA OpB Res   Flags   Desc
        LDI     R20, $DF             ; 14  DF --   11111111  Load $DF into R20
        MOV     R8, R20              ; 08  14 DF   11111111  R8 should now contain $DF
        LDI     R20, $04             ; 14  04 --   11111111  Load $04 into R20
        MOV     R9, R20              ; 09  14 04   11111111  R9 should now contain $04
        LDI     R20, $7F             ; 14  7F --   11111111  Load $7F into R20
        MOV     R10, R20             ; 0A  14 7F   11111111  R10 should now contain $7F
        LDI     R20, $01             ; 14  01 --   11111111  Load $01 into R20
        MOV     R11, R20             ; 0B  14 01   11111111  R11 should now contain $01
        LDI     R20, $FD             ; 14  FD --   11111111  Load $FD into R20
        MOV     R12, R20             ; 0C  14 FD   11111111  R12 should now contain $FD
        LDI     R20, $40             ; 14  40 --   11111111  Load $40 into R20
        MOV     R13, R20             ; 0D  14 40   11111111  R13 should now contain $40
        LDI     R20, $F7             ; 14  F7 --   11111111  Load $F7 into R20
        MOV     R14, R20             ; 0E  14 F7   11111111  R14 should now contain $F7
        LDI     R20, $10             ; 14  10 --   11111111  Load $10 into R20
        MOV     R15, R20             ; 0F  14 10   11111111  R15 should now contain $10

                                     ; OpA OpB Res   Flags   Desc
                                     ;                       These are going to test the
                                     ;                       BST functionality.  On any
                                     ;                       given BST, we should see only
                                     ;                       the T bit of the flags change
        BST     R8,  5               ; DF  05  --  10111111  T flag = R8(5) = 0
        BST     R9,  2               ; 04  02  --  11111111  T flag = R9(2) = 1
        BST     R10, 7               ; 7F  07  --  10111111  T flag = R10(7) = 0
        BST     R11, 0               ; 01  00  --  11111111  T flag = R11(0) = 1
        BST     R12, 1               ; FD  01  --  10111111  T flag = R12(1) = 0
        BST     R13, 6               ; 40  06  --  11111111  T flag = R13(6) = 1
        BST     R14, 3               ; F7  03  --  10111111  T flag = R14(3) = 0
        BST     R15, 4               ; 10  04  --  11111111  T flag = R15(4) = 1

                                     ; initialize for ALU ops
        LDI     R16, $FF             ; 10  FF  --  11111111
        LDI     R17, $FF             ; 11  FF  --  11111111
        LDI     R18, 0               ; 12  00  --  11111111
        LDI     R19, $70             ; 13  70  --  11111111
        LDI     R20, 0               ; 14  00  --  11111111
        LDI     R21, $7E             ; 15  7E  --  11111111
        LDI     R22, $7E             ; 16  7E  --  11111111
        LDI     R23, $80             ; 17  80  --  11111111
        LDI     R24, $45             ; 18  45  --  11111111
        LDI     R25, $80             ; 19  80  --  11111111
        LDI     R26, $F0             ; 1A  F0  --  11111111
        LDI     R27, $FF             ; 1B  FF  --  11111111
        LDI     R28, $55             ; 1C  55  --  11111111
        LDI     R29, $AA             ; 1D  AA  --  11111111
        LDI     R30, $70             ; 1E  70  --  11111111
        LDI     R31, $3F             ; 1F  3F  --  11111111

                                     ; OpA OpB Res   Flags   Desc
                                     ;                       Test the add with carry
                                     ;                       functionality
        ADC     R16, R17             ; FF  FF  FF  11110101  Testing with all 1's
        PUSH    R16                  ;WFFFFFF
        POP     R16                  ;RFFFFFF
        ADC     R16, R18             ; FF  00  00  11100011  Show (FF + carry) = 0
        PUSH    R16                  ;W00FFFF
        POP     R16                  ;R00FFFF
        ADC     R18, R17             ; 00  FF  00  11100011  Show (FF + carry) = 0
        PUSH    R18                  ;W00FFFF
        POP     R18                  ;R00FFFF
        ADC     R25, R21             ; 80  7E  FF  11010100  Show sum without rollover
        PUSH    R25                  ;WFFFFFF
        POP     R25                  ;RFFFFFF
        ADC     R22, R0              ; 7E  80  FE  11010100  Show sum with carry = 0
        PUSH    R22                  ;WFEFFFF
        POP     R22                  ;RFEFFFF

                                     ; OpA OpB Res   Flags   Desc
                                     ;                       Test add without carry
        ADD     R25, R21             ; FF  7E  7D  11100001  Wraparound should set carry
        PUSH    R25                  ;W7DFFFF
        POP     R25                  ;R7DFFFF
        ADD     R16, R17             ; 00  FF  FF  11010100  Add zero should give itself
        PUSH    R16                  ;WFFFFFF
        POP     R16                  ;RFFFFFF
        ADD     R18, R20             ; 00  00  00  11000010  Show 0 + 0 is 0
        PUSH    R18                  ;W00FFFF
        POP     R18                  ;R00FFFF
        ADD     R23, R0              ; 80  80  00  11011011  Show that we get wrap around
        PUSH    R23                  ;W00FFFF                to 0 with 0x80+0x80 = 0
        POP     R23                  ;R00FFFF

                                     ; OpA OpB Res   Flags   Desc
                                     ;                       Test add immediate to word.
                                     ;                       We show adding zero returns
                                     ;                       itself and we show a
                                     ;                       wrapped case
        ADIW    R24, $00             ; 45  00  45  --------  Adding 0 gives itself
                                     ; 7D  XX  7D  11000000  High word
        PUSH    R24                  ;W45FFFF
        PUSH    R25                  ;W7DFFFE
        POP     R25
        POP     R24
        ADIW    R26, $10             ; F0  10  00  --------  Wraparound case
                                     ; FF  XX  00  11000011  High word
        PUSH    R26                  ;W00FFFF
        PUSH    R27                  ;W00FFFE
        POP     R27
        POP     R26

                                     ; OpA OpB Res   Flags   Desc
                                     ;                       Test AND functionality
                                     ;                       between two registers
        AND     R17, R28             ; FF  55  55  11000001  FF AND anything is itself
        AND     R17, R29             ; 55  AA  00  11000011  No overlap bits gives 0
        PUSH    R17                  ;W00FFFF
        POP     R17
        AND     R13, R0              ; 40  80  00  11000011  No overlap bits gives 0
        PUSH    R13                  ;W00FFFF
        POP     R13

                                     ; OpA OpB Res   Flags   Desc
                                     ;                       Test AND between a register
                                     ;                       and an immediate
        ANDI    R28, $FF             ; 55  FF  55  11000001  FF AND anything is itself
        ANDI    R16, $FF             ; FF  FF  FF  11010101  FF AND FF is FF
        PUSH    R28                  ;W55FFFF
        PUSH    R16                  ;WFFFFFE
        POP     R16
        POP     R28
        ANDI    R16, $00             ; FF  00  00  11000011  00 AND anything is 0
        ANDI    R29, $FF             ; AA  FF  AA  11010101  anything AND FF is itself
        PUSH    R16                  ;W00FFFF
        PUSH    R29                  ;WAAFFFE
        POP     R29
        POP     R16

                                     ; OpA OpB Res   Flags   Desc
                                     ;                       Test arithmetic shift right
        LDI     R16, $FF
        ASR     R16                  ; FF  XX  FF  11010101  FF shifted always give FF
        ASR     R27                  ; 00  XX  00  11000010  00 shifted always gives 00
        ASR     R30                  ; 70  XX  38  11000000  Test a random case where 0
                                     ;                       is shifted out
        ASR     R8                   ; DF  XX  EF  11010101  Test a random case where 1
                                     ;                       is shifted out
        PUSH    R16                  ;WFFFFFF
        PUSH    R27                  ;W00FFFE
        PUSH    R30                  ;W38FFFD
        PUSH    R8                   ;WEFFFFC
        POP     R8
        POP     R30
        POP     R27
        POP     R16

                                     ; OpA OpB Res   Flags   Desc
                                     ;                       Test one's complement, which
                                     ;                       inverts all of the bits
        COM     R16                  ; FF  XX  00  11000011  Invert all 1s to all 0s
        PUSH    R16                  ;W00FFFF
        POP     R16
        COM     R16                  ; 00  XX  FF  11010101  Invert all 0s to all 1s
        PUSH    R16                  ;WFFFFFF
        POP     R16
        COM     R28                  ; 55  XX  AA  11010101  Arbitrary case
        PUSH    R28                  ;WAAFFFF
        POP     R28
        COM     R28                  ; AA  XX  55  11000001  Arbitrary case
        PUSH    R28                  ;W55FFFF
        POP     R28

                                     ; OpA OpB Res   Flags   Desc
                                     ;                       Compare should set flags
                                     ;                       according to OpA - OpB
        CP      R17, R16             ; 00  FF  --  11100001  Carry flag sets when A < B
        CP      R21, R26             ; 7E  00  --  11000000  CF clear when A >= B
        CP      R31, R31             ; 3F  3F  --  11010110  ZF set when A = B

                                     ; OpA OpB Res   Flags   Desc
                                     ;                       Compare with carry should
                                     ;                       set flags according to
                                     ;                       OpA - OpB - CF
        CPC     R17, R16             ; 00  FF  --  11100001  CF set when A < B + CF
        CPC     R21, R21             ; 7E  7E  --  11110110  ZF clear when A = B and CF = 1
                                     ;                       CF set when A = B and CF = 1
        CPC     R21, R17             ; 7E  00  --  11000000  CF clear when A >= B + CF
        CPC     R21, R21             ; 7E  7E  --  11110110  ZF set when A = B and CF = 0
        CPC     R21, R16             ; 7E  FF  --  11100001  CF set when A < B + 1

        LDI     R30, $40
        LDI     R31, $7F
                                     ; OpA OpB Res   Flags   Desc
                                     ;                       Test compare with immediate
        CPI     R17, $7F             ; 00  7F  --  11110101  CF set when A < B
        CPI     R30, $40             ; 40  40  --  11010110  ZF set when A = B
        CPI     R31, $30             ; 7F  30  --  11001101  CF clear when A > B

        MOV     R30, R19
                                     ; OpA OpB Res   Flags   Desc
                                     ;                       Test decrement on zero value
                                     ;                       and nonzero value to check
                                     ;                       wraparound
        DEC     R17                  ; 00  XX  FF  11010101  0 - 1 wraps around to FF
        DEC     R0                   ; 80  XX  7F  11011001  0x80 - 1 sets all other bits
        DEC     R17                  ; FF  XX  FE  11010101  FF - 1 only clears bit 0
        PUSH    R17                  ;WFEFFFF
        POP     R17

        MOV     R17, R28
                                     ; OpA OpB Res   Flags   Desc
                                     ;                       Test exclusive or
        EOR     R17, R29             ; 55  AA  FF  11010101  No like bits should yield FF
        EOR     R17, R28             ; FF  55  AA  11010101  Ditto with operands switched
        EOR     R18, R17             ; 00  AA  AA  11010101  00 XOR anything yields itself
        EOR     R18, R16             ; AA  FF  55  11000001  FF XOR anything inverts itself
        EOR     R24, R24             ; 45  45  00  11000011  anything XOR itself is 00
        PUSH    R18                  ;W55FFFF
        POP     R18

                                     ; OpA OpB Res   Flags   Desc
                                     ;                       Test increment.  We want to
                                     ;                       see that 0 goes to 1, FF
                                     ;                       wraps to 0, 7F carries through
                                     ;                       every bit
        INC     R24                  ; 00  XX  01  11000001  0 successfully incs to 1
        INC     R22                  ; FE  XX  FF  11010101  Can have all bits set
        INC     R22                  ; FF  XX  00  11000011  FF + 1 wraps to 0
        INC     R0                   ; 7F  XX  80  11001101  7F + 1 carries +1 for each bit
        PUSH    R0                   ;W80FFFF
        POP     R0

        DEC     R26
        LDI     R31, $80
                                     ; OpA OpB Res   Flags   Desc
                                     ;                       Logical shift right should
                                     ;                       act exactly as arithmetic
                                     ;                       shift right
        LSR     R26                  ; FF  XX  7F  11011001  Successfully bring in 0 on
                                     ;                       all 1s shifted right
        LSR     R30                  ; 6F  XX  37  11011001  Arbitrary case
        LSR     R20                  ; 00  XX  00  11000010  00 shifted right should be
                                     ;                       still 00
        LSR     R31                  ; 80  XX  40  11000000  Shift right with only 1 bit
                                     ;                       set
        PUSH    R31                  ;W40FFFF
        POP     R31

                                     ; OpA OpB Res   Flags   Desc
                                     ;                       Negates what is in the
                                     ;                       register by computing two's
                                     ;                       complement
        NEG     R16                  ; FF  XX  01  11100001  FF = -1 negated = 1
        NEG     R22                  ; 00  XX  00  11000010  Negative 0 is still 0
        NEG     R0                   ; 80  XX  80  11001101  Only sign bit set negated
                                     ;                       leaves high bit set
        NEG     R21                  ; 7E  XX  82  11110101  Arbitrary case with clear
                                     ;                       sign bit should set sign bit
        PUSH    R21                  ;W82FFFF
        POP     R21

                                     ; OpA OpB Res   Flags   Desc
                                     ;                       Test logical OR on two
                                     ;                       registers
        OR      R18, R17             ; 55  AA  FF  11110101  No like bits OR'd gives FF
        OR      R18, R28             ; FF  55  FF  11110101  FF OR anything is FF
        OR      R22, R17             ; 00  AA  AA  11110101  00 OR anything is itself
        PUSH    R18                  ;WFFFFFF
        PUSH    R22                  ;WAAFFFE
        POP     R22
        POP     R18

                                     ; OpA OpB Res   Flags   Desc
        ORI     R17, $FF             ; AA  FF  FF  11110101  anything OR FF is FF
        ORI     R17, $00             ; FF  00  FF  11110101  anything OR 00 is itself
        ORI     R25, $7D             ; 7D  7D  7D  11100001  anything OR itself is itself
        PUSH    R25                  ;W7DFFFF
        POP     R25

                                     ; OpA OpB Res   Flags   Desc
                                     ;                       Test rotate right through
                                     ;                       carry
        ROR     R17                  ; FF  XX  FF  11110101  FF rotated will always be FF
                                     ;                       when CF = 1
        ROR     R19                  ; 70  XX  B8  11101100  Move 0 into CF and CF = 1
                                     ;                       into high bit
        ROR     R16                  ; 01  XX  00  11111011  Move CF = 0 into high bit
                                     ;                       and 1 into CF
        ROR     R0                   ; 80  XX  C0  11101100  Move 0 into CF and CF = 1 into
                                     ;                       high bit
        ROR     R16                  ; 00  XX  00  11100010  00 rotated is always 0 when
                                     ;                       CF = 0
        PUSH    R19                  ;WB8FFFF
        POP     R19

        LDI     R31, $50
                                     ; OpA OpB Res   Flags   Desc
                                     ;                       Test subtract with carry,
                                     ;                       which computes A - B - CF,
                                     ;                       both with two registers and
                                     ;                       with one register and an
                                     ;                       immediate
        SBC     R16, R17             ; 00  FF  01  11100001  CF = 0, so 00 - FF should
                                     ;                       borrow such that result is 1
        SBCI    R31, $70             ; 50  70  DF  11110101  CF = 1, show that we subract
                                     ;                       1 after in operation
        SBC     R10, R20             ; 7F  00  7E  11000000  Test subtracting 0 with CF = 1
                                     ;                       results in reg - 1
        PUSH    R10                  ;W7EFFFF
        POP     R10

        LDI     R25, $7F
        LDI     R24, $71
                                     ; OpA OpB Res   Flags   Desc
                                     ;                       Subtraction test continued
        SBCI    R26, $7F             ; 7F  7F  00  11000000  Anything minus itself is 0
                                     ;                       when CF = 0
        PUSH    R26                  ;W00FFFF
        POP     R26
        SBC     R25, R17             ; 7F  FF  80  11000000  Arbitrary case with CF = 0
        SBCI    R24, $A0             ; 71  A0  D0  11001101  Arbitrary case with CF = 1

        LDI     R24, $0D
        LDI     R25, 0
                                     ; OpA OpB Res   Flags   Desc
                                     ;                       Test subtract immediate from
                                     ;                       word, Rd+1:Rd - K
        SBIW    R24, $10             ; 0D  10  FD  --------  Test wraparound case
                                     ; 00  XX  FF  11010101  High word
        SBIW    R24, $00             ; FD  00  FD  --------  Anything minus 00 is itself
                                     ; FF  XX  FF  11010100  High word
        PUSH    R25                  ;WFFFFFF
        PUSH    R24                  ;WFDFFFE
        POP     R24
        POP     R25

        LDI     R30, $7F
        LDI     R31, $7F
                                     ; OpA OpB Res   Flags   Desc
                                     ;                       Test regular subtraction
        SUB     R16, R17             ; 01  FF  02  11100001  Test when A < B that we see
                                     ;                       wrap around
        SUB     R31, R20             ; 7F  00  7F  11000000  Test A - 0 = A
        PUSH    R16                  ;W02FFFF
        POP     R16

        LDI     R30, $50
        LDI     R31, $71
                                     ; OpA OpB Res   Flags   Desc
                                     ;                       Test subtraction with register
                                     ;                       and immediate
        SUBI    R20, $7F             ; 00  7F  81  11110101  Test wraparound
        SUBI    R30, $70             ; 50  70  E0  11010101  Test when A < B
        SUBI    R31, $00             ; 71  00  71  11000000  Test A - 0 = A
        PUSH    R30                  ;WE0FFFF
        POP     R30

                                     ; OpA OpB Res   Flags   Desc
                                     ;                       Test swap nibbles function,
                                     ;                       which changes high nibble
                                     ;                       for low nibble in only
                                     ;                       operand
        SWAP    R21                  ; 82  XX  28  11001101  Arbitrary case
        SWAP    R10                  ; 7E  XX  E7  11001101  Arbitrary case
        SWAP    R27                  ; 00  XX  00  11001101  00 should swap to be 00
        PUSH    R10                  ;WE7FFFF
        POP     R10

                                     ; Test PUSH and POP instructions, which will write
                                     ; and read from data memory
        PUSH    R26                  ;W00FFFF                Push 0 at bottom of the
                                     ;                       stack
        PUSH    R27                  ;W00FFFE                Push 0
        PUSH    R28                  ;W55FFFD                Pushing different, arbitrary
                                     ;                       registers now...
        PUSH    R29                  ;WAAFFFC
        PUSH    R30                  ;WE0FFFB
        PUSH    R31                  ;W71FFFA
                                     ; Pop everything
        POP     R0                   ;R71FFFA
        POP     R1                   ;RE0FFFB
        POP     R2                   ;RAAFFFC
        POP     R3                   ;R55FFFD
        POP     R4                   ;R00FFFE
        POP     R5                   ;R00FFFF

                                     ; setup addresses for writing
        LDI     R27, $FF             ; X = FFFF
        LDI     R26, $FF
        LDI     R29, $FF             ; Y = FFC0
        LDI     R28, $C0
        LDI     R31, $00             ; Z = 0080
        LDI     R30, $80

                                     ;                       Desc
                                     ;                       Test direct storing of data
                                     ;                       into data space.  We will do
                                     ;                       at least one write with each
                                     ;                       register (except 26+ because)
                                     ;                       they were demonstrated with
                                     ;                       PUSHes
        STS      $5555, R0           ;W715555                Test write R0, immediate
        STS      $AAAA, R1           ;WE0AAAA                Test write R1, immediate

                                     ;                       Test store data into address
                                     ;                       contained in X, Y and Z with
                                     ;                       pre-DEC and post-INC, showing
                                     ;                       wrapping.
        ST       X, R2               ;WAAFFFF                Test at X with no INC/DEC, R2
        ST      -X, R3               ;W55FFFE                Test at X with pre-DEC, R3
        ST      X+, R4               ;W00FFFE                Test at X with post-INC, R4
        ST      X+, R5               ;W00FFFF                Show that post-INC wraps, R5
        ST       X, R6               ;W100000                Proof of wrap, R6
        ST      -X, R4               ;W00FFFF                Show wrap pre-DEC wraps, R7
        ST      Y+, R7               ;W04FFC0                Test post-INC with Y
        ST       Y, R8               ;WEFFFC1                Test write at (Y), R8
        ST      -Y, R9               ;W04FFC0                Test pre-DEC with Y, R9
        ST       Y, R10              ;WE7FFC0                Test write R10
        STD     Y + 60, R11          ;W01FFFC                Test write displacement, R11
        STD     Y + 2, R12           ;WFDFFC2                Test write R12
        STD     Y + 22, R13          ;W00FFD6                Test write R13
        STD     Y + 1, R14           ;WF7FFC1                Test write R14
        ST      Z+, R15              ;W100080                Test at Z with post-INC, R15
        ST       Z, R16              ;W020081                Test write R16
        ST      -Z, R17              ;WFF0080                Test at Z with pre-DEC, R17
        ST       Z, R18              ;WFF0080                Test write R18
        STD     Z + 30, R19          ;WB8009E                Test write R19
        STD     Z + 1, R20           ;W810081                Test write R20
        STD     Z + 63, R21          ;W2800BF                Test write R21
        STD     Z + 32, R22          ;WAA00A0                Test write R22

                                     ; setup another address for writing
        LDI     R29, $FF             ; Y = FFE0
        LDI     R28, $E0

        ST      -Y, R23              ;W00FFDF                Test write R23
        ST      Y+, R24              ;WFDFFDF                Test write R24
        STD     Y + 63, R25          ;WFF001F                Test write R25


                                     ;setup new addresses for reading
        LDI     R27, 0               ; X = 0
        LDI     R26, 0
        LDI     R29, $FF             ; Y = FFFF
        LDI     R28, $FF
        LDI     R31, $FF             ; Z = FFC0
        LDI     R30, $C0

                                     ;                       Do a read with every register
                                     ;                       not already tested with POP.
                                     ;                       We will also test the
                                     ;                       functionality of pre-DEC and
                                     ;                       post-INC on some writes, as
                                     ;                       well as displacement and with
                                     ;                       immediate addresses
        LDS     R6, $AAAA            ;RE0AAAA                Test read R6, immediate
        LDS     R7, $5555            ;R715555                Test read R7, immediate

        LD      R8, X                ;R100000                Test read R8
        LD      R9, -X               ;R00FFFF                Test read R9 with pre-DEC
        LD      R10, X+              ;R00FFFF                Test read R10 with X post-INC
        LD      R11, X               ;R100000                Test read R11 and wrapping
        LD      R12, Y+              ;R00FFFF                Test read R12 with Y post-INC
        LD      R13, Y               ;R100000                Test read R13, show wrapping
        LD      R14, -Y              ;R00FFFF                Test read R14 with Y pre-DEC
        LDD     R15, Y + 32          ;RFF001F                Test read R15 with Y displace
        LD      R16, Z+              ;RE7FFC0                Test read R16 with Z post-INC
        LD      R17, Z               ;RF7FFC1                Test read R17
        LD      R18, -Z              ;RE7FFC0                Test read R18 with Z pre-DEC
        LDD     R19, Z + 60          ;R01FFFC                Test read R19 with Z displace

                                     ;                       Read from rest of registers
        PUSH    R8                   ;W10FFFF
        PUSH    R9                   ;W00FFFE
        PUSH    R10                  ;W00FFFD
        PUSH    R11                  ;W10FFFC
        PUSH    R12                  ;W00FFFB
        PUSH    R13                  ;W10FFFA
        PUSH    R14                  ;W00FFF9
        PUSH    R15                  ;WFFFFF8
        PUSH    R16                  ;WE7FFF7
        PUSH    R17                  ;WF7FFF6
        PUSH    R18                  ;WE7FFF5
        PUSH    R19                  ;W01FFF4

        POP     R20                  ;R01FFF4                Test read from R20
        POP     R21                  ;RE7FFF5                Test read from R21
        POP     R22                  ;RF7FFF6                Test read from R22
        POP     R23                  ;RE7FFF7                Test read from R23
        POP     R24                  ;RFFFFF8                Test read from R24
        POP     R25                  ;R00FFF9                Test read from R25
        POP     R26                  ;R10FFFA                Test read from R26
        POP     R27                  ;R00FFFB                Test read from R27
        POP     R28                  ;R10FFFC                Test read from R28
        POP     R29                  ;R00FFFD                Test read from R29
        POP     R30                  ;R00FFFE                Test read from R30
        POP     R31                  ;R10FFFF                Test read from R31


TestJumps:                           ; test unconditional jumping
        JMP     JumpTest             ;                       Test regular jump
        LDI     R27, 0               ;                       These should not execute if
        LDI     R28, 0               ;                       jump works
JumpTest:
        LDI     R22, $5A
        LDI     R23, $5A
        RJMP    TestRJump            ; Test relative jump
        LDI     R27, 0               ;                       These should not execute if
        LDI     R28, 0               ;                       jump works
TestRJump:
        LDI     R30, LOW(TestIJump)  ; Load Z for indirect jump
        LDI     R31, HIGH(TestIJump)
        IJMP
        LDI     R27, 0               ; These should not execute
        LDI     R28, 0
TestIJump:

TestCalls:                           ; Test subroutine calls
        EOR     R0, R0
        CALL    Subr1                ;                       Direct subroutine call
        RCALL   Subr1                ;                       Relative direct call
        LDI     R30, LOW(Subr1)      ; Load Z for indirect call
        LDI     R31, HIGH(Subr1)
        ICALL                        ; Indirect subroutine call


TestBranches:                        ; test some conditional branches
        LDI     R28, $7F
        LDI     R27, $FF
        CP      R28, R27
        BRLO    Branch1              ;                       Should branch: $7F U< $FF
        JMP     TestBranches         ;                       Will infinitely loop if fails
Branch1:
        BRLT    TestBranches         ;                       should not branch: $7F S> $FF
        BREQ    TestBranches         ;                       should not branch: $7F != $FF
        BRNE    Branch2              ;                       should branch: $7F != $FF
        JMP     TestBranches         ;                       Will infinitely loop if fails
Branch2:
        LDI     R21, $69
        ADD     R21, R21
        BRHC    TestBranches         ;                       should not branch (HC is set)
        OR      R27, R27             ;                       this is a negative number
        BRMI    Branch3              ;                       Branch minus (N = 1)
        JMP     TestBranches         ;                       Will infinitely loop if fails
Branch3:
        OR      R28, R28             ;                       this is a positive number
        BRMI    TestBranches         ;                       Don't branch minus (N = 0)
        BRPL    Branch4              ;                       Branch postive (N = 0)
        JMP     TestBranches         ;                       Will infinitely loop if fails
Branch4:
        OR      R27, R27             ;                       this is a negative number
        BRPL    TestBranches         ;                       Don't branch positive (N = 0)
        SUB     R28, R27             ;                       this generates an overflow
        BRVS    Branch5              ;                       Branch overflow (O = 1)
        JMP     TestBranches         ;                       Will infinitely loop if fails
Branch5:                             ;
        DEC     R28                  ;                       80 - 1 -> 7F => overflow
        BRVC    TestBranches         ;                       Don't branch not overflow
        DEC     R28                  ;                       7F - 1 -> 7E => no overflow
        BRVC    Branch6              ;                       Branch on no overflow
        JMP     TestBranches         ;                       Will infinitely loop if fails
Branch6:
        CPI     R27, 1               ;                       -1 < 1
        BRGE    TestBranches         ;                       so should not take the branch
        CPI     R28, 1               ;                       7E > 1
        BRGE    Branch7              ;                       Should now branch
        JMP     TestBranches         ;                       Will infinitely loop if fails
Branch7:
        CLI                          ;                       clear interrupt flag
        BRIE    TestBranches         ;                       Don't branch on interrupt
        CALL    SubrI                ;                       call subroutine with RETI
        BRIE    Branch8              ;                       Now branch on interrupt
        JMP     TestBranches         ;                       Will infinitely loop if fails
Branch8:
        BRID    TestBranches         ;                       RETI set I flag, don't branch
        CLI                          ;                       clear interrupt flag
        BRID    Branch9              ;                       Now want to branch
        JMP     TestBranches         ;                       Will infinitely loop if fails
Branch9:
        BST     R30, 1               ;                       set the T flag
        BRTC    TestBranches         ;                       so should not branch
        BST     R30, 3               ;                       now clear the T flag
        BRTC    Branch10             ;                       Now want to branch on clear T
        JMP     TestBranches         ;                       Will infinitely loop if fails
Branch10:
        BRTS    TestBranches         ;                       Don't branch with clear T
        BST     R30, 1               ;                       set the T flag
        BRTS    Branch11             ;                       Now branch on set T
        JMP     TestBranches         ;                       Will infinitely loop if fails
Branch11:
        ADD     R30, R30             ;                       R30 is now $CC (no carry)
        BRSH    Branch12             ;                       so should take the branch
        JMP     TestBranches         ;                       Will infinitely loop if fails
Branch12:
        ADD     R30, R30             ;                       should set the carry and half carry
        BRSH    Branch12             ;                       should not take branch
        BRHS    TestSkips            ;                       but should take this one
        JMP     TestBranches         ;                       Will infinitely loop if fails

TestSkips:                           ;                       test skip instructions
        MOV     R22, R23
        CPSE    R22, R23             ;                       skip a 1 byte instruction
        RJMP    TestSkips
        CPSE    R22, R23             ;                       skip a 2 byte instruction
        JMP     TestSkips
        CPSE    R22, R24             ;                       don't skip
        LDI     R22, $80
        SBRC    R22, 6               ;                       should skip a 1 byte instruction
        LDI     R22, $FF
        SBRC    R22, 3               ;                       should skip a 2 byte instruction
        JMP     TestSkips
        SBRC    R22, 7               ;                       don't skip
        LDI     R22, $A5
        SBRS    R22, 0               ;                       should skip a 1 byte instruction
        LDI     R22, 0
        SBRS    R22, 5               ;                       should skip a 2 byte instruction
        JMP     TestSkips
        SBRS    R22, 1               ;                       don't skip
        JMP     Start                ;                       start over


Subr1:                               ; the subroutine
        LDI     R27, $FF
        LDI     R28, $7F
        LDI     R29, 0
        RET


SubrI:                               ; subroutine ending with RETI
        LDI     R25, $FF
        LDI     R26, $7F
        LDI     R30, $66
        RETI
