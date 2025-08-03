;
UARTD			EQU		20H			;Data address
UARTC			EQU		21H			;Control address
UARTS			EQU		21H			;Status address

RX_MASK			EQU		00000010B	; RXRDY flag bit in status byte
TX_MASK			EQU		00000001B	; TXRDY flag bit in status byte
DT_MASK 		EQU 	10000000B	; DSR flag bit in status byte

PPIPA 			EQU 	10H			; 8255A Port A
PPIPB			EQU 	11H			; 8255A Port B
PPIPC   		EQU 	12H 		; 8255A Port C
PPICTRL 		EQU 	13H			; 8255A Control Register

CTCC0			EQU		30H			; P8254 Counter 0
CTCC1   		EQU		31H			; P8254 Counter 1
CTCC2			EQU		32H			; P8254 Counter 2
CTCCMD  		EQU		33H			; P8254 Control register 
;
RAM				EQU 	0000H
ROM				EQU 	B000H
;ROM				EQU 	8000H
;
RAMSIG			EQU		000EH
;
				ORG		ROM
;
				JMP		START
				DB		"FIG-FORTH LOADER (C) WOEPER 2025"
;
;
;-----------------------------------------------------------------------
;	Initialize the PPI Chip 8255A
;-----------------------------------------------------------------------
;
START:			MVI		A,10000011B				; ppi ports  A and C High defined as output B and C Low as input
				OUT		PPICTRL					; Init PPI
;
;-----------------------------------------------------------------------
; Initialize the 8251 UART
;-----------------------------------------------------------------------					
				MVI		A,0						; insure not setup mode
				OUT		UARTC					; write once
				OUT		UARTC					; write again (now in operate mode)
				OUT		UARTC					; write again (now in operate mode)	
				MVI		A,040H					; return to setup mode
				OUT		UARTC					; write it
				IN      PPIPB					; read switches
				ANI     08H						; get switch 4 7n1 or 8e2
				JNZ		SETBD3					; 
				MVI		A,11111110B				; 8 data, 2 stop, even parity ,clock 16 time
				JMP		SETBD4
SETBD3:			MVI		A,01001010B				; 7 data, 1 stop, no parity ,clock 16 time
SETBD4:			OUT 	UARTC
				MVI		A,00100111B				; rts,dtr,enable rx and tx
				OUT		UARTC					; write it
				IN      PPIPB					; read switch
				ANI		07H						; select speed bits
SETBD0:			LXI		H,SPDTAB				; table
				LXI		D,2						; offset
SETBD1:			DCR		A						; at end of it
				JM		SETBD2					; yes found
				DAD		D						; add offset
				JMP		SETBD1					; next hit
SETBD2:			MVI		A,00110110B				; counter zero mode 3		; set pit
				OUT		CTCCMD					; 
				MOV		A,M						; get low
				OUT		CTCC0					; to pit
				INX		H						; get high
				MOV		A,M
				OUT		CTCC0					; to pit
				
;
;-----------------------------------------------------------------------
;   DISPLAY LOADER MESSAGE
;-----------------------------------------------------------------------			
;
				LXI		H,LDRMSG
				CALL	PMSGA
				JMP		CLEARMEM
LDRMSG:			DB		13,10, "Loading...",0
;
;-----------------------------------------------------------------------
;   CLEAR ALL RAM TO 0 IF SWITCH 8 IS SET
;-----------------------------------------------------------------------			
;
CLEARMEM:   	IN		PPIPB					; Read the switches
				ANI		10000000B				; Read switch 8
				CPI		0						; Switch is OFF ?
				JZ		MOVE					; Skip Ram clear
				MVI		A,01000000B				; Signal memory clear 
				OUT		PPIPC					; with green led
				LDA		RAMSIG					; Load signature location low
				CPI		055H					; See if it is 55
				JNZ		CLEARMEM0				; nope init ram after power on
				LDA		RAMSIG+1				; get signature location high
				CPI		0AAH					; see it it is AA
				JNZ		CLEARMEM0				; nope must be power on 
				JMP		MOVE			        ; warm start
CLEARMEM0:		LXI		H,0						; init HL to 0
				MVI		A,00					; Acu to 0
CLEARMEM4:		MOV		M,A						; clear memory byte
				INX		H						; increment pointer
				CMP		H						; when H is again zero
				JNZ		CLEARMEM4				; nope continue
				CMP		L						; and L is again zero	
				JNZ		CLEARMEM4				; All 64K ram is cleared ( even if it is rom					;-) )
				MVI		A,055H					; prepare signature	low
				STA		RAMSIG					; store it
				MVI		A,0AAH					; prepare signature high
				STA		RAMSIG+1				; store it
;
;-----------------------------------------------------------------------
;   MOVE FIG-FORTH TO 0100H
;-----------------------------------------------------------------------			
;
MOVE:			LXI		H,0100H
				LXI		D,ROM+100H
				LXI		B,27FFH
MOVE0:			LDAX	D
				MOV		M,A
				INX		H
				INX		D
				DCX		B
				MOV		A,B
				CPI		0
				JNZ		MOVE0
				MOV		A,C
				CPI		0
				JNZ		MOVE0
;
READY:			IN		PPIPC					; Read leds
				ORI		00010000H				; Set red led ON
				OUT		PPIPC					; execute
				JMP		0100H					; jump to fig-FORTH

;
;-----------------------------------------------------------------------
;	PRINT STRING POINTED BY HL UNTIL 0 TO PRIMARY
;-----------------------------------------------------------------------				
;
PMSGA:			MOV		A,M
				ORA		A
				RZ
				CALL    PUTCHR
				INX		H
				JMP		PMSGA
;				
;-----------------------------------------------------------------------
;	ACU -> 8251
;-----------------------------------------------------------------------
;
PUTCHR:			PUSH	PSW
PCLOP1:			IN		UARTS
				ANI		00000001B	;transmitter ready?
				JZ		PCLOP1
				POP		PSW
				OUT		UARTD		;transmit
				RET				
;
;-----------------------------------------------------------------------
;	SPEED TABLE
;-----------------------------------------------------------------------				
;				
SPDTAB:			DW		5					; 38400 baud
				DW	 	10					; 19200 baud
				DW	 	20					; 9600 baud
				DW		40					; 4800 baud
				DW		80					; 2400 baud
				DW		160					; 1200 baud
				DW		320					; 600 baud
					