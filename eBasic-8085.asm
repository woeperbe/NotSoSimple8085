;
; microsoft basic
;
; COPYRIGHT (C) 1977 BILL GATES
; 
; 
;
ROMSTRT	EQU		08000H					; START OF ROM
RAMSTRT	EQU		00000H					; START OF RAM
RAMEND	EQU		08000H					; END OF RAM+1
;
MONROM	EQU		0E000H
FIGROM	EQU		0B000H
;
RX_MASK	EQU		00000010B				; RXRDY flag bit in status byte
TX_MASK	EQU		00000101B				; TXRDY flag bit in status byte
TE_MASK EQU     00000100B
DS_MASK EQU 	10000000B				; DSR flag bit in status byte
RTS_ON  EQU     00100111B				; RTS ON -  RX and TX enable DTR on
RTS_OFF EQU 	00000111B				; RTS OFF -  RX and TX enable DTR on
;
PPIPA 	EQU 	10H						; 8255A Port A
PPIPB	EQU 	11H						; 8255A Port B
PPIPC   EQU 	12H 					; 8255A Port C
PPICTRL EQU 	13H						; 8255A Control Register
;
UARTD	EQU		20H						; Data address
UARTC	EQU		21H						; Control address
UARTS	EQU		21H						; Status address
;
CTCC0	EQU		30H						; P8254 Counter 0
CTCC1   EQU		31H						; P8254 Counter 1
CTCC2	EQU		32H						; P8254 Counter 2
CTCCMD  EQU		33H						; P8254 Control register 
;
UBRTD	EQU		40H						; Data address
UBRTC	EQU		41H						; Control address
UBRTS	EQU		41H						; Status address
										



;
; BASIC EQUATES
;
RUBOUT	EQU		07FH					; RUBOUT CHARACTER
FATAL   EQU     0CDH    				; OPCODE FOR FATAL ERROR IS "CALL" TO PRERR
;
;
;
		ORG     ROMSTRT
;
		JMP		GORAM
;
;		
		DB		" *** MICROSOFT BASIC BY BILL GATES (C) COPYRIGHT 1977 *** "
;
;
;-----------------------------------------------------------------------
;	INIT THE PPI CHIP 8255A
;-----------------------------------------------------------------------
;
GORAM:	DI
		MVI		A,10000011B				; ppi ports  a and c high defined as output b and c low as input
		OUT		PPICTRL	
;
;-----------------------------------------------------------------------
;	CLEAR RAM IF ACTIVATED
;-----------------------------------------------------------------------
;		
		IN		PPIPB					; Read the switches
		ANI		10000000B				; Read switch 8
		CPI		0						; Switch is OFF ?
		JZ		SETBDS					; Skip Ram clear
		MVI		B,3						; Select green Led
		MVI		C,1						; Set it ON
		CALL	SETLED					; Process	
		LDA		RAMSIG					; Load signature location low
		CPI		055H					; See if it is 55
		JNZ		INIT0					; nope init ram after power on
		LDA		RAMSIG+1				; get signature location high
		CPI		0AAH					; see it it is AA
		JNZ		INIT0					; noper must be power on 
		JMP		SETBDS					; skip copyright message and RUN basic
INIT0:	LXI		H,0						; init HL to 0
		MVI		A,00					; Acu to 0
INIT4:	MOV		M,A						; clear memory byte
		INX		H						; increment pointer
		CMP		H						; when H is again zero
		JNZ		INIT4					; nope continue
		CMP		L						; and L is again zero	
		JNZ		INIT4					; All 64K ram is cleared ( even if it is rom					; -) )
		MVI		A,055H					; prepare signature	low
		STA		RAMSIG					; store it
		MVI		A,0AAH					; prepare signature high
		STA		RAMSIG+1				; store it
		XRA     A       				; get a zero
        LHLD    DATAB   				; point data area
        MOV     M,A     				; clear it
        LXI     H,BEGPR 				; point start
        SHLD    PROGE   				; reset program end
        MOV     M,A     				; clear it
;		
;-----------------------------------------------------------------------
;  INIT SERIAL CONSOLE UART
;-----------------------------------------------------------------------
;	
SETBDS:	MVI		A,0						; insure not setup mode
		OUT		UARTC					; write once
		OUT		UARTC					; write again (now in operate mode)
		OUT		UARTC					; write again (now in operate mode)	
		MVI		A,040H					; return to setup mode
		OUT		UARTC					; write it
		IN      PPIPB					; read switches
		ANI     08H						; get switch 4 7n1 or 8e2
		JNZ		SETBD3					; 
		LXI		H,UART8E2
		SHLD	UARTMSG
		MVI		A,11111110B				; 8 data, 2 stop, even parity ,clock 16 time
		JMP		SETBD4
SETBD3:	LXI		H,UART7N1
		SHLD	UARTMSG
		MVI		A,01001010B				; 7 data, 1 stop, no parity ,clock 16 time
SETBD4:	OUT 	UARTC
		MVI		A,RTS_ON 				; rts on,dtr on ,enable rx and tx
		OUT		UARTC					; write it
		IN      PPIPB					; read switch
		ANI		07H						; select speed bits
SETBD0:	LXI		H,SPDTAB				; table
		LXI		D,9						; offset
SETBD1:	DCR		A						; at end of it
		JM		SETBD2					; yes found
		DAD		D						; add offset
		JMP		SETBD1					; next hit
SETBD2:	MVI		A,00110110B				; counter zero mode 3		; set pit
		OUT		CTCCMD					; 
		MOV		A,M						; get low
		OUT		CTCC0					; to pit
		INX		H						; get high
		MOV		A,M
		OUT		CTCC0					; to pit
		INX		H
		SHLD	SPDAMSG					; save for final message
;-----------------------------------------------------------------------
;  INIT IO DATA UART ALWAYS 9600,8E2
;-----------------------------------------------------------------------	
		MVI		A,0						; insure not setup mode
		OUT		UBRTC					; write once
		OUT		UBRTC					; write again (now in operate mode)
		OUT		UBRTC					; write again (now in operate mode)	
		MVI		A,40H					; return to setup mode
		OUT		UBRTC					; write it
		MVI		A,11111110B				; 8 data, 2 stop, even parity ,clock 16 time
SETBD6:	OUT 	UBRTC
		MVI		A,RTS_ON				; rts,dtr,enable rx and tx
		OUT		UBRTC					; write it
SETBD9:	MVI		A,01110110B				; counter one mode 3		
		OUT		CTCCMD					; 
		MVI		A,20					; get low
		OUT		CTCC1					; to pit
		MVI		A,0						; Get high
		OUT		CTCC1					; to pit
		
;-----------------------------------------------------------------------
;  BOOT LOADER SELECTOR
;-----------------------------------------------------------------------			
;
		LXI     H,BOOTMSG  				; point boot message
		CALL    TERMM   				; write it
		JMP		BOOT1
BOOTMSG:DB		12,13,10,"Booting ...",0		
BOOT1:	IN		PPIPB					; Read the switches
		ANI		01000000B				; Read switch 7
		CPI		0						; Switch is OFF ?
		JZ		MONITOR					; Jump to Monitor
;
;-----------------------------------------------------------------------
;	START BASIC
;-----------------------------------------------------------------------
;
GOBAS:	LXI     H,RAM+1024
		MVI     A,0AEH
;
;-----------------------------------------------------------------------		
;			INITIALIZATION ROUTINE	DETERMINE MEMORY SIZE.
;-----------------------------------------------------------------------
;
; test ram from ramstrt to ramend

		LXI     B,1024  				; 1k incr counter
INIT2:  MOV     A,M     				; get a byte from memory
        CMA             				; complement
        MOV     M,A     				; replace
        CMP     M       				; test if ram/rom/end
        JNZ     INIT3   				; brif out of ram
        CMA             				; re-complement
        MOV     M,A     				; put orig back
        DAD     B       				; point next block
		JNC		INIT2
INIT3:  SPHL            				; set stack pointer to end of memory
        LXI     B,-256  				; allow 256 bytes for stack
        DAD     B       				; add to address
        SHLD    DATAB   				; save addr of start of data

        XRA     A       				; get a zero in a
        PUSH    PSW     				; set stack 1 level deep without a gosub
        LXI     H,0     				; clear h,l
        DAD     SP      				; sp to h,l
        SHLD    STACK  					; save beg of stack
        CALL    IRAM    				; init ram
        LXI     D,NRNDX 				; point to random # series
        MVI     B,8     				; load count
        CALL    COPYD   				; copy to trnd<x> in ram table
        MVI     M,2     				; set random switch
        LXI     H,VERS  				; point version message
		CALL    TERMM   				; write it
		; 
		LHLD	SPDAMSG					; print console baudrate
		CALL	TERMM
		LHLD	UARTMSG 				; print uart mode
		CALL	TERMM
		; 
		LXI		H,MEMMSG				; prepare memory message
		CALL	TERMM					; print it
		CALL	FREER					; display free memory space

RDYM:
;
;-----------------------------------------------------------------------
; PRINT 'READY' AND SET ON READY STATUS LED
;-----------------------------------------------------------------------
;
RDY:    PUSH	B
		MVI		B,1						; Select SYSTEM READY Led
		MVI		C,1						; Set it ON
		CALL	SETLED					; Process
		POP		B
		LXI     H,READY 				; point ready msg
        CALL    TERMM   				; go print it
;
;-----------------------------------------------------------------------
; COMMAND INPUT ROUTINE
;
; READ A LINE FROM THE CONSOLE TTY
; IF STARTS WITH NUMERIC CHARACTER, ASSUME IT'S A LINE NUMBER BASIC STATEMENT
; IF NOT, IT IS EITHER AN IMMEDIATE STATEMENT, OR A COMMAND
;-----------------------------------------------------------------------
;

GETCM:  PUSH	B
		MVI		B,2						; Select RUN led
		MVI		C,0						; Clear it
		CALL	SETLED					; Process
		POP		B
		MVI     A,':'   				; prompt & on set for sw
        STA     EDSW    				; set mode=edit
        LHLD    STACK  	 				; get stack address
        SPHL            				; set reg sp
        CALL    TERMI   				; get a line
		PUSH 	H						; Save HL
		CALL	UCASE					; Uppercase command line 
		POP		H						; Restore HL
        CALL    PACK    				; go pack the number into b,c
        MOV     A,B     				; get hi byte of line number
        ORA     C       				; plus low byte
        JZ      EXEC    				; brif exec statement
        PUSH    B       				; save line number
        LXI     D,IMMED+1       		; point save area
        XCHG            				; flip/flop
        MOV     M,B    		 			; put lo line
        INX     H       				; point next
        MOV     M,C     				; put lo line
        INX     H       				; point next
        MVI     B,3     				; init count
EDIT1:  LDAX    D       				; get a byte
        MOV     M,A     				; put it down
        INR     B       				; count it
        INX     H       				; point next
        INX     D       				; ditto
        ORA     A       				; test byte just moved
        JNZ     EDIT1   				; loop
        MOV     A,B     				; get count
        STA     IMMED   				; store the count
        POP     B       				; get line num
        CALL    LOCAT   				; go find requested line number
        PUSH    H       				; save h,l
        JC      EDIT5   				; brif if line not found
EDIT2:  MOV     D,H     				; copy addr
        MOV     E,L     				; to d,e
        MVI     B,0     				; get a zero
        MOV     C,M    					; get len
        DAD     B       				; point next statement
EDIT3:  MOV     A,M     				; get len next statement
        ORA     A      					; test it
        JZ      EDIT8   				; brif end
        MOV     B,A     				; set length
        CALL    COPYH   				; else move line
        JMP     EDIT3   				; loop
EDIT8:  XCHG            				; put new addr to h,l
        MOV     M,A     				; mark end
        SHLD    PROGE   				; and update address
EDIT5:  LDA     IMMED   				; get len of insert
        CPI     4       				; test if delete
        JZ      GETCM   				; brif is
        MOV     C,A     				; set lo len
        MVI     B,0     				; zero hi len
        LHLD    PROGE   				; get end of prog
        MOV     D,H     				; copy to
        MOV     E,L     				; d,e
        DAD     B       				; disp len of insert
        SHLD    PROGE   				; update end point
        POP     B       				; get addr
EDIT6:  LDAX    D       				; get a byte
        MOV     M,A     				; copy it
        DCX     D       				; point prior
        DCX     H       				; ditto
        MOV     A,D    					; get hi addr
        CMP     B       				; compare
        JZ      EDIT7   				; brif hi equal
        JNC     EDIT6   				; brif not less
EDIT7:  MOV     A,E     				; get lo addr
        CMP     C       				; compare
        JNC     ED7A    				; must test for 00 boundary
        JMP     ED7B    				; go around boundary test code
ED7A:   CMA             				; compliment low line number
        CMP     C       				; and compare to start
        JNZ     EDIT6   				; brif not =
        ORA     A       				; not test for 00
        JNZ     EDIT6   				; this is usual case
ED7B:   INX     D       				; point forward
        LXI     H,IMMED 				; point insert
        MOV     B,M     				; get length
        CALL    COPYH   				; go move it
        JMP     GETCM   				; go get another command
;
;-----------------------------------------------------------------------
; IRAM          INITIALIZE RAM
;     			ZEROES RAM FROM BZERO TO EZERO
;		        INITS RANDOM # CONSTANTS
;      		RETURNS H=PTR TO TRND
;-----------------------------------------------------------------------
;
IRAM:   LXI     H,BZERO 				; clear bzero->ezero
        MVI     B,EZERO-BZERO
        CALL    ZEROM
        LXI     D,NRNDX 				; move random # series to rndx
        LXI     H,RNDX
        MVI     B,8     				; count
        JMP     COPYD   				; move it & return
;
;---------------------------------------------------
; 			DECODE COMMAND IN IOBUFF
; 			EXECUTE IF POSSIBLE
; 			THEN GOTO GET NEXT COMMAND
;---------------------------------------------------
;
EXEC:   STA     MULTI   				; reset multi sw
        STA     FNMOD   				; reset fn type
        INR     A       				; get a one
        STA     RUNSW   				; set immediate mode
        LXI     H,IOBUF+1       		; point smt
        LXI     D,IMMED 				; point new area
EXEC1:  MOV     A,M    					; get a byte
        STAX    D       				; put to (d,l)
        INX     D       				; point next
        INX     H       				; ditto
        ORA     A       				; test byte
        JNZ     EXEC1   				; continue
        LXI     H,NULLI 				; point no line num
        SHLD    LINE    				; save addr
        LXI     H,IMMED 				; point start of cmmd
        JMP     RUN3    				; go into run processor
;
;---------------------------------------------------
; NEW COMMAND
; 'NEW'==>CLEAR PROGRAM AND DATA
; 'NEW*'==>CLEAR PROGRAM ONLY
;---------------------------------------------------
;
NEW:    PUSH    H       				; save ptr
        LXI     H,GETCM 				; make subroutine
        XTHL            				; restore h
		CALL	TSTC					; call to ram address
	    SBI     '*'     				; test
        JZ      NEW1    				; brif program clear only
NEW0:   XRA     A       				; get a zero
        LHLD    DATAB   				; point data area
        MOV     M,A     				; clear it
NEW1:   LXI     H,BEGPR 				; point start
        SHLD    PROGE   				; reset program end
        MOV     M,A     				; clear it
        RET
;
;---------------------------------------------------
; FREE COMMAND
; COMPUTE AMOUNT OF AVAILABLE STORAGE (EXCLUDING DATA AREA)
;---------------------------------------------------
;
FREE: 	CALL	FREER
        JMP     GETCM   				; continue
;
FREER:	LHLD    DATAB   				; get data beg address
        XCHG            				; put in d,e
        LHLD    PROGE   				; get program end address
        MOV     A,E     				; lo addr to reg a
        SUB     L       				; subtract
        MOV     E,A     				; save it
        MOV     A,D     				; hi addr to reg a
        SBB     H       				; subtract
        MOV     D,A     				; save it
		CALL    BINFL   				; go float d,e
        LXI     H,IOBUF 				; point buffer
        CALL    FOUT    				; go convert to output
        MVI     M,0FEH  				; mark end
        CALL    TERMO   				; go write it
		LXI     H,FRBTS 				; point bytes message
		CALL    TERMM	
		RET
;
;---------------------------------------------------
; TAPE COMMAND. DON'T ECHO INPUT. CONTINUE UNTIL KEY
; COMMAND.
;---------------------------------------------------
;
TAPE:   MVI     A,1     				; set tape input switch
        STA     TAPES   				; store it
        JMP     GETCM   				; go process input
;
;---------------------------------------------------
; END COMMAND. IF TAPE PUNCH SWITCH IS ON, PUNCH 'KEY' THEN
; CONTINUE
;---------------------------------------------------
;
ENDIT:  LDA     TAPES   				; get paper tape switch
        CPI     2       				; test for save
        JNZ     RDY     				; brif not
        LXI     H,KEYL  				; point 'key'
        CALL    TERMM   				; write it
        CALL    HDRTL   				; go put trailer
;
;---------------------------------------------------
; KEY COMMAND. RESET TAPE SWITCH. TURN READER OFF
;---------------------------------------------------
;
KEY:    XRA     A       				; reset tape switch
        STA     TAPES
        LXI     H,PCHOF 				; point reader/punch off
        JMP     RDYM    				; print poff+ready message
;
;---------------------------------------------------
; PUNCH HEADER OR TRAILER ON PAPER TAPE.
;---------------------------------------------------
;
HDRTL:  MVI     B,25    				; load count
HDR1:   MVI     A,0FFH  				; load rubout
        CALL    TESTO   				; write it
        DCR     B       				; decrement count
        XRA     A       				; zero a
        CMP     B       				; test count
        RZ              				; return on zero
        JMP     HDR1    				; continue
;
;---------------------------------------------------
; RUN PROCESSOR, GET NEXT STATMENT, AND EXECUTE IT
; IF IN IMMEDIATE MODE, THEN RETURN TO GETCMMD
;---------------------------------------------------
;
RUNCM:	XRA     A       				; put a zero to a
        LHLD    DATAB   				; get address of data pool
        MOV     M,A     				; initialize to 0
XEQ     EQU     $       				; start for execution with old data
        MVI		A,RTS_ON
		OUT     UARTC
		XRA     A 
		CALL    IRAM    				; initalize start of ram
        LXI     H,BEGPR-1       		; point 1 prior to begin
        SHLD    DATAP   				; restore data statement pointer
        MVI     M,0     				; reset data statement pointer
        INX     H       				; point to start
        SHLD    STMT    				; save it
        JMP     RUN2    				; go process it
;
;---------------------------------------------------
; STATEMENTS RETURN HERE TO CONTINUE PROCESSING
;---------------------------------------------------
;
RUN:    PUSH	B
		MVI		B,2						; Select RUN led
		MVI		C,1						; Set it ON
		CALL	SETLED					; Process
		POP		B
		LXI     H,MULTI 				; point multiple switch
        MOV     A,M     				; get sw
        ORA     A       				; test it
        JZ      RUN1    				; brif not on
        MVI     M,0     				; else, reset it
        LHLD    ENDLI   				; get address
        JMP     RUN3    				; go process remain
RUN1:   LHLD    STMT    				; else, get addr of prev statement
        MOV     E,M     				; get len code
        MVI     D,0     				; clear high byte of addr
        DAD     D       				; incr statement pointer
        SHLD    STMT    				; save it
RUN2:   LDA     RUNSW   				; get run type
        ORA     A       				; test it
        JNZ     GETCM   				; brif immediate mode
        MOV     A,M     				; get len code
        ORA     A       				; test if end
        JZ      ENDIT   				; brif is
        INX     H       				; point line number
        SHLD    LINE    				; save addr
        INX     H       				; point 2nd byte
        INX     H       				; point 1st pgm byte
;
;---------------------------------------------------
; ENTER HERE TO DO IMMEDIATE COMMAND
;---------------------------------------------------

RUN3:	CALL	TSTC					; call to ram address
RUN4:   SHLD    ADDR1   				; save addr
        CALL    TSTCC   				; go see if control-c or o
        LXI     D,JMPTB 				; point to table
        CALL    SEEK1   				; go search command table
        JZ      RUN7    				; brif command not found
        PUSH    H      					; save h,l
        LDAX    D       				; load low byte
        MOV     L,A     				; low byte to l
        INX     D       				; point next
        LDAX    D       				; load high byte
        MOV     H,A     				; high byte to h
        XTHL            				; command address to stack
        RET             				; jump to routine
RUN7:   LHLD    ADDR1   				; restore h,l pointer
		JMP		LET
;
;---------------------------------------------------
; SAVE COMMAND. TURN THE PUNCH ON THEN LIST PROGRAM
;---------------------------------------------------
;
PUNCH:  MVI     A,2     				; set punch mode
        STA     TAPES
        MVI     A,12H   				; get dc2 (=punch on)
        CALL    TESTO   				; write it
        CALL    HDRTL   				; gp put header
;
;
;---------------------------------------------------
; LIST PROCESSOR
; DUMP THE SOURCE PROGRAM TO TTY ( CAN BE IN PROGRAM ) 
;---------------------------------------------------
;
LIST:   CALL	TSTC					; call to ram address
	    LXI     D,0     				; get a zero in d
        XCHG            				; flip to h,l
        SHLD    LINEL   				; save it
        LXI     H,9999H 				; get high number in h,l
        SHLD    LINEH   				; save it
        XCHG            				; flip back
        ORA     A       				; test if eol
        JZ      LIST1   				; brif it is
        CALL    PACK    				; go pack the number, if any
        MOV     D,B     				; copy number to d,l
        MOV     E,C     				; same
        XCHG            				; flip to h,l
        SHLD    LINEL   				; save it
        SHLD    LINEH   				; same
        XCHG            				; restore h,l
		CALL	TSTC					; call to ram address
		CPI     ','     				; test if comma
        JNZ     LIST1   				; brif not
        INX     H       				; point next
		CALL	TSTC					; call to ram address
        CALL    PACK    				; else, go get the number
        MOV     H,B     				; copy to
        MOV     L,C    					; d,l
        SHLD    LINEH   				; save it
LIST1:  LXI     H,BEGPR 				; point beginning of program
LIST2:  CALL    TSTCC   				; go see if control-c or control-o
        MOV     A,M     				; get len code
        ORA     A       				; test if end of program
        JZ      RUN   					; brif end of pgm ( changed from ENDIT )
        SUI     3       				; subtract three
        MOV     B,A     				; save len
        INX     H       				; point high byte of line#
        XCHG            				; flip h,l to d,e
        LHLD    LINEL   				; get low line to test
        XCHG            				; restore h,l
        MOV     A,M     				; get low byte of line number
        CMP     D       				; comp with linel
        JC      LIST8   				; brif less
        JNZ     LIST4   				; brif not equal
        INX     H       				; point next
        MOV     A,M     				; get next byte of line#
        DCX     H       				; point back
        CMP     E       				; comp low bytes
        JC      LIST8   				; brif less
LIST4:  XCHG            				; save h,l in d,e
        LHLD    LINEH   				; get high line for test
        XCHG            				; restore h,l
        MOV     A,M     				; get line byte
        CMP     D       				; compare high bytes
        JZ      LIST5   				; brif equal
        JNC     RUN   					; brif higher ( changed from ENDIT )
        JMP     LIST6   				; go around
LIST5:  INX     H       				; point next
        MOV     A,M     				; get next byte
        DCX     H       				; point back
        CMP     E       				; compare low bytes
        JZ      LIST6   				; brif equal
        JNC     RUN   					; brif higher ( changed from ENDIT )
LIST6:  LXI     D,IOBUF 				; point buffer area
        CALL    LINEO   				; convert line number
LIST7:  MOV     A,M     				; get a byte
        STAX    D       				; put it to buffer
        INX     D       				; point next buff
        INX     H       				; point next prog
        DCR     B       				; decr ctr
        JNZ     LIST7   				; loop
        PUSH    H       				; save hl addr
        CALL    TERMO   				; go type it
        POP     H       				; retrieve h addr
        JMP     LIST2   				; continue
LIST8:  MOV     E,B     				; put len  in e
        MVI     D,0     				; clear d
        DAD     D       				; point next statement
        INX     H       				; point next
        INX     H       				; point len code
        JMP     LIST2   				; go list it
;
;
;---------------------------------------------------
; CONTINUE EXECUTION AT STATEMENT FOLLOWING STOP OR AT
; STATEMENT THAT WAS INTERRUPTED WHEN CONTROL-C WAS TYPED
;---------------------------------------------------
;
CONTI:  LXI     H,LINEN 				; point line number of last stop/error/
        MOV     A,M     				; get 1st char
        ORA     A       				; test if immed cmmd
        JZ      LET     				; brif if immed cmmd
;
;
;---------------------------------------------------
; STATEMENT:  GOTO NNNN
;---------------------------------------------------
;
GOTO:   XRA     A       				; clear reg a
        STA     EDSW    				; reset immed mode (if it was set)
        STA     RUNSW   				; and run type
        CALL    NOTEO   				; error if end-of-line
        CALL    PACK    				; go get line number in b,c
        CALL    EOL     				; error if not end-of-line
GOTO2:  CALL    LOCAT   				; go search for requested line #
        JC      ULERR   				; brif not found
        SHLD    STMT    				; save addr
        XRA     A       				; get a zero
        STA     MULTI   				; turn off multiple statements
        JMP     RUN2    				; go process the statement
;
;---------------------------------------------------
; STATEMENT: RESTORE
;---------------------------------------------------
;
RESTO:  CALL    EOL     				; error if not end-of-line
        LXI     H,BEGPR-1       		; point 1 before start of program
        SHLD    DATAP   				; force next data to be at start
        JMP     RUN     				; go next statement
;
;---------------------------------------------------
; STATEMENT:  RETURN
;---------------------------------------------------
;
RETUR:  CALL    EOL     				; error if not end-of-line
        POP     PSW     				; pop the stack
        CPI     0FFH    				; test if gosub in effect
        JNZ     RTERR   				; brif error
        POP     H       				; get returned statment address
        SHLD    STMT    				; restore
        POP     H       				; get endline value
        SHLD    ENDLI   				; restore
        POP     PSW     				; get multi sw value
        STA     MULTI   				; restore
        JMP     RUN     				; continue (at statement following gosub)
;
;---------------------------------------------------
; STATEMENT:  GOSUB NNNN
;---------------------------------------------------
;
GOSUB:  CALL    NOTEO   				; error if end-of-line
        CALL    PACK    				; get line number
        CALL    EOL     				; error if not end-of-line
GOSU1:  LDA     MULTI   				; get sw setting
        PUSH    PSW     				; save on stack
        LHLD    ENDLI   				; get addr of end of statement
        PUSH    H       				; save one stack
        LHLD    STMT    				; get statement address
        PUSH    H       				; save return address in stack
        MVI     A,0FFH  				; mark as gosub
        PUSH    PSW     				; save status
        JMP     GOTO2  	 				; go lookup line and branch
;
;---------------------------------------------------
; STATEMENT: PRINT ....
;---------------------------------------------------
;
PRINT:  XRA     A       				; clear reg a
PRIN4:  STA     PRSW    				; set sw to say crlf at end of line
        LXI     D,IOBUF 				; point buffer
		CALL	TSTC					; call to ram address
        CALL    TSTEL   				; test if end of statement
        JZ      PRINC   				; brif it is
        CPI     ','     				; test if comma
        JZ      PRIN8   				; brif it is
        CPI     ';'     				; test if semi-colon
        JZ      PRIN9   				; brif it is
        PUSH    D       				; save d,e
        PUSH    H       				; save h,l
        LXI     D,TABLI 				; point literal
		CALL	COMP					; call to ram address
        JZ      PRINA   				; brif is
        POP     H       				; else, restore h,l
        CALL    EXPR    				; go evaluate expression
        POP     D       				; restore d,e
        PUSH    H       				; save h,l
        XCHG            				; flip/flop
        LDA     NS      				; get type of result
        CPI     0E7H    				; test if string
        JZ      PRIN5   				; brif is
        CALL    FOUT    				; go convert output
        INX     H       				; point next
PRIN7:  XCHG            				; flip/flop: end addr to de
        POP     H       				; restore h,l
;
;---------------------------------------------------
; HERE AFTER SETTING UP VALUE TO PRINT IN BUFFER
;---------------------------------------------------
;
PRIN2:  MVI 	A,0FEH      			; set end code=no crlf
        STAX 	D          				; put to buffer
        PUSH 	H          				; save h,l
        CALL 	TERMO      				; go print buffer
        POP 	H           			; restore hl
        JMP 	PRINT       			; repeat for next field
;
PRIN5:  LXI     D,STRIN 				; point string
        LDAX    D       				; get len
        ORA     A       				; test it
        JZ      PRIN7   				; brif null
        MOV     B,A     				; save len
PRIN6:  INX     D       				; point next
        LDAX    D       																																																																																																																																; get a byte
        MOV     M,A     				; store it
        INX     H       				; point next
        DCR     B       				; decr ctr
        JNZ     PRIN6   				; loop
        JMP 	PRIN7       			; diddle de, hl and continue
;
PRIN8:  CALL    TABST   				; go position next tab
PRIN9:  INX     H       				; print next
        MVI     A,1     				; get settting for sw
        JMP     PRIN4   				; go store a in prsw & do next field
PRINA:  POP     D       				; get rid of stack entry
        CALL    EXPR    				; go evaluate
        PUSH    H       				; save h,l
        CALL    FBIN    				; convert to binary
        PUSH    PSW     				; save specified column
        LXI     H,COLUM 				; point current position
        SUB     M       				; subtract (leaves number of fills)
        CM      CRLF    				; next line if already past
        POP     PSW     				; restore col
        SUB     M       				; get number fills
        POP     H
        POP     D
        MOV     B,A     				; save count
        MVI     A,' '   				; get fill
PRINB:  JZ      PRIN2   				; brif count zero
        STAX    D       				; put one space
        INX     D       				; point next
        DCR     B       				; decr ctr
        JMP     PRINB   				; loop
		; 
PRINC:  CALL 	EOL        				; save eol position here to print final cr/lf (or not) and go to next statement
        LDA     PRSW    				; get switch
        MOV     B,A     				; save , switch
        LDA     OUTSW   				; get control-o switch
        ORA     A       				; test if ^o in effect
        ORA     B      					; and if statement ended in , or					; 
        CZ      CRLF    				; crlf if neither
        JMP     RUN     				; continue next statement
;
;---------------------------------------------------
; STATEMENT:  FOR VAR = EXPR TO EXPR [STEP EXPR]
;---------------------------------------------------
;
; first evaluate arguments and store pointers and values,
; but do not make table entry yet
FOR:    CALL    VAR     				; next word must be variable
        XCHG           					; flip/flop
        SHLD    INDX    				; save variable name
        XCHG            				; flip/flop again
        CPI     '='     				; test for equal sign
        JNZ     SNERR   				; brif no equal
        INX     H       				; point next
        CALL    EXPR    				; go evaluate expr, if any
        XCHG            				; flip/flop again
        LHLD    INDX    				; get index name
        XCHG           					; flip/flop
        PUSH    H       				; save h,l
        CALL    SEARC   				; go locate name
        XCHG            				; put addr in h,l
        SHLD    ADDR1   				; save addr
		CALL	SFPA					; go store the value
        POP     H       				; restore pointer to statement
        LXI     D,TOLIT 				; get lit addr
		CALL	COMP					; go compare
        JNZ     SNERR   				; brif error
        CALL    EXPR    				; go evaluate to-expr
        PUSH    H       				; save h,l
        LXI     H,TVAR1 				; point 'to' value
		CALL	SFPA					; go store the value
        LXI     H,ONE   				; point constant: 1
		CALL	LDFPA					; load it
        POP     H       				; get h,l
        MOV     A,M     				; get the char
        ORA     A       				; test for end of statement
        JZ      FOR2    				; brif no step
        PUSH    H       				; re-save
        LXI     D,STEPL 				; test for lit 'step'
		CALL	COMP					; go compare
        JZ      FOR1    				; brif step
        POP     H       				; restore h,l
        JMP     FOR2    				; go no step value
FOR1:   POP     D       				; pop off the stack
        CALL    EXPR    				; go evaluate expression
FOR2:   PUSH    H       				; save h,l to end of statement
        LXI     H,TVAR2 				; point step value
		CALL	SFPA					; go store the value
		POP     H       				; restore h,l
        CALL    EOL     				; error if not end-of-line
;---------------------------------------------------		
; DETERMINE WHETHER LOOP IS TO BE EXECUTED AT ALL
; (IF VALUE > "TO" VALUE AND STEP POSITIVE,
;   JUST SKIP TO NEXT, ETC)
;---------------------------------------------------
        CALL    FTEST   				; get status of facc
        PUSH    PSW     				; save a,status
        LXI     H,TVAR1 				; get end value
		CALL	LDFPA					; load it
	    POP     PSW     				; restore status
        JP      FOR4    				; brif for is positive
        LHLD    ADDR1   				; get address of index
        CALL    FSUB    				; compare this against end value
        JZ      FOR5    				; brif start = end
        JM      FOR5    				; brif start > end
        JMP     FOR9    				; go locate matching next
FOR4:   LHLD    ADDR1   				; get address of index
        CALL    FSUB    				; compare
        JZ      FOR5    				; brif start = end
        JM      FOR9    				; brif start > end: skip to "next"
;---------------------------------------------------
; LOOP IS TO BE EXECUTED AT LEAST ONCE:
; NEED AN ENTRY IN FOR-NEXT TABLE.
; SEE IF THERE IS ALREADY ENTRY FOR THIS VARIABLE
; (IE PROGRAM JUMPED OUT OF LOOP EARLIER)
;---------------------------------------------------
FOR5:   LXI     D,FORNE 				; point table
        LHLD    INDX    				; get index variable name
        XCHG            				; flip/flop
        MOV     A,M     				; get count of entries now in table
        MOV     B,A     				; store it
        MVI     C,1     				; new ctr
        ORA     A       				; test if zero
        INX     H       				; point
        JZ      FOR8    				; brif table empty
FOR6:   MOV     A,M     				; get 1st byte of table variable
        CMP     D       				; test if equal to this for's index
        JNZ     FOR7    				; brif not
        INX     H       				; point next
        MOV     A,M     				; get next byte
        DCX     H       				; point back
        CMP     E       				; test if equal
        JZ      FOR8    				; brif equal
FOR7:	CALL	INCPC					; adjust h,l
	    DB      14
        INR     C       				; count it
        DCR     B       				; decr ctr
        JNZ     FOR6   					; loop
;---------------------------------------------------
; ENTER THIS FOR IN TABLE (WHERE HL POINTS)
;---------------------------------------------------
FOR8:   MOV     A,C     				; get udpate count
        CPI     9      				 	; test if tbl exceeded
        JNC     NXERR   				; error if more than 8 open for/next
        STA     FORNE   				; put in table
        MOV     M,D     				; hi byte index variable name
        INX     H       				; point next
        MOV     M,E     				; store lo byte
        INX     H       				; point next
        PUSH    H       				; save h,l
        LXI     H,TVAR2 				; point step value
		CALL	LDFPA					; load it
        POP     H       				; restore h,l
		CALL	SFPA					; go store the value
        PUSH    H       				; save h,l
        LXI     H,TVAR1 				; point 'to' value
		CALL	LDFPA					; load it
        POP     H       				; restore h,l
		CALL	SFPA					; go store the value
        XCHG            				; flip/flop
        LHLD    ENDLI   				; get end addr
        DCX     H       				; point one prior
        XCHG            				; flip back
        MOV     M,D     				; store it
        INX     H      					; point next
        MOV     M,E     				; store it
        INX     H       				; point next
        LDA     STMT+1  				; get high statement addr
        MOV     M,A     				; put it
        INX     H       				; point next
        LDA     STMT    				; get low statement addr
        MOV     M,A     				; put it
        JMP     RUN     				; continue
;
;---------------------------------------------------
; IF HERE, THIS LOOP IS TO BE EXECUTED ZERO TIMES:
; SCAN THRU PROGRAM TO FIND MATCHING "NEXT".
; THIS CODE WILL FAIL IF USER'S PROGRAM IS TOO
; COMPLEX SINCE IT WON'T FOLLOW GOTO'S, IF'S, ETC.
;---------------------------------------------------
;
FOR9:   LHLD    STMT    				; get address of statment
        MOV     E,M    					; get length code
        MVI     D,0    					; init increment
        DAD     D      					; compute addr of next statement
        MOV     A,M    					; get new len code
        ORA     A      					; see if end of pgm
        JZ      NXERR  					; brif it is
        SHLD    STMT   					; save address
		CALL	INCPC					; adjust h,l
	    DB      3
		CALL	TSTC					; call to ram address
		LXI     D,NEXTL					; point 'next'
		CALL	COMP					; go compare
		JNZ     FOR9   					; loop if not
		CALL	TSTC					; call to ram address
		LDA     INDX+1 					; get first char
        CMP     M      					; compare
        JNZ     FOR9   					; brif not match next
        LDA     INDX   					; get 2nd char
        INX     H      					; ditto
        CPI     ' '    					; see if single char
        JZ      FORA   					; brif it is
        CMP     M      					; compare the two
        JNZ     FOR9   					; brif not equal
FORA:	CALL	TSTC					; call to ram address
        MOV     A,M    					; get the non blank
        ORA     A      					; see if end
        JNZ     FOR9   					; brif end
        JMP     RUN    					; else, go next statement
;
;---------------------------------------------------
; STATEMENT: IF EXPR RELATION EXPR THEN STATEMENT#
;---------------------------------------------------
;
IFSTM:  CALL    EXPR   					; go evaluate left expr
        PUSH    H      					; save h,l
        LDA     NS     					; get type code
        STA     IFTYP  					; save it
        CPI     0E7H   					; test if string
        JNZ     IF1    					; brif not
        LXI     H,IOBUF					; point buffer
        LXI     D,STRIN					; point result
        LDAX    D      					; get len
        INR     A      					; plus one
        MOV     B,A    					; save it
        CALL    COPYD  					; go move it
        JMP     IF2    					; go around
IF1:    LXI     H,TVAR1					; get addr of temp storage
		CALL	SFPA					; go store the value
IF2:    POP     H      					; restore h,l
        XRA     A      					; clear a
        MOV     C,A    					; save in reg c
        MOV     B,A    					; init reg
IF3:    MOV     A,M    					; get operator
        INR     B      					; count
        CPI     '='    					; test for equal
        JNZ     IF4    					; brif it is not
        INR     C      					; add 1 to c
        INX     H      					; point next
IF4:    CPI     '>'    					; test for greater than
        JNZ     IF5    					; brif it is not
        INR     C      					; add two
        INR     C      					; to rel code
        INX     H      					; point next
IF5:    CPI     '<'    					; test for less than
        JNZ     IF6    					; brif it is not
        MOV     A,C    					; get rel code
        ADI     4      					; plus four
        MOV     C,A    					; put back
        INX     H      					; point next
IF6:    MOV     A,C    					; get rel code
        ORA     A      					; test it
        PUSH    B      					; save b,c
        JZ      SNERR  					; brif some error
        POP     B      					; restore b,c
        STA     REL    					; save code
        MOV     A,B    					; get count
        CPI     2      					; test for two
        JNZ     IF3    					; see if multiple relation
        CALL    EXPR   					; go evaluate right side
        SHLD    ADDR1  					; save location of then (if any)
        LDA     NS     					; get type code
        LXI     H,IFTYP					; point left type
        CMP     M      					; compare
        JNZ     SNERR  					; brif mixed
        CPI     0E7H   					; test if string
        JZ      IFF    					; brif is
        LXI     H,TVAR1					; point left
        CALL    FSUB   					; subtract left from right
        LDA     REL    					; get relation
        RAR            					; test bit d0
        JNC     IF8    					; brif no equal test
        CALL    FTEST  					; get status of facc
        JZ      TRU    					; brif left=right
IF8:    LDA     REL    					; load relation
        ANI     02H    					; mask it
        JZ      IF9    					; brif no >
        CALL    FTEST  					; get status of facc
        JM      TRU    					; brif gt
IF9:    LDA     REL    					; load relation
        ANI     04H    					; mask it
        JZ      FALS   					; brif no <
        CALL    FTEST  					; get status of facc
        JM      FALS   					; brif gt
        JZ      FALS   					; brif zero (not equal)
TRU:    LHLD    ADDR1  					; get pointer to statement
        LXI     D,GOTOL					; point 'go to'
		CALL	COMP					; go compare
        JZ      GOTO   					; brif if ... goto nn
        LHLD    ADDR1  					; get pointer to statement
        LXI     D,GOSBL					; point literal
		CALL	COMP					; go compare
        JZ      GOSUB  					; brif if ... gosub nn
        LHLD    ADDR1  					; get pointer to statement
        LXI     D,THENL					; get addr 'then'
		CALL	COMP					; go compare
	    JNZ     SNERR  					; brif error
        CALL    NUMER  					; test if numeric
        JZ      GOTO   					; brif it is
        JMP     RUN4   					; else, may be any statement
FALS    EQU     RUN
IFF:    LXI     H,IOBUF					; point prior
        MOV     B,M    					; get len
        LXI     D,STRIN					; point this
        LDAX    D      					; get len
        MOV     C,A    					; save it
IFG:    INX     D      					; point next
        INX     H      					; ditto
        MOV     A,B    					; get left len
        ORA     A      					; test it
        JNZ     IFH    					; brif not zero
        MVI     M,' '  					; extend with space
IFH:    MOV     A,C    					; get right len
        ORA     A      					; test it
        JNZ     IFI    					; brif not zero
        MVI     A,' '  					; get space
        STAX    D      					; extend
IFI:    LDAX    D      					; get right char
        CMP     M      					; test with left
        JC      IFM    					; brif left>right
        JNZ     IFN    					; brif left<right
        MOV     A,B    					; get left count
        DCR     A      					; subt one
        JM      IFJ    					; brif was zero
        MOV     B,A    					; update ctr
IFJ:    MOV     A,C    					; get right len
        DCR     A      					; subt one
        JM      IFK    					; brif was zero
        MOV     C,A    					; updt ctr
IFK:    MOV     A,B    					; get left len
        ORA     C      					; compare to right
        JNZ     IFG    					; brif both not zero
        MVI     B,1    					; set sw= equal
IFL:    LDA     REL    					; get relation
        ANA     B      					; and with result
        JZ      FALS   					; brif false
        JMP     TRU    					; else, true
IFM:    MVI     B,2    					; set code
        JMP     IFL    					; jump
IFN:    MVI     B,4    					; set code
        JMP     IFL    					; jump
;
;---------------------------------------------------
; STATEMENT: [LET] VAR = EXPR
;---------------------------------------------------
;
LET:    CALL    GETS8  					; go get address of variable
        PUSH    B      					; save name
        PUSH    D      					; save address
		CALL	TSTC					; get next non-blank char
	    CPI     '='    					; test for equal sign
        JZ      LET1   					; brif is
        LDA     EDSW   					; get mode sw
        ORA     A      					; test it
        JZ      SNERR  					; brif let error
        LXI     H,WHATL					; point literal
        CALL    TERMM  					; go print it
        JMP     GETCM  					; go to command
LET1:   INX     H      					; point next
        CALL    EXPR   					; go evaluate expression
        CALL    EOL    					; error if not end-of-line
        POP     H      					; restore addresss
        POP     D      					; restore name
        MOV     A,E    					; get type
        ORA     A      					; test it
        LDA     NS     					; get result type
        JM      LET2   					; brif string
        CPI     0E3H   					; test if numeric
        JNZ     SNERR  					; brif mixed mode
		CALL	SFPA					; go store the value
	    JMP     RUN    					; continue
LET2:   CPI     0E7H   					; test if string
        JNZ     SNERR  					; brif mixed mode
        CALL    LET2A  					; go store it
        JMP     RUN    					; continue
;
LET2A:  LXI     D,STRIN					; point string buffer
        LDAX    D      					; get new len
        SUB     M      					; minus old len
        JZ      LET8   					; brif same length
        MOV     D,H    					; copy h,l
        MOV     E,L    					; to d,e
        MOV     A,M    					; get len
        INR     A      					; true len
LET3:   INX     D      					; point next
        DCR     A      					; decr ctr
        JNZ     LET3   					; loop
        INX     D      					; skip
        INX     D      					; again
        LDAX    D      					; get lo nam
        MOV     C,A    					; save
        INX     D      					; get hi name
        LDAX    D      					; load it
        MOV     B,A    					; save
        PUSH    B      					; save name
        DCX     H      					; point next entry
LET4:   MOV     A,M    					; get next
        ORA     A      					; test if end
        JZ      LET6   					; brif is
        PUSH    H      					; save h,l
        DCX     H      					; skip next
        DCX     H      					; point len
        MOV     B,M    					; get hi len
        DCX     H      					; point lo
        MOV     C,M    					; get lo len
        POP     H      					; restore h,l
LET5:   MOV     A,M    					; get a byte
        STAX    D      					; copy
        DCX     H      					; point next
        DCX     D      					; ditto
        INX     B      					; add to ctr
        MOV     A,B    					; get hi
        ORA     C      					; test if zero
        JNZ     LET5   					; loop
        JMP     LET4   					; continue
LET6:   XCHG           					; put new addr to h,l
        POP     B      					; get name
        MOV     M,B    					; store hi byte
        DCX     H      					; point next
        MOV     M,C    					; store lo
        LXI     D,STRIN					; get new len
        LDAX    D      					; load it
        MVI     B,0FFH 					; init hi complement
        ADI     5      					; compute entry length
        JZ      LET7   					; brif 256 bytes
        JNC     LET7   					; brif less 256
        MVI     B,0FEH 					; set bit off
LET7:   CMA            					; 1's complement
        INR     A      					; then 2's
        MOV     C,A    					; save lo len
        DCX     H      					; point next
        MOV     M,B    					; store hi len
        DCX     H      					; point next
        MOV     M,C    					; store lo len
		CALL	INCPC					; adjust h,l
	    DB      3
        DAD     B      					; compute end of entry
        MVI     M,0    					; mark new end
        INX     H      					; point 1st byte
LET8:   LDAX    D      					; get len
        INR     A      					; true len
        MOV     B,A    					; save len
LET9:   LDAX    D      					; get a byte
        MOV     M,A    					; copy it
        INX     H      					; point next
        INX     D      					; ditto
        DCR     B      					; subt ctr
        JNZ     LET9   					; loop
        RET            					; return
;
;---------------------------------------------------
; STATEMENT:  NEXT VAR
;---------------------------------------------------
;
NEXT:   CALL    VAR    					; get variable name
        CALL    EOL    					; error if not end-of-lne
        XCHG           					; flip/flop
        SHLD    INDX   					; save var name
        PUSH    H      					; save var name
        LXI     H,FORNE					; point for/next table
        MOV     B,M    					; get size
        MOV     A,B    					; load it
        ORA     A      					; test it
        JZ      NXERR  					; brif table empty
        INX     H      					; point next
        POP     D      					; restore var name
NEXT1:  MOV     A,M    					; get 1st byte
        INX     H      					; point next
        CMP     D      					; compare
        JNZ     NEXT2  					; brif not equal
        MOV     A,M    					; get 2nd byte
        CMP     E      					; compare
        JZ      NEXT3  					; brif equal
NEXT2:	CALL	INCPC					; adjust h,l
	    DB      13
        DCR     B      					; decr count
        JNZ     NEXT1  					; loop
        JMP     NXERR  					; go put error msg
NEXT3:  LDA     FORNE  					; get orig count
        SUB     B      					; minus remain
        INR     A      					; plus one
        STA     FORNE  					; store new count
        INX     H      					; point addr
        PUSH    H      					; save h,l addr
        CALL    SEARC  					; go get addr of index
        XCHG           					; put to h,l
        SHLD    ADDR1  					; savr it
		CALL	LDFPA					; load it
	    POP     H      					; get h,l (tbl)
        PUSH    H      					; re-save
        CALL    FADD   					; add step value
        LXI     H,TVAR1					; point temp area
		CALL	SFPA					; go store the value
        POP     H      					; get h,l (tbl)
        PUSH    H      					; re-save
		CALL	INCPC					; adjust h,l
        DB      4
        CALL    FSUB   					; subtract to value
        JZ      NEXT6  					; brif zero
        POP     H      					; get h,l (ptr to step)
        PUSH    H      					; re-save
        MOV     A,M    					; get sign&exponent of step
        ORA     A      					; test it
        LDA     FACC   					; get sign & expon of diff
        JM      NEXT5  					; brif negative
        ORA     A      					; test sign of diff
        JM      NEXT6  					; brif less than to-expr
NEXT7:  LXI     H,FORNE					; get addr table
        DCR     M      					; subtract one from count
        POP     D      					; adjust stack
        JMP     RUN    					; go statement after next
NEXT5:  ORA     A      					; test sign of difference
        JM      NEXT7  					; brif end of loop
NEXT6:  POP     H      					; get ptr to tbl
		CALL	INCPC					; adjust h,l
        DB      8
        MOV     D,M    					; get hi byte
        INX     H      					; point next
        MOV     E,M    					; get low byte
        INX     H      					; point next
        MOV     A,M    					; get hi byte
        STA     STMT+1 					; save
        INX     H      					; point next
        MOV     A,M    					; get low byte
        STA     STMT   					; save
        XCHG           					; h,l = addr of statement aftr for
        CALL    EOL    					; setup multi ptp
        LHLD    STMT   					; get addr of for statement
        INX     H      					; point line num
        SHLD    LINE   					; save addr line
        LXI     H,TVAR1					; point updted value
		CALL	LDFPA					; load it
	    LHLD    ADDR1  					; get addr of index
		CALL	SFPA					; go store the value
	    JMP     RUN    					; continue with statement after for
;
;---------------------------------------------------
; STATEMENT:  INPUT VAR [, VAR, VAR]
;---------------------------------------------------
;
;
INPUT:  LXI     D,LLINE					; point 'line'
        PUSH    H      					; save h,l addr
		CALL	COMP					; go compare
        JZ      INPL   					; brif equal
        POP     D      					; else, restore h,l addr
        LXI     H,IOBUF					; get addr of buffer
        SHLD    ADDR1  					; save addr
        MVI     M,0    					; mark buffer empty
        XCHG           					; flip/back
INPU1:	CALL	TSTC					; call to ram address
        CPI     27H    					; test if quote
        JZ      INPU2  					; brif is
        CPI     '"'    					; test if input literal
        JNZ     INPU6  					; brif not
INPU2:  MOV     C,A    					; save delim
        LXI     D,IOBUF					; point buffer
INPU3:  INX     H      					; point next
        MOV     A,M    					; load it
        CMP     C      					; test if end
        JZ      INPU4  					; brif is
        STAX    D      					; put to buff
        INX     D      					; point next
        JMP     INPU3  					; loop
INPU4:  INX     H      					; skip trailing quote
        XCHG           					; put addr to h,l
        MVI     M,0FEH 					; mark end
        CALL    TERMO  					; go print prompt
        XCHG           					; get h,l
		CALL	TSTC					; call to ram address
        CPI     ','    					; test if comma
        JZ      INPU5  					; brif is
        CPI     ';'    					; test if comma
        JNZ     INPU6  					; brif not
INPU5:  INX     H      					; skip it
INPU6:  CALL    GETS8  					; go get var addr
        PUSH    H      					; save h addr
        PUSH    D      					; save var addr
        LHLD    ADDR1  					; get addr prev buffer
        MOV     A,M    					; load char
        CPI     ','    					; test if comma
        INX     H      					; point next
        JZ      INPU7  					; brif continue from prev
        MVI     A,'?'  					; load prompt
        CALL    TERMI  					; go read from tty
INPU7:	CALL	TSTC					; call to ram address
        MOV     A,C    					; get lo name
        ORA     A      					; test it
        JM      INPUA  					; brif string
        CALL    FIN    					; go convert to floating
		CALL	TSTC					; call to ram address
        CPI     ','    					; test if comma
        JZ      INPU8  					; brif is
        ORA     A      					; test if end of line
        JNZ     CVERR  					; brif error
INPU8:  SHLD    ADDR1  					; save address
        POP     H      					; get var addr
		CALL	SFPA					; go store the value
INPU9:  POP     H      					; restore statement pointer
        MOV     A,M    					; get char
        CPI     ','    					; test for comma
        INX     H      					; point next
        JZ      INPU1  					; recdursive if comma
        DCX     H      					; point back
INPUB:  CALL    EOL    					; error if not end of line
        JMP     RUN    					; continue next statement
INPUA:  CALL    GETST  					; go get the string
        SHLD    ADDR1  					; save address
        JMP     INPU9  					; continue
;
;---------------------------------------------------
; STATEMENT: INPUT LINE A$
;---------------------------------------------------
;
INPL:   POP     D      					; dummy pop to adjust stack
        CALL    VAR    					; get string name
        MOV     A,E    					; load lo byte
        ORA     A      					; test it
        JP      SNERR  					; brif not string variable
        CALL    SEARC  					; else, get address
        PUSH    D      					; save on stack
        CALL    EOL    					; error if not end-of-line
        MVI     A,1    					; get on setting
        STA     ILSW   					; set input line switch
        MVI     A,'?'  					; load prompt
        CALL    TERMI  					; go read a line
        MVI     B,0    					; init count
        LXI     D,STRIN+1      			; point string buffer
        LXI     H,IOBUF+1      			; point input buffer
INPL1:  MOV     A,M    					; get next byte
        ORA     A      					; test it
        JZ      INPL2  					; brif end
        INR     B      					; add to count
        STAX    D      					; put to string buff
        INX     D      					; point next
        INX     H      					; ditto
        JMP     INPL1  					; loop
INPL2:  STA     ILSW   					; reset switch
        MOV     A,B    					; get count
        STA     STRIN  					; set string length
        POP     H      					; get address of variable
        CALL    LET2A  					; go store the string
        JMP     RUN    					; go next statement
;
;
;---------------------------------------------------
; STATEMENT: READ VAR [,VAR ...]
;---------------------------------------------------
;
READ:	CALL	TSTC					; call to ram address
        CALL    GETS8  					; get var addr
        PUSH    H      					; save h,l
        PUSH    D      					; save d,e
        LHLD    DATAP  					; get data statement pointer
        MOV     A,M    					; load the char
        ORA     A      					; test if end of statement
        JNZ     READ2  					; brif not end of statement
        INX     H      					; point start next statement
READ1:  MOV     A,M    					; load len
        SHLD    DATAP  					; save addr
        ORA     A      					; test if end of pgm
        JZ      DAERR  					; brif out of data
		CALL	INCPC					; adjust h,l
        DB      3
        LXI     D,DATAL					; point 'data'
		CALL	COMP					; go compare
        JZ      READ2  					; brif it is data statement
        LHLD    DATAP  					; get addr start
        MOV     E,M    					; get len code
        MVI     D,0    					; clear d
        DAD     D      					; point next statement
        JMP     READ1  					; loop next statement
READ2:	CALL	TSTC					; call to ram address
        MOV     A,C    					; load lo name
        ORA     A      					; test it
        JM      READ6  					; brif string
        CALL    FIN    					; go convert value
        MOV     A,M    					; get char which stopped us
        CPI     ','    					; test if comma
        JNZ     READ5  					; brif not
        INX     H      					; point next
READ3:  SHLD    DATAP  					; save address
        POP     H      					; restore addr of var
		CALL	SFPA					; go store the value
READ4:  POP     H      					; restore pointer to stm
        MOV     A,M    					; get the char
        CPI     ','    					; test if comma
        INX     H      					; point next
        JZ      READ   					; recursive if it is
        DCX     H      					; reset
        JMP     INPUB  					; continue
READ5:  ORA     A      					; test if end of statement
        JZ      READ3  					; brif ok
        JMP     CVERR  					; go process error
READ6:  CALL    GETST  					; go get string
        MOV     A,M    					; get char
        CPI     ','    					; test if comma
        JZ      READ7  					; brif is
        ORA     A      					; test if end
        JNZ     READ5  					; brif not
        JMP     READ8  					; go around
READ7:  INX     H      					; point past
READ8:  SHLD    DATAP  					; save address
        JMP     READ4  					; continue
;
;---------------------------------------------------
; STATEMENT; OUT ADDR,VALUE
;---------------------------------------------------
;
OUTP:   CALL    EXPR   					; go evaluate address
        MOV     A,M    					; get delim
        CPI     ','    					; test if comma
        JNZ     SNERR  					; brif not
        INX     H      					; skip over comma
        CALL    FBIN   					; convert to binary in a-reg
        LXI     D,OUTA 					; point instr
        XCHG           					; put to h,l
        MVI     M,0D3H 					; out instr
        INX     H      					; point next
        MOV     M,A    					; put addr
        INX     H      					; point next
        MVI     M,0C9H 					; ret instr
        XCHG           					; restore orig h,l
        CALL    EXPR   					; go eval data byte
        CALL    EOL    					; error if not end of statement
        CALL    FBIN   					; convert to binary
        CALL    OUTA   					; go put the byte
        JMP     RUN    					; go next statement
;
;---------------------------------------------------
; STATEMENT: STOP
;---------------------------------------------------
;
STOP:   CALL    EOL    					; point end of line
        LXI     H,STOPM					; point message: "stop at line "
        CALL    TERMM  					; go write it
        CALL    PRLIN  					; go print line number
        LDA     RUNSW  					; get run type
        ORA     A      					; test it
        JNZ     RDY    					; brif immed
        STA     MULTI  					; clear multi sw
        LHLD    STMT   					; get addr of prev statement
        MOV     E,M    					; get len
        MVI     D,0    					; clear hi byte
        DAD     D      					; point next
        INX     H      					; point line number
        SHLD    LINE   					; save addr
        LXI     D,LINEN					; point area
        CALL    LINEO  					; go convert line number
        XCHG           					; flip to h,l
        MVI     M,0    					; mark end
        JMP     RDY    					; go to ready msg
;
;---------------------------------------------------
; STATEMENT: RANDOMIZE
;---------------------------------------------------
;
RANDO:  CALL    EOL    					; error if not end-of-line
        MVI     A,1    					; load a one
        STA     RNDSW  					; set switch = true random
        LXI     D,TRNDX					; point 'true' random numbers
        LXI     H,RNDX 					; point receive
        MVI     B,8    					; loop ctr
        CALL    COPYD  					; go move it
        JMP     RUN    					; continue
;
;---------------------------------------------------
; STATEMENT: ON EXPR GOTO NNN NNNN NNNN
;              GOSUB
;---------------------------------------------------
;
ON:     CALL    EXPR   					; go evaluate expression
        CALL    FBIN   					; get binary number in acc
        ORA     A      					; test result
        JZ      SNERR  					; brif zero (error)
        MOV     C,A    					; save value
        DCR     C      					; less one
        XRA     A      					; get a zero
        STA     REL    					; turn off switch
        LXI     D,GOTOL					; point literal
        PUSH    H      					; save h,l address
		CALL	COMP					; go compare
        JZ      ON3    					; brif on...goto
        POP     H      					; else, restore h,l
        LXI     D,GOSBL					; point literal
		CALL	COMP					; go compare
        JNZ     SNERR  					; brif error
        MVI     A,1    					; get on setting
        STA     REL    					; set switch
        PUSH    H      					; dummy push
ON3:    POP     D      					; adjust stack
ON3A:   MOV     A,C    					; get count
        ORA     A      					; test it
        JZ      ON6    					; brif value 1
		CALL	TSTC					; call to ram address
        ORA     A      					; test if end of line
        JZ      SNERR  					; brif is
        CPI     ','    					; test is comma
        JNZ     ON4    					; brif not
        INX     H      					; skip comma
        JMP     ON3A   					; continue
ON4:    CALL    NUMER  					; go test if numeric
        JNZ     ON5    					; brif not
        INX     H      					; point next
        JMP     ON4    					; loop
ON5:    DCR     C      					; sub one from count
        JNZ     ON3A   					; loop till just before statement#
ON6:    CALL    NOTEO  					; error if not end-of-line
        CPI     ','    					; test if comma
        JNZ     ON7    					; brif not
        INX     H      					; point next
        JMP     ON6    					; loop
ON7:    CALL    NUMER  					; test if numeric
        JNZ     SNERR  					; brif not
        CALL    PACK   					; get the line number
ON8:    MOV     A,M    					; get next char
        CALL    TSTEL  					; test if end statement
        JZ      ON9    					; brif end
        INX     H      					; point next
        JMP     ON8    					; loop
ON9:    CALL    EOL    					; set end of line pointers
        LDA     REL    					; get type (goto or gosub)
        ORA     A      					; test it
        JNZ     GOSU1  					; brif gosub
        JMP     GOTO2  					; br to goto lookup
;
;---------------------------------------------------
; STATEMENT: CHANGE A$ TO X     - OR -
;           CHANGE X TO A$
;---------------------------------------------------
;
CHANG:  CALL    VAR    					; next word must be var
        MOV     A,E    					; test type
        ORA     A      					; set flags
        JP      CHA2   					; brif not-string
        CALL    SEARC  					; get addr
        PUSH    D      					; save it
        LXI     D,TOLIT					; point 'to'
		CALL	COMP					; go compare
        JNZ     SNERR  					; brif error
        CALL    VAR    					; get next variable
        MOV     A,D    					; get hi name
        ORI     80H    					; set mask for array
        MOV     D,A    					; replace
        CALL    SEARC  					; get address
		CALL	INCPC					; adjust h,l
        DB      -11
        POP     D      					; get ptr to statement
        XCHG           					; flip
        CALL    EOL    					; next must be e-o-l
        XCHG           					; flip again
        POP     D      					; get addr string
        LDAX    D      					; get count
        MOV     B,A    					; save it
        INR     B      					; bump
CHA1:   PUSH    B      					; save ctr
        PUSH    D      					; save addr string
        PUSH    H      					; save addr num
        CALL    FDEC   					; convert to f.p.
        POP     H      					; get addr
		CALL	SFPA					; go store the value
		CALL	INCPC					; adjust h,l
        DB      -8
        POP     D      					; restore string
        POP     B      					; and ctr
        INX     D      					; point next char
        LDAX    D      					; load it
        DCR     B      					; decr ctr
        JNZ     CHA1   					; loop
        JMP     RUN
;
;
CHA2:   MOV     A,D    					; get hi name
        ORI     80H    					; make array name
        MOV     D,A    					; save
        CALL    SEARC  					; get addr
		CALL	INCPC					; adjust h,l
        DB      -11
        XTHL           					; save on stack
        LXI     D,TOLIT					; point 'to'
		CALL	COMP					; go compare
        JNZ     SNERR  					; brif error
        CALL    VAR    					; get name
        MOV     A,E    					; get type
        ORA     A      					; set flags
        JP      SNERR  					; brif not string
        CALL    EOL    					; brif not e-o-l
        CALL    SEARC  					; get addr
        POP     H      					; get addr var
        PUSH    D      					; save d,e
        LXI     D,STRIN					; point string buffer
        PUSH    D      					; save it
		CALL	LDFPA					; load it
		CALL	INCPC					; adjust h,l
        DB      -8
        PUSH    H      					; save h,l
        CALL    FBIN   					; convert
        POP     H      					; restore
        POP     D      					; ditto
        MOV     B,A    					; save count
        INR     B      					; bump it
CHA3:   STAX    D      					; put to string
        INX     D      					; point next str loc.
        PUSH    B      					; save ctrs
        PUSH    D      					; and ad^dr
		CALL	LDFPA					; load it
		CALL	INCPC					; adjust h,l
        DB      -8
        PUSH    H      					; and h addr
        CALL    FBIN   					; convert
        POP     H      					; restore h,l
        POP     D      					; and d,e
        POP     B      					; and ctrs
        DCR     B      					; decr ctr
        JNZ     CHA3   					; loop
        POP     H      					; get addr of var (string)
        CALL    LET2A  					; go store it
        JMP     RUN    					; continue

;---------------------------------------------------
; STATEMENT: DIM VAR(A,B),...
;---------------------------------------------------
;
DIM:    CALL    VAR    					; go get var name
        JP      SNERR  					; brif no (
        CALL    SEARC  					; go locate the var
        XTHL           					; put addr in stack, get ptr to (
        PUSH    PSW    					; save status
        MVI     A,0FFH 					; turn on sw
        STA     DIMSW  					; set it
        CALL    EXPR   					; go evaluate
        POP     PSW    					; get status
        XTHL           					; swap ptrs
        PUSH    D      					; save row number
        PUSH    B      					; save col number
        INX     B      					; increment columns
        INX     D      					; and rows
        PUSH    H      					; save h,l
        PUSH    PSW    					; resave status
        LXI     H,0    					; get a zero
DIM1:   DAD     D      					; times one
        DCX     B      					; dcr cols
        MOV     A,B    					; get hi
        ORA     C      					; plus lo
        JNZ     DIM1   					; loop
        POP     PSW    					; get status
        POP     D      					; get address
        DAD     H      					; times two
        DAD     H      					; times four
        LXI     B,8    					; plus 2 (name and disp)
        JM      REDIM  					; go re-dimension
        PUSH    H      					; save product
        DAD     B      					; add it
        XCHG           					; flip/flop
        DCX     H      					; point lo name
        DCX     H      					; point hi disp
        MOV     A,E    					; get lo
        CMA            					; complement
        ADI     1      					; plus one
        MOV     E,A    					; restore
        MOV     A,D    					; get hi
        CMA            					; complement
        ACI     0      					; plus carry
        MOV     M,A    					; store it
        DCX     H      					; point next
        MOV     M,E    					; store lo
        XCHG           					; save in d,e
        POP     H      					; get product
        MOV     B,H    					; copy h,l
        MOV     C,L    					; to b,c
        XCHG           					; get locat
        POP     D      					; get columns
        DCX     H      					; point next
        MOV     M,D    					; move lo col
        DCX     H      					; point next
        MOV     M,E    					; move hi col
        POP     D      					; get rows
        DCX     H      					; point next
        MOV     M,D    					; move hi row
        DCX     H      					; point next
        MOV     M,E    					; move lo row
        DCX     H      					; point next
DIM2:   MVI     M,0    					; clear one byte
        DCX     H      					; point next
        DCX     B      					; decr ctr
        MOV     A,B    					; get hi
        ORA     C      					; plus lo
        JNZ     DIM2   					; loop
        MVI     M,0    					; mark end
DIM3:   POP     H      					; get ptr to statement
        MOV     A,M    					; load char
        CPI     ','    					; test if comma
        JNZ     DIM4   					; brif not
        INX     H      					; skip it
        JMP     DIM    					; continue
DIM4:   CALL    EOL    					; test end of line
        JMP     RUN    					; continue with program
REDIM:  DAD     B      					; compute len to next
        DCX     D      					; point lo name
        DCX     D      					; point hi disp
        LDAX    D      					; get it
        MOV     B,A    					; save
        DCX     D      					; point lo disp
        LDAX    D      					; get it
        MOV     C,A    					; save
        DAD     B      					; compute diff or prior dim and this
        MOV     A,H    					; get hi diff
        ORA     A      					; test it
        JM      REDM1  					; brif prev > new
        JNZ     SNERR  					; brif prev < new
        MOV     A,L    					; get lo diff
        ORA     A      					; test it
        JNZ     SNERR  					; brif prev < new
REDM1:  XCHG           					; put addr in h,l
        DCX     H      					; point hi col
        POP     D      					; get col
        MOV     M,D    					; move hi
        DCX     H      					; point lo col
        MOV     M,E    					; move lo
        POP     D      					; get row
        DCX     H      					; point hi row
        MOV     M,D    					; move hi
        DCX     H      					; point lo row
        MOV     M,E    					; move lo
        JMP     DIM3   					; continue
;
;---------------------------------------------------
; COMPUTE SINE OF X, (X IN RADIANS)
;
; USES 4TH DEGREE POLYNOMIAL APPROXIMATION
;
; FIRST, REDUCE ANGLE TO RANGE: (-PI/2,PI/2)
;---------------------------------------------------
;
SIN:    CALL    FTEST  					; get status of angle
        RZ             					; sin(0)=0
        PUSH    PSW    					; save sign of angle
        CALL    ABS
SIN1:   POP     PSW    					; complement sign for each pi sub'd
        CMA            					; ..
        PUSH    PSW    					; ..
        LXI     H,PI   					; reduce to -pi<x<0
        CALL    FSUB
        JP      SIN1
        LXI     H,HALFP					; now add pi for -pi<x<-pi/2
        PUSH    H
        CALL    FADD
        CP      NEG    					; and just negate for -pi/2<x<0
        POP     H
        CALL    FADD
        POP     PSW    					; restore sign
        ORA     A
        CP      NEG
;
;--------------------------------------------------------------------------
; INIT REGISTERS
;--------------------------------------------------------------------------
;
        LXI     H,TEMP1					; point it
		CALL	SFPA					; go store the value
        LDA     FACC   					; get sign&exponent
        CALL    FEXP   					; expand expon.
        JP      SIN3A  					; brif positive
        CPI     0FDH   					; test exponent
        RC             					; return if very small radian
;
;--------------------------------------------------------------------------
; ABOVE ROUTINE WILL APPROX SIN(X) == X FOR X: (-.06,.06)
;--------------------------------------------------------------------------
;
SIN3A:  LXI     H,HALFP					; point pi/2
        CALL    FDIV   					; compute x/pi/2
        LXI     H,TEMP2					; point t2
		CALL	SFPA					; go store the value
        LXI     H,TEMP2					; point back
        CALL    FMUL   					; compute square
        LXI     H,SINCO					; point constants
;
;--------------------------------------------------------------------------
; EVALUATE POWER SERIES
;
; EVALUATE STARTING FROM HIGH ORDER COEFFICIENT:
; F(X)=(...(CN*FACC+C(N-1))*FACC+...+C1)*FACC*TEMP2+TEMP1
;
;ON ENTRY:
;      TEMP1=CONSTANT TERM
;      TEMP2=X OR 1
;      FACC=X^2 OR X
;      (HL)=COEFFICIENT OF LAST TERM
;--------------------------------------------------------------------------
;
EVPS:   PUSH    H      					; save pointer to coefficients
        LXI     H,TEMP3					; save facc
		CALL	SFPA					; go store the value
        POP     H      					; restore h
        PUSH    H
        JMP     EVPS2
EVPS1:  PUSH    H      					; save ptr to next coefficient
        CALL    FADD   					; facc+cn->facc
        LXI     H,TEMP3					; pointer to x^n
EVPS2:  CALL    FMUL   					; facc*x^n->facc
        POP     H      					; coefficent ptr
		CALL	INCPC					; adjust h,l
        DB      -4
        MOV     A,M    					; get exponent
        DCR     A      					; test for 1
        JNZ     EVPS1  					; brif not 1
        LXI     H,TEMP2					; mul by temp2
        CALL    FMUL
        LXI     H,TEMP1					; point to constant term
        JMP     FADD   					; add it and return to caller
;
;-------------------------------------------------------
; COMPUTE COSINE OF ANGLE, X EXPRESSED IN RADIANS
; USES THE TRANSFORMATION: Y = PI/2 +- X
;    AND THEN COMPUTES SIN(Y).
;--------------------------------------------------------------------------
;
COS:    LXI     H,HALFP					; compute pi/2 + x
        CALL    FADD   					; go add
        JMP     SIN    					; go compute sine
;
;--------------------------------------------------------------------------
; COMPUTE TANGENT OF X, IN RADIANS
; USES THE RELATION:
;
;         SIN(X)
; TAN(X) = ------
;         COS(X)
;--------------------------------------------------------------------------
;
TAN:    LXI     H,TEMP4					; point save area
		CALL	SFPA					; go store the value
        CALL    COS    					; compute cos(x)
        LXI     H,TEMP7					; save cos(x)->temp7
		CALL	SFPA					; go store the value
        LXI     H,TEMP4					; move x->facc
		CALL	LDFPA					; load it
        CALL    SIN    					; compute sine
        LXI     H,TEMP7					; point cos
        JMP     FDIV   					; divide and return to caller
;
;--------------------------------------------------------------------------
; COMPUTES THE ARCTANGENT OF X
; USES A SEVENTH DEGREE POLYNOMIAL APPROXIMATION
;--------------------------------------------------------------------------
;
ATN:    CALL    FTEST  					; check sign of argument
        JP      ATN1   					; brif positive
        CALL    NEG    					; reverse sign
        CALL    ATN1   					; get positive atn
        JMP     NEG    					; make neg & return
;
ATN1:   LXI     H,ONE  					; point: 1
        CALL    FADD   					; go add
        LXI     H,TEMP1					; point save
		CALL	SFPA					; go store the value
        LXI     H,TWO  					; point: 2
        CALL    FSUB   					; go subtract
        LXI     H,TEMP1					; point saved
        CALL    FDIV   					; divide
        LXI     H,TEMP2					; point save
		CALL	SFPA					; save x'=(x-1)/(x+1)
        LXI     H,QTRPI					; x'+pi/4 -> temp1
        CALL    FADD
        LXI     H,TEMP1
		CALL	SFPA					; go store the value
        PUSH    H      					; save ptr to temp2
		CALL	LDFPA					; load it
        POP     H
        CALL    FMUL   					; facc=x'*x'
        LXI     H,ATNCO					; point list coefficients
        JMP     EVPS   					; go compute & return
;
;--------------------------------------------------------
; COMPUTES THE NATRUAL LOGRITHM, LN(X)
; USES A 7TH DEGREE POLYNOMIAL APPROXIMATION
;--------------------------------------------------------
;
LN:     CALL    FTEST  					; test the argument
        JM      ZMERR  					; ln(-x)=no no
        JZ      ZMERR  					; ln(0)=no no also
        LXI     H,TEMP2					; point save area
		CALL	SFPA					; go store the value
        LDA     FACC   					; get expon
        CALL    FEXP   					; expand to 8 bits
        JZ      LN0    					; brif 0.5 < x < 1.0
        JP      LN1    					; brif positive exponent
LN0:    CMA            					; else compliment
        ADI     2      					; plus two
        CALL    FDEC   					; convert to float point
        CALL    NEG    					; then negate
        JMP     LN2    					; go around
LN1:    SBI     1      					; minus one
        CALL    FDEC   					; convert to floating point
LN2:    LXI     H,LN2C 					; point ln(2)
        CALL    FMUL   					; multiply
        LXI     H,TEMP1					; point save area
		CALL	SFPA					; go store the value
		CALL	LDFPA					; load it
        MVI     A,1    					; get exponent: 1
        STA     FACC   					; adjust to range (1,2)
        LXI     H,ONE  					; point 1
        PUSH    H      					; save ptr to one
        CALL    FSUB   					; subtract one
        POP     D      					; set temp2=1
        LXI     H,TEMP2
        CALL    CPY4D
        LXI     H,LNCO 					; point coefficients
        JMP     EVPS   					; approximate & return
;
;--------------------------------------------------------
; X=LOG(X) --- THIS IS LOG BASE 10.
;--------------------------------------------------------
;
LOG:    CALL    LN     					; compute natural log
        LXI     H,LNC  					; point log(e)
        JMP     FMUL   					; multiply and return

;
;---------------------------------------------------
; COMPUTES EXP(X) USING ALGORITHM EXP(X)=(2^I)*(2^FP) WHERE
; 2^I=INT(X*LN BASE 2 OF E) AND,
; 2^FP=5TH DEGREE POLY. APPROXIMATION
; FP=FRACTIONAL PART OF INT(X*LN2E)
;---------------------------------------------------
;
EXP:    CALL    FTEST  					; check sign
        JP      EXP1   					; brif positive
        CALL    NEG    					; else, reverse sign
        CALL    EXP1   					; compute positive exp
        LXI     H,TEMP1					; point save area
		CALL	SFPA					; go store the value
	    LXI     H,ONE  					; point 1
		CALL	LDFPA					; load it
	    LXI     H,TEMP1					; point prev
        JMP     FDIV   					; reciprical and return
;
EXP1:   LXI     H,LN2E 					; point ln base 2 of e
        CALL    FMUL   					; facc=x*(ln2e)
        LXI     H,TEMP3					; point save area
		CALL	SFPA					; go store the value
        CALL    INT    					; facc=int(x*ln2e)
        LXI     H,TEMP4					; point save area
		CALL	SFPA					; go store the value
		CALL	SFPA					; go store the value
        LDA     FACC   					; get the exponent count
        MOV     B,A    					; save count in b
        LDA     FACC+1 					; get mantissa
ELOOP:  RLC            					; rotate left
        DCR     B      					; reduce count
        JNZ     ELOOP  					; continue shifting
        INR     A      					; adjust exponent
        STA     TEMP4  					; store exponent
        MVI     A,80H  					; load constant
        STA     TEMP4+1					; store as mantissa
        LXI     H,ONE  					; 1 -> temp1, temp2
		CALL	LDFPA					; load it
        LXI     H,TEMP1
		CALL	SFPA					; go store the value
		CALL	SFPA					; go store the value
		CALL	LDFPA					; load temp3=int(x*ln2e)
        LXI     H,TEMP5					; get facc=fp(x*ln2e)
        CALL    FSUB
        LXI     H,EXPCO					; point constants
        CALL    EVPS   					; compute polynomial
        LXI     H,TEMP4					; point 2^(int(x*ln2e))
        JMP     FMUL   					; multiply,normalize and return
;
;--------------------------------------------------------
; RETURN THE ABSOLUTE VALUE OF THE FLOATING ACCUMULATOR
;--------------------------------------------------------
;
;
ABS:    LDA     FACC   					; get exponent
        ANI     7FH    					; strip negative sign
        STA     FACC   					; replace
        RET            					; return
;
;--------------------------------------------------------
; RETURNS THE SIGN OF THE FLOATING ACCUMULATOR
; THAT IS:
; 1 IF FACC > 0
; 0 IF FACC = 0
; -1 IF FACC < 0
;--------------------------------------------------------
;
SGN:    CALL    FTEST  					; get status of facc
        RZ             					; return if zero
        ANI     80H    					; isolate sign
SGN1:   ORI     1      					; create exponent
        PUSH    PSW    					; save it
        LXI     H,ONE  					; get address of constant 1
		CALL	LDFPA					; load it
        POP     PSW    					; restore sign
        STA     FACC   					; set the sign
        RET            					; return
;
;--------------------------------------------------------;
; RETURNS THE GREATEST INTEGER NOT LARGER THAN VALUE IN FACC
; E.G.:
;   INT(3.14159) =  3
;   INT(0)       =  0
;   INT(-3.1415) = -4
;--------------------------------------------------------
;
;
INT:    LXI     H,FACC 					; point float acc
        MOV     A,M    					; get exponent
        ANI     40H    					; get sign of characteristic
        JZ      INT2   					; brif ge zero
        MVI     B,4    					; loop ctr
        JMP     ZEROM  					; go zero the facc
INT2:   MOV     A,M    					; get exponent again
        ORA     A      					; test sign
        JP      INT3   					; brif positive or zero
        LXI     H,NEGON					; point constant: -.9999999
        CALL    FADD   					; add to facc
        LXI     H,FACC 					; point expontent again
        MOV     A,M    					; load it
INT3:   ANI     3FH    					; isolate characteristic
        CPI     24     					; test if any fraction
        RP             					; return if not
        MOV     B,A    					; save exponent
        MVI     A,24   					; get constant
        SUB     B      					; minus exponent = loop ctr
        MOV     C,A    					; save it
INT4:   LXI     H,FACC+1       			; point msb
        XRA     A      					; clear cy flag
        MVI     B,3    					; byte count
INT5:   MOV     A,M    					; load a byte
        RAR            					; shift right
        MOV     M,A    					; replace
        INX     H      					; point next
        DCR     B      					; decr byte ctr
        JNZ     INT5   					; loop
        DCR     C      					; decr bit ctr
        JNZ     INT4   					; loop
        LXI     H,FACC 					; point sign & exp
        MOV     A,M    					; load it
        ANI     80H    					; isolate sign
        ADI     24     					; plus integer
        MOV     M,A    					; replace it
        JMP     FNORM  					; go normalize & return
;
;--------------------------------------------------------
; COMPUTE SQAURE ROOT OF ARG IN FACC, PUT RESULT IN FACC
;
; USE HERON'S ITERATIVE PROCESS
;--------------------------------------------------------
;
SQR:    CALL    FTEST  					; test the argument
        RZ             					; return if zero
        JM      ZMERR  					; error if negative
        STA     DEXP   					; save orig exponent
        XRA     A      					; get a zero
        STA     FACC   					; put arg in range [.5, 1]
        LXI     H,TEMP2					; point save area
		CALL	SFPA					; go store the value
;
;---------------------------------------------------
; INITIAL APPROXIMATION 0.41730759 + 0.59016206 * MANTISSA
;---------------------------------------------------
;
        LXI     H,SQC1 					; point .59016
        CALL    FMUL   					; go multiply
        LXI     H,SQC2 					; pint .4173
        CALL    FADD   					; go add
        LXI     H,TEMP1					; point save area
		CALL	SFPA					; go store the value
;
;---------------------------------------------------
; NEWTON'S METHOD OF ITERATION TO THE APPROXIMATE
; VALUE OF THE SQR OF MANTISSA
;---------------------------------------------------
;
        CALL    SQR1   					; first iteration
        LXI     H,TEMP1					; point save area
		CALL	SFPA					; go store the value
        CALL    SQR1   					; second iteration
;
;---------------------------------------------------
; RESTORE RANGE TO OBTAIN THE FINAL RESULT
;---------------------------------------------------
;
        LDA     DEXP   					; get save exponent
        CALL    FEXP   					; expand it
        RAR            					; divide by 2
        STA     FACC   					; store it
        RNC            					; return if expon even
        LXI     H,SQC3 					; else, point sqr(2)
        JMP     FMUL   					; go multiply and return
;
;---------------------------------------------------
; THIS ROUTINE PERFORMS ONE NEWTON ITERATION
; TO THE SQUARE ROOT FUNCTION
;---------------------------------------------------
;
SQR1:   LXI     H,TEMP2					; point mantissa
		CALL	LDFPA					; load it
        LXI     H,TEMP1					; point prev guess
        CALL    FDIV   					; form mant/temp1
        LXI     H,TEMP1					; point prev
        CALL    FADD   					; form temp1 + mant/temp1
        SUI     1      					; divide by 2
        STA     FACC   					; form (temp1 + mant/temp1)/2
        RET            					; return
;
;--------------------------------------------------------
; REVERSES THE SIGN OF THE FLOATING ACC
;--------------------------------------------------------
;
NEG:    CALL    FTEST  					; get status of facc
        RZ             					; return if zero
        XRI     80H    					; reverse sign
        STA     FACC   					; restore exponent
        RET            					; continue evaluation

;
;--------------------------------------------------------
; PSEUDO RANDOM NUMBER GENERATOR
;--------------------------------------------------------
;
RND:    LXI     H,TEMP7					; save arg
		CALL	SFPA					; go store the value
        MVI     B,4    					; loop ctr
        LXI     H,FACC 					; point float accum
        CALL    ZEROM  					; go zero the facc
        MVI     C,3    					; outter lop ctr
        LXI     H,FACC+1       			; point msb
        PUSH    H      					; save h,l
RND1:   LXI     H,RNDZ+1       			; point x,y,z
        MVI     B,6    					; loop ctr
        ORA     A      					; turn off cy
RND2:   MOV     A,M    					; get a byte
        RAL            					; shift left (mult by 2)
        MOV     M,A    					; replace the byte
        DCX     H      					; point next
        DCR     B      					; decr ctr
        JNZ     RND2   					; loop
        INX     H      					; point msd x,y,z
        LXI     D,RNDP 					; point to modulo
        MVI     B,3    					; loop ctr
FND3:   LDAX    D      					; get byte of p,q,r
        CMP     M      					; compare with x,y,z
        INX     D      					; point next
        INX     H      					; ditto
        JC      RND4   					; brif p<x
        JNZ     RND5   					; brif p>x
        LDAX    D      					; get low byte
        CMP     M      					; cmpare
        JNC     RND5   					; brif p>=x
RND4:   XCHG           					; flip d,e to h,l
        LDAX    D      					; get low x byte
        SUB     M      					; subtract low p byte
        STAX    D      					; store it
        DCX     D      					; point high
        DCX     H      					; ditto
        LDAX    D      					; get high x byte
        SBB     M      					; sub high p byte
        STAX    D      					; store it
        INX     D      					; point low
        INX     H      					; ditto
        XCHG           					; restore addrs
RND5:   INX     D      					; point next
        INX     H      					; ditto
        DCR     B      					; decr ctr
        JNZ     FND3   					; loop
        MVI     B,3    					; loop ctr
RND6:   LXI     D,RNDS+1       			; point low s
        LDAX    D      					; get low s
        ADD     M      					; add low x,y,z
        STAX    D      					; put s
        DCX     D      					; point high
        DCX     H      					; ditto
        LDAX    D      					; get high s
        ADC     M      					; add high x,y,z
        ANI     3FH    					; turn off high bits
        STAX    D      					; store it
        DCX     H      					; point next x,y,z
        DCR     B      					; decr ctr
        JNZ     RND6   					; loop
        MVI     A,8    					; constant
        SUB     C      					; less ctr
        RAR            					; divide by two
        POP     H      					; get h,l addr
        LDA     RNDS+1 					; get lsb of s
        MOV     M,A    					; store it
        INX     H      					; point next
        PUSH    H      					; save h,l
        DCR     C      					; decr ctr
        JNZ     RND1   					; loop
        POP     H      					; restore sp ptr
        LDA     RNDSW  					; get switch
        ORA     A      					; test it
        JZ      RND7   					; brif no randomize
        LXI     D,TRNDX					; point saved values
        LXI     H,RNDX 					; point next values
        MVI     B,8    					; loop ctr
        CALL    COPYH  					; go copy
RND7:   CALL    FNORM
        LXI     H,TEMP7					; multiply by range
        JMP     FMUL
;
;--------------------------------------------------------
; INPUT A BYTE FROM THE DEVICE IN FACC
; PUT THE RESULT IN THE FACC
;--------------------------------------------------------
;
INP:    CALL    FBIN   					; convert facc to binary
        LXI     H,OUTA 					; point instr buffer
        MVI     M,0DBH 					; in instr
        INX     H      					; point next
        MOV     M,A    					; move addr
        INX     H      					; point next
        MVI     M,0C9H 					; ret instr
        CALL    OUTA   					; go input a byte
FDEC:   MOV     E,A    					; move byte to lo d,e
        MVI     D,0    					; zero hi d,e
        JMP     BINFL  					; go convert to dec & ret

;
;--------------------------------------------------------
; RETURNS THE CURRENT POSITION OF THE TTY CURSOR
;--------------------------------------------------------
;
POS:    LDA     COLUM  					; get position
        JMP     FDEC   					; convert to float and return
;
CONCA   EQU     $
;
;
; CONCATONATE TWO STRING TOGETHER
; COMBINE LENGTH <= 255
;
        POP     D      					; adjust stack
        LXI     D,STRIN					; point string buffer
        LDAX    D      					; get current length
        MOV     C,A    					; store it
        MVI     B,0    					; clear hi
        XCHG           					; flip flop
        DAD     B      					; compute next
        XCHG           					; flip back
        ADD     M      					; compute combine length
        MOV     B,M    					; save len2
        JNC     CONC2  					; brif no ovflw
        MVI     A,255  					; max len
        SUB     C      					; minus 1st part
        MOV     B,A    					; save len
        MVI     A,255  					; updated length
CONC2:  STA     STRIN  					; store it
        MOV     A,B    					; get len to move
        ORA     A      					; test it
        JZ      CONC4  					; brif null
CONC3:  INX     H      					; point next
        INX     D      					; ditto
        MOV     A,M    					; get next char
        STAX    D      					; put it
        DCR     B      					; decr count
        JNZ     CONC3  					; loop
CONC4:  POP     H      					; get h,l
        DCX     H      					; point back
        LDA     STRIN  					; get len
        RAR            					; divide by two
        INR     A      					; plus one
        XCHG           					; save h,l
        LHLD    SPCTR  					; get ctr
        MOV     C,A    					; save ctr
        MVI     B,0    					; zero hi byte
        DAD     B      					; add len this string
        SHLD    SPCTR  					; save ctr
        POP     B
        LXI     H,0    					; get addr zero
CONC5:  PUSH    H      					; 2 byte word
        DCR     A      					; decr ctr
        JNZ     CONC5  					; continue
        DAD     SP     					; get address in h,l
        XCHG           					; put stack ptr in d,e
        MOV     M,D    					; move hi addr
        INX     H      					; point next
        MOV     M,E    					; move lo addr
        INX     H      					; point next
        MVI     M,0E7H 					; type=string
        PUSH    H      					; save h,l
        LXI     H,STRIN					; get temp str
        MOV     A,M    					; get length
        INR     A      					; plus one
        MOV     C,A    					; save it
CONC6:  MOV     A,M    					; get a byte
        STAX    D      					; put it down
        INX     D      					; point next
        INX     H      					; ditto
        DCR     C      					; subt ctr
        JNZ     CONC6  					; loop
        POP     H      					; restore h,l
		CALL	INCPC					; adjust h,l
        DB      -7
        MVI     A,4    					; delete 4 bytes
        CALL    SQUIS  					; go compress
        JMP     EVAL   					; continue evaluation
;
;--------------------------------------------------------
; X=LEN(A$)
; RETURN THE LENGTH OF THE STRING
;--------------------------------------------------------
;
LENFN:  LDA     STRIN  					; get len in acc
        JMP     FDEC   					; go convert to decimal & return

;
;--------------------------------------------------------
; A$=CHR$(X)
; RETURNS A ONE CHAR STRING HAVING THE ASCII VALUE - X
;--------------------------------------------------------
;
CHRFN:  CALL    FBIN   					; convert facc to binary
        LXI     H,STRIN					; point out area
        MVI     M,1    					; len=1
        INX     H      					; point next
        MOV     M,A    					; store the char
        RET            					; return

;
;--------------------------------------------------------
; X=ASCII(A$)
; RETURNS THE ASCII VALUE OF THE FIRST CHAR IN STRING
;--------------------------------------------------------
;
ASCII:  LXI     H,STRIN					; point string
        MOV     A,M    					; get length
        ORA     A      					; test if > zero
        JZ      FDEC   					; brif zero & return a zero
        INX     H      					; point 1st char
        MOV     A,M    					; load it
        JMP     FDEC   					; go convert to decimal & return
;
;--------------------------------------------------------
; A$=NUM$(X)
; RETURNS A STRING REPRESENTING X AS IT WOULD HAVE
; BEEN PRINTED (INCLUDING TRAILING SPACE)
;--------------------------------------------------------
;
NUMFN:  LXI     H,STRIN					; point string area
        MVI     M,0    					; init count
        INX     H      					; skip to 1st position
        CALL    FOUT   					; go convert to extrn dec
        XRA     A      					; get a zero
        MOV     B,A    					; init ctr
NUM1:   DCX     H      					; point prior
        INR     B      					; count it
        CMP     M      					; test if zero
        JNZ     NUM1   					; loop till at start
        MOV     M,B    					; set len code
        RET            					; then return
;
;--------------------------------------------------------
; X = VAL(A$)
; RETURNS THE VALUE OF THE STRING OF NUMERIC CHARACTERS
;--------------------------------------------------------
;
VAL:    LXI     H,STRIN					; point string area
        MOV     A,M    					; get len
        ORA     A      					; test for null string
        MOV     B,A    					; save len
        JZ      FDEC   					; brif is (returns a 0.00)
        LXI     D,STRIN					; point buffer
VAL1:   INX     H      					; point next
        MOV     A,M    					; get a char
        CPI     ' '    					; test if space
        JZ      VAL2   					; brif is
        STAX    D      					; put the char
        INX     D      					; incr addr
VAL2:   DCR     B      					; decr ctr
        JNZ     VAL1   					; loop
        XRA     A      					; get a zero
        STAX    D      					; put in buff
        LXI     H,STRIN					; point start of buffer
        CALL    FIN    					; go convert
        MOV     A,M    					; get non-numeric
        ORA     A      					; test it
        JNZ     CVERR  					; brif error
        RET            					; else, return
;
;--------------------------------------------------------
; A$=SPACE$(X)
; CREATES A STRING FO SPACES LENGTH = X
;--------------------------------------------------------
;
SPACE:  CALL    FBIN   					; get binary length
        LXI     H,STRIN					; point temp string
        MOV     M,A    					; put len
        ORA     A      					; test it
SPAC1:  RZ             					; return if zero
        INX     H      					; else, point next
        MVI     M,' '  					; move 1 space
        DCR     A      					; decr ctr
        JMP     SPAC1  					; loop
;
;--------------------------------------------------------
; A$=STRING$(X,Y)
; CREATES STRING OF LNGTH X CONTAINING REPETITION OF CHR$(Y)
;--------------------------------------------------------
;
STRFN:  CALL    FBIN   					; get binary length
        STA     STRIN  					; put to string
        CALL    ARGNU  					; get next argument
        LXI     H,STRIN					; point string
        MOV     B,M    					; get count
STR11:  INX     H      					; point next
        MOV     M,A    					; store the char
        DCR     B      					; decr ctr
        JNZ     STR11  					; loop
        RET            					; return
;
;--------------------------------------------------------
; B$=LEFT$(A$,X)
; SUBSTRING FROM THE LEFTMOST X CHARACTERS OF A$
;--------------------------------------------------------
;
LEFT:   CALL    ARGNU  					; get 2nd argument
        MOV     C,A    					; save len
        MVI     B,1    					; init start
        JMP     MID0   					; continue
;
;--------------------------------------------------------
; B$=RIGHT$(A$,X)
; SUBSTRING STARTING AT POSITION X TO END OF STRING
;--------------------------------------------------------
;
RIGHT:  CALL    ARGNU  					; get 2nd argument
        MOV     B,A    					; save start
        MVI     C,255  					; max len
        JMP     MID0   					; continue
;
;--------------------------------------------------------
; B$=MID$(A$,X,Y)
; SUBSTRING OF THE STRING A$ STARTING WITH CHARACTER @ X
; AND Y CHARACTERS LONG
;--------------------------------------------------------
;
MIDFN:  CALL    ARGNU  					; load x
        MOV     B,A    					; save start
        PUSH    B      					; put on stack
        CALL    ARGNU  					; get 3rd arg
        POP     B      					; retreive
        MOV     C,A    					; save len
MID0:   MOV     A,B    					; load start
        LXI     H,STRIN					; point string
        CMP     M      					; test if x>l
        JC      MID1   					; brif x>l
        JZ      MID1   					; or equal
        MVI     M,0    					; else, result is null
        RET            					; return
MID1:   ADD     C      					; compute end position
        JC      MID2   					; brif overflow
        SBI     1      					; compute x+y-1
        JC      MID2   					; brif overflow
        CMP     M      					; compare to existing len
        JC      MID3   					; brif x+y-1<len(a$)
MID2:   MOV     A,M    					; else get orig len
        SUB     B      					; minus x
        INR     A      					; plus one
        MOV     C,A    					; save (replace y)
MID3:   MOV     M,C    					; put new len
        MOV     E,B    					; put start in lo
        MVI     D,0    					; zero in hi
        DAD     D      					; compute start
        LXI     D,STRIN					; get begin
MID4:   MOV     A,M    					; get a char
        INX     D      					; point next
        INX     H      					; ditto
        STAX    D      					; put down
        DCR     C      					; decr ctr
        JNZ     MID4   					; loop
        RET            					; then return
;
;--------------------------------------------------------
; X = INSTR(Y,A$,B$)
;
; SEARCH FOR SUBSTRING B$ IN STRING A$ STARTING AT POS Y.
; RETURN 0 IF B$ IS NOT IN A$
; RETURN 1 IF B$ IS NULL
; ELSE RETURN THE CHARACTER POSITION
;--------------------------------------------------------
;
INSTR:  CALL    ARGNU  					; get a$
        LXI     H,STRIN					; point a$
        ORA     A      					; test y
        JNZ     INST2  					; brif y not zero
INST1:  MVI     M,0    					; else a$ is null
        JMP     INST3  					; go around
INST2:  CMP     M      					; test y to len(a$)
        JZ      INST3  					; brif equal
        JNC     INST1  					; brif y > len(a$)
INST3:  MOV     C,A    					; save y
        MVI     B,0    					; zero hi incr
        MOV     A,M    					; get len(a$)
        SUB     C      					; minus y
        INR     A      					; plus one
        DAD     B      					; compute start addr
        MOV     B,A    					; # chars remain in a$
        PUSH    H      					; save addr
        LHLD    ADDR1  					; get addr of arg
        INX     H      					; point next
        MOV     D,M    					; get hi addr
        INX     H      					; point next
        MOV     E,M    					; get lo addr
        INX     H      					; point next
        SHLD    ADDR1  					; updated ptr
        POP     H      					; restore addr
        LDAX    D      					; get len(b$)
        ORA     A      					; test if null
        JNZ     INST6  					; brif not
        MVI     C,1    					; set posit = 1
INST5:  MOV     A,C    					; get posit
        JMP     FDEC   					; convert to decimal & return
INST6:  XCHG           					; flip/flop
        MOV     A,B    					; get len of a$
        CMP     M      					; compare to len b$
        JC      INSTA  					; brif len(b$)< len(rem a$)
        PUSH    B      					; save ctr, posit
        PUSH    D      					; save addr a$
        PUSH    H      					; save addr b$
        MOV     C,M    					; get len b$
        XCHG           					; flip/flop
INST8:  INX     D      					; point next b$
        LDAX    D      					; get b$ char
        CMP     M      					; compare a$ char
        JNZ     INST9  					; brif not equal
        INX     H      					; point next a$
        DCR     C      					; decr ctr (len(b$))
        JNZ     INST8  					; loop
        POP     H      					; dummy pop
        POP     H      					; get dummy stack
        POP     B      					; get position
        JMP     INST5  					; we found a match
INST9:  POP     D      					; get ptr b$
        POP     H      					; get ptr a$
        POP     B      					; get ctrs, posit
        INR     C      					; up ptr num
        INX     H      					; point next a$
        DCR     B      					; decr b
        JNZ     INST6  					; loop
INSTA:  MVI     C,0    					; else b$ not in a$
        JMP     INST5  					; return
;
;--------------------------------------------------------
; STATEMENT: DEF FNX(A)=EXPR
; NOTE: ENTRY FROM EXPR ANALYZER (RECURSIVE)
;--------------------------------------------------------
;
FN:     PUSH    B      					; save b,c
        PUSH    D      					; save d,e
        PUSH    H      					; save h,l
        XCHG           					; put h,l to d,e
        LHLD    ADDR3  					; get addr
        PUSH    H      					; save it
        XCHG           					; put d,e back to h,l
        SHLD    ADDR3  					; update ptr
        LHLD    SPCTR  					; get sp count
        PUSH    H      					; save it
        LDA     PARCT  					; get paren count
        MOV     B,A    					; put to b
        LDA     FNMOD  					; get fn mode
        MOV     C,A    					; put to c
        PUSH    B      					; save b,c
        LDA     DIMSW  					; get dim sw
        PUSH    PSW    					; save it
        XRA     A      					; clear a
        STA     DIMSW  					; reset dim sw
        LHLD    FNARG  					; get old arg name
        PUSH    H      					; save
        LHLD    FNARG+2					; get old arg address
        PUSH    H      					; save
        LHLD    PROGE  					; get end of program
        PUSH    H      					; save it
        LHLD    EXPRS  					; get end of expr
        PUSH    H      					; save it
        SHLD    PROGE  					; save new 'end' of program
        MVI     A,1    					; get on setting
        STA     FNMOD  					; set in function
        LHLD    ADDR3  					; point to expr
        MOV     C,M    					; get fn char
        DCX     H      					; point back
        MOV     B,M    					; get hi name
        LXI     H,BEGPR					; point start of program
FN2:    MOV     A,M    					; load len to next statement
        ORA     A      					; test if at end
        JZ      SNERR  					; brif fn not found
        PUSH    H      					; save ptr
		CALL	INCPC					; adjust h,l
	    DB      3
        LXI     D,DEFLI					; literal
		CALL	COMP					; go compare
        JNZ     FN3    					; brif not equal
        PUSH    B      					; save test name
        CALL    VAR    					; go get name
        POP     B      					; restore name
        MOV     A,D    					; get hi name
        CMP     B      					; compare
        JNZ     FN3    					; brif not equal
        MOV     A,E    					; get lo
        CMP     C      					; compare
        JZ      FN4    					; brif equal
FN3:    POP     H      					; get old ptr
        MOV     E,M    					; get lo len
        MVI     D,0    					; zero hi len
        DAD     D      					; point next statement
        JMP     FN2    					; loop
FN4:    POP     D      					; adjust stack
		CALL	TSTC					; call to ram address
        CPI     '('    					; test if open paren
        JNZ     SNERR  					; brif not
        INX     H      					; skip it
        CALL    VAR    					; go get var name
        PUSH    H      					; save hl addr
        LXI     H,FNARG					; point dummy arg tbl
        MOV     M,D    					; store letter
        INX     H      					; point next
        MOV     M,E    					; store digit
        INX     H      					; point next
        XCHG           					; put h,l to d,e
        LHLD    ADDR3  					; point to expr stack
        INX     H      					; point code
        INX     H      					; point hi adr
        MOV     A,M    					; get hi
        STAX    D      					; put to table
        INX     D      					; point next
        INX     H      					; ditto
        MOV     A,M    					; get lo addr
        STAX    D      					; put to table
        POP     H      					; restore ptr to statement
		CALL	TSTC					; call to ram address
        CPI     ')'    					; test if close paren
        JNZ     SNERR  					; brif not
        INX     H      					; skip it
		CALL	TSTC					; call to ram address
        CPI     '='    					; test if equal sign
        JNZ     SNERR  					; brif not
        INX     H      					; skip it
        CALL    EXPR   					; go eval function
        CALL    EOL    					; must be end of line
        POP     H      					; get h,l
        SHLD    EXPRS  					; restore start of expr
        POP     H      					; get h,l
        SHLD    PROGE  					; restore 'end' of program
        POP     H      					; get h,l
        SHLD    FNARG+2					; store addr
        POP     H      					; get h,l
        SHLD    FNARG  					; store dummy arg
        POP     PSW    					; get a,status
        STA     DIMSW  					; restore dim sw
        POP     B      					; get b,c
        MOV     A,C    					; load c
        STA     FNMOD  					; restore moe
        MOV     A,B    					; load b
        STA     PARCT  					; restore paren count
        POP     H      					; get h,l
        SHLD    SPCTR  					; restore sp counter
        POP     H      					; get h,l
        SHLD    ADDR3  					; restore addr of eval
        POP     H      					; get h,l
        POP     D      					; get d,e
        DCX     H      					; point 2nd byte following op
        SHLD    ADDR2  					; save it
		CALL	INCPC					; adjust h,l
        DB      5
        SHLD    ADDR1  					; save addr
        JMP     EV3    					; go wrapup
;
;--------------------------------------------------------
; EVALUATE EXPRESSION ROUTINE
; LEAVE RESULT IN FACC
; RETURN WHEN EXPRESSION ENDS (TYPICALLY AT END OF LINE)
;--------------------------------------------------------
;
EXPR:   XRA     A      					; clear reg a
        STA     PARCT  					; set paren ctr
        XCHG           					; save h,l
        LXI     H,0    					; get a zero
        SHLD    SPCTR  					; init ctr
        LHLD    PROGE  					; point end of program area
        INX     H      					; point one more
        MVI     M,0    					; init start of stack
        SHLD    EXPRS  					; save it
        XCHG           					; restore h,l
;
LOOKD   EQU     $      					; look for con, var, or function
		CALL	TSTC					; call to ram address
        CALL    NUMER  					; go test if numeric
        JNZ     LDALP  					; brif not
LDNUM:  CALL    FIN    					; go convert numeric (put to facc)
LDF:    MOV     B,H    					; copy h,l to b,c
        MOV     C,L    					; same
        LHLD    EXPRS  					; get addr of expr area
        CALL    GTEMP  					; go store the facc in temp area
        SHLD    EXPRS  					; save updated address
        MOV     H,B    					; restore h
        MOV     L,C    					; restore l
        JMP     LOOKO  					; go get an operation code
LDALP:  CPI     '.'    					; see if leading decimal point
        JZ      LDNUM  					; brif is
        CALL    ALPHA  					; go see if alpha
        JNZ     LDDTN  					; brif not
        MOV     B,M    					; save 1st char
        INX     H      					; point next
        MVI     C,' '  					; default for 1 char var
        CALL    NUMER  					; go see if 2nd is numeric
        JNZ     LDFN   					; brif not
        INX     H      					; point next
        MOV     C,A    					; save the char
LDV1:	CALL	TSTC					; call to ram address
        CPI     '$'    					; test if string
        PUSH    PSW    					; save status
        JNZ     LDV2   					; brif not
        MOV     A,C    					; get low char
        ORI     80H    					; set string
        MOV     C,A    					; save it
        INX     H      					; skip $
		CALL	TSTC					; call to ram address
LDV2:   CPI     '('    					; test if paren
        JZ      LDV2A  					; brif is
        PUSH    H      					; save h,l
        MOV     D,B    					; copy b,c
        MOV     E,C    					; to d,e
        CALL    SEARC  					; go get var addr in d,e
LDV:    LHLD    EXPRS  					; get expr addr
        CALL    SADR   					; go store address
        SHLD    EXPRS  					; save address
        XCHG           					; h,l to d,e
        POP     H      					; get old h,l
        POP     PSW    					; get status
        JNZ     LOOKO  					; brif not string
        XCHG           					; get old h,l
        MVI     M,0E7H 					; mark as string address
        XCHG           					; restore h,l
        JMP     LOOKO  					; go look for opcode
LDFN:   CALL    ALPHA  					; go see if function
        JNZ     LDV1   					; brif it's not
LDFN1:  DCX     H      					; point back to 1st
        MOV     A,M    					; get that char
        CPI     ' '    					; test if space
        JZ      LDFN1  					; loop if true
        PUSH    H      					; save h,l
        LXI     D,RNDLI					; point literal
		CALL	COMP					; go compare
        JZ      LDRND  					; brif fnd
        POP     H      					; get h,l
        PUSH    H      					; resave
        LXI     D,FNLIT					; point literal
		CALL	COMP					; go compare
        JZ      FNL    					; brif is
        POP     H      					; get h,l
        PUSH    H      					; resave
        LXI     D,PILIT					; point lit
		CALL	COMP					; go compare
        JZ      LDPI   					; brif pi
FUNC0:  POP     H      					; get h,l
        LXI     D,FUNCT					; point function table
        PUSH    H      					; save pointer
        CALL    SEEK1  					; go search function table
        JZ      FUNC4  					; brif function not found
        LDAX    D      					; get a byte low
        MOV     C,A    					; save it
        INX     D      					; point next
        LDAX    D      					; get hi byte
        MOV     B,A    					; save it (b,c = addr of func)
		CALL	TSTC					; call to ram address
        CPI     '('    					; test for open paren
        JNZ     SNERR  					; brif missing paren
        INX     D      					; point type code
        LDAX    D      									; load it
        JMP     LDFNC  					; continue
FUNC4:  POP     H      					; get h,l
        MOV     B,M    					; get 1st char
        MVI     C,' '  					; space 2nd char
        INX     H      					; point to next
        JMP     LDV1   					; brif variable
FNL:    POP     D      					; dummy reset stack pointer
        CALL    VAR    					; go get fn name
        MOV     B,D    					; copy to b,c
        MOV     C,E    					; same
        XCHG           					; save h,l
        LHLD    EXPRS  					; point expr stack
        INX     H      					; point next
        MOV     M,B    					; move the letter
        INX     H      					; point next
        MOV     M,C    					; move digit ($??)
        INX     H      					; point next
        MVI     M,0AFH 					; move code
        MOV     A,C    					; get lo name
        ORA     A      					; test it
        JP      FNL3   					; brif not string
        MVI     M,0CFH 					; move code
FNL3:   SHLD    EXPRS  					; save pointer
        XCHG           					; get h,l
		CALL	TSTC					; call to ram address
        CPI     '('    					; test if open paren
        JNZ     SNERR  					; brif not
        JMP     LOOKD  					; continue
LDRND:  CPI     '('    					; test if rnd(x)
        JZ      FUNC0  					; brif is
        PUSH    H      					; else, save h,l
        LXI     H,ONE  					; use range (0,1)
		CALL	LDFPA					; load it
        CALL    RND    					; go get random number
        POP     H      					; restore h,l
        POP     D      					; restore stack pointer
        JMP     LDF    					; act as if constant
LDPI:   INR     A      					; set non zero
        POP     D      					; dummy stack pop
        PUSH    PSW    					; save status
        PUSH    H      					; save h,l
        LXI     D,PI   					; get address of 3.1415
        JMP     LDV    					; go act like variable
LDFNC:  POP     D      					; pop the stack
        XCHG           					; flip/flop
        LHLD    EXPRS  					; get addr
        INX     H      					; point next
        MOV     M,B    					; high addr
        INX     H      					; point next
        MOV     M,C    					; low addr
        INX     H      					; point next
        MOV     M,A    					; code
        SHLD    EXPRS  					; save addr
        XCHG           					; restore h,l
        JMP     LOOKD  					; next must be data too
LDDTN:  CPI     '-'    					; test if unary minus
        JNZ     LDDTP  					; brif not
        XCHG           					; save h,l
        LHLD    EXPRS  					; get expr end
        INX     H      					; point one more
        MVI     M,61H  					; code for neg
        SHLD    EXPRS  					; restore ptr
        XCHG           					; restore h,l
SKPP:   INX     H      					; point past this byte
        JMP     LOOKD  					; next must be data
LDDTP:  CPI     '+'    					; test if unary plus
        JZ      SKPP   					; ignore if is
        CPI     '('    					; else, test if open paren
        JZ      CERCE  					; brif is
        CPI     27H    					; test if literal (single quote)
        JZ      LITST  					; brif is
        CPI     '"'    					; test if literal
        JNZ     SNERR  					; brif not con, function, or var
LITST:  MOV     C,A    					; save delimiter
        LXI     D,STRIN					; point buffer
        MVI     B,0FFH 					; init ctr
LIT1:   INX     H      					; point next
        MOV     A,M    					; load next
        INX     D      					; point next
        STAX    D      					; store it
        ORA     A      					; test if end
        JZ      SNERR  					; brif error
        INR     B      					; count it
        CMP     C      					; test if end of string
        JNZ     LIT1   					; brif not
        INX     H      					; point next
        LXI     D,STRIN					; point begin
        MOV     A,B    					; get count
        STAX    D      					; put count
        RAR            					; divide by two
        INR     A      					; plus one
        MOV     C,A    					; save it
        MVI     B,0    					; zero high
        PUSH    H      					; save ptr
        LHLD    SPCTR  					; get ctr
        DAD     B      					; plus old
        SHLD    SPCTR  					; update it
        POP     D      					; get old h,l
        LXI     H,0    					; get a zero
LIT2:   PUSH    H      					; get 2 work bytes
        DCR     C      					; sub 1 from count
        JNZ     LIT2   					; continue
        DAD     SP     					; get addr of stack
        PUSH    D      					; save ptr to statement
        XCHG           					; save h,l in d,e
        LHLD    EXPRS  					; get start of expr
        INX     H      					; plus one
        MOV     M,D    					; hi byte
        INX     H      					; point next
        MOV     M,E    					; lo byte
        INX     H      					; point next
        MVI     M,0E7H 					; type code
        SHLD    EXPRS  					; save addr
        XCHG           					; d,e back to h,l
        LXI     D,STRIN					; point string area
        LDAX    D      					; get count
        INR     A      					; add one to count
        MOV     B,A    					; save ctr
LIT3:   LDAX    D      					; get a byte
        MOV     M,A    					; store it
        INX     H      					; point next
        INX     D      					; ditto
        DCR     B      					; decr ctr
        JNZ     LIT3   					; loop
        POP     H      					; restore h,l
        JMP     LOOKO  					; next is op
CERCE:  XCHG           					; save h,l
        LXI     H,PARCT					; point paren count
        INR     M      					; add 1
        LHLD    EXPRS  					; get addr
        INX     H      					; point next
        MVI     M,5    					; put code
        SHLD    EXPRS  					; save addr
        XCHG           					; restore h,l
        JMP     SKPP   					; go skip char
LOOKO:	CALL	TSTC					; call to ram address
        CPI     '+'    					; test if plus
        MVI     B,21H  					; code
        JZ      OP1    					; brif is
        CPI     '-'    					; test if minus
        MVI     B,25H
        JZ      OP1    					; brif is
        CPI     '/'    					; test if divide
        MVI     B,45H  					; code
        JZ      OP1    					; brif is
        CPI     '^'    					; test if expon
        MVI     B,81H  					; code
        JZ      OP1    					; brif is
        CPI     ')'    					; test if close paren
        JZ      OP3    					; brif is
        CPI     ','    					; test if comma
        JZ      OP2    					; brif is
        CPI     '*'    					; test if multiply
        MVI     B,41H  					; code
        JZ      OP1    					; brif is
										; else must be end of expression
ENDXP:  LDA     PARCT  					; get open paren count
        ORA     A      					; test it
        JNZ     SNERR  					; brif # of ('s not = # of )'s
        SHLD    ADDR3  					; save addr of statement
        JMP     EVAL   					; go evaluate
OP1:    PUSH    H      					; save place in ascii expression
        LXI     D,0105H					; d=byte count, e=code for "("
        LHLD    EXPRS  					; point to last byte
        MOV     A,B    					; b&e3 -> c
        ANI     0E3H
        MOV     C,A
;---------------------------------------------------
; INSERT ( AND EVALUATE IF PRECEDENCE REDUCTION,
;  ELSE INNSERT OP CODE
;---------------------------------------------------
OPLP1:  MOV     A,M    					; get type code from expression
        PUSH    PSW    					; save
        ANI     3      					; get length
OPLP2:  INR     D      					; bump byte count
        DCX     H      					; expression pointer
        DCR     A      					; loop moves to next element
        JNZ     OPLP2
        POP     PSW    					; restore type code
        ANI     0E3H   					; mask for variable
        CPI     0E3H   					; we skip over variables
        JZ      OPLP1  					; br if type = e3 or e7
        CMP     C      					; precedence reduction?
        JNC     INS    					; if nc, yes, insert 05
        LHLD    EXPRS  					; no, insert opcode before var at end
		CALL	INCPC					; adjust h,l
        DB      -3
        MVI     D,4    					; byte count
        MOV     E,B    					; insert this op code
INS:    MOV     B,E    					; save for branch after insertion
INS1:   INX     H      					; bump pointer
        MOV     C,M    					; pick up byte
        MOV     M,B    					; put down replacement
        MOV     B,C    					; save for next loop
        DCR     D      					; done?
        JNZ     INS1   					; if nz, no
        SHLD    EXPRS  					; store pointer
        POP     H      					; restore ascii expression pointer
        MOV     A,E    					; get flag saved in e
        CPI     5      					; stored a "("?
        JNZ     SKPP   					; if nz, no, process next element
        JMP     OP4    					; yes, go evaluate
OP2:    LDA     PARCT  					; get open paren count
        ORA     A      					; test it
        JZ      ENDXP  					; brif end of expr
        XCHG           					; else save h,l
        LHLD    EXPRS  					; get expr begin
        INX     H      					; point next
        MVI     M,1    					; move a comma
        SHLD    EXPRS  					; update pointer
        XCHG           					; flip back
        JMP     SKPP
OP3:    LDA     PARCT  					; get open paren count
        DCR     A      					; subtract one
        STA     PARCT  					; save it
        JM      SNERR  					; brif too many )'s
        INX     H      					; point next source
OP4:    SHLD    ADDR3  					; save addr
EVAL:   LHLD    EXPRS  					; get end of expr
        LXI     B,0    					; init b,c to zero
EV1:    INR     B      					; count each byte
        MOV     A,M    					; get code in reg a
        DCX     H      					; point next
        CPI     0E3H   					; test if data
        JNZ     EV2    					; brif not data
EV1A:   DCX     H      					; point next
        DCX     H      					; ditto
        INR     B      					; bump ctr
        INR     B      					; by two
        INR     C      					; count the term
        JMP     EV1    					; loop
EV2:    CPI     0AFH   					; test if numeric user fn
        JZ      FN     					; brif is
        CPI     0CFH   					; test if string user fn
        JZ      FN     					; brif is
        PUSH    PSW    					; else, save status
        ANI     0E3H   					; mask it
        CPI     0A3H   					; test if numeric function
        JZ      EV2A   					; brif is
        CPI     0C3H   					; test if string function
        JZ      EV2A   					; brif is
        POP     PSW    					; restore code
        CPI     0E7H   					; test if string addr
        JZ      EV1A   					; brif is
        JMP     EV5    					; br around
EV2A:   INX     H      					; reset to type code
        SHLD    ADDR1  					; save addr
        POP     D      					; dummy pop
        PUSH    B      					; save ctrs
        DCX     H      					; point to low jmp addr
        MOV     E,M    					; low byte
        DCX     H      					; point back
        MOV     D,M    					; high back
        SHLD    ADDR2  					; save location
        LXI     H,EV3  					; get return address
        PUSH    H      					; save on stack
        PUSH    D      					; save address
        CALL    ARG    					; go get 1st arg
        POP     H      					; get h,l address
        PCHL           					; go execute the function
EV3     EQU     $      					; functions return here
        LHLD    ADDR2  					; get addr func
        INX     H      					; point lo
        INX     H      					; point type
        MOV     A,M    					; load it
        ANI     0E0H   					; mask it
        CPI     0C0H   					; test if string
        JZ      EV4    					; brif is
        POP     B      					; get ctrs
        LHLD    SPCTR  					; get counter
        INX     H      					; plus
        INX     H      					; two words
        SHLD    SPCTR  					; store it
        LXI     H,0    					; load zero to h,l
        PUSH    H      					; get block of
        PUSH    H      					; bytes
        DAD     SP     					; get stack addr
        PUSH    B      					; save ctrs
        PUSH    H      					; save addr
		CALL	SFPA					; go store the value
        MVI     A,0E3H 					; type=num
EV3A:   POP     D      					; get addr in stack
        LHLD    ADDR1  					; get addr lst arg
        MOV     M,A    					; store type code
        DCX     H      					; point one back
        MOV     M,E    					; store lo addr
        DCX     H      					; point back
        MOV     M,D    					; store hi addr
        LHLD    ADDR2  					; get location function
        INX     H      					; point lo
        INX     H      					; point type
        MOV     A,M    					; load type
        MOV     B,M    					; get type
		CALL	INCPC					; adjust h,l
        DB      -3
        MOV     A,B    					; load type
        POP     B      					; restore ctrs
        ANI     18H    					; isolate #args
        RAR            					; shift right
        RAR            					; again
        RAR            					; once more
        MOV     D,A    					; save it
        ADD     D      					; times 2
        ADD     D      					; times 3
        INR     B      					; point
        INR     B      					; lst posit in loc
        CALL    SQUIS  					; go compress stack
        JMP     EVAL   					; start at beginning
EV4:    LXI     D,STRIN					; point string buffer
        LDAX    D      					; load it
        RAR            					; divide by two
        INR     A      					; add 1
        LHLD    SPCTR  					; get sp count
        MOV     C,A    					; save lo
        MVI     B,0    					; set hi
        DAD     B      					; add number words
        SHLD    SPCTR  					; save sp count
        LXI     H,0    					; get some zeros
        POP     B      					; get ctrs
EV4A:   PUSH    H      					; get 1 word
        DCR     A      					; decr ctr
        JNZ     EV4A   					; loop
        DAD     SP     					; get address in h,l
        PUSH    B      					; re-save ctrs
        PUSH    H      					; save addr
        LDAX    D      					; get count
        INR     A      					; plus one
        MOV     B,A    					; save it
EV4B:   LDAX    D      					; get a byte
        MOV     M,A    					; store it
        INX     D      					; point next
        INX     H      					; ditto
        DCR     B      					; decr ctr
        JNZ     EV4B   					; loop
        MVI     A,0E7H 					; type code
        JMP     EV3A   					; continue
EV5:    CPI     5      					; test if open paren
        JNZ     EV6    					; brif not
        MVI     A,1    					; delete 1 byte
        CALL    SQUIS  					; go compress it
        LHLD    ADDR3  					; restore statement pointer
        LDA     DIMSW  					; get subsr switch
        ORA     A      					; test it
        JZ      LOOKO  					; brif not in subs^cript
        LDA     PARCT  					; get open paren count
        ORA     A      					; test
        JNZ     LOOKO  					; brif not zero
        JMP     EVAL   					; else evaluate complete subscr
EV6:    ORA     A      					; test if end of expression
        JNZ     EV9    					; brif not
        LDA     DIMSW  					; get dim sw
        ORA     A      					; test it
        CNZ     EDM1   					; brif not off
        MOV     A,C    					; get term count
        CPI     1      					; test if one
        JNZ     STERR  					; error if not one
        INX     H      					; point high addr
        INX     H      					; same
        MOV     D,M    					; high to d
        INX     H      					; point low
        MOV     E,M    					; low to e
        CALL    EVLD   					; go load value
        LHLD    SPCTR  					; get stack ctr
EV7:    MOV     A,L    					; get lo byte
        ORA     H      					; plus hi
        JZ      DV8    					; brif zero
        POP     D      					; return 2 bytes
        DCX     H      					; decr ctr
        JMP     EV7    					; loop
DV8:    LDA     DIMSW  					; get dim sw
        ORA     A      					; test it
        CNZ     EDM4   					; brif on
        LHLD    ADDR3  					; restore statement ptr
        RET            					; return to statement processor
EV9:    CPI     21H    					; test if plus
        LXI     D,FADDJ					; addr
        JZ      EV10   					; brif is
        CPI     25H    					; test if minus
        LXI     D,FSUB 					; addr
        JZ      EV10   					; brif is
        CPI     41H    					; test if mul
        LXI     D,FMUL 					; addr
        JZ      EV10   					; brif is
        CPI     45H    					; test if div
        LXI     D,FDIV 					; addr
        JZ      EV10   					; brif is
        CPI     1      					; test if comma
        JZ      EVCOM  					; brif is
        CPI     61H    					; test if unary minus
        JZ      EVNEG  					; brif is
        CPI     81H    					; test if exponential
        LXI     D,POWER					; addr
        JNZ     STERR  					; error if not
EV10:   INX     H      					; point to
        INX     H      					; 1st data
        PUSH    B      					; save ctrs
        PUSH    D      					; save routine addr
        MOV     D,M    					; high to d
        INX     H      					; point next
        MOV     E,M    					; low to e
        PUSH    H      					; save pointer
        CALL    EVLD   					; go load value
        POP     H      					; restore h,l
        INX     H      					; point 2nd data
        INX     H      					; same
        MOV     D,M    					; high to d
        INX     H      					; point next
        MOV     E,M    					; low to e
        INX     H      					; point next
        LDA     NS     					; get prev type
        CMP     M      					; test this type
        JNZ     SNERR  					; brif mixed mode
        DCX     H      					; point back
        XTHL           					; pop addr from stack, push h onto
        LXI     B,EV11 					; return address
        PUSH    B      					; save on stack
        PUSH    H      					; save jump addr
        XCHG           					; put var addr to h,l
        RET            					; fake call to routine
FADDJ:  CPI     0E7H   					; test if strings
        JZ      CONCA  					; brif is
        JMP     FADD   					; else, go add
POWER:  PUSH    H      					; save addr of var
        LXI     H,TEMP1					; point save area
		CALL	SFPA					; go store the value
        POP     H      					; restore h,l
		CALL	LDFPA					; load it
        CALL    FTEST  					; test for zero
        JZ      SGN1   					; give result = 1 if power = 0
        LXI     H,TEMP7					; point save area
		CALL	SFPA					; go store the value
        LXI     H,TEMP1					; point x
		CALL	LDFPA					; load it
        CALL    FTEST  					; test for zero
        RZ             					; 0^x = 0
        CALL    LN     					; get natural lnrithm
        LXI     H,TEMP7					; point b
        CALL    FMUL   					; go multiply
        JMP     EXP    					; get exp func
										; x^b = exp(b*ln(x))
XSQR:   LXI     H,TEMP1					; point x
		CALL	LDFPA					; load it
        LXI     H,TEMP1					; point x
        JMP     FMUL   					; times x
EV11:   POP     H      					; get h,l
        POP     B      					; get ctrs
        DCX     H      					; point back
        DCX     H      					; and again
        CALL    GTEMP  					; go save facc
		CALL	INCPC					; adjust h,l
	        DB      -7
        MVI     A,4    					; delete 4 bytes
        CALL    SQUIS  					; go compress
        JMP     EVAL   					; continue
EVNEG:  INX     H      					; point back to op
        PUSH    B      					; save ctrs
        PUSH    H      					; save h,l
        INX     H      					; ditto
        MOV     D,M    					; get hi byte
        INX     H      					; point next
        MOV     E,M    					; get lo byte
        CALL    EVLD   					; go load var
        CALL    NEG    					; go negate it
        POP     H      					; get locatino
        POP     B      					; get ctrs
        CALL    GTEMP  					; go store facc in stack
		CALL	INCPC					; adjust h,l
        DB      -4
EVCOM:  MVI     A,1    					; delete 1 byte
        CALL    SQUIS  					; compress
        LXI     H,CMACT					; get count
        INR     M      					; incr
        JMP     EVAL   					; continue
EVLD:   INX     H      					; point type
        MOV     A,M    					; load it
        STA     NS     					; save it
        XCHG           					; save h,l in d,e
        CPI     0E7H   					; test if string
        JNZ     LDFPA   				; load floating point
        LXI     D,STRIN					; point buffer
        MOV     A,M    					; get count
        INR     A      					; add one
        MOV     B,A    					; save count
EVLD1:  MOV     A,M    					; get next
        STAX    D      					; store it
        INX     H      					; point next
        INX     D      					; ditto
        DCR     B      					; decr count
        JNZ     EVLD1  					; loop
        RET            					; return
;
EDM1:   MOV     A,C    					; get item count
        PUSH    H      					; save h,l
        CPI     1      					; test if 1
        JNZ     EDM3   					; brif not
        MVI     B,4    					; get count
        LXI     H,TEMP1					; point area
        CALL    ZEROM  					; go zero it
EDM2A:  POP     H      					; restore h,l
        MVI     C,1    					; set count
        RET            					; return
EDM3:   CPI     2      					; test if 2
        JNZ     SNERR  					; else, error
		CALL	INCPC					; adjust h,l
        DB      5
        MOV     D,M    					; get hi addr
        INX     H      					; point next
        MOV     E,M    					; get lo addr
        CALL    EVLD   					; load the arg
        LXI     H,TEMP1					; point area
		CALL	SFPA					; go store the value
        JMP     EDM2A  					; continue
EDM4:   CALL    FACDE  					; convert facc to d,e
        PUSH    D      					; put d,e to b,c
        POP     B
        PUSH    B      					; save col
        LXI     H,TEMP1					; point 2nd argument
		CALL	LDFPA					; load it
        CALL    FACDE  					; convert to d,e
        POP     B      					; get col
        XRA     A      					; get a zero
        STA     DIMSW  					; reset sw
        RET            					; return
LDV2A:  MOV     A,B    					; get hi name
        ORI     80H    					; set bit
        MOV     B,A    					; restore
        PUSH    B      					; save name
        XCHG           					; save h,l in d,e
        LDA     PARCT  					; get paren count
        PUSH    PSW    					; save
        XRA     A      					; clear reg a
        STA     PARCT  					; reset count
        LHLD    SPCTR  					; get stack counter
        PUSH    H      					; save it
        LXI     H,0    					; get a zero
        SHLD    SPCTR  					; reset ctr
        LHLD    EXPRS  					; get exprst
        PUSH    H      					; save it
        INX     H      					; point next
        MVI     M,0    					; set new start
        SHLD    EXPRS  					; save it
        LDA     DIMSW  					; get prev se
        PUSH    PSW    					; save it
        XCHG           					; restore h,l
        MVI     A,0FFH 					; get on value
        STA     DIMSW  					; set sw
        CALL    LOOKD  					; recursive call
        POP     PSW    					; get dim sw
        STA     DIMSW  					; replace it
        SHLD    ADDR3  					; save h,l
        POP     H      					; get exprst
        SHLD    EXPRS  					; save it
        POP     H      					; get stack counter
        SHLD    SPCTR  					; restore it
        POP     PSW    					; get paren count
        STA     PARCT  					; restore it
        POP     H      					; get name
        PUSH    D      					; save row
        PUSH    B      					; save col
        XCHG           					; put name in d,e
        CALL    SEARC  					; go find address (put in d,e)
        POP     D      					; get addr
        POP     B      					; restore col
        POP     D      					; restore row
        CALL    SUBSC  					; get subscript (returns addr in h,l)
        XCHG           					; save in d,e
        LHLD    ADDR3  					; get h,l
        PUSH    H      					; save on stack
        JMP     LDV    					; continue
;      
;--------------------------------------------------------
; FLOATING POINT INPUT CONVERSION ROUTINE
;
; THIS SUBROUTINE CONVERTS AN ASCII STRING OF CHARACTERS
; TO THE FLOATING POINT ACCUMULATOR.  THE INPUT FIELD
; MAY CONTAIN ANY VALID NUMBER, INCLUDING SCIENTIFIC
; NOTATION (NNN.NNNNE+NN).
; THE INPUT STRING IS TERMINATED BY ANY NON-NUMERIC CHAR
;--------------------------------------------------------
;
FIN:    XCHG           					; put addr to d,e
        MVI     C,0    					; initial value excess digit count
        CALL    FIN8   					; get integer portion
        MVI     B,0    					; clear digit count
        CPI     '.'    					; test if dec-point
        JNZ     FIN2   					; brif not
        CALL    FIN9   					; get fraction
FIN2:   POP     PSW    					; get sign
        ORI     24     					; set up for float
        STA     FACC
        MOV     A,B    					; get # fraction digits
        ADD     C      					; + excess digits
        PUSH    PSW    					; save power of ten
        PUSH    D      					; save ptr
        CALL    FNORM  					; normalize number
        LDAX    D      					; get next character
        CPI     'E'    					; test if exponent
        JNZ     FIN4   					; brif not
        LXI     H,FTEMP					; point save area
		CALL	SFPA					; go store the value
        POP     D      					; restore ptr
        INX     D      					; skip 'e'
        CALL    FIN8   					; get numeric exp
        LDA     FACC+3 					; get exponent
        POP     B      					; exponent sign
        INR     B      					; test
        JP      FIN3   					; brif not neg
        CMA            					; negate exponent
        INR     A
FIN3:   POP     B      					; power of ten
        ADD     B      					; add exponent
        PUSH    PSW    					; save count
        LXI     H,FTEMP					; restore number
        PUSH    D      					; save ptr
		CALL	LDFPA					; load it
FIN4:   POP     H      					; restore ptr
        POP     PSW    					; restore count
FIN5:   RZ             					; return if zero
        PUSH    H      					; save h,l
        LXI     H,TEN  					; point constant: 10
        JM      FIN7   					; brif divide needed
        DCR     A      					; decr count
        PUSH    PSW    					; save count
        CALL    FMUL   					; go multiply by 10
FIN6:   POP     PSW    					; restore count
        POP     H      					; restore h,l
        JMP     FIN5   					; continue
FIN7:   INR     A      					; incr count
        PUSH    PSW    					; save count
        CALL    FDIV   					; go divide by 10
        JMP     FIN6   					; loop
;
;---------------------------------------------------
; FIN8  CONVERT NUMBER STRING TO FACC
; ON ENTRY, C=INIT VALUE EXCESS DIGIT COUNT
;            DE=INPUT STRING
; ON EXIT, SIGN IS ON STACK
;      B=DIGIT COUNT
;      C=EXCESS DIGIT COUNT
;---------------------------------------------------
;
FIN8:   LXI     H,FACC 					; clear facc
        MVI     B,4
        CALL    ZEROM
        LXI     H,8000H					; assume minus
        LDAX    D      					; get char
        CPI     '-'
        JZ      FIN8A
        MOV     H,L    					; nope, must be plus
                       					; (b is cleared by zerom)
        CPI     '+'
        JZ      FIN8A
        DCX     D      					; neither, back up pointer
FIN8A:  XTHL           					; get return, push sign
        PUSH    H      					; restore return
FIN9:   INX     D      					; point next
        LDAX    D      					; get char
        CPI     '0'    					; test if less zero
        RC             					; return if is
        CPI     '9'+1  					; test if gt nine
        RNC            					; return if is
        DCR     B      					; digit count
        PUSH    D      					; save ptr
        PUSH    B      					; save counters
        CALL    FMTEN  					; multiply facc*ten
        ORA     A      					; test for overflow
        JZ      FINB   					; brif no overflow
        LXI     H,FTEMP+4
		CALL	LDFPA					; load it
        POP     B      					; restore counters
        INR     C      					; excess digit
        POP     D
        JMP     FIN9
FINB:   POP     B      					; rstore counters
        POP     D      					; & ptr
        LDAX    D      					; get the digit
        ANI     0FH    					; mask off zone
        LXI     H,FACC+3       					; point acc
        ADD     M      					; add
        MOV     M,A    					; store
        DCX     H      					; point next
        MOV     A,M    					; load
        ACI     0      					; plus carry
        MOV     M,A    					; store
        DCX     H      					; point next
        MOV     A,M    					; load
        ACI     0      					; plus carry
        MOV     M,A    					; store
        JMP     FIN9   					; loop
;
;---------------------------------------------------
; MULTIPLY FACC BY TEN
;---------------------------------------------------
;
FMTEN:  LXI     H,FTEMP+4
		CALL	SFPA					; go store the value
        CALL    FIND   					; *2
        CALL    FIND   					; *4
        LXI     H,FTEMP+7
        CALL    FIND0  					; *5
FIND:   LXI     H,FACC+3       			; double facc
FIND0:  LXI     D,FACC+3
        MVI     B,4    					; byte count
        JMP     FADDT  					; add & return
;
;--------------------------------------------------------
; FLOATING POINT OUTPUT FORMAT ROUTINE
;
; THIS SUBROUTINE CONVERTS A NUMBER IN FACC TO A
; FORMAT SUITABLE FOR PRINTING.  THAT IS, THE
; NUMBER WILL BE IN SCIENTIFIC NOTATION IF EXPONENT
; IS > 5 OR < -2, OTHERWISE IT will be zero supressed
; ON BOTH SIDES.
;--------------------------------------------------------
;
FOUT:   LXI     D,FACC+3       			; point lsb
        LDAX    D      					; load it
        ORI     7      					; mask for output
        STAX    D      					; replace
        CALL    FTEST  					; get sign of number
        MVI     M,' '  					; default space
        JP      FOUT0  					; brif not minus
        MVI     M,'-'  					; move dash
FOUT0:  INX     H      					; point next
        JNZ     FOUT2  					; brif not zero
        MVI     M,'0'  					; move the zero
        INX     H      					; point next
        MVI     M,' '  					; move space following
        RET            					; return
FOUT2:  LDA     FACC   					; get sign & exp
        CALL    FEXP   					; expand exponent
        JNZ     FOUTV  					; brif not zero
        MVI     A,80H  					; set neg
FOUTV:  ANI     80H    					; isolate
        STA     DEXP   					; save sign
        PUSH    H      					; save h,l
FOUT3:  LDA     FACC   					; get sign & exp
        CALL    FEXP   					; expand exp
        CPI     1      					; test range
        JP      FOUT6  					; brif in range
FOUT4:  LXI     H,DEXP 					; point dec.exp
        INR     M      					; incr it
        LXI     H,TEN  					; point const: 10
        JP      FOUT5  					; brif pos.
        CALL    FMUL   					; multiply
        JMP     FOUT3  					; loop
FOUT5:  CALL    FDIV   					; divide
        JMP     FOUT3  					; loop
FOUT6:  CPI     5      					; test high range
        JP      FOUT4  					; brif 5 or greater
        LXI     H,FTEMP					; point save area
		CALL	SFPA					; go store the value
        LDA     FACC   					; get exponent
        CALL    FEXP   					; expand
        MVI     C,6    					; digit count
        CALL    FOUTB  					; shift left
        CPI     10     					; test if decimal point
        JM      FOUTU  					; brif lt
        LXI     H,FTEMP					; point save area
		CALL	LDFPA					; load it
        JMP     FOUT4  					; once more
FOUTU:  CALL    FOUT9  					; put digit
FOUT7:  XRA     A      					; clear status
        STA     FACC   					; and overflow
        CALL    FMTEN  					; multiply by ten
        CALL    FOUT9  					; put digit
        JNZ     FOUT7  					; loop
        JMP     FOUTH  					; go around
FOUT9:  ORI     30H    					; dec. zone
        POP     H      					; get return addr
        XTHL           					; exch with top (ptr)
        MOV     M,A    					; put digit
        INX     H      					; point next
        MOV     A,C    					; get count
        CPI     6      					; test if 1st
        JNZ     FOUTA  					; brif not
        MVI     M,'.'  					; move dec. pt.
        INX     H      					; point next
FOUTA:  XTHL           					; exch with rtn
        DCR     C      					; decr count
        PCHL           					; return
FOUTB:  MOV     E,A    					; save bit count
        XRA     A      					; clear acc flags
        STA     FACC   					; and overflow
FOUTC:  LXI     H,FACC+3       			; point lsb
        MVI     B,4    					; byte count
FOUTD:  MOV     A,M    					; get a byte
        RAL            					; shift left
        MOV     M,A    					; store
        DCX     H      					; point next
        DCR     B      					; decr ctr
        JNZ     FOUTD  					; loop
        DCR     E      					; decr bit ctr
        JNZ     FOUTC  					; loop
        RET            					; return
FOUTH:  POP     H      					; get ptr
        MVI     M,'E'  					; exponent
        INX     H      					; point next
        LDA     DEXP   					; get exponent
        MVI     M,'+'  					; default
        MOV     D,A    					; save number
        ORA     A      					; test it
        JP      FOUTI  					; brif pos
        MVI     M,'-'  					; else, dash
        ANI     7FH    					; strip dumb sign
        CMA            					; complement
        INR     A      					; plus one (twos comp)
        MOV     D,A    					; save it
        CMA            					; re-complement
        INR     A      					; plus one
FOUTI:  INX     H      					; point next
        PUSH    H      					; save ptr
        MVI     E,-1   					; init ctr (tens)
FOUTJ:  INR     E      					; add one
        SUI     10     					; less 10
        JP      FOUTJ  					; loop
        ADI     10     					; correct units
        MOV     B,A    					; save units
        MOV     A,E    					; get tens
        CALL    FOUT9  					; output
        MOV     A,B    					; get units
        CALL    FOUT9  					; output
        POP     H      					; get ptr
        MVI     M,' '  					; space after
        MOV     A,D    					; get dec expon
        ORA     A      					; set flags
        JP      FOUTK  					; brif pos.
        CPI     -2     					; test for min
        RC             					; return if less than -2
        JMP     FOUTL  					; go around
FOUTK:  CPI     6      					; test if too big
        RNC            					; return if 6 or greater
FOUTL:  MOV     C,A    					; save exponent
        MVI     B,5    					; ctr
FOUTM:  MVI     M,' '  					; space out exponent
        DCX     H      					; point prior
        DCR     B      					; decr ctr
        JNZ     FOUTM  					; loop
        XCHG           					; flip/flop
        MOV     A,E    					; get low byte
        SUI     5      					; point to dot
        MOV     L,A    					; put down
        MOV     A,D    					; get high
        SBI     0      					; in case of borrow
        MOV     H,A    					; put down
        MOV     A,C    					; get exponent
        ORA     A      					; test sign
        JZ      FOUTO  					; brif zero
        JM      FOUTR  					; brif negative
FOUTN:  MOV     B,M    					; get high byte
        INX     H      					; point next
        MOV     A,M    					; get low byte
        MOV     M,B    					; shift dot to right
        DCX     H      					; point back
        MOV     M,A    					; move the digit left
        INX     H      					; point next
        DCR     C      					; decr ctr
        JNZ     FOUTN  					; loop
FOUTO:  XCHG           					; point end
FOUTP:  MOV     A,M    					; get a digit/dot
        CPI     '0'    					; test for trailing zero
        JNZ     FOUTQ  					; brif not
        MVI     M,' '  					; space fill
        DCX     H      					; point prior
        JMP     FOUTP  					; loop
FOUTQ:  CPI     '.'    					; test for trailing dot
        INX     H      					; just in case not
        RNZ            					; return if not
        DCX     H      					; reset ptr
        MVI     M,' '  					; space it out
        RET            					; return
FOUTR:  CPI     0FFH   					; test if -1
        JNZ     FOUTS  					; else -2
        DCX     H      					; point significant
        MOV     A,M    					; get the char
        MVI     M,'.'  					; move the dot
        INX     H      					; point next
        MOV     M,A    					; shift the digit
        JMP     FOUTO  					; go zero suppress
FOUTS:  DCX     H      					; point one to left
        MOV     A,M    					; pick up digit
        MVI     M,'0'  					; replace
        INX     H      					; point right
        MOV     M,A    					; put the digit
        MOV     H,D    					; get low addr
        MOV     L,E    					; point last digit
        MVI     B,6    					; ctr
FOUTT:  DCX     H      					; point prito
        MOV     A,M    					; get a digit
        INX     H      					; point
        MOV     M,A    					; put it one to right
        DCX     H      					; point
        DCR     B      					; decr ctr
        JNZ     FOUTT  					; loop
        MVI     M,'.'  					; move the dot
        JMP     FOUTO  					; continue
;
;--------------------------------------------------------
; FLOATING POINT ADD THE NUMBER AT (H,L) TO THE FACC
;--------------------------------------------------------
;
FADD:   INX     H      					; point first digit
        MOV     A,M    					; load it
        ORA     A      					; test it
        JZ      FTEST  					; brif zero
        DCX     H      					; point back
        CALL    FTEST  					; go test sign of facc
        JZ      LDFPA   				; just load if facc = 0
        CALL    FEXP   					; go get exponent
        MOV     B,A    					; save exponent
        MOV     A,M    					; get exponent of addr
        CALL    FEXP   					; go get exponent
        MOV     C,A    					; save the exponent
        SUB     B      					; get difference of two exponents
        JZ      FADD4  					; brif they're eq
        JP      FADD3  					; brif difference is positive
        CMA            					; complement acc
        INR     A      					; plus one (two's complement)
FADD3:  CPI     24     					; compare difference to max
        JC      FADD4  					; brif less
        MOV     A,B    					; get expon of adduend
        SUB     C      					; get true difference again
        JP      FTEST  					; brif facc > adder
        JMP     LDFPA   				; else, adder > facc
FADD4:  PUSH    PSW    					; save difference
        PUSH    B      					; save exponents
        LXI     D,FTEMP					; get addr of temp acc
        CALL    CPY4H
        POP     B      					; get exponents
        POP     PSW    					; get difference
        JZ      FADD9  					; just add if zero
        LXI     H,FTEMP+1      			; default
        PUSH    PSW    					; save difference
        MOV     A,B    					; get facc expon
        SUB     C      					; minus ftemp expon
        JP      FADD6  					; brif temp must be shifted
        LXI     H,FACC 					; point float acc
        MOV     A,C    					; get exponent, sign
        ANI     7FH    					; strip exp sign
        MOV     C,A    					; put back
        MOV     A,M    					; get the exp
        ANI     80H    					; strip off old expon
        ORA     C      					; move addr expon to it
        MOV     M,A    					; replace
        INX     H      					; point first data byte
FADD6:  POP     PSW    					; get differ
        MOV     C,A    					; save it
FADD7:  MVI     B,3    					; loop ctr (inner)
        XRA     A      					; init carry to z
        PUSH    H      					; save addr
        CALL    FSHFT  					; go shift
        POP     H      					; get addr
        DCR     C      					; decr ctr
        JNZ     FADD7  					; loop
FADD9   EQU     $
        LXI     H,FTEMP
        LDA     FACC   					; get exponent
        XRA     M      					; see if signs the same
        LXI     D,FACC+3       			; point least sign byte
        LXI     H,FTEMP+3
        JM      FADDA  					; brif signs different
        CALL    FADT3  					; add 3 bytes
        JNC     FTEST  					; brif no overflow
        XCHG           					; point hl to facc
        CALL    SVSGN  					; save sign, return exponent
        INR     A      					; increment exponent
        CALL    RSSGN  					; restore sign to exponent
        INX     H      					; point data
        STC            					; set cy
        MVI     B,3    					; ctr
        CALL    FSHFT  					; go shift it
        JMP     FTEST  					; return
FADDA   EQU     $
        MVI     B,3
        CALL    FSUBT  					; subtract
        JNC     FNORM  					; brif no borrow
        LXI     H,FACC+3       			; must negate
        MVI     B,3
        STC
FNEG1:  MOV     A,M    					; get byte
        CMA
        JNC     FNEG2
        ADI     1      					; increment + complement=negate
FNEG2:  MOV     M,A
        DCX     H
        DCR     B
        JNZ     FNEG1
        CALL    FNORM
        JMP     NEG    					; reverse sign
;
;--------------------------------------------------------
; NORMALIZE THE FLOATING ACCUMULATOR
; THAT IS, THE FIRST BIT MUST BE SIGNIFICANT
;--------------------------------------------------------
;
;
FNORM:  LXI     H,FACC+3       			; point lsb
        MOV     A,M    					; load it
        DCX     H      					; point prior
        ORA     M      					; merge
        DCX     H      					; point prior
        ORA     M      					; merge
        DCX     H
        MOV     B,M    					; save exponent
        MOV     M,A    					; clear
        RZ             					; return on nothing to normalize
        MOV     M,B    					; restore exp
        PUSH    B      					; save c for caller
        CALL    SVSGN  					; save sign
        MOV     M,A    					; store expanded exponent
FNRM1:  INX     H      					; point to most sign byte
        MOV     A,M    					; get msb
        ORA     A      					; test it
        JM      FNRM3  					; brif normalized
        INX     H      					; point lsb
        INX     H
        MVI     B,3    					; shift count
FNRM2:  MOV     A,M    					; shift left
        RAL
        MOV     M,A
        DCX     H
        DCR     B
        JNZ     FNRM2
        DCR     M      					; adjust exponent
        JMP     FNRM1  					; loop
FNRM3:  DCX     H      					; point back to exponent
        MOV     A,M
        CALL    RSSGN  					; restore sign
        POP     B      					; restore c
        RET
;
;
;--------------------------------------------------------
; FLOATING POINT SUBTRACT THE NUMBER AT (H,L) FROM THE FACC
;--------------------------------------------------------
;
FSUB:   CALL    NEG    					; negate facc
        CALL    FADD   					; add
        CALL    NEG    					; negate result
        JMP     FTEST
;
;---------------------------------------------------
; FLOATING POINT MULTIPLY THE NUMBER AT (H,L) TO THE FACC
;---------------------------------------------------
;
FMUL:   CALL    FTEST  					; test facc
        RZ             					; return if zero
        INX     H      					; point 1st digit of multiplier
        MOV     A,M    					; load it
        DCX     H      					; restore
        ORA     A      					; test if zero
        JZ      LDFPA   				; go load to facc if it is
        PUSH    H      					; save multiplier address
        CALL    MDSGN  					; get sign product, & both exponents
        ADD     B      					; add exponents
        CALL    RSSGN  					; restore sign
        POP     H      					; restore
        LXI     D,FTEMP+9      			; point temp storage
        MVI     B,3    					; byte count
        INX     H      					; point msd
        CALL    COPYH  					; move multiplier
        LXI     H,FTEMP					; point digit 7 of result
        MVI     B,6    					; loop ctr
        CALL    ZEROM  					; go zero eight bytes
        LXI     D,FACC+1       			; point 1st digit of acc
        MVI     B,3    					; loop ctr
FMUL5:  LDAX    D      					; get an acc digit pair
        MOV     M,A    					; put to temp storage
        XRA     A      					; zero a
        STAX    D      					; clear acc
        INX     D      					; point next
        INX     H      					; ditto
        DCR     B      					; decr ctr
        JNZ     FMUL5  					; loop
        MVI     C,24   					; outter loop ctr
FMUL6:  MVI     B,3    					; ctr
        LXI     H,FTEMP+9      			; point multiplier
        XRA     A      					; clear cy
FMUL7:  MOV     A,M    					; get byte
        RAR            					; shift right
        MOV     M,A    					; put down
        INX     H      					; point next
        DCR     B      					; decr ctr
        JNZ     FMUL7  					; loop
        JNC     FMUL8  					; brif zero bit
        LXI     D,FTEMP+2      			; point result
        LXI     H,FTEMP+8      			; point multiplicand
        MVI     B,6    					; six byte add
        CALL    FADDT  					; go add
FMUL8:  MVI     B,6    					; siz byte shift
        LXI     H,FTEMP+8      			; point multiplicand
        XRA     A      					; clear cy
FMUL9:  MOV     A,M    					; get byte
        RAL            					; shift left
        MOV     M,A    					; put bact
        DCX     H      					; point next byte
        DCR     B      					; decr ctr
        JNZ     FMUL9  					; loop
        DCR     C      					; dec bit count
        JNZ     FMUL6  					; continue
        JMP     FNORM  					; go normalize
;
;---------------------------------------------------
; MDSGN   GET SIGN PRODUCT AND EXPONENTS FOR MULT & DIV
; ON ENTRY:
;      (HL) = ONE NUMBER
;      (FACC)=THE OTHER
; ON RETURN:
;      A = EXPONENT OF FACC(EXPANDED)
;      B = OTHER EXPONENT
;      C = SIGN PRODUCT
;      HL DESTROYED
;---------------------------------------------------
;
MDSGN:  CALL    SVSGN  					; get sign in c, exp in a
        MOV     B,A    					; save exponent
        LXI     H,FACC
        MOV     A,C    					; get sign
        ADD     M      					; multiply signs
        MOV     M,A    					; put down
;
;---------------------------------------------------
; SVSGN         GET SIGN AND EXP
; ON ENTRY:
;      (HL) = EXPONENT
; ON RETURN:
;      A = EXPANDED EXPONENT
;      C = SIGN IN HI ORDER BIT
;---------------------------------------------------
;
SVSGN:  MOV     A,M    					; get exponent
        ANI     80H    					; isolate sign
        MOV     C,A
        MOV     A,M
        JMP     FEXP   					; expand exp and return
;
;---------------------------------------------------
; RSSGN         RESTORE SIGN TO EXPONENT
; ON ENTRY:
;      (HL)=EXPONENT
;      A = EXPANDED EXPONENT
;      C = SIGN
; ON RETURN:
;      A = EXPONENT
;      (HL) = EXPONENT WITH SIGN
;      Z,M BITS SET FOR EXPONENT
;---------------------------------------------------
;
RSSGN:  CALL    FOVUN  					; check for over/underflow
        ANI     7FH    					; remove exponent sign
        ORA     C      					; add sign
        MOV     M,A    					; set down
        JMP     FTEST  					; set z,m bits
;
;---------------------------------------------------
; FLOATING POINT DIVIDE THE NUMBER AT (H,L) INTO THE FACC
;---------------------------------------------------
;
FDIV:   CALL    FTEST  					; test if facc zero
        RZ             					; return if it is
        INX     H      					; point 1st digit of divisor
        MOV     A,M    					; load it
        DCX     H      					; point back
        ORA     A      					; test if zero
        JZ      ZMERR  					; division by zero = error
        PUSH    H      					; save divisor ptr
        CALL    MDSGN  					; get sign on stack, exps into a,b
        SUB     B      					; subtract exponents
        INR     A      					; plus one
        CALL    RSSGN  					; set sign/exponent in facc
        LXI     D,FACC+1
        LXI     H,FTEMP					; point temporary storage
        MVI     M,0    					; clear msb
        INX     H      					; point next
        MVI     B,3    					; loop ctr
FDIV3:  LDAX    D      					; get byte from facc
        MOV     M,A    					; put to ftemp
        XRA     A      					; clear a
        STAX    D      					; zero facc
        INX     H      					; point next
        INX     D      					; ditto
        DCR     B      					; decr ctr
        JNZ     FDIV3  					; loop
        POP     D      					; get addr
        MVI     B,3    					; loop ctr
        INX     D      					; point msd of divisor
        MVI     M,0    					; clear msb
        INX     H      					; point next
        CALL    COPYD  					; go move it
        MVI     C,24   					; outer loop ctr
FDIV5:  LXI     D,FTEMP+3      			; point dividend
        LXI     H,FTEMP+7      			; and divisor
        MVI     B,4    					; ctr
        CALL    FSUBT  					; go subtract
        JNC     FDIV6  					; brif no go
        LXI     D,FTEMP+3      			; point dividend
        LXI     H,FTEMP+7      			; and divisor
        MVI     B,4    					; ctr
        CALL    FADDT  					; go re-add
        STC            					; turn on cy
FDIV6:  CMC            					; reverse cy
        MVI     B,3    					; ctr
        LXI     H,FACC+3       			; point lsb
FDIV7:  MOV     A,M    					; load byte
        RAL            					; shift left
        MOV     M,A    					; replace
        DCX     H      					; point next
        DCR     B      					; decr ctr
        JNZ     FDIV7  					; loop
        XRA     A      					; clear flags
        MVI     B,4    					; ctr
        LXI     H,FTEMP+3      			; point-dividend
FDIV8:  MOV     A,M    					; load byte
        RAL            					; shift left
        MOV     M,A    					; replace
        DCX     H      					; point enxt
        DCR     B      					; decr ctr
        JNZ     FDIV8  					; loop
        DCR     C      					; decr otr ctr
        JNZ     FDIV5  					; loop
        JMP     FNORM  					; wrapup
;
;---------------------------------------------------
; UTILITY ROUTINE TO GET A VARIABLE'S ADDRESS TO H,L
;---------------------------------------------------
;
GETST:  LXI     D,STRIN					; point buffer
        MVI     B,0    					; init ctr
        MOV     A,M    					; get the char
        CPI     '"'    					; test if lit type
        JZ      GETS2  					; brif is
        CPI     27H    					; test if quoted literal
        JZ      GETS2  					; brif is
GETS1:  CPI     ','    					; test if comma
        JZ      GETS5  					; brif is
        ORA     A      					; test if end
        JZ      GETS5  					; brif is
        INR     B      					; count it
        INX     D      					; point next
        STAX    D      					; put char
        INX     H      					; point next
		CALL	TSTC					; call to ram address
        JMP     GETS1  					; loop
GETS2:  MOV     C,A    					; save delim
GETS3:  INX     H      					; skip the quote
        MOV     A,M    					; get next char
        CMP     C      					; test if end of literal
        JZ      GETS4  					; brif is
        ORA     A      					; test if end of line
        JZ      CVERR  					; brif is
        INR     B      					; count it
        INX     D      					; point next
        STAX    D      					; put char
        JMP     GETS3  					; loop
GETS4:  INX     H      					; skip end quote
		CALL	TSTC					; call to ram address
GETS5:  LXI     D,STRIN					; point begin buffer
        MOV     A,B    					; get count
        STAX    D      					; put count
        POP     D      					; get return addr
        XCHG           					; flip/flop
        XTHL           					; put ret on stack, hl of var in hl
        PUSH    D      					; save h,l of loc
        CALL    LET2A  					; go store string
        POP     H      					; restore location
        RET            					; return
GETS8:  CALL    VAR    					; get var name
        PUSH    D      					; save on stack
        MOV     A,D    					; get hi byte
        ORA     A      					; test if array
        JP      GETS9  					; brif not
        CALL    SEARC  					; go get array params
        MVI     A,0FFH 					; turn on sw
        STA     DIMSW  					; set it
        XTHL           					; swap addr on stack
        CALL    EXPR   					; go get row, col ptrs
        XTHL           					; swap addr on stack
        CALL    SUBSC  					; go point to entry
        XCHG           					; exchange
        POP     H      					; get address of statement
        POP     B      					; get name
        RET            					; return
GETS9:  CALL    SEARC  					; find addr
        POP     B      					; restore name
        RET            					; return
;
;---------------------------------------------------
; TEST EXPONENT FOR OVERFLO OR UNDERFLOW
;---------------------------------------------------
;
FOVUN:  ORA     A      					; test it
        JP      FOV1   					; brif pos.
        CPI     0C1H   					; test for max neg
        RNC            					; return if no under.
        MVI     A,0C1H 					; set exponent at minimum
        JMP     UNERR
FOV1:   CPI     40H    					; test max pos
        RC             					; return if no over.
        MVI     A,3FH  					; set exponent at maximum
        JMP     OVERR
;
;---------------------------------------------------
; COMPUTES SUBSCR ADDR
; INPUT: B HAS ROW NUMBER (1ST SUB)
;       D HAS COL NUMBER (2ND SUB)
;       H HAS ADDR NAME
;---------------------------------------------------
;
SUBSC:  PUSH    D      					; save col
		CALL	INCPC					; adjust h,l
        DB      -4     					; by four
        MOV     D,M    					; get hi
        DCX     H      					; point lo
        MOV     E,M    					; get lo
        MOV     A,D    					; get hi
        CMP     B      					; compare
        JC      SNERR  					; brif excess
        JNZ     SUB1   					; brif not equal
        MOV     A,E    					; get lo
        CMP     C      					; compare
        JC      SNERR  					; brif excess
SUB1:   DCX     H      					; point hi cols
        MOV     D,M    					; load it
        DCX     H      					; point lo cols
        MOV     E,M    					; load it
        XTHL           					; save address
        PUSH    H      					; save sub col
        PUSH    D      					; save dim cols
        INX     D      					; make cols=max+1 (account for 0 b??ke
        LXI     H,0    					; get a zero
SUB2:   MOV     A,B    					; get hi
        ORA     C      					; plus lo
        JZ      SUB3   					; brif zero
        DAD     D      					; add once
        DCX     B      					; sub once
        JMP     SUB2   					; loop
SUB3:   POP     D      					; get dim col
        POP     B      					; get sub col
        MOV     A,D    					; get hi
        CMP     B      					; compare
        JC      SNERR  					; brif gt
        JNZ     SUB4   					; brif not zero
        MOV     A,E    					; get lo
        CMP     C      					; compare
        JC      SNERR  					; brif gt
SUB4:   DAD     B      					; add to prod
        DAD     H      					; times two
        DAD     H      					; times four
        MOV     A,L    					; get low
        CMA            					; complement
        ADI     1      					; plus one
        MOV     E,A    					; save it
        MOV     A,H    					; get hi
        CMA            					; complement
        ACI     0      					; plus carry
        MOV     D,A    					; save
        POP     H      					; get addr (0,0)
        DAD     D      					; compute (i,j) right side
		CALL	INCPC					; adjust h,l
        DB      -4
        RET            					; return
;
;---------------------------------------------------
; TEST THE SIGN OF THE NUMBER IN THE FACC
; RETURN WITH S & Z SET TO SIGN
;---------------------------------------------------
;
FTEST:  LDA     FACC+1 					; get msd
        ORA     A      					; test it
        RZ             					; return if zero
        LDA     FACC   					; get sign&expon byte
        ORI     7FH    					; test sign bit only
        LDA     FACC   					; re-load expon byte
        RET            					; then return
;
;---------------------------------------------------
; EXPAND EXPONENT INTO 8 BINARY BITS
;---------------------------------------------------
;
FEXP:   ANI     7FH    					; mask mantisa sign
        ADI     40H    					; propagate char sign to leftmost bit
        XRI     40H    					; restore original sign bit
        RET            					; return
;
;---------------------------------------------------
; SUBTRACT THE TWO MULTIPRECISION NUMBERS (D,E) & (H,L)
;---------------------------------------------------
;
FSUBT:  XRA     A      					; turn of cy
FSB1:   LDAX    D      					; get a byte
        SBB     M      					; sub other byte
        STAX    D      					; put down
        DCX     D      					; point next
        DCX     H      					; ditto
        DCR     B      					; decr ctr
        JNZ     FSB1   					; loop
        RET            					; return
;
;---------------------------------------------------
; ADD TWO MULTI-PRECISION NUMBERS (D,E) & (H,L)
;---------------------------------------------------
;
FADT3:  MVI     B,3
FADDT:  XRA     A      					; clear status
FAD1:   LDAX    D      					; get byte
        ADC     M      					; add other byte
        STAX    D      					; put down
        DCX     D      					; point next
        DCX     H      					; ditto
        DCR     B      					; decr loop ctr
        JNZ     FAD1   					; loop
        RET            					; return
;
;---------------------------------------------------
; INCREMENTING SHIFT RIGHT
;---------------------------------------------------
;
FSHFT:  MOV     A,M    					; get a byte
        RAR            					; shift right
        MOV     M,A    					; put down
        INX     H      					; point next
        DCR     B      					; decr ctr
        JNZ     FSHFT  					; loop
        RET            					; return
;
;---------------------------------------------------
; READ A LINE FROM THE TTY
; FIRST PROMPT WITH THE CHAR IN THE A REG
; TERMINATE THE LINE WITH A X'00'
; IGNORE EMPTY LINES
; CONTROL C WILL CANCEL THE LINE
; CONTROL O WILL TOGGLE THE OUTPUT SWITCH
; RUBOUT WILL DELETE THE LAST CHAR INPUT
;---------------------------------------------------
;
TERMI:  STA     PROMP  					; save the prompt char
REIN:   LXI     H,IOBUF					; point to input buffer
        MVI     M,0    					; mark begin
        INX     H      					; point start
        LDA     PROMP  					; get the prompt again
        CALL    TESTO  					; write to terminal
        CPI     '?'    					; test if q.m.
        JNZ     TREAD  					; brif not
        MVI     A,' '  					; get space
        CALL    TESTO  					; write to terminal
;
TREAD:	MVI		A,RTS_ON
		OUT     UARTC
;
READ0:	IN      UARTS  					; get tty status
        ANI     RX_MASK					; test if rxrdy
        JZ      READ0  					; loop til char
;		
        CALL    GETCH  					; go read the char
		PUSH	PSW
		MVI		A,RTS_OFF
		OUT     UARTC
		POP		PSW
;
        MOV     M,A    					; put in buffer
        CPI     0AH    					; test if line feed
        JZ      TREAD  					; ignore if it is
        CPI     0DH    					; test if cr
        JNZ     NOTCR  					; brif not
        LDA     TAPES  					; get paper tape switch
        RAR            					; test if load
        CNC     CRLF   					; cr/lf if not
CR1:    MVI     M,0    					; mark end
        LDA     ILSW   					; get input line sw
        ORA     A      					; test it
        RNZ            					; return if on
        DCX     H      					; point prior
        MOV     A,M    					; load it
        CPI     20H    					; test if space
        JZ      CR1    					; brif space
        ORA     A      					; test if at beginning
        JZ      REIN   					; brif is (null line)
        LXI     H,IOBUF+1      			; point begin
        RET            					; else, return
;		
;*********************************************************************************
; CHARACTER OUTPUT 
;*********************************************************************************
;
TESTO:  PUSH    PSW     				; save char
TEST1:  IN      UARTS   				; get status
		ANI		TX_MASK 				; test if txrdy
        JZ      TEST1   				; loop till ready
        POP     PSW     				; get char
        OUT     UARTD   				; write it
TEST2:  IN      UARTS					; get status
		ANI		TE_MASK					; charecter send ?
		JZ      TEST2					; wait until so
        RET             				; return
;
;*********************************************************************************
; CARIAGE RETURN TO DISPLAY 
;*********************************************************************************
;
CRLF:   MVI     A,0DH   				; load a CR
        CALL    TESTO   				; write it to console
        MVI     A,0AH   				; Load a LF
        CALL    TESTO   				; write it to console
		MVI		A,0						; Prepare 0 value
		STA		COLUM					; To reset current tab position
		RET             				; return to caller

NOTCR:  CPI     15H     				; test if control-u
        JNZ     NOTCO   				; brif not
        CALL    PRCNT   				; go print ^u
        CALL    CRLF    				; get cr/lf
        JMP     REIN    				; go re-enter
NOTCO:  CPI     7FH     				; test if rubout
        JNZ     NOTBS   				; brif not
        LDA     TAPES   				; get paper tape sw
        RAR             				; test if load
        JC      TREAD   				; ignore if load
        DCX     H       				; point prior
        MOV     A,M     				; load prev char
        ORA     A       				; test if begin
        JZ      ECHO    				; brif is
        MVI     A,'\\'  				; back slash
        CALL    TESTO   				; write it
        MOV     A,M     				; fetch character to be discarded
        CALL    TESTO   				; write it
        MVI     A,'\\'  				; back slash
        CALL    TESTO   				; write it
        JMP     TREAD   				; get replacement character
NOTBS:  EQU     $
	    CPI     8       				; test for ascii backspace
        JNZ     NOTCH   				; brif not control h
        DCX     H       				; point prior
        MOV     A,M     				; fetch character
        ORA     A       				; test for beginning
        JZ      ECHO    				; brif it is
        PUSH    H       				; save position
        LXI     H,RBOUT 				; point rubout sequence
        CALL    TERMM   				; write it
        POP     H       				; restore h,l
        JMP     TREAD   				; get replacement character
NOTCH:  LDA     TAPES   				; get paper tape switch
        RAR             				; flag to carry
        JC      ECHO    				; no echo if tape
        MOV     A,M     				; else, load the char
        CALL    TESTO   				; echo the charcter
ECHO:   INX     H      					; point next posit
        JMP     TREAD   				; loop for next
;
;---------------------------------------------------
; TTY PRINT ROUTINE
;
; OUTPUT STRING OF CHARS
; STARTING AT IOBUF +0 THRU END (FF OR FE OR 00)
; FOLLOWING IMBEDDED CHARACTERS ARE INTERPRETED AS CONTROLS:
; X'00' END OF BUFFER, TYPE CR/LF AND RETURN
; X'FE' END OF BUFFER, RETURN (NO CR/LF)
; X'FD' TYPE CR/LF, CONTINUE
;
; RETURN WITHOUT OUTPUT IF OUTPUT SW IS OFF
;---------------------------------------------------
;
TERMO:  LDA     OUTSW  					; get output sw
        ORA     A      					; test it
        RNZ            					; return if no print
        LXI     H,IOBUF					; point i/o buffer
OT1:    MOV     A,M    					; load a byte
        CPI     0FEH   					; see if end of line (no cr/lf)
        RZ             					; return if equal
        CPI     0FDH   					; see if imbedded cr/lf
        JNZ     OT2    					; brif not
        CALL    CRLF   					; line feed
        JMP     OT4    					; continue
OT2:    ORA     A      					; test if end of output
        JZ      CRLF   					; brif is
        MOV     A,M    					; load the byte
        CALL    TESTO  					; type it
        LDA     COLUM  					; get column pointer
        INR     A      					; add one
        STA     COLUM  					; restore it
OT4:    INX     H      					; point next
        JMP     OT1    					; loop
TERMM   EQU     OT1
;
;---------------------------------------------------
; POSITION TTY AT NEXT TAB STOP
;---------------------------------------------------
;
;
TABST:  LDA     OUTSW  					; get output switch
        ORA     A      					; test it
        RNZ            					; return if suppressed
        LDA     COLUM  					; get column pointer
        CPI     56     					; compare to 56
        JNC     CRLF   					; brif no room left
        MOV     B,A    					; save it
        XRA     A      					; init position
TBLP:   CMP     B      					; compare
        JZ      TBLP2
        JNC     TBON   					; brif shy of tab
TBLP2:  ADI     14     					; point next stop
        JMP     TBLP   					; loop
TBON:   STA     COLUM  					; update ctr
        SUB     B      					; compute number of spaces
        MOV     B,A    					; save it
TBSPA:  MVI     A,' '  					; space to reg a
        CALL    TESTO  					; output it
        DCR     B      					; sub 1 from ctr
        RZ             					; return if zero
        JMP     TBSPA  					; else, loop
;
;---------------------------------------------------
; UNPACK LINE NUMBER FROM (H,L) TO (D,E)
; ZERO SUPPRESS LEADING ZEROS
;---------------------------------------------------
;
;
LINEO:  PUSH    B       				; push b,c
        MVI     B,1     				; set switch
        CALL    LOUT    				; go format 2 bytes
        CALL    LOUT    				; then the next 2
        POP     B       				; restore b,c
        RET             				; return
;
LOUT:   MOV     A,M     				; get byte
        ANI     0F0H    				; isolate left half
        RAR             				; shift right 1 bit
        RAR             				; again
        RAR             				; again
        RAR             				; last time
        JNZ     NOTZ1   				; brif not zero
        ORA     B       				; merge in b
        JNZ     Z1      				; brif zero
NOTZ1:  MVI     B,0     				; reset switch
        ORI     30H     				; zone
        STAX    D       				; put to buffer
        INX     D       				; point next
Z1:     MOV     A,M     				; load byte
        ANI     0FH     				; mask
        JNZ     NOTZ2   				; brif not zero
        ORA     B       				; merge switch
        JNZ     Z2      				; brif zero
NOTZ2:  MVI     B,0     				; set switch off
        ORI     30H     				; zone
        STAX    D       				; put to buffer
        INX     D       				; point to next
Z2:     INX     H       				; and next line byte
        RET             				; return
;
;
;---------------------------------------------------
; CHARACTER INPUT 
; TEST IF KEY WAS PRESSED DURING EXECUTION
; CANCEL IF CONTROL-C
; TOGGLE OUTPUT SUPPRESS SW IF CONTROL-O
;---------------------------------------------------
;
TSTCC:  MVI		A,RTS_ON				; Activate Request to send
		OUT 	UARTC					; Send to UART	
		IN      UARTS					; get tty status
        ANI     RX_MASK					; mask for rxrdy
        RZ              				; return if no char
;		
GETCH:	MVI		A,RTS_ON				; Activate Request to send
		OUT 	UARTC					; To UART

GETCH0:	IN      UARTS					; get tty status
        ANI     RX_MASK					; Character ready ?
		JZ		GETCH0					; NO wait until one is recieved
		IN      UARTD					; read the char
		PUSH	PSW						; Save it
		MVI		A,RTS_OFF				; Reset RTS
		OUT 	UARTC					; to uart
		POP 	PSW						; Restore character
		ANI     7FH     				; turn off high bit 
        CPI     3       				; test if control c
        JNZ     TSTC1   				; brif not
        CALL    PRCNT   				; go print ^c
        LDA     EDSW    				; get mode sw
        ORA     A       				; test it
        JNZ     KEY    					; **;brif command mode
        LXI     H,STOPM 				; point msg
        CALL    TERMM   				; go print it
        CALL    PRLIN   				; go print line
        JMP     KEY     				; goto ready
TSTC1:  CPI     0FH     				; test if control o
        RNZ             				; return if not
        CALL    PRCNT   				; go print ^o
        LDA     OUTSW   				; get output swtich
        XRI     1       				; toggle
        STA     OUTSW   				; put sw
        RET             				; return
;
;---------------------------------------------------
; PRINTS ^ AND CHAR
;---------------------------------------------------
;
PRCNT:  PUSH    PSW     				; save char
        MVI     A,'^'   				; get up arrow
        CALL    TESTO   				; write it
        POP     PSW     				; get char
        ADI     64      				; translate to ascii
        JMP     TESTO   				; write it
;
;---------------------------------------------------
; CONTINUATION OF COMPARE (RST 2) ROUTINE
;---------------------------------------------------
;
COMP2:  ORA     A       				; test it
        JNZ     COMP5   				; brif not end
COMP3:  XRA     A       				; set equal status
COMP4:  MOV     A,M     				; get last char
        POP     B       				; restore b,c
        RET             				; return
COMP5:  CMP     M       				; compare the two chars
        JZ      COMP6   				; brif equal
        MOV     A,B     				; get count
        CPI     3       				; get if >= 3
        JNC     COMP3   				; brif not less than 3
        JMP     COMP4   				; brif less than 3 and not equal
COMP6:  INR     B       				; count it
        INX     D       				; point next lit
        INX     H       				; point next var
        JMP     COMP1   				; continue
;
;---------------------------------------------------
; TESTS IF (H,L) IS END OF LINE
; ERROR-DL IF NOT
;---------------------------------------------------
;
EOL:	CALL	TSTC					; call to ram address
        CALL    TSTEL   				; test if end line
        JNZ     SNERR   				; error if not
        CPI     ':'    					; test for multiple statement
        JNZ     EOL1    				; brif not
        STA     MULTI   				; set switch
EOL1:   INX     H       				; point next
        SHLD    ENDLI   				; save pointer
        RET             				; return
;
;---------------------------------------------------
; TEST (H,L) FOR END OF STATEMENT (00H OR ':')
; RETURN WITH Z SET IF IT IS
;---------------------------------------------------
;
TSTEL:  ORA     A       				; test for zero
        RZ              				; return if is
        CPI     ':'     				; test for multiple statement
        RET             				; return
;
;
;---------------------------------------------------
; TEST IF (H,L) IS END OF LINE
; RETURN IF NOT, ERROR-DL IF IS
;---------------------------------------------------
;
NOTEO:	CALL	TSTC					; call to ram address
        CALL    TSTEL   				; test if end of line
        JZ      SNERR   				; error if is
        RET             				; else, return
;
;---------------------------------------------------
; PACK LINE NUMBER FROM (H,L) TO B,C
;---------------------------------------------------
;
;
PACK:   LXI     B,0     				; clear b and c
        MVI     A,4     				; init digit counter
        STA     PRSW    				; save a
PK1:    MOV     A,M     				; get char
        CALL    NUMER   				; test for numeric
        RNZ             				; return if not numeric
        ANI     0FH     				; strip off zone
        MOV     D,A     				; save it
        LDA     PRSW    				; get count
        DCR     A       				; subtract one
        JM      SNERR   				; brif error
        STA     PRSW    				; save ctr
        MVI     E,4     				; 4 bit shift loop
PK3:    MOV     A,C     				; get low byte
        RAL             				; rotate left 1 bit
        MOV     C,A     				; replace
        MOV     A,B     				; get high byte
        RAL             				; rotate left 1 bit
        MOV     B,A     				; replace
        DCR     E       				; decr ctr
        JNZ     PK3     				; loop
        MOV     A,C     				; get low
        ORA     D       				; put digit in right half of byte
        MOV     C,A     				; replace
        INX     H       				; point next byte
        JMP     PK1     				; loop
;
;---------------------------------------------------
; COMPRESS THE EXPR STACK
; REG A CONTAINS # OF BYTES TO REMOVE STARTING AT (H,L+1)
; CONTAINS TOTAL NUMBER OF CHARACTERS IN STACK THUS FAR
;---------------------------------------------------
;
SQUIS:  PUSH    H       				; save h,l
        MOV     E,A     				; count to e
        MVI     D,0     				; zero hi byte
        DAD     D       				; compute start
        XCHG            				; put to d,e
        POP     H      					; get h,l
        CMA             				; complement count
        INR     A       				; then 2's complement
        ADD     B       				; compute b-a
        MOV     B,A     				; put to b
SQUI2:  INX     D       				; point next send
        INX     H       				; point next receive
        LDAX    D       				; get a char
        MOV     M,A     				; put it down
        DCR     B       				; decr ctr
        JNZ     SQUI2   				; loop
        SHLD    EXPRS   				; update new start of expr
        RET             				; return
;
;---------------------------------------------------
; FIND END OF LITERAL IN (D,E)
;---------------------------------------------------
;
SKP2Z:  LDAX    D       				; get byte of lit
        ORA     A       				; test it
        RZ              				; return if zero (end)
        INX     D       				; else, point next
        JMP     SKP2Z   				; loop
;
;---------------------------------------------------
; GETS FOUR BYTE TEMPORARY STORAGE AREA,
; STORES THE FACC THERE,
; PUTS ADDR OF AREA IN EXPR STACK (H,L)
;---------------------------------------------------
;
GTEMP:  XCHG            				; save h,l in d,e
        XTHL            				; exchange 0 and ret addr
        PUSH    H       				; put new ret addr
        PUSH    H      					; doit it again
        LXI     H,0     				; zero h,l
        DAD     SP      				; get sp addr in h,l
        INX     H       				; plus one
        INX     H       				; plus one more (point to new area)
        PUSH    B       				; save ctrs
        PUSH    D       				; save expr addr
        PUSH    H       				; save temp addr
		CALL	SFPA					; go store the value
        POP     D       				; restore temp addr
        LHLD    SPCTR   				; get count
        INX     H       				; plus one
        INX     H       				; one more
        SHLD    SPCTR   				; put back
        POP     H       				; restore expr addr
        POP     B       				; restore ctrs
SADR:   INX     H       				; point next byte
        MOV     M,D     				; high byte to exprstk
        INX     H       				; point next
        MOV     M,E     				; low byte to expr stk
        INX     H       				; point next
        MVI     M,0E3H  				; code = numeric data
        RET             				; return
;
;
;---------------------------------------------------
; TESTS THE CHAR AT (H,L)
; RETURNS WITH Z SET IF CHAR IS ALPHA (A-Z)
; RETURNS WITH Z OFF IF NOT ALPHA
; CHAR IS LEFT IN REG A
;---------------------------------------------------
;
ALPHA:  MOV     A,M     				; put char to reg a
        CPI     'A'     				; test if a or higher
        RC              				; return if not alpha (z is off)
        CPI     'Z'     				; test if z or less
        JMP     NUMEN   				; go wrapup
;
;
;---------------------------------------------------
; TESTS THE CHAR AT (H,L)
; RETURNS WITH Z SET IF NUMERIC (0-9)
; ELSE Z IS OFF
; CHAR IS LEFT IN THE A REG
;---------------------------------------------------
;
NUMER:  MOV     A,M     				; get char to reg a
        CPI     '0'     				; test if zero or greater
        RC              				; return if less than zero
        CPI     '9'     				; test if 9 or less
NUMEN:  RZ              				; return if 9
        RNC             				; return if not numeric
        CMP     A       				; set z
        RET             				; return
;
;---------------------------------------------------
; SEARCHES FOR THE VARIABLE IN D,E
; RETURNS WITH ADDR OF DATA AREA FOR VARIABLE
;---------------------------------------------------
;
SEARC:  PUSH    H      					; save h,l
        LDA     FNMOD  					; get function mode
        ORA     A      					; test it
        JNZ     SCH6   					; brif in a function
SCH0:   LHLD    DATAB  					; get addr of data pool
SCH1:   MOV     A,M    					; get the byte
        ORA     A      					; test if end
        JZ      SCH3   					; brif end
        DCX     H      					; point next
        DCX     H      					; ditto
        MOV     B,M    					; get hi len
        DCX     H      					; point next
        MOV     C,M    					; get lo len
		CALL	INCPC					; adjust h,l
        DB      3
        MOV     A,M    					; load 1st char
        CMP     D      					; compare 1st char
        JNZ     SCH2   					; brif not equal
        DCX     H      					; point next
        MOV     A,M    					; load 2nd digit
        INX     H      					; point back
        CMP     E      					; compare 2nd char
        JNZ     SCH2   					; brif not equal
        MOV     A,D    					; get hi name
        ORA     A      					; test it
        JM      SCH9   					; return if matrix
        DAD     B      					; point next entry
        INX     H      					; plus one
        XCHG           					; flip/flop
        POP     H      					; restore h
        RET            					; return
SCH2:   DAD     B      					; minus len
        JMP     SCH1   					; loop
SCH3:   MOV     M,D    					; put 1st char
        DCX     H      					; point next
        MOV     M,E    					; put 2nd char
        DCX     H      					; point next
        MOV     A,D    					; get hi name
        ORA     A      					; test it
        JM      SCH7   					; brif array
        MVI     M,0FFH 					; hi len
        DCX     H      					; point next
        MOV     A,E    					; get lo name
        ORA     A      					; test type
        JM      SCH4   					; brif char
        MVI     M,0F8H 					; lo len
        MVI     B,4    					; loop ctr
        JMP     SCH5   					; brarnd
SCH4:   MVI     M,0FBH 					; lo len
        MVI     B,1    					; loop ctr
SCH5:   DCX     H      					; point next
        MVI     M,0    					; zero the value
        DCR     B      					; decr ctr
        JNZ     SCH5   					; loop
        DCX     H      					; point next
        MVI     M,0    					; mark new end
        INX     H      					; point addr of variable
        XCHG           					; put location to d,e
        POP     H      					; restore h,l
        RET            					; return
SCH6:   LXI     H,FNARG					; point dummy arg
        MOV     A,M    					; load 1st char
        CMP     D      					; compare
        JNZ     SCH0   					; brif not equal
        INX     H      					; point next
        MOV     A,M    					; load 2nd char
        CMP     E      					; compare
        JNZ     SCH0   					; brif not equal
        INX     H      					; point next
        MOV     D,M    					; get hi addr
        INX     H      					; point next
        MOV     E,M    					; get lo addr
        POP     H      					; restore h,l
        RET            					; return
SCH7:   PUSH    H      					; save address
        MVI     M,0FEH 					; move hi disp
        DCX     H      					; point next
        MVI     M,14H  					; move lo disp
        DCX     H
        MVI     M,0    					; move a zero
        DCX     H      					; point next
        MVI     M,10   					; move 10
        DCX     H      					; point next
        MVI     M,0    					; move a zero
        DCX     H      					; point next
        MVI     M,10   					; move a 10 (default is 10 x 10)
        LXI     B,485  					; total # of bytes taken by array
SCH8:   DCX     H      					; point next
        MVI     M,0    					; clear one byte
        DCX     B      					; dcr ctr
        MOV     A,B    					; get hi
        ORA     C      					; plus lo
        JNZ     SCH8   					; loop
        POP     H      					; restore ptr to start
        INX     H      					; point lo name
        INX     H      					; point hi name
SCH9:   POP     B      					; need to xchange last 2 stack entries
        POP     D      					; so doit
        PUSH    B
        PUSH    D
        RET            					; return
;
;---------------------------------------------------
; TEST (H,L) FOR A VARIABLE NAME
; PUTS THE NAME IN D,E IF FOUND
; ERROR SN IF NONE FOUND
;---------------------------------------------------
;
VAR:	CALL	TSTC					; call to ram address
        CALL    ALPHA  					; test if alpha
        JNZ     SNERR  					; brif not alpha
        MOV     D,A    					; first char
        MVI     E,' '  					; default
        INX     H      					; point next
		CALL	TSTC					; call to ram address
        CALL    NUMER  					; test if numeric
        JNZ     VAR2   					; brif not numeric
        MOV     E,A    					; save 2nd char
        INX     H      					; point next
		CALL	TSTC					; call to ram address
VAR2:   CPI     '$'    					; test if string
        JNZ     VAR3   					; brif not
        MOV     A,E    					; get 2nd char
        ORI     80H    					; set type
        MOV     E,A    					; save it
        INX     H      					; skip $
        RET            					; then return
VAR3:   CPI     '('    					; test if array
        RNZ            					; return if not
        MOV     A,D    					; get hi name
        ORI     80H    					; turn on d7
        MOV     D,A    					; restore
        RET            					; return
;
;---------------------------------------------------
; PRINTS LINE NUMBER FOLLOWED BY CR/LF
;---------------------------------------------------
;
PRLIN:  LXI     D,LINEN					; point area
        LHLD    LINE   					; get addr of line number
        CALL    LINEO  					; go unpack
        XCHG           					; put to h,l
        MVI     M,0    					; end of msg
        LXI     H,LINEN					; point area
        JMP     TERMM  					; go print it
;
;---------------------------------------------------
; ERROR MESSAGE ROUTINES
; FATAL ERROR MUST BE FIRST
;---------------------------------------------------
;
EM      EQU     0FEH
;
ULERR:	CALL	PRERR					; call error routine
        DB      "UL",EM
		CALL	PRERR					; call error routine
ZMERR   EQU     $-3            			; log(x<=0),sqr(-x),0 divide
        DB      "OF",EM
		CALL	PRERR					; call error routine
STERR   EQU     $-3            			; error in expression stack
        DB      "ST",EM
		CALL	PRERR					; call error routine
SNERR   EQU     $-3            			; delimiter error
        DB      "SN",EM
		CALL	PRERR					; call error routine
RTERR   EQU     $-3            			; return & no gosub
        DB      "RT",EM
		CALL	PRERR					; call error routine
DAERR   EQU     $-3            			; out of data
DAERR   EQU     $-3            			; out of data
        DB      "DA",EM
		CALL	PRERR					; call error routine
NXERR   EQU     $-3            			; next & no for / >8 for's
        DB      "NX",EM
		CALL	PRERR					; call error routine
CVERR   EQU     $-3            			; conversion error
        DB      "CV",EM
		CALL	PRERR					; call error routine
CKERR   EQU     $-3            			; checksum error
        DB      "CK",EM
		CALL	PRERR					; call error routine

;
;---------------------------------------------------
; NON-FATAL ERRORS
;---------------------------------------------------
;
OVERR   EQU     $-1            			; overflow error
        DB      "OV",EM
        RET                    			; return to routine
UNERR:	CALL	PRERR					; call error routine
	    DB      "UN",EM
        RET
;
;---------------------------------------------------
; CONTINUATION OF ERROR MESSAGE ROUTINE (RST 6)
;---------------------------------------------------
;
ERROR:  CALL    TERMM  					; print 'xx'
        PUSH    H      					; save return
        LXI     H,ERRMS					; print 'error in line'
        CALL    TERMM
        CALL    PRLIN  					; print line #
        POP     H
        INX     H      					; return address
        MOV     A,M    					; get instruction
        CPI     FATAL  					; is it an rst 6?
        JZ      KEY    					; if zero, yes, abort
        POP     B      					; restore registers
        POP     D
        POP     PSW
        XTHL
        RET
;
;---------------------------------------------------
; MOVE THE STRING FROM (D,E) TO (H,L) COUNT IN B
;---------------------------------------------------
;
;
CPY4D:  MVI     B,4
COPYD:  LDAX    D      					; get a byte
        MOV     M,A    					; move it
        INX     H      					; point next
        INX     D      					; ditto
        DCR     B      					; decr ctr
        JNZ     COPYD  					; loop
        RET            					; then return
;
;---------------------------------------------------
; MOVE THE STRING FROM (H,L) TO (D,E) COUNT IN B
;---------------------------------------------------
;
;
CPY4H:  MVI     B,4
COPYH:  XCHG           					; flip/flop
        CALL    COPYD  					; go copy
        XCHG           					; flip/flop back
        RET            					; return
;
;---------------------------------------------------
; MOVES A STRING OF BINARY ZEROS, COUNT IN B
;---------------------------------------------------
;
ZEROM:  MVI     M,0    					; move a zero
        INX     H      					; point next
        DCR     B      					; decr ctr
        JNZ     ZEROM  					; loop
        RET            					; return
;
;---------------------------------------------------
; CONVERT FLOAT ACC TO UNSIGNED BINARY NUMBER IN A REG
; RETURNS 0 IN A REG IF FACC<0 OR FACC>255
;---------------------------------------------------
;
;
FBIN:   PUSH    H      					; save h,l
        PUSH    D      					; save d,e
        CALL    FACDE  					; convert facc to d,e
        XRA     A      					; zero a
        ORA     D      					; test high value
        JNZ     FBIN1  					; brif not zero
        MOV     A,E    					; value to a
FBIN1:  POP     D      					; restore d,e
        POP     H      					; restore h,l
        RET            					; return
;
;---------------------------------------------------
; GET NEXT ARGUMENT FROM POLISH STACK
;---------------------------------------------------
;
ARG:    LHLD    ADDR1  					; get address
        INX     H      					; point next
        MOV     D,M    					; get hi address
        INX     H      					; point next
        MOV     E,M    					; get lo address
        INX     H      					; point type
        SHLD    ADDR1  					; get address
        DCX     H      					; point back
        JMP     EVLD   					; call evload and return
;
ARGNU:  CALL    ARG    					; get argument
        JMP     FBIN   					; then convert facc to bin
;
;---------------------------------------------------
; CONVERT D,E TO FLOATING POINT NUMBER IN FAC
;---------------------------------------------------
;
;
BINFL:  LXI     H,FACC 					; point acc
        MVI     M,24   					; max bits
        INX     H      					; point next
        MVI     M,0    					; clear msb
        INX     H      					; point next
        MOV     M,D    					; move mid
        INX     H      					; point next
        MOV     M,E    					; move lsb
        JMP     FNORM  					; go normalize & return
		
;
;---------------------------------------------------
; SKIP CHARS POINTED BY H,L UNTIL NON-BLANK,
; LEAVE IN REG A
;---------------------------------------------------
;
TSTC:   MOV     A,M    					; load the byte at (h,l)
        CPI     ' '    					; test if blank
        RNZ            					; return if not
        INX     H      					; point next
        JMP     TSTC   					; loop
;
;---------------------------------------------------
; COMPARE STRING AT (H,L) TO STRING AT (D,E)
; RETURN IF EQUAL (THRU X'00' IN D,E) OR ON FIRST NOT EQUAL
; ONLY THE FIRST THREE CHARS NEED BE EQUAL
; IGNORE ALL SPACES
;---------------------------------------------------
;

COMP:   PUSH    B      					; save b,c
        MVI     B,0    					; init count
COMP1:	CALL	TSTC					; call to ram address
        LDAX    D      					; get char to match with
        JMP     COMP2  					; continue elsewhere
;
;
;---------------------------------------------------
; STORE THE FLOATING POINT ACCUMULATOR AT (H,L)
;---------------------------------------------------
;
SFPA:   LXI     D,FACC 					; point float acc
        MVI     B,4    					; byte count
        JMP     COPYD  					; go move it
;
;---------------------------------------------------
; INCREMENT H,L BY BYTE AT (SP), RETURN TO (SP)+1
;---------------------------------------------------
;
INCPC:  XTHL           					; get return address in h,l
        MOV     A,M    					; get the increment
        INX     H      					; point true return
        XTHL           					; put back to stack
        PUSH    D      					; save d,e
        JMP     INCPCA  				; continue
;
;
;---------------------------------------------------
; LOAD THE FLOATING POINT ACCUM WITH THE 4 BYTES AT (H,L)
;---------------------------------------------------
;
LDFPA:  LXI     D,FACC 					; point float acc
        MVI     B,4    					; byte count
        JMP     COPYH  					; go move it
;
;
;---------------------------------------------------
; PRINT:  'XX ERR & NNN'
; **** IF ERROR MESSAGE CHANGES TO A DIFFERENT RST,
; **** ...CHANGE "FATAL" EQUATE
;---------------------------------------------------
;
PRERR:  XTHL           					; save hl, get error code ptr
        PUSH    PSW    					; save regs
        PUSH    D
        PUSH    B
        JMP     ERROR  					; continue
;
;
INCPCA:	MOV     E,A    					; put in low
        ORA     A      					; test sign
        MVI     D,0    					; default positive
        JP      INCPCB 					; brif +
        MVI     D,0FFH 					; else, neg
INCPCB: DAD     D      					; bump h,l
        POP     D      					; restore d,e
        RET            					; return
	
;
;---------------------------------------------------
; FUNCTION TABLE. FORMAT IS:
;    DB <LITERAL>,0
;    DW <ADDRESS>
;    DB <FUNCTION TYPE>
;
; TABLE IS TERMINATED WITH A '00'
;---------------------------------------------------
;
FUNCT   EQU     $
        DB      "ABS",0
        DW      ABS
        DB      0ABH
        DB      "SQR",0
        DW      SQR
        DB      0ABH
        DB      "INT",0
        DW      INT
        DB      0ABH
        DB      "SGN",0
        DW      SGN
        DB      0ABH

RNDLI:  DB      "RND",0
        DW      RND
        DB      0ABH
        DB      "SIN",0
        DW      SIN
        DB      0ABH
        DB      "COS",0
        DW      COS
        DB      0ABH
        DB      "TAN",0
        DW      TAN
        DB      0ABH
        DB      "ATN",0
        DW      ATN
        DB      0ABH
        DB      "INP",0
        DW      INP
        DB      0ABH
        DB      "LN",0
        DW      LN
        DB      0ABH
        DB      "LOG",0
        DW      LOG
        DB      0ABH
        DB      "EXP",0
        DW      EXP
        DB      0ABH
        DB      "POS",0
        DW      POS
        DB      0ABH
        DB      "LEN",0
        DW      LENFN
        DB      0ABH
        DB      "CHR$",0
        DW      CHRFN
        DB      0CBH
        DB      "ASCII",0
        DW      ASCII
        DB      0ABH
        DB      "NUM$",0
        DW      NUMFN
        DB      0CBH
        DB      "VAL",0
        DW      VAL
        DB      0ABH
        DB      "SPACE$",0
        DW      SPACE
        DB      0CBH
        DB      "STRING$",0
        DW      STRFN
        DB      0D3H
        DB      "LEFT$",0
        DW      LEFT
        DB      0D3H
        DB      "RIGHT$",0
        DW      RIGHT
        DB      0D3H
        DB      "MID$",0
        DW      MIDFN
        DB      0DBH
        DB      "INSTR",0
        DW      INSTR
        DB      0BBH
        DB      "PEEK",0
        DW      PEEK
        DB      0ABH
        DB      0,0,0,0					; room for one more function
        DB      0,0,0,0
        DB      0      					; end of function table
;
;---------------------------------------------------
; program constants
;---------------------------------------------------
;
PCHOF:  DB      19,20,0
RNDP:   DB      3FH,0FDH       			; 16381
        DB      3FH,0EBH       			; 16363
        DB      3FH,0DDH       			; 16349
NRNDX:  DB      1BH,0ECH
        DB      33H,0D3H
        DB      1AH,85H
        DB      2BH,1EH
WHATL:  DB      13,10,"SN Error",0
LINERR:	DB      "Error at line ",0FEH
VERS:   DB      0DH,0AH,"Microsoft BASIC Version 1.4.2",0DH,0AH,"Modified by WOEPER 2025",0DH,0AH
		DB		"Console speed is ",0FEH
MEMMSG: DB      "Memory",0FEH
FRBTS:	DB 		" Bytes free",0
UART8E2:DB     "-8E2",0
UART7N1:DB     "-7N1",0

FGTAB:  DB  27,"[31m",0FEH
		DB  27,"[31m",0FEH
		DB	27,"[32m",0FEH
		DB  27,"[33m",0FEH
		DB 	27,"[34m",0FEH
		DB 	27,"[35m",0FEH
		DB 	27,"[36m",0FEH
		DB 	27,"[37m",0FEH
		DB 	27,"[39m",0FEH

BGTAB: 	DB  27,"[40m",0FEH
		DB  27,"[41m",0FEH
		DB	27,"[42m",0FEH
		DB  27,"[43m",0FEH
		DB 	27,"[44m",0FEH
		DB 	27,"[45m",0FEH
		DB 	27,"[46m",0FEH
		DB 	27,"[47m",0FEH
		DB 	27,"[49m",0FEH

CURONS:	DB 	27,"[?25h",0FEH
CUROFS: DB  27,"[?25l",0FEH
ERASES:	DB	27,"[2J",0FEH
		
RBOUT:  DB      08H,20H,08H,0FEH					; rubout sequence 
LLINE:  DB      "LINE",0
TABLI:  DB      "TAB",0
STEPL:  DB      "STEP",0
THENL:  DB      "THEN",0
PILIT:  DB      "PI",0
TWO:    DB      02H,80H,00H,00H   					; constant:  2
TEN:    DB      04H,0A0H,00H,00H  					; constant:  10
PI:     DB      02H,0C9H,0FH,0D7H 					; constant:  3.141593
QTRPI:  DB      00H,0C9H,0FH,0D7H 					; constant:  0.7853892
NEGON:  DB      80H,0FFH,0FFH,0FFH					; constant: -0.9999999
LN2C:   DB      00H,0B1H,72H,16H  					; constant:  0.6931472
SQC1:   DB      00H,97H,14H,0EBH  					; constant:  0.59016206
SQC2:   DB      7FH,0D5H,0A9H,56H 					; constant:  0.41730759
;
;---------------------------------------------------
; THE FOLLOWING CONSTANTS MUST BE IN THIS ORDER ***********
;
;      CONSTANT WITH EXPONENT OF 1
;      COEFFICIENT OF FIRST TERM
;      ...
;      COEEFICIENT OF NTH TERM
;
; SINCE ALL COEFFICIENTS ARE LESS THAN 1,
; THE ITERATION LOOP USES THE
; CONSTANT WITH EXPONENT 1 TO TERMINATE THE EVALUATION.
;---------------------------------------------------
;
SQC3:   DB      01H,0B5H,04H,0F3H   					; constant:  1.41421356
        DB      0FFH,0AAH,95H,0BCH  					; constant: -0.3331738
        DB      7EH,0CAH,0D5H,20H   					; constant:  0.1980787
        DB      0FEH,87H,82H,0D6H   					; constant: -0.1323351
        DB      7DH,0A3H,13H,1CH    					; constant:  0.07962632
        DB      0FCH,89H,0A6H,0B8H  					; constant: -0.03360627
ATNCO:  DB      79H,0DFH,3AH,9EH    					; constant:  0.006812411
;
HALFP:  DB      01H,0C9H,0FH,0D7H   					; constant:  1.570796
        DB      80H,0A5H,5DH,0DEH   					; constant: -0.64596371
        DB      7DH,0A3H,34H,55H    					; constant:  0.076589679
        DB      0F9H,99H,38H,60H    					; constant: -0.0046737656
SINCO:  DB      74H,9EH,0D7H,0B6H   					; constant:  0.00015148419
;
ONE:    DB      001H,080H
NULLI:  DB      00H,00H             					; constant:  1.0
        DB      00H,0FFH,0FEH,0C1H  					; constant:  0.99998103
        DB      0FFH,0FFH,0BAH,0B0H 					; constant: -0.4994712
        DB      7FH,0A8H,0EH,2BH    					; constant:  0.3282331
        DB      0FEH,0E7H,4BH,55H   					; constant: -0.2258733
        DB      7EH,89H,0DEH,0E3H   					; constant:  0.134693
        DB      0FCH,0E1H,0C5H,078H 					; constant: -0.05511996
LNCO:   DB      7AH,0B0H,3FH,0AEH   					; constant:  0.01075737
;
LN2E:   DB      001H,0B8H,0AAH,03BH 					; constant:  1.44269504
        DB      000H,0B1H,06FH,0E6H 					; c=.69311397
        DB      07EH,0F6H,02FH,070H 					; c=.24041548
        DB      07CH,0E1H,0C2H,0AEH 					; c=.05511732
        DB      07AH,0A0H,0BBH,07EH 					; c=.00981033
EXPCO:  DB      077H,0CAH,009H,0CBH 					; c=.00154143
;
LNC:    DB      07FH,0DEH,05BH,0D0H    					; c=log base 10 of e
READY:  DB      0FDH
        DB      "Ready",0
STOPM:  DB      0FDH
        DB      "Stop at line ",0FEH
ERRMS:  DB      " Error in line ",0FEH

SPDTAB:	DW		5					; 38400 baud
		db      '38400',0FEH,0		
		dw	 	10					; 19200 baud
		db		'19200',0FEH,0
		dw	 	20					; 9600 baud
		db		'9600',0FEH,0,0
		dw		40					; 4800 baud
		db		'4800',0FEH,0,0
		dw		80					; 2400 baud
		db		'2400',0FEH,0,0
		dw		160					; 1200 baud
		db		'1200',0FEH,0,0
		dw		320					; 600 baud
		db		'600',0FEH,0,0,0
		dw		640					; 300 baud
		db		'300',0FEH,0,0,0

;
;--------------------------------------------------------------------------
; VERB (STATEMENT/COMMAND) TABLE
; FORMAT IS: DB 'VERB',0
;           DW ADDR
;           DB 'NEXT VERB',0
;           ETC
; END OF TABLE IS MARKED BY DB 0
;---------------------------------------------------
;
JMPTB   EQU     $
        DB      "LIST",0
        DW      LIST
        DB      "RUN",0
        DW      RUNCM
        DB      "XEQ",0
        DW      XEQ
        DB      "NEW",0
        DW      NEW
        DB      "CON",0
        DW      CONTI
        DB      "TAPE",0
        DW      TAPE
        DB      "PUNCH",0
        DW      PUNCH
KEYL:   DB      "KEY",0
        DW      KEY
        DB      "FRE",0
        DW      FREE
        DB      "IF",0
        DW      IFSTM
        DB      "READ",0
        DW      READ
        DB      "RESTORE",0
        DW      RESTO
DATAL:  DB      "DATA",0
        DW      RUN
        DB      "FOR",0
        DW      FOR
NEXTL:  DB      "NEXT",0
        DW      NEXT
GOSBL:  DB      "GOSUB",0
        DW      GOSUB
        DB      "RETURN",0
        DW      RETUR
        DB      "INPUT",0
        DW      INPUT
        DB      "PRINT",0
        DW      PRINT
GOTOL:  DB      "GO"
TOLIT:  DB      "TO",0
        DW      GOTO
        DB      "LET",0
        DW      LET
        DB      "STOP",0
        DW      STOP
        DB      "END",0
        DW      ENDIT
        DB      "REM",0
        DW      RUN
        DB      "!",0
        DW      RUN
        DB      "?",0
        DW      PRINT
        DB      "RANDOMIZE",0
        DW      RANDO
        DB      "ON",0
        DW      ON
        DB      "OUT",0
        DW      OUTP
        DB      "DIM",0
        DW      DIM
        DB      "CHANGE",0
        DW      CHANG
DEFLI:  DB      "DEF"
FNLIT:  DB      "FN",0
        DW      RUN
        DB      "POKE",0
        DW      POKE
        DB      "CALL",0
        DW      JUMP
        DB      "EDIT",0
        DW      FIX
		DB      "MONITOR",0
		DW		MONITOR
		DB		"FIGFORTH",0
		DW		FIGFORTH
		DB		"CLEAR",0
		DW  	CLEAR
		DB      "COLOR",0
		DW		COLOR
		DB		"CURSOR",0
		DW 		CURSOR 
		DB		"BELL",0
		DW		SNDBELL
		DB 		"IOCMD",0
		DW 		CONTROL
		DB 		"SAVE",0
		DW 		SAVEFILE
		DB		"LOAD",0
		DW 		LOADFILE
		DB 		"FILES",0
		DW 		FILES
		DB		"DELETE",0
		DW		DELETESD
		DB		"IORST",0
		DW		RESETSD
		DB		"IOFORM",0
		DW		FORMATSD
		DB      0      					; end of table
;
;---------------------------------------------------
; THIS ROUTINE CONVERTS THE FACC TO AN ADDRESS IN D,E
;---------------------------------------------------
;
FACDE:  CALL    INT     				; integerize the facc
        LDA     FACC    				; get the exponent
        ORA     A       				; test it
        JM      OVERR   				; brif negative address
        SUI     16      				; subtract max exponent
        JZ      FDE2    				; brif equal max
        JP      OVERR   				; brif greater than 64k
        CMA             				; 2's compliment of a yields..
        INR     A       				; 16-a
        MOV     C,A     				; save shift count
FDE1:   XRA     A       				; clear carry
        LXI     H,FACC+1   				; point mantissa
        MVI     B,2     				; words to shift
        CALL    FSHFT   				; go shift facc+1 and facc+2
        DCR     C       				; reduce count
        JNZ     FDE1    				; loop till complete
FDE2:   LXI     H,FACC+1   				; point high byte
        MOV     D,M     				; load d
        INX     H       				; point low byte
        MOV     E,M     				; loade e
        RET             				; return
;
;
;---------------------------------------------------
; THIS ROUTINE SEARCHES FOR A LINE IN THE PROGRAM FILE.
; Z SET, C RESET==>LINE FOUND. ADDRESS IS IN H,L
; C SET, Z RESET==>NOT FOUND. H,L POINT TO NEXT LINE
; C SET, Z SET==>NOT FOUND. H,L POINT AT END OF PROGRAM
;---------------------------------------------------
;
LOCAT:  LXI     H,BEGPR 				; point start
FIND1:  MOV     A,M     				; fetch length of line
        PUSH    H      					; save pointer
        ORA     A       				; test
        JZ      FIND3   				; brif end
        INX     H       				; point line #
        MOV     A,M     				; fetch hi #
        CMP     B       				; compare to requested
        JC      FIND2   				; brif low
        JNZ     FIND3   				; brif past and not found
        INX     H       				; point lo #
        MOV     A,M     				; fetch it
        CMP     C       				; compare to requested
        JC      FIND2   				; brif low
        JNZ     FIND3   				; brif past and not found
        POP     H       				; point begin if match
        RET             				; return
;
;---------------------------------------------------
; BUMP H,L TO NEXT LINE
;---------------------------------------------------
;
FIND2:  POP     H      					; point start of line
        MOV     E,M    					; lenght to e
        MVI     D,0    					; clear d
        DAD     D      					; bump h,l
        JMP     FIND1  					; continue
;
;---------------------------------------------------
; LINE NOT FOUND
;---------------------------------------------------
;
FIND3:  STC            					; set carry
        POP     H      					; point line just past requested
        RET            					; return
;
;
;
;---------------------------------------------------
; THIS CODE FINDS AN ENTRY IN THE TABLE POINTED TO BY D,E.
; THE SOUGHT ENTRY IS POINTED TO BY H,L.
;---------------------------------------------------
;
SEEK    EQU     $
SEEK1:  PUSH    H      					; save address of string
        LDAX    D      					; get byte from table
        ORA     A      					; test it
        JZ      SEEK3  					; brif end of table
		CALL	COMP					; go compare
        JNZ     SEEK2  					; brif not found
        XTHL           					; put current h,l on stack
        CALL    SKP2Z  					; find end to literal in table
        INX     D      					; point low byte
        POP     H      					; restore line pointer
        INR     A      					; put 1 in a
        ORA     A      					; reset z bit
        RET            					; return
SEEK2:  CALL    SKP2Z  					; find end of table literal
        INX     D      					; 
        INX     D      					; point next lit in table
        INX     D      					; 
        POP     H      					; get original string
        LDAX    D      					; get byte
        RAL            					; high bit to carry
        JNC     SEEK1  					; not a function search
        INX     D      					; point next byte in function table
        JMP     SEEK1  					; continue search
SEEK3:  POP     H      					; restore original string
        RET            					; return

;
;---------------------------------------------------
; EDIT COMMAND
; EDIT <LINE #><DELIMITER><OLD TEXT><DELIMITER><NEW TEXT>
;---------------------------------------------------
;
FIX:    EQU     $
		CALL	TSTC					; call to ram address
        CALL    PACK   					; get line # in b,c
		CALL	TSTC					; call to ram address
        SHLD    ADDR2  					; save command pointer
        CALL    LOCAT  					; search for line # in program
        JC      ULERR  					; brif not found
        PUSH    H      					; save addr of existing line <source>
        PUSH    B      					; save line #
        MOV     B,M    					; get length of <source>
        XCHG           					; d,e point <source>
        LXI     H,STRIN					; point string buffer
        CALL    COPYD  					; <source> to string buffer
        LDA     STRIN  					; length of <source> to a
        SUI     2      					; adjust
        STA     STRIN  					; store
        LXI     D,IOBUF+1      			; point buffer
        LHLD    ADDR2  					; fetch command pointer
        MOV     B,M    					; fetch <delimiter>
;
;---------------------------------------------------
; FIND LENGTH OF <OLD TEXT>. STORE IT IN IOBUF.
;---------------------------------------------------
;
        MVI     C,0    					; initial length
FIX1:   INX     H      					; point next character
        MOV     A,M    					; fetch
        ORA     A      					; test
        JZ      SNERR  					; missing 2nd <delimiter>.
        CMP     B      					; test
        JZ      FIX2   					; brif 2nd <delimiter> found
        INR     C      					; else, bump c
        STAX    D      					; store character in iobuf
        INX     D      					; bump iobuf pointer
        JMP     FIX1   					; continue
;
;---------------------------------------------------
; GET READY TO SEARCH <SOURCE> FOR <OLD TEXT>
;---------------------------------------------------
;
FIX2:   	MOV     A,C    				; length of <ot> to a
			STA     IOBUF  				; store
			SHLD    ADDR2  				; save command pointer
			MVI     A,3    				; search will start in pos 3.
			LHLD    PROGE  				; point end of program
			INX     H      				; bump twice
			INX     H
			SHLD    ADDR1  				; save expr. stack pointer
			INX     H      				; point next
			LXI     D,IOBUF				; point buffer area
			MOV     M,D    				; store address
			INX     H
			MOV     M,E
			LXI     H,STRIN				; point <source>
;
;---------------------------------------------------
; USE THE INSTR ROUTINE TO SEARCH
;---------------------------------------------------
;
			CALL    INST2  				; go search
			MOV     A,E    				; result to a
			ORA     A      				; test
			JZ      DAERR  				; br if not found
			MOV     C,A    				; save position in c
			DCR     A      				; adjust
			MOV     B,A    				; copy to b
			LXI     H,STRIN+1      		; point <old source>
			LXI     D,IOBUF+1      		; piont <new line area>
			CALL    COPYH  				; copy <old source> up to <old text>
			PUSH    D      				; save dest pointer
;
;---------------------------------------------------
; SKIP OVER <OLD TEXT> IN <SOURCE>
;---------------------------------------------------
;
			MVI     D,0    				; clear d
			LDA     IOBUF  				; get length of <ot>
			MOV     E,A    				; length to e
			DAD     D      				; bump h,l past <ot>
			POP     D      				; restore <dest> pointer
			PUSH    H      				; save <remaining source> pointer
;
;---------------------------------------------------
; APPEND <NEW TEXT> TO <DEST>
;---------------------------------------------------
;
			LHLD    ADDR2  				; fetch command pointer
FIX3:   	INX     H      				; point next
			MOV     A,M    				; fetch character
			ORA     A      				; test it
			JZ      FIX4   				; brif no more <new text>
			INR     C      				; bump length count
			STAX    D      				; store character
			INX     D      				; bump <dest> pointer
			JMP     FIX3   				; continue
;
;---------------------------------------------------
; APPEND <REMAINING SOURCE> TO <DEST>
;---------------------------------------------------
;
FIX4:   	POP     H      				; get remaining source pointer
FIX4A:  	MOV     A,M    				; fetch character
			ORA     A      				; test
			JZ      FIX5   				; brif done
			STAX    D      				; store character
			INR     C      				; bump char count
			INX     D      				; bump dest pointer
			INX     H      				; bump <source> pointer
			JMP     FIX4A  				; continue
;
;---------------------------------------------------
; PREPARE <DEST> FOR SUBMISSION AS NEW LINE  
;---------------------------------------------------
;
FIX5:   	STAX    D      				; buffer terminator
			INR     C      				; bump length count
			MOV     A,C    				; fetch count
			STA     IOBUF  				; store it
			MOV     B,A    				; copy count to b
			LXI     H,IMMED				; point new line area
			LXI     D,IOBUF				; point where it is now
			CALL    COPYD  				; copy it
			POP     B      				; restore line #
			POP     H      				; restore program pointer
			PUSH    H      				; save it
			JMP     EDIT2  				; process as new line
;
;---------------------------------------------------
; SOUND BELL IN PROGRAM
;---------------------------------------------------
;
SNDBELL:	MVI		A,7
			CALL 	TESTO
			JMP		RUN
;
;---------------------------------------------------
; BELL          RING USER'S CHIMES
;---------------------------------------------------
;
BELL:   	MVI     A,7    				; code for bell
			CALL    TESTO
			JMP     RDY
;---------------------------------------------------
; A=PEEK(X). RETURNS DECIMAL VALUE OF MEMORY ADDRESS X.
;---------------------------------------------------
;
PEEK:		CALL    FACDE  				; get address in d,e
			XCHG           				; address to h,l
			LXI     D,0    				; clear d,e
			MOV     E,M    				; put memory byte in e
			JMP     BINFL  				; convert d,e to binary and return
;--------------------------------------------------
; POKE <ADDRESS>,<VALUE>.  PUTS IN MEMORY ADDRESS.
;---------------------------------------------------
;
POKE:		CALL    EXPR   				; evaluate address expression
			MOV     A,M    				; load next character
			CPI     ','    				; test
			JNZ     SNERR  				; brif error
			INX     H      				; point next
			PUSH    H      				; save h,l
			CALL    FACDE  				; put address in d,e
			POP     H      				; restore h,l
			PUSH    D      				; save address
			CALL    EXPR   				; evaluate value expression
			CALL    EOL    				; test for end of line
			CALL    FBIN   				; convert facc to a register value
			POP     H      				; get d,e address in h,l
			MOV     M,A    				; move byte
			JMP     RUN    				; continue
			
;---------------------------------------------------
; CALL <ADDRESS>. EXECUTES CODE AT MEMORY ADDRESS.
;---------------------------------------------------
;
JUMP:		CALL    EXPR   				; evaluate address expression
			CALL    EOL    				; test for end of line
			CALL    FACDE  				; convert facc to address in d,e
			LXI     H,RUN  				; make into subroutine
			PUSH    H
			XCHG           				; move address to hl
			PCHL           				; execute user's routine
;
;---------------------------------------------------
; SET LED IN B to VALUE IN C
;---------------------------------------------------

SETLED:		MOV		A,C					; Move value to ACU
			ANI		1					; make sure it is only LSB bit
			MOV		C,A					; Move to C again
			PUSH	B
			MVI		A,4
			ADD		B
			MOV		B,A
			IN		PPIPC				; Get current led bits
SETLD0: 	DCR		B					; Decrement led number count
			JZ		SETLD1				; When right bit is at LSB 0
			RRC
			JMP		SETLD0				; else continue
SETLD1:		ANI		0FEH
			ORA		C					; Set or reset this bit
			POP		B					; restore BC
			PUSH	PSW
			MVI		A,4
			ADD		B
			MOV		B,A
			POP		PSW
SETLD2: 	DCR		B					; inc counter
			JZ		SETLD3				; loop again
			RLC							; Rotate
			JMP		SETLD2
SETLD3:		OUT		PPIPC				; Set the led
			RET							; Return to caller	
			
;---------------------------------------------------
; JUMP TO CPU8085 MONITOR
;---------------------------------------------------
;
MONITOR: 	CALL    EOL    				; test for end of line
			JMP		MONROM 
;			
;---------------------------------------------------
; JUMP TO FIG-FORTH
;---------------------------------------------------
;
FIGFORTH: 	CALL    EOL    				; test for end of line
			JMP		FIGROM
;
;---------------------------------------------------
; CLEAR SCREEN
;---------------------------------------------------
;
CLEAR:		LXI		H,ERASES
			CALL	TERMM
			JMP		RUN
;
;---------------------------------------------------
; SET COLOR
;---------------------------------------------------
;
COLOR: 		CALL    EXPR   				; evaluate address expression
			MOV     A,M    				; load next character
			CPI     ','    				; test
			JNZ     SNERR  				; brif error
			INX     H      				; point next
			PUSH    H      				; save h,l
			CALL    FACDE  				; put address in d,e
			POP     H      				; restore h,l
			PUSH    D      				; save address
			CALL    EXPR   				; evaluate value expression
			CALL    EOL    				; test for end of line
			CALL    FBIN   				; convert facc to a register value
			POP     H      				; get d,e address in h,l
			PUSH	PSW					; save background 
			MOV		A,L    				; get forground color
			CPI		0
			JC		SNERR
			CPI		8
			JZ		COLOR00
			JNC		SNERR
COLOR00:	LXI		H,FGTAB				; point to table
			LXI		D,6					; offset for each entry
COLOR0:		DCR		A					; decrement value
			JM      COLOR1				; if minus value is reached
			DAD		D					; add offset to point to next entry
			JMP		COLOR0				; loop until found
COLOR1:		CALL	TERMM				; output the string
			POP		PSW					; get background color
			CPI		0
			JC		SNERR
			CPI		8
			JZ		COLOR11
			JNC		SNERR
COLOR11:	LXI		H,BGTAB				; point to table	
			LXI		D,6					; offset
COLOR2:		DCR		A					; decrement value
			JM		COLOR3				; if minus value reached
			DAD		D					; add offset 
			JMP		COLOR2				; loop until found
COLOR3:		CALL	TERMM				; output string	
			JMP		RUN
;			
;---------------------------------------------------
; POSITION CURSOR ON ANSI-BBS TERMINAL
;---------------------------------------------------
;
CURSOR:		CALL    EXPR    			; go evaluate address
			MOV     A,M     			; get delim
			CPI     ','     			; test if comma
			JNZ     SNERR   			; brif not
			INX     H       			; skip over comma
			CALL    FBIN    			; convert to binary in a-reg
			CPI		100					; must be below 100
			JNC		SNERR				; else error
			STA		XPOS				; store x position
        	CALL    EXPR    			; go eval data byte
			CALL    EOL     			; error if not end of statement
			CALL    FBIN    			; convert to binary
			CPI		100					; must be below 100
			JNC		SNERR				; else error
			STA		YPOS				; save
			MVI		A,27				; build cursor position string
			STA		CURSORSTR
			MVI		A,'['
			STA		CURSORSTR+1
			MVI		A,';'
			STA		CURSORSTR+4
			MVI		A,'H'
			STA		CURSORSTR+7
			MVI		A,0FEH
			STA		CURSORSTR+8
			LDA		XPOS
			CALL	TOASCI
			MOV		A,B
			STA		CURSORSTR+2
			MOV		A,C 
			STA		CURSORSTR+3
			LDA		YPOS
			CALL	TOASCI
			MOV		A,B
			STA		CURSORSTR+5
			MOV		A,C 
			STA		CURSORSTR+6
			LXI		H,CURSORSTR
			CALL	TERMM				; display the string
			JMP		RUN 
;
;---------------------------------------------------
; CONVERT ACU TO ASCII IN B AND C (00 TO 99)
;---------------------------------------------------
;
TOASCI:		MVI		C,10				; prepare 10
			MVI		B,'0'				; prepare digit 2 with 0
TOASCI1:	CPI		10					; bigger then 10
			JC		TOASCI2				; nope skip
			SUB		C					; substract 10 from acu
			INR    	B					; add 1 to digit left
			JMP 	TOASCI1				; next hit
TOASCI2:    ADI		'0'					; add 0 to value left in acu to make ascii
			MOV		C,A					; move it to c
			RET							; return
;
;-----------------------------------------------------------------------
; CONVERT STRING TO UPPERCASE IGNORING QUOTED TEXT
;-----------------------------------------------------------------------
;

UNEXT:		INX		H					; convert string under [hl] to upper case
UCASE:		MOV		A,M					; get character <- entry point is here
			CPI		0					; done?
			RZ							; if so, stop
UNEXT1:		CPI		'"'					; is it text to print ?
			JNZ		UNEXT3				; quotes text start no continue ?
UNEXT0:		INX		H					; get next character pointer
			MOV		A,M					; get it
			CPI		0					; eol ?
			RZ							; yes done exit line is in error
			CPI		'"'					; find next quote character delimiter
			JZ		UNEXT				; found continue uppercase change
			JMP     UNEXT0				; next delimited character continue the skip
UNEXT3:		CPI		'a'					; >= 'a'?
			JC		UNEXT				; if not, next character
			CPI		'z'+1				; <= 'z'?
			JNC		UNEXT				; if not, next character
			SUI		32					; subtract 32
			MOV		M,A					; write character back
			JMP		UNEXT				; process next 
;
;-----------------------------------------------------------------------
; convert string to lowercase
;-----------------------------------------------------------------------
;											
LNEXT:		INX	H						; convert string under [hl] to lower case	
LCASE:		MOV	A,M						; get character <- entry point is here
			CPI	0						; done?
			RZ							; if so, stop
			CPI	'A'						; >= 'a'?
			JC	LNEXT					; if not, next character
			CPI	'Z'+1					; <= 'z'?
			JNC	LNEXT					; if not, next character
			ADI	32						; subtract 32
			MOV	M,A						; write character back
			JMP	LNEXT
;
;
;-----------------------------------------------------------------------
; ** BINAIR TO ASCII  5 DIGITS IN HL
;-----------------------------------------------------------------------
DECOUT:		MVI		B,80H				; flag to no print
			LXI		D,-10000			; setup 10000
			CALL	DECNO				; see how many
			LXI		D,-1000				; setup 1000
			CALL	DECNO				; see howmany
			LXI		D,-100				; setup 100
			CALL	DECNO				; see how many
			LXI		D,-10				; setup 10
			CALL	DECNO				; see howmany
			MVI		B,0					; clear inhibit print flag
			LXI		D,-1				; setup 1
DECNO:		INR		B					; see howmany
			DAD		D					; substract it
			JC		DECNO				; carry ?
			DCR		B					; yes adjust count
			XCHG						; to de
			XRA		A					; clear carry clear acu
			SUB		L					; substarct l
			MOV		L,A					; save l
			MVI		A,0					; save carry but a to 0
			SBB		H					; substract h
			MOV		H,A					; save h
			XCHG						; back to hl
			DAD		D					; readd
			MOV		A,B					; get no print flag
			CPI		80H					; still set
			RZ							; yes then return
			MOV		A,B					; get flag
			ANI		7FH					; mask out flag
			ORI		'0'					; make ascii  
			PUSH	H					; save hl
			CALL	CAOUT				; echo it
			POP		H					; restore hl
			MVI		B,0					; disable flag now
			RET	
;

;-----------------------------------------------------------------------
;
; CONTROL SDCARD COMMANDER
;
;-----------------------------------------------------------------------
;
CONTROL:		MVI		A,RTS_ON		; Enable RTS
				OUT 	UARTC			; execute
				CALL    EOL				; End of line ?
				JMP		CONTROL1		; yes	
CONTROL0:		DB		"IO Commander",0H
CONTROL1:		LXI		H,CONTROL0 		; Point to title
				CALL	TERMM			; display it
				MVI		A,13			; CR
				CALL	CBOUT			; To IOCONTROLLER
CONTROL2:		MVI		A,RTS_ON		; RTS ON
				OUT 	UARTC			; Command it
				MVI		A,RTS_ON		; RTS ON
				OUT		UBRTC			; IO Controller also
;
CIN:			IN      UARTS			; 
				ANI		RX_MASK			; receiver ready?
				JZ		CAN
				IN 		UARTD
				CPI		'%'
				JZ		CONTROL4
				PUSH	PSW
CIN0:			IN		UBRTS
				ANI		TX_MASK			; transmitter ready?
				JZ		CIN0		
				POP 	PSW 
				OUT     UBRTD
;
CAN:			IN      UBRTS
				ANI		RX_MASK			; receiver ready?
				JZ		CIN
				IN 		UBRTD
				CPI		'%'
				JZ		CONTROL5 
				PUSH 	PSW
CAN0:			IN		UARTS		
				ANI		TX_MASK			; transmitter ready?
				JZ		CAN0
				POP 	PSW 
				OUT 	UARTD
				JMP 	CIN
;
CONTROL3:		DB		13,10,"Exit IO Commander",0
CONTROL4:		LXI		H,CONTROL3
				CALL 	TERMM
				JMP		RUN
CONTROL5:		CALL 	RESET 
				JMP 	RUN
;
;-----------------------------------------------------------------------
; CHARACTER TO TERMINAL 
;-----------------------------------------------------------------------
;				
CAOUT:			PUSH	PSW
CAOUT1:			IN		UARTS
				ANI		TX_MASK			; transmitter ready?
				JZ		CAOUT1
				POP		PSW
				OUT		UARTD			; transmit
CAOUT2:			IN		UBRTS			; get status
				ANI		TE_MASK			; check if transmitter is empty	
				JZ		CAOUT2			; wait until so
				RET
				
;
;-----------------------------------------------------------------------
; CHARACTER TO SDCARD CONTROLLER 
;-----------------------------------------------------------------------
;				
CBOUT:			PUSH	PSW
CBOUT0:			IN		UBRTS
				ANI		DS_MASK
				JZ		CBOUT0
CBOUT1:			IN		UBRTS
				ANI		TX_MASK			; transmitter ready?
				JZ		CBOUT1		
				POP		PSW				; restore character
				OUT		UBRTD			; transmit
CBOUT2:			IN		UBRTS			; get status
				ANI		TE_MASK			; check if transmitter is empty	
				JZ		CBOUT2			; wait until so
				RET
;
;-----------------------------------------------------------------------
; CHARACTER FROM SDCARD CONTROLLER 
;-----------------------------------------------------------------------
;
CBIN:			IN		UBRTS			; Read status
				ANI		RX_MASK			; char ready ?
				JZ		CBIN			; No wait for it
				IN		UBRTD			; read char in A
				RET						; return to caller
;
;-----------------------------------------------------------------------
; CHARACTER FROM CONSOLE 
;-----------------------------------------------------------------------
;
CAIN:			IN		UARTS			; Read status
				ANI		RX_MASK			; char ready ?
				JZ		CAIN			; No wait for it
				IN		UARTD			; read char in A
				RET						; return to caller
;
;-----------------------------------------------------------------------
;	PRINT STRING POINTED BY HL UNTIL 0 TO PRIMARY
;-----------------------------------------------------------------------				
;
PMSGA:			MOV		A,M				; Get a character
				ORA		A				; test if 0
				RZ						; return if so
				CALL    CAOUT			; output it
				INX		H				; bump index
				JMP		PMSGA			; next character
;
;-----------------------------------------------------------------------
;	PRINT STRING POINTED BY HL UNTIL 0 TO SECONDARY
;-----------------------------------------------------------------------				
;
PMSGB:			MOV		A,M
				ORA		A
				RZ
				CALL    CBOUT
				INX		H
				JMP		PMSGB

;
;------------------------------------------------------------------------ 
; PRINT HL AS HEX 
;------------------------------------------------------------------------				
;
PRNHL:			PUSH	H					; Save HL
				PUSH	D					; Save DE
				PUSH	B					; Save BC
				PUSH	PSW					; Save PSW
				MOV 	A,H                 ; Get HIGH byte
				CALL 	PRNHL1              ; Output it
				MOV 	A,L                 ; Get LOW byte
				CALL	PRNHL1				; Output it
				POP		PSW					; Restore PSW
				POP		B					; Restore BC
				POP		D					; Restore DE
				POP		H					; Restore HL
				RET							; Return to caller
PRNHL1: 		PUSH 	PSW                 ; Save low digit
				RRC                         ; Shift
				RRC                         ; high
				RRC                         ; digit
				RRC                         ; into low
				CALL 	PRNHL2              ; Display a single digit
				POP 	PSW                 ; Restore low digit
				CALL	PRNHL2				; Display a single digit
				RET							; Return from subroutine
PRNHL2:			ANI 	0Fh                 ; Remove high digit
				CPI	 	10                  ; Convert to ASCII
				SBI 	2Fh					; Adjust
				DAA							; Digital adjust acu
				CALL	CAOUT				; Output the digit
				RET
;
;-----------------------------------------------------------------------
;	RESET SDCARD ATMEGA328 CONTROLLER
;-----------------------------------------------------------------------				
;
RESETSD:		CALL	RESET
				JMP		RUN
;
;-----------------------------------------------------------------------
;	RESET ATMEGA328 FROM SDCARD
;-----------------------------------------------------------------------				
;	
RESET:		LXI		H,RESET1
			CALL    PMSGB
			RET
RESET1:		DB		"reset",13,0
;
;-----------------------------------------------------------------------
;	LOAD BASIC PROGRAM FROM FILE ON SDCARD
;-----------------------------------------------------------------------				
;	
LOADFILE:	CALL	TSTC				; call to ram address
			PUSH    H       			; save ptr
			LXI		H,CMDSTRW			; Prepare "read ....
			MVI		A,'r'
			MOV		M,A 
			INX		H
			MVI		A,'e'
			MOV		M,A 
			INX		H 
			MVI		A,'a'
			MOV		M,A 
			INX		H 
			MVI		A,'d'
			MOV		M,A 
			INX		H 
			MVI		A,' '
			MOV		M,A 
			LXI     H,CMDSTRW+5			; point buffer	
			MVI     B,12     			; file name length
			POP     D       			; restore cmd ptr
LOADF0:  	LDAX    D       			; fetch file nameF
			CPI		'"'
			JNZ     LOADF00
			INX		D
			JMP     LOADF0
LOADF00:	ORA     A       			; test
			JZ      LOADF2
			MOV     M,A     			; store char
			INX     D       			; name ptr
LOADF1:  	INX     H       			; buffer ptr
			DCR     B       			; count
			JNZ     LOADF0
LOADF2:		MVI		A,13				; add carriage return
			MOV     M,A
			INX		H 
			MVI		A,0
			MOV     M,A
			LXI		H,CMDSTRW			; point to command
			CALL	PMSGB				; send it
 										; CLEAR PROGRAM AND DATA BEFORE LOAD
			XRA     A       			; get a zero 
			LHLD    DATAB   			; point data area
			MOV     M,A     			; clear it
			LXI     H,BEGPR 			; point start
			SHLD    PROGE   			; reset program end
			MOV     M,A     			; clear it
;
LOADF3:		LXI     H,IOBUF
			MVI		A,00
			MOV		M,A 
			INX		H 

			MVI		A,RTS_ON
			OUT		UBRTC
			PUSH	B
			MVI		B,4
			MVI		C,0
			CALL	SETLED
			POP		B

LOADF4:		CALL	CBIN
			CPI		10
			JZ		LOADF4
			CPI		13
			JZ		LOADF5
			MOV		M,A 
			INX		H 
			JMP		LOADF4
LOADF5:		MVI		A,00
			MOV		M,A
			MVI		A,RTS_OFF
			OUT		UBRTC
			LXI		H,IOBUF+1
			MOV		A,M 
			CPI		's'
			JZ		LOADF3
			CPI		0
			JZ		LOADF6
			PUSH	B
			MVI		B,4
			MVI		C,1
			CALL	SETLED
			POP		B
			CALL	LOAD0
			JMP		LOADF3
LOADF6:		MVI		A,RTS_ON
			OUT		UBRTC
			PUSH	B
			MVI		B,4
			MVI		C,0
			CALL	SETLED
			POP		B
			CALL    RESET
			JMP		GETCM
;
;-----------------------------------------------------------------------
;	LOAD BASIC LINE IN STORAGE
;-----------------------------------------------------------------------				
;
LOAD0:		LXI     H,IOBUF+1
			CALL    PACK    			; go pack the number into b,c
			MOV     A,B     			; get hi byte of line number
			ORA     C       			; plus low byte
			JZ      EXEC    			; brif exec statement
			PUSH    B       			; save line number
			LXI     D,IMMED+1       	; point save area
			XCHG            			; flip/flop
			MOV     M,B    		 		; put lo line
			INX     H       			; point next
			MOV     M,C     			; put lo line
			INX     H       			; point next
			MVI     B,3     			; init count
LOAD1:  	LDAX    D       			; get a byte
			MOV     M,A     			; put it down
			INR     B       			; count it
			INX     H       			; point next
			INX     D       			; ditto
			ORA     A       			; test byte just moved
			JNZ     LOAD1   			; loop
			MOV     A,B     			; get count
			STA     IMMED   			; store the count
			POP     B       			; get line num
			CALL    LOCAT   			; go find requested line number
			PUSH    H       			; save h,l
			JC      LOAD5   			; brif if line not found
LOAD2:  	MOV     D,H     			; copy addr
			MOV     E,L     			; to d,e
			MVI     B,0     			; get a zero
			MOV     C,M    				; get len
			DAD     B       			; point next statement
LOAD3:  	MOV     A,M     			; get len next statement
			ORA     A      				; test it
			JZ      LOAD8   			; brif end
			MOV     B,A     			; set length
			CALL    COPYH   			; else move line
			JMP     LOAD3   			; loop
LOAD8:  	XCHG            			; put new addr to h,l
			MOV     M,A     			; mark end
			SHLD    PROGE   			; and update address
LOAD5:  	LDA     IMMED   			; get len of insert
			CPI     4       			; test if delete
			JZ      GETCM   			; brif is
			MOV     C,A     			; set lo len
			MVI     B,0     			; zero hi len
			LHLD    PROGE   			; get end of prog
			MOV     D,H     			; copy to
			MOV     E,L     			; d,e
			DAD     B       			; disp len of insert
			SHLD    PROGE   			; update end point
			POP     B       			; get addr
LOAD6:  	LDAX    D       			; get a byte
			MOV     M,A     			; copy it
			DCX     D       			; point prior
			DCX     H       			; ditto
			MOV     A,D    				; get hi addr
			CMP     B       			; compare
			JZ      LOAD7   			; brif hi equal
			JNC     LOAD6   			; brif not less
LOAD7:  	MOV     A,E     			; get lo addr
			CMP     C       			; compare
			JNC     LOADA    			; must test for 00 boundary
			JMP     LOADB    			; go around boundary test code
LOADA:  	CMA             			; compliment low line number
			CMP     C       			; and compare to start
			JNZ     LOAD6   			; brif not =
			ORA     A       			; not test for 00list
			JNZ     LOAD6   			; this is usual case
LOADB:  	INX     D       			; point forward
			LXI     H,IMMED 			; point insert
			MOV     B,M     			; get length
			CALL    COPYH   			; go move it
			RET

				
;
;-----------------------------------------------------------------------
;	SAVE CURRENT BASIC PROGRAM TO FILE ON SDCARD
;-----------------------------------------------------------------------				
;				
SAVEFILE:	CALL	TSTC				; call to ram address
			PUSH    H       			; save ptr
			LXI		H,CMDSTRW
			MVI		A,'w'
			MOV		M,A 
			INX		H
			MVI		A,'r'
			MOV		M,A 
			INX		H 
			MVI		A,'i'
			MOV		M,A 
			INX		H 
			MVI		A,'t'
			MOV		M,A 
			INX		H 
			MVI		A,'e'
			MOV		M,A 
			INX		H 
			MVI		A,' '
			MOV		M,A 
			LXI     H,CMDSTRW+6			; point buffer	
			MVI     B,12     			; file name length
			POP     D       			; restore cmd ptr
SAVEF0:  	LDAX    D       			; fetch file name
			CPI		'"'
			JNZ     SAVEF00
			INX		D
			JMP     SAVEF0
SAVEF00		ORA     A       			; test
			JZ      SAVEF2
			MOV     M,A     			; store char
			INX     D       			; name ptr
SAVEF1:  	INX     H       			; buffer ptr
			DCR     B       			; count
			JNZ     SAVEF0
SAVEF2:		MVI		A,13
			MOV     M,A
			INX		H 
			MVI		A,0
			MOV     M,A
										; Open file for write
			LXI		H,CMDSTRW	
			CALL	PMSGB
			LXI		H,0					; init counter
			SHLD	COUNTER
			LXI     D,0     			; get a zero in d
			XCHG            			; flip to h,l
			SHLD    LINEL   			; save it
			LXI     H,9999H 			; get high number in h,l
			SHLD    LINEH   			; save it
			XCHG            			; flip back
SAVEL1:  	LXI     H,BEGPR 			; point beginning of program
SAVEL2:  	MOV     A,M     			; get len code
			ORA     A       			; test if end of program
			JZ      SAVEL9   			; brif end of pgm
			SUI     3       			; subtract three
			MOV     B,A     			; save len
			PUSH  	H
			PUSH  	D
			LHLD	COUNTER 			; get Counter
			MOV		E,B					; 
			MVI		D,0
			INX 	D 
			INX		D 
			DAD		D 					; Add line length
			SHLD 	COUNTER 			; Store again
			POP 	D 
			POP 	H 
			INX     H       			; point high byte of line#
			XCHG            			; flip h,l to d,e
			LHLD    LINEL   			; get low line to test
			XCHG            			; restore h,l
			MOV     A,M     			; get low byte of line number
			CMP     D       			; comp with linel
			JC      SAVEL8   			; brif less
			JNZ     SAVEL4   			; brif not equal
			INX     H       			; point next
			MOV     A,M     			; get next byte of line#
			DCX     H       			; point back
			CMP     E       			; comp low bytes
			JC      SAVEL8   			; brif less
SAVEL4:  	XCHG            			; save h,l in d,e
			LHLD    LINEH   			; get high line for test
			XCHG            			; restore h,l
			MOV     A,M     			; get line byte
			CMP     D       			; compare high bytes
			JZ      SAVEL5   			; brif equal
			JNC     SAVEL9   			; brif higher
			JMP     SAVEL6   			; go around
SAVEL5: 	INX     H       			; point next
			MOV     A,M     			; get next byte
			DCX     H       			; point back
			CMP     E       			; compare low bytes
			JZ      SAVEL6   			; brif equal
			JNC     SAVEL9   			; brif higher
SAVEL6:  	LXI     D,IOBUF 			; point buffer area
			CALL    LINEO   			; convert line number
SAVEL7:  	MOV     A,M     			; get a byte
			STAX    D       			; put it to buffer
			INX     D       			; point next buff
			INX     H       			; point next prog
			DCR     B       			; decr ctr
			JNZ     SAVEL7   			; loop
			DCX 	D
			MVI		A,13
			STAX	D
			INX		D 
			MVI		A,00 
			STAX	D 
			PUSH    H       			; save hl addr
			LXI     H,IOBUF				; point to start of buffer 
			CALL    PMSGB   			; go type it
			POP     H       			; retrieve h addr
			PUSH	B
			MVI		B,4
			MVI		C,1
			CALL	SETLED
			POP		B
			JMP     SAVEL2   			; continue
SAVEL8:  	MOV     E,B     			; put len  in e
			MVI     D,0     			; clear d
			DAD     D       			; point next statement
			INX     H       			; point next
			INX     H       			; point len code
			JMP     SAVEL2   			; go list it
SAVEL9:		LXI		H,EOFMSG
			CALL	PMSGB
			PUSH	B
			MVI		B,4
			MVI		C,0
			CALL	SETLED
			POP		B
			CALL    RESET
			LXI     H,BYTESMSG1
			CALL    PMSGA
			LHLD	COUNTER 
			INX  	H
			INX		H 
			INX     H 
			CALL 	DECOUT
			LXI     H,BYTESMSG2
			CALL    PMSGA
			JMP		GETCM
EOFMSG:		DB		13,0
BYTESMSG1:	DB 		"File length is ",0
BYTESMSG2:	DB		" bytes",13,10,0
;
;
;
;-----------------------------------------------------------------------
;	FORMAT SDCARD
;-----------------------------------------------------------------------				
;
FORMATSD:	LXI		H,FORMATCMD
			CALL	PMSGB
			MVI		A,RTS_ON
			OUT		UBRTC
			CALL	CBIN
			JMP		CONTROL2
;
FORMATCMD:	DB 		"format",13,0
;
;
;-----------------------------------------------------------------------
;	LIST FILE(S) ON SDCARD
;-----------------------------------------------------------------------				
;
FILES:		LXI		H,DIRCMD
			CALL	PMSGB
			MVI		A,RTS_ON
			OUT		UBRTC
			CALL	CBIN
			JMP		CONTROL2
;
DIRCMD:		DB 		"files",13,0
;
;
;-----------------------------------------------------------------------
;	DELETE FILE ON SDCARD
;-----------------------------------------------------------------------				
;	
DELETESD:	CALL	TSTC				; call to ram address
			PUSH    H       			; save ptr
			LXI		H,CMDSTRW			; Prepare "read ....
			MVI		A,'d'
			MOV		M,A 
			INX		H
			MVI		A,'e'
			MOV		M,A 
			INX		H 
			MVI		A,'l'
			MOV		M,A 
			INX		H 
			MVI		A,' '
			MOV		M,A 
			LXI     H,CMDSTRW+4			; point buffer	
			MVI     B,12     			; file name length
			POP     D       			; restore cmd ptr
DELSD0:  	LDAX    D       			; fetch file name 
			CPI		'"'
			JNZ     DELSD00
			INX		D
			JMP     DELSD0
DELSD00:	ORA     A       			; test
			JZ      DELSD2
			MOV     M,A     			; store char
			INX     D       			; name ptr
DELSD1:  	INX     H       			; buffer ptr
			DCR     B       			; 	count
			JNZ     DELSD0
DELSD2:		MVI		A,13				; add carriage return
			MOV     M,A
			INX		H 
			MVI		A,0
			MOV     M,A
			LXI		H,CMDSTRW			; point to command
			CALL	PMSGB				; send it
			MVI		A,RTS_ON
			OUT		UBRTC
			CALL	CBIN
			JMP		CONTROL2
;
; ------------------------------------------------------------------------------------			
;
;
ROMEND  EQU     $-1
;
        ORG     RAMSTRT					; ram starts of 32k boundary

;
; all code above this point is read only and can be prom'ed
;
;
RAM     		EQU     $
;
BZERO   		EQU     $
FORNE:  		DS      1      			; # entrys in table (must be here)
				DS      112    			; room for 8 nests (must be here)
TAPES:  		DS      1      			; tape switch (must be here)
DIMSW:  		DS      1      			; dim switch (must be here)
OUTSW:  		DS      1      			; output switch (must be here)
ILSW:   		DS      1      			; input line switch (must be here)
RUNSW:  		DS      1      			; run switch(must be here)
EDSW:   		DS      1      			; mode switch(must be here)
EZERO   		EQU     $
;
LINEN:  		DS      5
IMMED:  		DS      82     			; immediate command storage area
IOBUF:  		DS      82     			; input/output buffer
STRIN:  		DS      256    			; string buffer area
OUTA:   		DS      3      			; *** filled in at run time
INDX:   		DS      2      			; holds variable name of for/next
REL:    		DS      1      			; holds the relation in an if statement
IFTYP:  		DS      1      			; holds type code of left side
TVAR1:  		DS      4      			; temp storage
TVAR2:  		DS      4      			; ditto
TEMP1:  		DS      4      			; temp storage for functions
TEMP2:  		DS      4
TEMP3:  		DS      4
TEMP4:  		DS      4
TEMP5:  		DS      4
TEMP6:  		DS      4
TEMP7:  		DS      4
LINEL:  		DS      2      			; holds min line number in list
LINEH:  		DS      2      			; holds max line number in list
PROMP:  		DS      1      			; holds prompt char
EXPRS:  		DS      2      			; holds addr of expression
ADDR1:  		DS      2      			; holds temp address
ADDR2:  		DS      2      			; holds temp address
ADDR3:  		DS      2      			; holds statement add during expr eval
FACC:   		DS      4
FTEMP:  		DS      12
PARCT:  		DS      1
SPCTR:  		DS      2
CMACT:  		DS      1      			; count of commas
FNARG:  		DS      4      			; symbolic arg & address
STMT:   		DS      2      			; holds addr of current statement
ENDLI: 			DS      2      			; holds addr of multi statement ptr
MULTI:  		DS      1      			; switch 0=no, 1=multi statement line
DEXP:   		DS      1
COLUM:  		DS      1      			; current tty column
RNDX:   		DS      2      			; random variable storage
RNDY:   		DS      2      			; the rnd<x>,trnd<x>,and rndsw
RNDZ:   		DS      2      			; must be kept in order
RNDS:   		DS      2
TRNDX:  		DS      2
TRNDY:  		DS      2
TRNDZ:  		DS      2
TRNDS: 			DS      2
RNDSW:  		DS      1
FNMOD:  		DS      1      			; switch, 0=not, <>0 = in def fn
LINE:   		DS      2      			; hold add of prev line num
STACK:  		DS      2      			; holds addr of start of return stack
PRSW:   		DS      1      			; on=print ended with , or					; 
NS:     		DS      1      			; holds last type (numeric/string)
DATAP:  		DS      2      			; address of current data statement
DATAB:  		DS      2      			; address of data pool
PROGE:  		DS      2      			; address of program end
				DS      1      			; data statement flag (must be here)
;
SPDAMSG:		DS 	    2				; baudrate message pointer console
UARTMSG:		DS 		2
;
CURSORSTR:		DS      16
XPOS:			DS		1				; x position
YPOS:			DS		1				; y position
COUNTER:		DS 		2
BOOTMODE:		DS		1
;
RAMSIG:			DS		2				; Contains 55AA if ram initialized
CMDSTRW:		DS      80

;
;
				DS      1      			; data statement flag (must be here)
		
BEGPR	EQU		$
;

        END
