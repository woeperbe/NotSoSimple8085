;		.TITLE		'8080 FIG-FORTH 1.3 VERSION 0  18JUL81'
;		FIG-FORTH  RELEASE 1.3  FOR THE 8080 PROCESSOR
;
;		ALL PUBLICATIONS OF THE FORTH INTEREST GROUP
;		ARE PUBLIC DOMAIN.  THEY MAY BE FURTHER
;		DISTRIBUTED BY THE INCLUSION OF THIS CREDIT
;		NOTICE:
;
;		THIS PUBLICATION HAS BEEN MADE AVAILABLE BY THE
;				     FORTH INTEREST GROUP
;				     P. O. BOX 1105
;				     SAN CARLOS, CA 94070
;
;		IMPLEMENTATION BY:		( 790528 )
;				JOHN CASSADY
;				339 15TH STREET
;				OAKLAND,CA 94612
;		MODIFIED BY:
;		   		KIM HARRIS
;               Bill McGee 30 Mar 83 for Apple ][
;		ACKNOWLEDGEMENTS:
;				GEORGE SHAW
;				TERRY HOLMES
;				MIKE PERRY
;				DON COLBURN
;				GEORGE FLAMMER
;				ROBT. D. VILLWOCK
; Changed by J4F for the Z80-MBC2 (HW ref. A040618) on May 2020
		.PAGE
;
;----------------------------------------------------------
;
;		RELEASE & VERSION NUMBERS
;
FIGREL		.EQU		1		; FIG RELEASE #
FIGREV		.EQU		3		; FIG REVISION #
USRVER		.EQU		1		; USER VERSION #
;
;		ASCII CHARACTERS USED
;
ABL					.EQU		20H		; SPACE
ACR					.EQU		0DH		; CARRIAGE RETURN
ADOT				.EQU		02EH		; PERIOD
BELL				.EQU		07H		; (^G)
BSIN				.EQU		08H		; INPUT BACKSPACE
BSOUT				.EQU		08H		; OUTPUT BACKSPACE (^H)
DLE					.EQU		10H		; (^P)
LF					.EQU		0AH		; LINE FEED
FF					.EQU		0CH		; FORM FEED (^L)
;
;		MEMORY ALLOCATION
;
EM			.EQU			08000H
NSCR		.EQU			4					; NUMBER OF 1024 BYTE SCREENS
KBBUF		.EQU			1024				; DATA BYTES PER DISK BUFFER
US			.EQU			40H					; USER VARIABLES SPACE
RTS			.EQU			0A0H				; RETURN STACK & TERM BUFF SPACE
;
CO			.EQU			KBBUF+4				; DISK BUFFER + 2 HEADER + 2 TAIL
NBUF		.EQU			NSCR*1024/KBBUF		; NUMBER OF BUFFERS
BUF1		.EQU			EM-CO*NBUF			; ADDR FIRST DISK BUFFER
INITR0		.EQU			BUF1-US				; (R0)
INITS0		.EQU			INITR0-RTS			; (S0)
;
		.PAGE
;
;-------------------------------------------------------
;
FALSE		.EQU		0
TRUE		.EQU		1
APPLE		.EQU		FALSE
HAVEDISK 	.EQU  		TRUE
HAVECPM		.EQU 		FALSE
HAVEZ80MBC2 .EQU 		TRUE						; Flag for the Z80-MBC2
DEBUG 		.EQU 		FALSE

			.ORG		100H
; ENTRY FOR INITIAL EXECUTION AND COLD START
ORIG		NOP
			JMP		CLD			; VECTOR TO COLD START
; ENTRY FOR WARM START
			NOP
			JMP		WRM			; VECTOR TO WARM START
;
			.DB		FIGREL		; FIG RELEASE #
			.DB		FIGREV		; FIG REVISION #
			.DB		USRVER		; USER VERSION #
			.DB		0EH			; IMPLEMENTATION ATTRIBUTES
OFOR		.DW		FLAST		; TOPMOST WORD IN FORTH VOCABULARY
			.DW		BSIN		; BKSPACE CHARACTER
			.DW		INITR0		; INIT (UP)
;<<<<<< FOLLOWING USED BY COLD;
;		MUST BE IN SAME ORDER AS USER VARIABLES
OCLD0		.DW		INITS0		; INIT (S0)
			.DW		INITR0		; INIT (R0)
			.DW		INITS0		; INIT (TIB)
			.DW		31				; INIT (WIDTH)
			.DW		0				; INIT (WARNING)
			.DW		FLAST				; INIT (FENCE)
			.DW		INITDP				; INIT (DP)
			.DW		FORTH+6				; INIT (VOC-LINK)
OCLD1		.EQU		$
;<<<<<< END DATA USED BY COLD
			.DW		5H,0B320H		; CPU NAME		( HW,LW )
;								     ( 32 BIT, BASE 36 INTEGER )
OED			.DW		ELAST		; LAST EDITOR DEF.
;OASM		.DW		ALAST		; SAME FOR ASSEMBLER IF RESIDENT
;
;
;						+---------------+
;		B +ORIGIN		| . . .W:I.E.B.A|		IMPLEMENTATION
;						+---------------+		ATTRIBUTES
;						       ^ ^ ^ ^ ^
;						       | | | | +-- PROCESSOR ADDR =
;						       | | | |     { 0 BYTE | 1 WORD }
;						       | | | +---- HIGH BYTE AT
;						       | | |       { 0 LOW ADDR |
;						       | | |		     1 HIGH ADDR }
;						       | | +------ ADDR MUST BE EVEN
;						       | |		   { 0 YES | 1 NO }
;						       | +-------- INTERPRETER IS
;						       |		   { 0 PRE | 1 POST }
;						       |		   INCREMENTING
;						       +---------- { 0 ABOVE SUFFICIENT
;										     | 1 OTHER DIFFER-
;										     ENCES EXIST }
;
		.PAGE
;
;------------------------------------------------------
;
;		FORTH REGISTERS
;
;		FORTH		8080		FORTH PRESERVATION RULES
;		-----		----		------------------------
;		IP		BC		SHOULD BE PRESERVED ACROSS
;						  FORTH WORDS
;		W		DE		SOMETIMES OUTPUT FROM NEXT
;						MAY BE ALTERED BEFORE JMP'ING TO NEXT
;						INPUT ONLY WHEN 'DPUSH' CALLED
;		SP		SP		SHOULD BE USED ONLY AS DATA STACK
;						  ACROSS FORTH WORDS
;						MAY BE USED WITHIN FORTH WORDS
;						  IF RESTORED BEFORE 'NEXT'
;				HL		NEVER OUTPUT FROM NEXT
;						INPUT ONLY WHEN 'HPUSH' CALLED
;
UP		.DW		INITR0		; USER AREA POINTER
RPP		.DW		INITR0		; RETURN STACK POINTER
;
;------------------------------------------------------
;
;		COMMENT CONVENTIONS:
;
;		=		MEANS		"IS EQUAL TO"
;		<-		MEANS		ASSIGNMENT
;
;		NAME		=		ADDRESS OF NAME
;		(NAME)		=		CONTENTS AT NAME
;		((NAME))=		INDIRECT CONTENTS
;
;		CFA		=		ADDRESS OF CODE FIELD
;		LFA		=		ADDRESS OF LINK FIELD
;		NFA		=		ADDR OF START OF NAME FIELD
;		PFA		=		ADDR OF START OF PARAMETER FIELD
;
;		S1		=		ADDR OF 1ST WORD OF PARAMETER STACK
;		S2		=		ADDR OF 2ND WORD OF PARAMETER STACK
;		R1		=		ADDR OF 1ST WORD OF RETURN STACK
;		R2		=		ADDR OF 2ND WORD OF RETURN STACK
;		( ABOVE STACK POSITIONS VALID BEFORE & AFTER EXECUTION
;		OF ANY WORD, NOT DURING. )
;
;		LSB		=		LEAST SIGNIFICANT BIT
;		MSB		=		MOST SIGNIFICANT BIT
;		LB		=		LOW BYTE
;		HB		=		HIGH BYTE
;		LW		=		LOW WORD
;		HW		=		HIGH WORD
;		( MAY BE USED AS SUFFIX TO ABOVE NAMES )
;
		.PAGE
;
;---------------------------------------------------
;		DEBUG SUPPORT
;
;		TO USE:
;		(1)		SET 'BIP' TO IP VALUE TO HALT, CANNOT BE CFA
;		(2)		SET MONITOR'S BREAKPOINT PC TO 'BREAK'
;						OR PATCH 'HLT' INSTR. THERE
;		(3)		PATCH A 'JMP TNEXT' AT 'NEXT'
;		WHEN (IP) = (BIP) CPU WILL HALT
;
BIP			.DW		0		; BREAKPOINT ON IP VALUE
;
TNEXT		LXI		H,BIP
			MOV		A,M		; LB
			CMP		C
			JNZ		TNEXT1
			INX		H
			MOV		A,M		; HB
			CMP		B
			JNZ		TNEXT1
BREAK		NOP				; PLACE BREAKPOINT HERE
			NOP
			NOP
TNEXT1		LDAX		B
			INX		B
			MOV		L,A
			JMP		NEXT+3
;
;--------------------------------------------------
;
;		NEXT, THE FORTH ADDRESS INTERPRETER
;		  ( POST INCREMENTING VERSION )
;
TRACE:	.DB 0
DPUSH	PUSH	D
HPUSH	PUSH	H
NEXT:	LDAX	B		;(W) <- ((IP))
		INX		B		;(IP) <- (IP)+2
		MOV		L,A
		LDAX	B
		INX		B
		MOV		H,A		; (HL) <- CFA
		.IF DEBUG
		PUSH PSW
		LDA TRACE
		ORA A
		JZ NOTRACE
		PUSH H
		CALL PRINTI
		.DB "NEXT:",0
		LHLD RPP
		MVI A,INITR0 & 0FFH		;INITAL RP
		SUB L						; DIFFERENCE
		MOV L,A
		MVI A,' '
TRCILP:	JZ TRCILX				; INDENT AS PER RPP "DEPTH"
		CALL PUTCHAR
		DCR L
		JNZ TRCILP
TRCILX:		
		POP H
		PUSH H
		CALL HEXHL
		.IF 0
		MVI A,'.'
		CALL PUTCHAR
		CALL HEXHL				;OUTPUT RPP VAL
		.ENDIF
		MVI A,'.'
		CALL PUTCHAR
		LXI H,0
		DAD SP
		CALL HEXHL
		MVI A, 13
		CALL PUTCHAR
		MVI A, 10
		CALL PUTCHAR
		POP H
NOTRACE: POP PSW
		.ENDIF
NEXT1:	MOV		E,M		;(PC) <- ((W))
		INX		H
		MOV		D,M
		XCHG
		PCHL				; NOTE: (DE) = CFA+1
;
		.PAGE
;
;				FORTH DICTIONARY
;
;
;		DICTIONARY FORMAT:
;
;								BYTE
;		ADDRESS		NAME				CONTENTS
;		------- ----				--------
;										  ( MSB=1
;										  ( P=PRECEDENCE BIT
;										  ( S=SMUDGE BIT
;		NFA		NAME FIELD		1PS<LEN>  < NAME LENGTH
;								0<1CHAR>  MSB=0, NAME'S 1ST CHAR
;								0<2CHAR>
;								  ...
;								1<LCHAR>  MSB=1, NAME'S LAST CHR
;		LFA		LINK FIELD		<LINKLB>  = PREVIOUS WORD'S NFA
;								<LINKHB>
;LABEL:		CFA		CODE FIELD		<CODELB>  = ADDR CPU CODE
;								<CODEHB>
;		PFA		PARAMETER		<1PARAM>  1ST PARAMETER BYTE
;				FIELD				<2PARAM>
;								  ...
;
;
DP0		.DB		83H		; LIT
		.DB		"LI"
		.DB		'T'+80H
		.DW		0		; (LFA)=0 MARKS END OF DICTIONARY
LIT		.DW		$+2		;(S1) <- ((IP))
		LDAX		B		; (HL) <- ((IP)) = LITERAL
		INX		B		; (IP) <- (IP) + 2
		MOV		L,A		; LB
		LDAX		B		; HB
		INX		B
		MOV		H,A
		JMP		HPUSH		; (S1) <- (HL)
 ;
		.DB		87H		; EXECUTE
		.DB		"EXECUT"
		.DB		'E'+80H
		.DW		LIT-6
EXEC	.DW		$+2
		POP		H		; (HL) <- (S1) = CFA
		JMP		NEXT1
;
		.DB		86H		; BRANCH
		.DB		"BRANC"
		.DB		'H'+80H
		.DW		EXEC-0AH
BRAN	.DW		$+2		;(IP) <- (IP) + ((IP))
BRAN1	MOV		H,B		; (HL) <- (IP)
		MOV		L,C
		MOV		E,M		; (DE) <- ((IP)) = BRANCH OFFSET
		INX		H
		MOV		D,M
		DCX		H
		DAD		D		; (HL) <- (HL) + ((IP))
		MOV		C,L		; (IP) <- (HL)
		MOV		B,H
		JMP		NEXT
;
		.DB		87H		; 0BRANCH
		.DB		"0BRANC"
		.DB		'H'+80H
		.DW		BRAN-9
ZBRAN	.DW		$+2
		POP		H
		MOV		A,L
		ORA		H
		JZ		BRAN1		; IF (S1)=0 THEN BRANCH
		INX		B		; ELSE SKIP BRANCH OFFSET
		INX		B
		JMP		NEXT
;
		.DB		86H		; (LOOP)		1.3
		.DB		"(LOOP"
		.DB		')'+80H
		.DW		ZBRAN-0AH
XLOOP	.DW		$+2
		LHLD		RPP		; ((HL)) = INDEX = (R1)
		MOV		E,M		; (DE) <- INDEX
		INX		H
		MOV		D,M
		INX		D		; INDEX <- INDEX + 1
		MOV		M,D		; (R1) <- NEW INDEX
		DCX		H
		MOV		M,E
		INX		H
		INX		H		; ((HL)) = LIMIT
		MOV		A,E		; IF INDEX < LIMIT
		SUB		M
		MOV		A,D
		INX		H
		SBB		M
		JM		BRAN1		; THEN LOOP AGAIN
		INX		H		; ELSE DONE
		SHLD		RPP		; DISCARD R1 & R2
		INX		B		; SKIP BRANCH OFFSET
		INX		B
		JMP		NEXT
;
		.DB		87H		; (+LOOP)		1.3
		.DB		"(+LOOP"
		.DB		')'+80H
		.DW		XLOOP-9
XPLOO	.DW		$+2
		POP		D		; (DE) <- INCR
		LHLD		RPP		; ((HL)) = INDEX
		MOV		A,M		; INDEX <- INDEX + INCR
		ADD		E
		MOV		M,A
		MOV		E,A
		INX		H
		MOV		A,M
		ADC		D
		MOV		M,A
		INX		H		; ((HL)) = LIMIT
		INR		D
		DCR		D
		MOV		D,A		; (DE) <- NEW INDEX
		JM		XLOO2		; IF INCR > 0
		MOV		A,E		; THEN (A) <- INDEX - LIMIT
		SUB		M
		MOV		A,D
		INX		H
		SBB		M
		JMP		XLOO3
XLOO2	MOV		A,M		; ELSE (A) <- LIMIT - INDEX
		SUB		E
		INX		H
		MOV		A,M
		SBB		D
;						  IF (A) < 0
XLOO3	JM		BRAN1		; THEN LOOP AGAIN
		INX		H		; ELSE DONE
		SHLD		RPP		; DROP R1 AND R2
		INX		B		; SKIP BRANCH OFFSET
		INX		B
		JMP		NEXT
;
		.DB		84H		; (DO)
		.DB		"(DO"
		.DB		')'+80H
		.DW		XPLOO-0AH
XDO		.DW		$+2
		LHLD		RPP		; (RP) <- (RP) - 4
		DCX		H
		DCX		H
		DCX		H
		DCX		H
		SHLD		RPP
		POP		D		; (R1) <- (S1) = INIT INDEX
		MOV		M,E
		INX		H
		MOV		M,D
		POP		D		; (R2) <- (S2) = LIMIT
		INX		H
		MOV		M,E
		INX		H
		MOV		M,D
		JMP		NEXT
;
		.DB		81H		; I		1.3
		.DB		'I'+80H
		.DW		XDO-7
IDO		.DW		$+2		;(S1) <- (R1) , (R1) UNCHANGED
		LHLD		RPP
IDO1	MOV		E,M		; (DE) <- (R1)
		INX		H
		MOV		D,M
		PUSH		D		; (S1) <- (DE)
		JMP		NEXT
;
		.DB		82H		; I'		1.3
		.DB		"I"
		.DB		27H+80H
		.DW		IDO-4
IPRIM	.DW		$+2
		LHLD		RPP
		INX		H
		INX		H		; ((HL)) = (R2)
		JMP		IDO1
;
		.DB		81H		; J		1.3
		.DB		'J'+80H
		.DW		IPRIM-5
J		.DW		$+2
		LHLD		RPP
		INX		H
		INX		H
		INX		H
		INX		H		; ((HL)) = (R3)
		JMP		IDO1
;
		.DB		85H		; DIGIT				1.3
		.DB		"DIGI"
		.DB		'T'+80H
		.DW		IDO-4
DIGIT	.DW		$+2
		POP		H		; (L) <- (S1)LB = ASCII CHR TO BE
;						 CONVERTED
		MVI		H,0
		POP		D		; (DE) <- (S2) = BASE VALUE
		MOV		A,E		; (BASE) < 255 ASSUMED
		SUI		30H		; IF CHR > "0"
		CPI		0AH		; AND IF CHR > "9"
		JC		DIGI1		; THEN GO TEST BASE
		SUI		7
		CPI		0AH		; OR IF CHR >= "A"
		JC		DIGI2
;						; THEN VALID NUMERIC OR ALPHA CHR
DIGI1	CMP		L		; IF DIGIT VALUE < BASE VALUE
		MOV		E,A		; (E) <- CONVERTED DIGIT
		MVI		L,1		; (L) <- TRUE
		JC		DPUSH		; THEN SUCCESSFUL
;						  (S2) <- CONVERTED DIGIT
;						  (S1) <- TRUE
;						; ELSE INVALID DIGIT CHR
DIGI2	MOV		L,H		; (HL) <- FALSE
		JMP		HPUSH		; (S1) <- FALSE
;
		.DB		86H		; (FIND)		1.3
		.DB		"(FIND"		;
		.DB		')'+80H
		.DW		DIGIT-8
PFIND	.DW		$+2
		POP		D		; (DE) <- NFA
PFIN1	POP		H		; (HL) <- STRING ADDR
		PUSH	H		; SAVE STRING ADDR FOR NEXT ITERATION
;;; 		call		PRINTNFA
		LDAX	D
		XRA		M		; CHECK LENGTHS & SMUDGE BIT
		ANI		3FH
		JNZ		PFIN4		; LENGTHS DIFFERENT
;						; LENGTHS MATCH, CHECK EACH CHR
PFIN2	INX		H		; (HL) <- ADDR NEXT CHR IN STRING
		INX		D		; (DE) <- ADDR NEXT CHR IN NF
		LDAX	D
		XRA		M		; IGNORE MSB
		JZ		PFIN2		; MATCH SO FAR, LOOP AGAIN
		ADD		A
		JNZ		PFIN3		; NO MATCH
		LXI		H,5		; STRING MATCHES
		DAD		D		; ((SP)) <- PFA
		XTHL
;						; BACK UP TO LENGTH BYTE OF NF = NFA
PFIN6	DCX		D
		LDAX	D
		ORA		A
		JP		PFIN6		; IF MSB = 1 THEN (DE) = NFA
		MOV		E,A		; (DE) <- LENGTH BYTE
		MVI		D,0
		LXI		H,1		; (HL) <- TRUE
		JMP		DPUSH  ; RETURN, NF FOUND
;		ABOVE NF NOT A MATCH, TRY ANOTHER
PFIN3	JC		PFIN5		; IF NOT END OF NF
PFIN4	INX		D		; THEN FIND END OF NF
		LDAX	D
		ORA		A
		JP		PFIN4
PFIN5	INX		D		; (DE) <- LFA
		XCHG
		MOV		E,M		; (DE) <- (LFA)
		INX		H
		MOV		D,M
		MOV		A,D
		ORA		E		; IF (LFA) <> 0
		JNZ		PFIN1		; THEN TRY PREVIOUS DICT. DEF.
;						; ELSE END OF DICTIONARY
		POP		H		; DISCARD STRING ADDR
		PUSH	D		; (S1) <- FALSE
		JMP		NEXT
;
		.DB		87H		; ENCLOSE		1.3
		.DB		"ENCLOS"
		.DB		'E'+80H
		.DW		PFIND-9
ENCL	.DW		$+2
		POP		D		; (DE) <- (S1) = DELIMITER CHAR
		POP		H		; (HL) <- (S2) = ADDR TEXT TO SCAN
		PUSH	H		; (S4) <- ADDR
		MOV		A,E		; (E) <- DELIM CHR
		LXI		D,-1		; INIT CHR OFFSET COUNTER
		DCX		H		; (HL) <- ADDR-1
;						; SKIP OVER LEADING DELIMITER CHRS
ENCL1	INX		H
		INX		D
		CMP		M		; IF TEXT CHR = DELIM CHR
		JZ		ENCL1		; THEN LOOP AGAIN
;						; ELSE NON-DELIM CHR FOUND
		PUSH	D		; (S3) <- (DE) = OFFSET TO 1ST NON-DELIM
		MOV		D,A		; SAVE A
		MOV		A,M		; IF 1ST NON-DELIM = NULL
		ANA		A
		MOV		A,D		; RESTORE A
		POP		D
		PUSH	D
		JNZ		ENCL2
		INX		D		; THEN (S2) <- OFFSET TO BYTE
		PUSH	D		;   FOLLOWING NULL
		DCX		D		; (S1) <- OFFSET TO NULL
		PUSH	D
		JMP		NEXT
;						; ELSE TEXT CONTAINS NON-DELIM &
;						  NON-NULL CHR
ENCL2	PUSH	B		; SAVE IP
		MOV		B,A		; (B) <- DELIM CHR
ENCL5	INX		H		; (HL) <- ADDR NEXT CHR
		INX		D		; (DE) <- OFFSET TO NEXT CHR
		MOV		A,M		; IF NEXT CHR <> DELIM CHR
		CMP		B
		JZ		ENCL4
		ANA		A		; AND IF NEXT CHR <> NULL
		JNZ		ENCL5		; THEN CONTINUE SCAN
;						; ELSE CHR = NULL
ENCL3	POP		B		; RESTORE IP
		PUSH	D		; (S2) <- OFFSET TO NULL
		PUSH	D		; (S1) <- OFFSET TO NULL
		JMP		NEXT
;						; ELSE CHR = DELIM CHR
ENCL4	POP		B		; RESTORE IP
		PUSH	D		; (S2) <- OFFSET TO BYTE
;						  FOLLOWING TEXT
		INX		D		; (S1) <- OFFSET TO 2 BYTES AFTER
;						    END OF WORD
		PUSH	D
		JMP		NEXT
;
		.DB		84H		; EMIT
		.DB		"EMI"
		.DB		'T'+80H
		.DW		ENCL-0AH
EMIT	.DW		DOCOL
		.DW		PEMIT
		.DW		ONE,OUTT
		.DW		PSTOR,SEMIS
;
		.DB		83H		; KEY
		.DB		"KE"
		.DB		'Y'+80H
		.DW		EMIT-7
KEY		.DW		$+2
		JMP		PKEY
;
		.DB		89H		; ?TERMINAL
		.DB		"?TERMINA"
		.DB		'L'+80H
		.DW		KEY-6
QTERM	.DW		$+2
		LXI		H,0
		JMP		PQTER
;
		.DB		82H		; CR
		.DB		"C"
		.DB		'R'+80H
		.DW		QTERM-0CH
CR		.DW		$+2
		JMP		PCR
;
		.DB		85H		; CMOVE
		.DB		"CMOV"
		.DB		'E'+80H
		.DW		CR-5
CMOVE	.DW		$+2
		MOV		L,C		; (HL) <- (IP)
		MOV		H,B
		POP		B		; (BC) <- (S1) = #CHRS
		POP		D		; (DE) <- (S2) = DEST ADDR
		XTHL				; (HL) <- (S3) = SOURCE ADDR
;						; (S1) <- (IP)
		JMP		CMOV2		; RETURN IF #CHRS = 0
CMOV1	MOV		A,M		; ((DE)) <- ((HL))
		INX		H		; INC SOURCE ADDR
		STAX		D
		INX		D		; INC DEST ADDR
		DCX		B		; DEC #CHRS
CMOV2	MOV		A,B
		ORA		C
		JNZ		CMOV1		; REPEAT IF #CHRS <> 0
		POP		B		; RESTORE (IP) FROM (S1)
		JMP		NEXT
;
		.DB		86H		; >CMOVE		1.3
		.DB		">CMOV"
		.DB		'E'+80H
		.DW		CMOVE-8
GCMOV	.DW		$+2
		MOV		L,C		; (HL) <- (IP)
		MOV		H,B
		POP 		B		; (BC) <- (S1) = #CHRS
		POP		D		; (DE) <- (S2) = DEST ADDR
		XTHL				; (HL) <- (S3) = SOURCE ADDR
;						  (S1) <- (IP) TEMP.
		DAD		B		; (HL) <- END SOURCE ADDR
		DCX		H
		XCHG
		DAD		B
		DCX		H
		XCHG				; (DE) <- END DEST ADDR
		JMP		GCMOV2		; RETURN IF #CHRS = 0
GCMOV1	MOV		A,M		; ((DE)) <- ((HL))
		DCX		H		; DECR SOURCE ADDR
		STAX		D
		DCX		D		; DECR DEST ADDR
		DCX		B		; DECR #CHRS
GCMOV2	MOV		A,B		; IF #CHRS LEFT <> 0
		ORA		C
		JNZ		GCMOV1		; THEN LOOP AGAIN
		POP		B		; RESTORE IP
		JMP		NEXT
;
		.DB		82H		; U*				1.3
;								16X16 UNSIGNED MULTIPLY
		.DB		'U'		; AVG EXECUTION TIME = 880 CYCLES
		.DB		'*'+80H
		.DW		GCMOV-9
USTAR	.DW		$+2
		POP		D		; (DE) <- MPLIER
		POP		H		; (HL) <- MPCAND
		PUSH	B		; SAVE IP
		MOV		B,H
		MOV		A,L		; (BA) <- MPCAND
		CALL	MPYX		; (AHL)1 <- MPCAND.LB * MPLIER
;						       1ST PARTIAL PRODUCT
		PUSH	H		; SAVE (HL)1
		MOV		H,A
		MOV		A,B
		MOV		B,H		; SAVE (A)1
		CALL	MPYX		; (AHL)2 <- MPCAND.HB * MPLIER
;						       2ND PARTIAL PRODUCT
		POP		D		; (DE) <- (HL)1
		MOV		C,D		; (BC) <- (AH)1
;		FORM SUM OF PARTIALS:
;						   (AHL) 1
;						+ (AHL)  2
;						--------
;						  (AHLE)
		DAD		B		; (HL) <- (HL)2 + (AH)1
		ACI		0		; (AHLE) <- (BA) * (DE)
		MOV		D,L
		MOV		L,H
		MOV		H,A		; (HLDE) <- MPLIER * MPCAND
		POP		B		; RESTORE IP
		PUSH	D		; (S2) <- PRODUCT.LW
		JMP		HPUSH		; (S1) <- PRODUCT.HW
;
;		MULTIPLY PRIMITIVE
;				(AHL) <- (A) * (DE)
;		#BITS =		 24		  8		16
MPYX	LXI		H,0		; (HL) <- 0 = PARTIAL PRODUCT.LW
		MVI		C,4		; LOOP COUNTER
MPYX1	DAD		H		; LEFT SHIFT (AHL) 24 BITS
		RAL
		JNC		MPYX2		; IF NEXT MPLIER BIT = 1
		DAD		D		; THEN ADD MPCAND
		ACI		0
MPYX2	DAD		H
		RAL
		JNC		MPYX3
		DAD		D
		ACI		0
MPYX3	DCR		C		; IF NOT LAST MPLIER BIT
		JNZ		MPYX1		; THEN LOOP AGAIN
		RET				; ELSE DONE
;
		.DB		82H		; U/				1.3
		.DB		"U"
		.DB		'/'+80H
		.DW		USTAR-5
USLAS	.DW		$+2
		MOV		H,B
		MOV		L,C		; (HL) <- (IP)
		POP		B		; (BC) <- (S1) = DENOMINATOR
		POP		D		; (DE) <- (S2) = NUMERATOR.HIGH
		XTHL				; (S1) <- (IP)
		XCHG				; (HLDE) = NUMERATOR, 32 BITS
		MOV		A,L
		SUB		C
		MOV		A,H		; IF OVERFLOW
		SBB		B
		JNC		USBAD		; THEN RETURN BAD VALUE
		MOV		A,H
		MOV		H,L
		MOV		L,D		; (AHL) <- 24 BITS OF NUMERATOR
		MVI		D,8		; (D) <- INIT COUNTER
		PUSH	D		; SAVE D & E
		CALL	USLA		; PARTIAL DIVISION
		POP		D		; RESTORE COUNTER & NUM.MSBYTE
		PUSH	H		; (S1) <- (L) = BYTE OF QUOTIENT
		MOV		L,E
		CALL	USLA
		MOV		D,A
		MOV		E,H		; (DE) <- REMAINDER
		POP		B		; RESTORE QUOTIENT.HIGH
		MOV		H,C		; (HL) <- QUOTIENT
		POP		B		; RESTORE (IP)
		JMP		DPUSH		; SUCCESSFULLY DONE
;
USL0	MOV		E,A
		MOV		A,H
		SUB		C
		MOV		H,A
		MOV		A,E
		SBB		B
		JNC		USL1		; IF CARRY
		MOV		A,H		; THEN ADD (BC) INTO (AH)
		ADD		C
		MOV		H,A
		MOV		A,E
		DCR		D
		RZ				; RETURN FROM USLA
;
USLA	DAD		H		; 24BIT LEFT-SHIFT ( *2 )
		RAL
		JNC		USL0		; SUBTRACT & TEST
		MOV		E,A
		MOV		A,H
		SUB		C		; (AH) <- (AH) - (BC)
		MOV		H,A
		MOV		A,E
		SBB		B
USL1	INR		L		; 1 BIT OF QUOT INTO RIGHT SIDE
		DCR		D		;   OF (AHL)
		JNZ		USLA		; CONTINUE DIVISION
		RET				; ALL 8 TRIAL COMPLETE
;
USBAD	LXI		H,-1		; OVERFLOW, RETURN 32BIT -1
		POP		B		; RESTORE (IP)
		PUSH	H
		JMP		HPUSH
;
		.DB		85H		; U/MOD				1.3
		.DB		"U/MO"		; SAME AS U/
		.DB		'D'+80H
		.DW		USLAS-5
USLMD	.DW		USLAS+2
;
		.DB		83H		; AND
		.DB		"AN"
		.DB		'D'+80H
		.DW		USLMD-8
ANDD	.DW		$+2		; (S1) <- (S1) AND (S2)
		POP		D
		POP		H
		MOV		A,E
		ANA		L
		MOV		L,A
		MOV		A,D
		ANA		H
		MOV		H,A
		JMP		HPUSH
;
		.DB		82H		; OR
		.DB		"O"
		.DB		'R'+80H
		.DW		ANDD-6
ORR		.DW		$+2		; (S1) <- (S1) OR (S2)
		POP		D
		POP		H
		MOV		A,E
		ORA		L
		MOV		L,A
		MOV		A,D
		ORA		H
		MOV		H,A
		JMP		HPUSH
;
		.DB		83H		; XOR
		.DB		"XO"
		.DB		'R'+80H
		.DW		ORR-5
XORR	.DW		$+2		; (S1) <- (S1) XOR (S2)
		POP		D
		POP		H
		MOV		A,E
		XRA		L
		MOV		L,A
		MOV		A,D
		XRA		H
		MOV		H,A
		JMP		HPUSH
;
		.DB		83H		; SP@
		.DB		"SP"
		.DB		'@'+80H
		.DW		XORR-6
SPAT	.DW		$+2		;(S1) <- (SP)
		LXI		H,0
		DAD		SP		; (HL) <- (SP)
		JMP		HPUSH		; (S1) <- (HL)
;
		.DB		83H		; STACK POINTER STORE
		.DB		"SP"
		.DB		'!'+80H
		.DW		SPAT-6
SPSTO	.DW		$+2		;(SP) <- (S0) ( USER VARIABLE )
		LHLD		UP		; (HL) <- USER VAR BASE ADDR
		LXI		D,6
		DAD		D		; (HL) <- S0
		MOV		E,M		; (DE) <- (S0)
		INX		H
		MOV		D,M
		XCHG
		SPHL				; (SP) <- (S0)
		JMP		NEXT
;
		.DB		83H		; RP@
		.DB		"RP"
		.DB		'@'+80H
		.DW		SPSTO-6
RPAT	.DW		$+2		;(S1) <- (RP)
		LHLD		RPP
		JMP		HPUSH
;
		.DB		83H		; RETURN STACK POINTER STORE
		.DB		"RP"
		.DB		'!'+80H
		.DW		RPAT-6
RPSTO	.DW		$+2		;(RP) <- (R0) ( USER VARIABLE )
		LHLD		UP		; (HL) <- USER VARIABLE BASE ADDR
		LXI		D,8
		DAD		D		; (HL) <- R0
		MOV		E,M		; (DE) <- (R0)
		INX		H
		MOV		D,M
		XCHG
		SHLD		RPP		; (RP) <- (R0)
		JMP		NEXT
;
		.DB		82H		; ;S
		.DB		";"
		.DB		'S'+80H
		.DW		RPSTO-6
SEMIS	.DW		$+2		;(IP) <- (R1)
		LHLD		RPP
		MOV		C,M		; (BC) <- (R1)
		INX		H
		MOV		B,M
		INX		H
		SHLD		RPP		; (RP) <- (RP) + 2
		JMP		NEXT
;
		.DB		84H		; EXIT				1.3
		.DB		"EXI"
		.DB		'T'+80H
		.DW		SEMIS-5
EXIT	.DW		SEMIS+2
;
		.DB		85H		; LEAVE
		.DB		"LEAV"
		.DB		'E'+80H
		.DW		EXIT-7
LEAVE		.DW		$+2		;LIMIT <- INDEX
		LHLD		RPP
		MOV		E,M		; (DE) <- (R1) = INDEX
		INX		H
		MOV		D,M
		INX		H
		MOV		M,E		; (R2) <- (DE) = LIMIT
		INX		H
		MOV		M,D
		JMP		NEXT
;
		.DB		82H		; >R
		.DB		">"
		.DB		'R'+80H
		.DW		LEAVE-8
TOR		.DW		$+2		;(R1) <- (S1)
		POP		D		; (DE) <- (S1)
		LHLD		RPP
		DCX		H		; (RP) <- (RP) - 2
		DCX		H
		SHLD		RPP
		MOV		M,E		; ((HL)) <- (DE)
		INX		H
		MOV		M,D
		JMP		NEXT
;
		.DB		82H		; R>
		.DB		"R"
		.DB		'>'+80H
		.DW		TOR-5
FROMR	.DW		$+2		;(S1) <- (R1)
		LHLD	RPP
		MOV		E,M		; (DE) <- (R1)
		INX		H
		MOV		D,M
		INX		H
		SHLD	RPP		; (RP) <- (RP) + 2
		PUSH	D		; (S1) <- (DE)
		JMP		NEXT
;
		.DB		81H		; R
		.DB		'R'+80H
		.DW		FROMR-5
RR		.DW		IDO+2
;
		.DB		82H		; R@				1.3
		.DB		'R'
		.DB		'@'+80H
		.DW		RR-4
RAT		.DW		IDO+2
;
		.DB		82H		; 0=
		.DB		'0'
		.DB		'='+80H
		.DW		RAT-5
ZEQU	.DW		$+2
		POP		H		; (HL) <- (S1)
		MOV		A,L
		ORA		H		; IF (HL) = 0
		LXI		H,0		; THEN (HL) <- FALSE
		JNZ		ZEQU1
		INX		H		; ELSE (HL) <- TRUE
ZEQU1	JMP		HPUSH		; (S1) <- (HL)
;
		.DB		83H		; NOT				1.3
		.DB		"NO"
		.DB		'T'+80H
		.DW		ZEQU-5
NOTT	.DW		ZEQU+2
;
		.DB		82H		; 0<
		.DB		'0'
		.DB		'<'+80H
		.DW		NOTT-6
ZLESS	.DW		$+2
		POP		PSW		; (A) <- (S1.HIGH)
		ORA		A		; IF (A) < 0
		LXI		H,0
		JP		HPUSH		; THEN (S1) <- FALSE
		INR		L
		JMP		HPUSH		; ELSE (S1) <- TRUE
;
		.DB		81H		; +
		.DB		'+'+80H
		.DW		ZLESS-5
PLUS	.DW		$+2		;(S1) <- (S1) + (S2)
		POP		D
		POP		H
		DAD		D
		JMP		HPUSH
;
		.DB		82H		; D+		(4-2)
		.DB		'D'		; XLW XHW  YLW YHW  ---  SLW SHW
		.DB		'+'+80H		; S4  S3   S2  S1        S2  S1
		.DW		PLUS-4
DPLUS	.DW		$+2
		LXI		H,6
		DAD		SP		; ((HL)) = XLW
		MOV		E,M		; (DE) = XLW
		MOV		M,C		; SAVE IP ON STACK
		INX		H
		MOV		D,M
		MOV		M,B
		POP		B		; (BC) <- YHW
		POP		H		; (HL) <- YLW
		DAD		D
		XCHG				; (DE) <- YLW + XLW = SUM.LW
		POP		H		; (HL) <- XHW
		MOV		A,L
		ADC		C
		MOV		L,A		; (HL) <- YHW + XHW + CARRY
		MOV		A,H
		ADC		B
		MOV		H,A
		POP		B		; RESTORE IP
		PUSH		D		; (S2) <- SUM.LW
		JMP		HPUSH		; (S1) <- SUM.HW
;
		.DB		85H		; MINUS
		.DB		"MINU"
		.DB		'S'+80H
		.DW		DPLUS-5
MINUS	.DW		$+2		;(S1) <- -(S1)		( 2'S COMPLEMENT )
		POP		H
		MOV		A,L
		CMA
		MOV		L,A
		MOV		A,H
		CMA
		MOV		H,A
		INX		H
		JMP		HPUSH
;
		.DB		86H		; NEGATE		1.3
		.DB		"NEGAT"
		.DB		'E'+80H
		.DW		MINUS-8
NEG		.DW		MINUS+2
;
		.DB		86H		; DMINUS
		.DB		"DMINU"
		.DB		'S'+80H
		.DW		NEG-9
DMINU	.DW		$+2
		POP		H		; (HL) <- HW
		POP		D		; (DE) <- LW
		SUB		A
		SUB		E		; (DE) <- 0 - (DE)
		MOV		E,A
		MVI		A,0
		SBB		D
		MOV		D,A
		MVI		A,0
		SBB		L		; (HL) <- 0 - (HL)
		MOV		L,A
		MVI		A,0
		SBB		H
		MOV		H,A
		PUSH		D		; (S2) <- LW
		JMP		HPUSH		; (S1) <- HW
;
		.DB		84H		; OVER
		.DB		"OVE"
		.DB		'R'+80H
		.DW		DMINU-9
OVER	.DW		$+2
		POP		D
		POP		H
		PUSH		H
		JMP		DPUSH
;
		.DB		84H		; DROP
		.DB		"DRO"
		.DB		'P'+80H
		.DW		OVER-7
DROP	.DW		$+2
		POP		H
		JMP		NEXT
;
		.DB		84H		; SWAP
		.DB		"SWA"
		.DB		'P'+80H
		.DW		DROP-7
SWAP	.DW		$+2
		POP		H
		XTHL
		JMP		HPUSH
;
		.DB		83H		; DUP
		.DB		"DU"
		.DB		'P'+80H
		.DW		SWAP-7
DUP		.DW		$+2
		POP		H
		PUSH		H
		JMP		HPUSH
;
		.DB		84H		; 2DUP
		.DB		"2DU"
		.DB		'P'+80H
		.DW		DUP-6
TDUP	.DW		$+2
		POP		H
		POP		D
		PUSH		D
		PUSH		H
		JMP		DPUSH
;
		.DB		85H		; 2DROP				1.3
		.DB		"2DRO"
		.DB		'P'+80H
		.DW		TDUP-7
TDROP	.DW		$+2
		POP		H
		POP		H
		JMP		NEXT
;
		.DB		85H		; 2SWAP				1.3
		.DB		"2SWA"
		.DB		'P'+80H
		.DW		TDROP-8
TSWAP	.DW		$+2
;						NOTE: THIS WON'T WORK WITH INTERRUPTS
		POP		H		; (HL) <- (S1)
		POP		D		; (DE) <- (S2)
		XTHL				; (HL) <- (S3)
;						; (S3) <- (HL)
		XCHG				; (DE) <- (HL)
;						; (HL) <- (DE)
		INX		SP
		INX		SP
		XTHL				; (HL) <- (S4)
;						; (S4) <- (HL)
		DCX		SP
		DCX		SP
		XCHG				; (DE) <- (HL)
;						; (HL) <- (DE)
		JMP		DPUSH		; (S1) <- (HL)
;						; (S2) <- (DE)
;
		.DB		85H		; 2OVER
		.DB		"2OVE"
		.DB		'R'+80H
		.DW		TSWAP-8
TOVER	.DW		$+2
;				NOTE: THIS WON'T WORK WITH INTERRUPTS
		INX		SP
		INX		SP
		INX		SP
		INX		SP
		POP		H		; (HL) <- (S3)
		PUSH		H
		INX		SP
		INX		SP
		POP		D		; (DE) <- (S4)
		PUSH		D
		DCX		SP
		DCX		SP
		DCX		SP
		DCX		SP
		DCX		SP
		DCX		SP
		JMP		DPUSH		; (S1) <- (HL)
;						; (S2) <- (DE)
;
		.DB		82H		; PLUS STORE
		.DB		"+"
		.DB		'!'+80H
		.DW		TOVER-8
PSTOR	.DW		$+2		;((S1)) <- ((S1)) + (S2)
		POP		H		; (HL) <- (S1) = ADDR
		POP		D		; (DE) <- (S2) = INCR
		MOV		A,M		; ((HL)) <- ((HL)) + (DE)
		ADD		E
		MOV		M,A
		INX		H
		MOV		A,M
		ADC		D
		MOV		M,A
		JMP		NEXT
;
		.DB		86H		; TOGGLE
		.DB		"TOGGL"
		.DB		'E'+80H
		.DW		PSTOR-5
TOGGL	.DW		$+2		;((S2)) <- ((S2)) XOR (S1)LB
		POP		D		; (E) <- BYTE MASK
		POP		H		; (HL) <- ADDR
		MOV		A,M
		XRA		E
		MOV		M,A		; (ADDR) <- (ADDR) XOR (E)
		JMP		NEXT
;
		.DB		81H		; @
		.DB		'@'+80H
		.DW		TOGGL-9
AT		.DW		$+2		;(S1) <- ((S1))
		POP		H		; (HL) <- ADDR
		MOV		E,M		; (DE) <- (ADDR)
		INX		H
		MOV		D,M
		PUSH		D		; (S1) <- (DE)
		JMP		NEXT
;
		.DB		82H		; C@
		.DB		"C"
		.DB		'@'+80H
		.DW		AT-4
CAT		.DW		$+2		;(S1) <- ((S1))LB
		POP		H		; (HL) <- ADDR
		MOV		L,M		; (HL) <- (ADDR)LB
		MVI		H,0
		JMP		HPUSH
;
		.DB		82H		; 2@
		.DB		"2"
		.DB		'@'+80H
		.DW		CAT-5
TAT		.DW		$+2
		POP		H		; (HL) <- ADDR HW
		LXI		D,2
		DAD		D		; (HL) <- ADDR LW
		MOV		E,M		; (DE) <- LW
		INX		H
		MOV		D,M
		PUSH		D		; (S2) <- LW
		LXI		D,-3		; (HL) <- ADDR HW
		DAD		D
		MOV		E,M		; (DE) <- HW
		INX		H
		MOV		D,M
		PUSH		D		; (S1) <- HW
		JMP		NEXT
;
		.DB		81H		; STORE
		.DB		'!'+80H
		.DW		TAT-5
STORE		.DW		$+2		;((S1)) <- (S2)
		POP		H		; (HL) <- (S1) = ADDR
		POP		D		; (DE) <- (S2) = VALUE
		MOV		M,E		; ((HL)) <- (DE)
		INX		H
		MOV		M,D
		JMP		NEXT
;
		.DB		82H		; C STORE
		.DB		"C"
		.DB		'!'+80H
		.DW		STORE-4
CSTOR	.DW		$+2		;((S1))LB <- (S2)LB
		POP		H		; (HL) <- (S1) = ADDR
		POP		D		; (DE) <- (S2) = BYTE
		MOV		M,E		; ((HL))LB <- (E)
		JMP		NEXT
;
		.DB		82H		; 2 STORE
		.DB		"2"
		.DB		'!'+80H
		.DW		CSTOR-5
TSTOR	.DW		$+2
		POP		H		; (HL) <- ADDR
		POP		D		; (DE) <- HW
		MOV		M,E		; (ADDR) <- HW
		INX		H
		MOV		M,D
		INX		H		; (HL) <- ADDR LW
		POP		D		; (DE) <- LW
		MOV		M,E		; (ADDR+2) <- LW
		INX		H
		MOV		M,D
		JMP		NEXT
;
		.DB		0C1H		; :
		.DB		':'+80H
		.DW		TSTOR-5
COLON	.DW		DOCOL
		.DW		QEXEC
		.DW		SCSP
		.DW		CURR
		.DW		AT
		.DW		CONT
		.DW		STORE
		.DW		CREAT
		.DW		RBRAC
		.DW		PSCOD
;						EXECUTION-TIME CODE:
DOCOL		LHLD		RPP		;				1.3
		DCX		H		; (RP) <- (RP) - 2
		DCX		H
		SHLD		RPP
		MOV		M,C
		INX		H
		MOV		M,B		; (R1) <- (IP)
		INX		D		; (DE) <- CFA+2 = (W)
		MOV		C,E		; (IP) <- (DE) = (W)
		MOV		B,D
		JMP		NEXT
;
		.DB		0C1H		; ;
		.DB		';'+80H
		.DW		COLON-4
SEMI	.DW		DOCOL
		.DW		QCSP
		.DW		COMP
		.DW		SEMIS
		.DW		SMUDG
		.DW		LBRAC
		.DW		SEMIS
;
		.DB		84H		; NOOP
		.DB		"NOO"
		.DB		'P'+80H
		.DW		SEMI-4
NOOP	.DW		DOCOL
		.DW		SEMIS
 ;
		.DB		88H		; CONSTANT
		.DB		"CONSTAN"
		.DB		'T'+80H
		.DW		NOOP-7
CON		.DW		DOCOL
		.DW		CREAT
		.DW		SMUDG
		.DW		COMMA
		.DW		PSCOD
DOCON	INX		D		; (DE) <- PFA
		XCHG
		MOV		E,M		; (DE) <- (PFA)
		INX		H
		MOV		D,M
		PUSH		D		; (S1) <- (PFA)
		JMP		NEXT
;
		.DB		88H		; VARIABLE
		.DB		"VARIABL"
		.DB		'E'+80H
		.DW		CON-0BH
VAR		.DW		DOCOL
		.DW		CON
		.DW		PSCOD
DOVAR	INX		D		; (DE) <- PFA
		PUSH		D		; (S1) <- PFA
		JMP		NEXT
;
		.DB		84H		; USER
		.DB		"USE"
		.DB		'R'+80H
		.DW		VAR-0BH
USER	.DW		DOCOL
		.DW		CON
		.DW		PSCOD
DOUSE	INX		D		; (DE) <- PFA
		XCHG
		MOV		E,M		; (DE) <- USER VARIABLE OFFSET
		MVI		D,0
		LHLD		UP		; (HL) <- USER VARIABLE BASE ADDR
		DAD		D		; (HL) <- (HL) + (DE)
 .if 0		
		CALL PRINTI
		 .DB "DOUSE:",0
		CALL HEXHL
 .endif
		JMP		HPUSH		; (S1) <- BASE + OFFSET
;
		.DB		81H		; 0
		.DB		'0'+80H
		.DW		USER-7
ZERO	.DW		DOCON
		.DW		0
;
		.DB		81H		; 1
		.DB		'1'+80H
		.DW		ZERO-4
ONE		.DW		DOCON
		.DW		1
;
		.DB		81H		; 2
		.DB		'2'+80H
		.DW		ONE-4
TWO		.DW		DOCON
		.DW		2
;
		.DB		81H		; 3
		.DB		'3'+80H
		.DW		TWO-4
THREE	.DW		DOCON
		.DW		3
;
		.DB		82H		; BL
		.DB		"B"
		.DB		'L'+80H
		.DW		THREE-4
BL		.DW		DOCON
		.DW		20H
;
		.DB		83H		; C/L ( CHARACTERS/LINE )
		.DB		"C/"
		.DB		'L'+80H
		.DW		BL-5
CSLL	.DW		DOCON
		.DW		64
;
		.DB		85H		; FIRST
		.DB		"FIRS"
		.DB		'T'+80H
		.DW		CSLL-6
FIRST	.DW		DOCON
		.DW		BUF1
;
		.DB		85H		; LIMIT
		.DB		"LIMI"
		.DB		'T'+80H
		.DW		FIRST-8
LIMIT	.DW		DOCON
		.DW		EM
;
		.DB		85H		; B/BUF ( BYTES/BUFFER )
		.DB		"B/BU"
		.DB		'F'+80H
		.DW		LIMIT-8
BBUF	.DW		DOCON
		.DW		KBBUF
;
		.DB		85H		; B/SCR ( BUFFERS/SCREEN )
		.DB		"B/SC"
		.DB		'R'+80H
		.DW		BBUF-8
BSCR	.DW		DOCON
		.DW		1024/KBBUF		; 1024 BYTES/SCREEN
;
		.DB		87H		; +ORIGIN
		.DB		"+ORIGI"
		.DB		'N'+80H
		.DW		BSCR-8
PORIG	.DW		DOCOL
		.DW		LIT
		.DW		ORIG
		.DW		PLUS
		.DW		SEMIS
;
;		USER VARIABLES
;
		.DB		82H		; S0
		.DB		"S"
		.DB		'0'+80H
		.DW		PORIG-0AH
SZERO	.DW		DOUSE
		.DB		6
;
		.DB		82H		; R0
		.DB		"R"
		.DB		'0'+80H
		.DW		SZERO-5
RZERO	.DW		DOUSE
		.DB		8
;
		.DB		83H		; TIB
		.DB		"TI"
		.DB		'B'+80H
		.DW		RZERO-5
TIB		.DW		DOUSE
		.DB		0AH
;
		.DB		85H		; WIDTH
		.DB		"WIDT"
		.DB		'H'+80H
		.DW		TIB-6
WIDTH	.DW		DOUSE
		.DB		0CH
;
		.DB		87H		; WARNING
		.DB		"WARNIN"
		.DB		'G'+80H
		.DW		WIDTH-8
WARN	.DW		DOUSE
		.DB		0EH
;
		.DB		85H		; FENCE
		.DB		"FENC"
		.DB		'E'+80H
		.DW		WARN-0AH
FENCE	.DW		DOUSE
		.DB		10H
;
		.DB		82H		; DP
		.DB		"D"
		.DB		'P'+80H
		.DW		FENCE-8
DP		.DW		DOUSE
		.DB		12H
;
		.DB		88H		; VOC-LINK
		.DB		"VOC-LIN"
		.DB		'K'+80H
		.DW		DP-5
VOCL	.DW		DOUSE
		.DB		14H
;
		.DB		83H		; BLK
		.DB		"BL"
		.DB		'K'+80H
		.DW		VOCL-0BH
BLK		.DW		DOUSE
		.DB		16H
;
		.DB		82H		; IN
		.DB		"I"
		.DB		'N'+80H
		.DW		BLK-6
INN		.DW		DOUSE
		.DB		18H
;
		.DB		83H		; OUT
		.DB		"OU"
		.DB		'T'+80H
		.DW		INN-5
OUTT	.DW		DOUSE
		.DB		1AH
;
		.DB		83H		; SCR
		.DB		"SC"
		.DB		'R'+80H
		.DW		OUTT-6
SCR		.DW		DOUSE
		.DB		1CH
;
		.DB		86H		; OFFSET
		.DB		"OFFSE"
		.DB		'T'+80H
		.DW		SCR-6
OFSET	.DW		DOUSE
		.DB		1EH
;
		.DB		87H		; CONTEXT
		.DB		"CONTEX"
		.DB		'T'+80H
		.DW		OFSET-9
CONT	.DW		DOUSE
		.DB		20H
;
		.DB		87H		; CURRENT
		.DB		"CURREN"
		.DB		'T'+80H
		.DW		CONT-0AH
CURR	.DW		DOUSE
		.DB		22H
;
		.DB		85H		; STATE
		.DB		"STAT"
		.DB		'E'+80H
		.DW		CURR-0AH
STATE	.DW		DOUSE
		.DB		24H
;
		.DB		84H		; BASE
		.DB		"BAS"
		.DB		'E'+80H
		.DW		STATE-8
BASE	.DW		DOUSE
		.DB		26H
;
		.DB		83H		; DPL
		.DB		"DP"
		.DB		'L'+80H
		.DW		BASE-7
DPL		.DW		DOUSE
		.DB		28H
;
		.DB		83H		; FLD
		.DB		"FL"
		.DB		'D'+80H
		.DW		DPL-6
FLD		.DW		DOUSE
		.DB		2AH
;
		.DB		83H		; CSP
		.DB		"CS"
		.DB		'P'+80H
		.DW		FLD-6
CSPP	.DW		DOUSE
		.DB		2CH
;
		.DB		82H		; R#
		.DB		"R"
		.DB		'#'+80H
		.DW		CSPP-6
RNUM	.DW		DOUSE
		.DB		2EH
;
		.DB		83H		; HLD
		.DB		"HL"
		.DB		'D'+80H
		.DW		RNUM-5
HLD		.DW		DOUSE
		.DB		30H
;
;		END OF USER VARIABLES
;
		.DB		82H		; 1+
		.DB		"1"
		.DB		'+'+80H
		.DW		HLD-6
ONEP	.DW		$+2
		POP		H
		INX		H
		JMP		HPUSH
;
		.DB		82H		; 2+
		.DB		"2"
		.DB		'+'+80H
		.DW		ONEP-5
TWOP	.DW		$+2
		POP		H
		INX		H
		INX		H
		JMP		HPUSH
;
		.DB		82H		; 1-				1.3
		.DB		"1"
		.DB		'-'+80H
		.DW		TWOP-5
ONEM	.DW		$+2
		POP		H
		DCX		H
		JMP		HPUSH
;
		.DB		82H		; 2-				1.3
		.DB		"2"
		.DB		'-'+80H
		.DW		ONEM-5
TWOM	.DW		$+2
		POP		H
		DCX		H
		DCX		H
		JMP		HPUSH
;
		.DB		82H		; 2*				1.3
		.DB		"2"
		.DB		'*'+80H
		.DW		TWOM-5
TWOT	.DW		$+2
		POP		H
		STC
		CMC
		MOV		A,L
		RAL		
		MOV		L,A
		MOV		A,H
		RAL
		MOV		H,A
		JMP		HPUSH
;
		.DB		82H		; 2/				1.3
		.DB		"2"
		.DB		'/'+80H
		.DW		TWOT-5
TWOD	.DW		$+2
		POP		H
		MOV		A,H
		RLC
		RRC
		RAR
		MOV		H,A
		MOV		A,L
		RAR
		MOV		L,A
		JMP		HPUSH
;
		.DB		84H		; HERE
		.DB		"HER"
		.DB		'E'+80H
		.DW		TWOD-5
HERE	.DW		DOCOL
		.DW		DP
		.DW		AT
		.DW		SEMIS
;
		.DB		85H		; ALLOT
		.DB		"ALLO"
		.DB		'T'+80H
		.DW		HERE-7
ALLOT	.DW		DOCOL
		.DW		DP
		.DW		PSTOR
		.DW		SEMIS
;
		.DB		81H		; ,
		.DB		','+80H
		.DW		ALLOT-8
COMMA	.DW		DOCOL
		.DW		HERE
		.DW		STORE
		.DW		TWO
		.DW		ALLOT
		.DW		SEMIS
;
		.DB		82H		; C,
		.DB		"C"
		.DB		','+80H
		.DW		COMMA-4
CCOMM	.DW		DOCOL
		.DW		HERE
		.DW		CSTOR
		.DW		ONE
		.DW		ALLOT
		.DW		SEMIS
;
;		SUBROUTINE USED BY - AND <
;						; (HL) <- (HL) - (DE)
SSUB	MOV		A,L		; LB
		SUB		E
		MOV		L,A
		MOV		A,H		; HB
		SBB		D
		MOV		H,A
		RET
;
		.DB		81H		; -
		.DB		'-'+80H
		.DW		CCOMM-5
SUBB	.DW		$+2
		POP		D		; (DE) <- (S1) = Y
		POP		H		; (HL) <- (S2) = X
		CALL		SSUB
		JMP		HPUSH		; (S1) <- X - Y
;
		.DB		81H		; =
		.DB		'='+80H
		.DW		SUBB-4
EQUAL	.DW		DOCOL
		.DW		SUBB
		.DW		ZEQU
		.DW		SEMIS
;
		.DB		82H		; <>				1.3
		.DB		"<"
		.DB		'>'+80H
		.DW		EQUAL-4
NEQU	.DW		DOCOL
		.DW		SUBB
		.DW		ZEQU
		.DW		ZEQU
		.DW		SEMIS
;
		.DB		84H		; =NOT				1.3
		.DB		"=NO"
		.DB		'T'+80H
		.DW		NEQU-5
ENOT	.DW		DOCOL
		.DW		NEQU
		.DW		SEMIS
;
		.DB		81H		; <
		.DB		'<'+80H				; X  <  Y
		.DW		ENOT-7				; S2    S1
LESS	.DW		$+2
		POP		D		; (DE) <- (S1) = Y
		POP		H		; (HL) <- (S2) = X
		MOV		A,D		; IF X & Y HAVE SAME SIGNS
		XRA		H
		JM		LES1
		CALL		SSUB		; (HL) <- X - Y
LES1	INR		H		; IF (HL) >= 0
		DCR		H
		JM		LES2
		LXI		H,0		; THEN X >= Y
		JMP		HPUSH		; (S1) <- FALSE
LES2	LXI		H,1		; ELSE X < Y
		JMP		HPUSH		; (S1) <- TRUE
;
		.DB		82H		; U< ( UNSIGNED < )
		.DB		"U"
		.DB		'<'+80H
		.DW		LESS-4
ULESS	.DW		DOCOL,TDUP
		.DW		XORR,ZLESS
		.DW		ZBRAN
		.DW		ULES1-$		; IF
		.DW		DROP,ZLESS
		.DW		ZEQU
		.DW		BRAN
		.DW		ULES2-$
ULES1	.DW		SUBB,ZLESS		; ELSE
ULES2	.DW		SEMIS				; ENDIF
;
		.DB		81H		; >
		.DB		'>'+80H
		.DW		ULESS-5
GREAT	.DW		DOCOL
		.DW		SWAP
		.DW		LESS
		.DW		SEMIS
;
		.DB		82H		; 0>				1.3
		.DB		"0"
		.DB		'>'+80H
		.DW		GREAT-4
ZGREA	.DW		DOCOL
		.DW		ZERO,GREAT
		.DW		SEMIS
;
		.DB		83H		; ROT
		.DB		"RO"
		.DB		'T'+80H
		.DW		ZGREA-5
ROT		.DW		$+2
		POP		D
		POP		H
		XTHL
		JMP		DPUSH
;
		.DB		84H		; -ROT				1.3
		.DB		"-RO"
		.DB		'T'+80H
		.DW		ROT-6
DROT	.DW		DOCOL
		.DW		ROT,ROT
		.DW		SEMIS
;
		.DB		85H		; SPACE
		.DB		"SPAC"
		.DB		'E'+80H
		.DW		DROT-7
SPACE	.DW		DOCOL
		.DW		BL
		.DW		EMIT
		.DW		SEMIS
;
		.DB		84H		; -DUP
		.DB		"-DU"
		.DB		'P'+80H
		.DW		SPACE-8
DDUP	.DW		DOCOL
		.DW		DUP
		.DW		ZBRAN		; IF
		.DW		DDUP1-$
		.DW		DUP		; ENDIF
DDUP1	.DW		SEMIS
;
		.DB		84H		; ?DUP				1.3
		.DB		"?DU"
		.DB		'P'+80H
		.DW		DDUP-7
QDUP	.DW		DDUP+2
;
		.DB		88H		; TRAVERSE
		.DB		"TRAVERS"
		.DB		'E'+80H
		.DW		QDUP-7
TRAV	.DW		DOCOL
		.DW		SWAP
TRAV1	.DW		OVER		; BEGIN
		.DW		PLUS
		.DW		LIT
		.DW		7FH
		.DW		OVER
		.DW		CAT
		.DW		LESS
		.DW		ZBRAN		; UNTIL
		.DW		TRAV1-$
		.DW		SWAP
		.DW		DROP
		.DW		SEMIS
;
		.DB		86H		; LATEST
		.DB		"LATES"
		.DB		'T'+80H
		.DW		TRAV-0BH
LATES	.DW		DOCOL
		.DW		CURR
		.DW		AT
		.DW		AT
		.DW		SEMIS
;
		.DB		83H		; LFA
		.DB		"LF"
		.DB		'A'+80H
		.DW		LATES-9
LFA		.DW		DOCOL
		.DW		LIT
		.DW		4
		.DW		SUBB
		.DW		SEMIS
;
		.DB		83H		; CFA
		.DB		"CF"
		.DB		'A'+80H
		.DW		LFA-6
CFA		.DW		DOCOL
		.DW		TWO
		.DW		SUBB
		.DW		SEMIS
;
		.DB		83H		; NFA
		.DB		"NF"
		.DB		'A'+80H
		.DW		CFA-6
NFA		.DW		DOCOL
		.DW		LIT
		.DW		5
		.DW		SUBB
		.DW		LIT
		.DW		-1
		.DW		TRAV
		.DW		SEMIS
;
		.DB		83H		; PFA
		.DB		"PF"
		.DB		'A'+80H
		.DW		NFA-6
PFA		.DW		DOCOL
		.DW		ONE
		.DW		TRAV
		.DW		LIT
		.DW		5
		.DW		PLUS
		.DW		SEMIS
;
		.DB		84H		; STORE CSP
		.DB		"!CS"
		.DB		'P'+80H
		.DW		PFA-6
SCSP	.DW		DOCOL
		.DW		SPAT
		.DW		CSPP
		.DW		STORE
		.DW		SEMIS
;
		.DB		86H		; ?ERROR
		.DB		"?ERRO"
		.DB		'R'+80H
		.DW		SCSP-7
QERR	.DW		DOCOL
		.DW		SWAP
		.DW		ZBRAN		; IF
		.DW		QERR1-$
		.DW		ERROR
		.DW		BRAN		; ELSE
		.DW		QERR2-$
QERR1	.DW		DROP		; ENDIF
QERR2	.DW		SEMIS
;
		.DB		85H		; ?COMP
		.DB		"?COM"
		.DB		'P'+80H
		.DW		QERR-9
QCOMP	.DW		DOCOL
		.DW		STATE
		.DW		AT
		.DW		ZEQU
		.DW		LIT
		.DW		11H
		.DW		QERR
		.DW		SEMIS
;
		.DB		85H		; ?EXEC
		.DB		"?EXE"
		.DB		'C'+80H
		.DW		QCOMP-8
QEXEC	.DW		DOCOL
		.DW		STATE
		.DW		AT
		.DW		LIT
		.DW		12H
		.DW		QERR
		.DW		SEMIS
;
		.DB		86H		; ?PAIRS
		.DB		"?PAIR"
		.DB		'S'+80H
		.DW		QEXEC-8
QPAIR	.DW		DOCOL
		.DW		SUBB
		.DW		LIT
		.DW		13H
		.DW		QERR
		.DW		SEMIS
;
		.DB		84H		; ?CSP
		.DB		"?CS"
		.DB		'P'+80H
		.DW		QPAIR-9
QCSP	.DW		DOCOL
		.DW		SPAT
		.DW		CSPP
		.DW		AT
		.DW		SUBB
		.DW		LIT
		.DW		14H
		.DW		QERR
		.DW		SEMIS
;
		.DB		88H		; ?LOADING
		.DB		"?LOADIN"
		.DB		'G'+80H
		.DW		QCSP-7
QLOAD	.DW		DOCOL
		.DW		BLK
		.DW		AT
		.DW		ZEQU
		.DW		LIT
		.DW		16H
		.DW		QERR
		.DW		SEMIS
;
		.DB		87H		; COMPILE
		.DB		"COMPIL"
		.DB		'E'+80H
		.DW		QLOAD-0BH
COMP	.DW		DOCOL
		.DW		QCOMP
		.DW		FROMR
		.DW		DUP
		.DW		TWOP
		.DW		TOR
		.DW		AT
		.DW		COMMA
		.DW		SEMIS
;
		.DB		0C1H		; [
		.DB		'['+80H
		.DW		COMP-0AH
LBRAC	.DW		DOCOL
		.DW		ZERO
		.DW		STATE
		.DW		STORE
		.DW		SEMIS
;
		.DB		81H		; ]
		.DB		']'+80H
		.DW		LBRAC-4
RBRAC	.DW		DOCOL
		.DW		LIT,0C0H
		.DW		STATE,STORE
		.DW		SEMIS
;
		.DB		86H		; SMUDGE
		.DB		"SMUDG"
		.DB		'E'+80H
		.DW		RBRAC-4
SMUDG	.DW		DOCOL
		.DW		LATES
		.DW		LIT
		.DW		20H
		.DW		TOGGL
		.DW		SEMIS
;
		.DB		83H		; HEX
		.DB		"HE"
		.DB		'X'+80H
		.DW		SMUDG-9
HEX		.DW		DOCOL
		.DW		LIT
		.DW		10H
		.DW		BASE
		.DW		STORE
		.DW		SEMIS
;
		.DB		87H		; DECIMAL
		.DB		"DECIMA"
		.DB		'L'+80H
		.DW		HEX-6
DEC		.DW		DOCOL
		.DW		LIT
		.DW		0AH
		.DW		BASE
		.DW		STORE
		.DW		SEMIS
;
		.DB		86H		; BINARY		1.3
		.DB		"BINAR"
		.DB		'Y'+80H
		.DW		DEC-10
BIN		.DW		DOCOL
		.DW		LIT,2
		.DW		BASE,STORE
		.DW		SEMIS
;
		.DB		87H		; (;CODE)
		.DB		"(;CODE"
		.DB		')'+80H
		.DW		BIN-9
PSCOD	.DW		DOCOL
		.DW		FROMR
		.DW		LATES
		.DW		PFA
		.DW		CFA
		.DW		STORE
		.DW		SEMIS
;
		.DB		0C5H		; ;CODE
		.DB		";COD"
		.DB		'E'+80H
		.DW		PSCOD-0AH
SEMIC	.DW		DOCOL
		.DW		QCSP
		.DW		COMP
		.DW		PSCOD
		.DW		LBRAC
SEMI1	.DW		NOOP		; ( ASSEMBLER )
		.DW		SEMIS
;
		.DB		87H		; <BUILDS
		.DB		"<BUILD"
		.DB		'S'+80H
		.DW		SEMIC-8
BUILD	.DW		DOCOL
		.DW		ZERO
		.DW		CON
		.DW		SEMIS
;
		.DB		85H		; DOES>
		.DB		"DOES"
		.DB		'>'+80H
		.DW		BUILD-0AH
DOES	.DW		DOCOL
		.DW		FROMR
		.DW		LATES
		.DW		PFA
		.DW		STORE
		.DW		PSCOD
DODOE	LHLD		RPP		; (HL) <- (RP)
		DCX		H
		MOV		M,B		; (R1) <- (IP) = PFA = (SUBSTITUTE CFA)
		DCX		H
		MOV		M,C
		SHLD		RPP		; (RP) <- (RP) - 2
		INX		D		; (DE) <- PFA = (SUBSTITUTE CFA)
		XCHG
		MOV		C,M		; (IP) <- (SUBSTITUTE CFA)
		INX		H
		MOV		B,M
		INX		H
		JMP		HPUSH		; (S1) <- PFA+2 = SUBSTITUTE PFA
;
		.DB		85H		; COUNT
		.DB		"COUN"
		.DB		'T'+80H
		.DW		DOES-8
COUNT	.DW		DOCOL
		.DW		DUP
		.DW		ONEP
		.DW		SWAP
		.DW		CAT
		.DW		SEMIS
;
		.DB		84H		; TYPE
		.DB		"TYP"
		.DB		'E'+80H
		.DW		COUNT-8
TYPE	.DW		DOCOL
		.DW		DDUP
		.DW		ZBRAN		; IF
		.DW		TYPE1-$
		.DW		OVER
		.DW		PLUS
		.DW		SWAP
		.DW		XDO		; DO
TYPE2	.DW		IDO
		.DW		CAT
		.DW		EMIT
		.DW		XLOOP		; LOOP
		.DW		TYPE2-$
		.DW		BRAN		; ELSE
		.DW		TYPE3-$
TYPE1	.DW		DROP		; ENDIF
TYPE3	.DW		SEMIS
;
		.DB		89H		; -TRAILING
		.DB		"-TRAILIN"
		.DB		'G'+80H
		.DW		TYPE-7
DTRAI	.DW		DOCOL
		.DW		DUP
		.DW		ZERO
		.DW		XDO		; DO
DTRA1	.DW		OVER
		.DW		OVER
		.DW		PLUS
		.DW		ONE
		.DW		SUBB
		.DW		CAT
		.DW		BL
		.DW		SUBB
		.DW		ZBRAN		; IF
		.DW		DTRA2-$
		.DW		LEAVE
		.DW		BRAN		; ELSE
		.DW		DTRA3-$
DTRA2	.DW		ONE
		.DW		SUBB		; ENDIF
DTRA3	.DW		XLOOP		; LOOP
		.DW		DTRA1-$
		.DW		SEMIS
;
		.DB		84H		; (.")
		.DB		"(.\""
		.DB		')'+80H
		.DW		DTRAI-0CH
PDOTQ	.DW		DOCOL
		.DW		RR
		.DW		COUNT
		.DW		DUP
		.DW		ONEP
		.DW		FROMR
		.DW		PLUS
		.DW		TOR
		.DW		TYPE
		.DW		SEMIS
;
		.DB		0C2H		; ."
		.DB		"."
		.DB		34+80H  		; "
		.DW		PDOTQ-7
DOTQ	.DW		DOCOL
		.DW		LIT
		.DW		22H
		.DW		STATE
		.DW		AT
		.DW		ZBRAN		; IF
		.DW		DOTQ1-$
		.DW		COMP
		.DW		PDOTQ
		.DW		WORD
		.DW		HERE
		.DW		CAT
		.DW		ONEP
		.DW		ALLOT
		.DW		BRAN		; ELSE
		.DW		DOTQ2-$
DOTQ1	.DW		WORD
		.DW		HERE
		.DW		COUNT
		.DW		TYPE		; ENDIF
DOTQ2	.DW		SEMIS
;
		.DB		86H		; EXPECT
		.DB		"EXPEC"
		.DB		'T'+80H
		.DW		DOTQ-5
EXPEC	.DW		DOCOL
		.DW		OVER
		.DW		PLUS
		.DW		OVER
		.DW		XDO		; DO
EXPE1
		.DW		KEY
		.DW		DUP
		.DW		LIT
		.DW		0EH
		.DW		PORIG
		.DW		AT
		.DW		EQUAL
		.DW		ZBRAN		; IF
		.DW		EXPE2-$
		.DW		DROP
		.DW		DUP
		.DW		IDO
		.DW		EQUAL
		.DW		DUP
		.DW		FROMR
		.DW		TWO
		.DW		SUBB
		.DW		PLUS
		.DW		TOR
		.DW		ZBRAN		; IF
		.DW		EXPE6-$
		.DW		LIT
		.DW		BELL
		.DW		BRAN		; ELSE
		.DW		EXPE7-$
EXPE6	.DW		LIT
		.DW		BSOUT		; ENDIF
EXPE7	.DW		BRAN		; ELSE
		.DW		EXPE3-$
EXPE2	.DW		DUP
		.DW		LIT
		.DW		0DH
		.DW		EQUAL
		.DW		ZBRAN		; IF
		.DW		EXPE4-$
		.DW		LEAVE
		.DW		DROP
		.DW		BL
		.DW		ZERO
		.DW		BRAN		; ELSE
		.DW		EXPE5-$
EXPE4	.DW		DUP		; ENDIF
EXPE5	.DW		IDO
		.DW		CSTOR
		.DW		ZERO
		.DW		IDO
		.DW		ONEP
		.DW		STORE		; ENDIF
EXPE3	.DW		EMIT
		.DW		XLOOP		; LOOP
		.DW		EXPE1-$
		.DW		DROP
		.DW		SEMIS
;
		.DB		85H		; QUERY
		.DB		"QUER"
		.DB		'Y'+80H
		.DW		EXPEC-9
QUERY	.DW		DOCOL
		.DW		TIB
		.DW		AT
		.DW		LIT
		.DW		50H
		.DW		EXPEC
		.DW		ZERO
		.DW		INN
		.DW		STORE
		.DW		SEMIS
;
;						  THE NULL WORD
;				  LISTED AS   X   IN FORTH SOURCE
		.DB		0C1H		; 0
		.DB		80H
		.DW		QUERY-8
NULL	.DW		DOCOL
		.DW		BLK
		.DW		AT
		.DW		ZBRAN		; IF
		.DW		NULL1-$
;		FOLLOWING NOT NEEDED IF KBBUF = 1024
;		.DW		ONE
;		.DW		BLK
;		.DW		PSTOR
;		.DW		ZERO
;		.DW		INN
;		.DW		STORE
;		.DW		BLK
;		.DW		AT
;		.DW		BSCR
;		.DW		ONE
;		.DW		SUBB
;		.DW		ANDD
;		.DW		ZEQU
;		.DW		ZBRAN		; IF
;		.DW		NULL2-$
;
		.DW		QEXEC
;
;		.DW		FROMR
;		.DW		DROP		; ENDIF
;NULL2		.DW		BRAN		; ELSE
;		.DW		NULL3-$
;
NULL1		.DW		FROMR
		.DW		DROP		; ENDIF
NULL3	.DW		SEMIS
;
		.DB		84H		; FILL
		.DB		"FIL"
		.DB		'L'+80H
		.DW		NULL-4
FILL	.DW		$+2
		MOV		L,C
		MOV		H,B
		POP		D
		POP		B
		XTHL
		XCHG
FILL1	MOV		A,B		; BEGIN
		ORA		C
		JZ		FILL2		; WHILE
		MOV		A,L
		STAX		D
		INX		D
		DCX		B
		JMP		FILL1		; REPEAT
FILL2	POP		B
		JMP		NEXT
;
		.DB		85H		; ERASE
		.DB		"ERAS"
		.DB		'E'+80H
		.DW		FILL-7
ERASEE	.DW		DOCOL
		.DW		ZERO
		.DW		FILL
		.DW		SEMIS
;
		.DB		86H		; BLANKS
		.DB		"BLANK"
		.DB		'S'+80H
		.DW		ERASEE-8
BLANK	.DW		DOCOL
		.DW		BL
		.DW		FILL
		.DW		SEMIS
;
		.DB		84H		; HOLD
		.DB		"HOL"
		.DB		'D'+80H
		.DW		BLANK-9
HOLD	.DW		DOCOL
		.DW		LIT
		.DW		-1
		.DW		HLD
		.DW		PSTOR
		.DW		HLD
		.DW		AT
		.DW		CSTOR
		.DW		SEMIS
;
		.DB		83H		; PAD
		.DB		"PA"
		.DB		'D'+80H
		.DW		HOLD-7
PAD		.DW		DOCOL
		.DW		HERE
		.DW		LIT
		.DW		44H
		.DW		PLUS
		.DW		SEMIS
;
		.DB		84H		; WORD
		.DB		"WOR"
		.DB		'D'+80H
		.DW		PAD-6
WORD	.DW		DOCOL
		.DW		BLK
		.DW		AT
		.DW		ZBRAN		; IF
		.DW		WORD1-$
		.DW		BLK
		.DW		AT
		.DW		BLOCK
		.DW		BRAN		; ELSE
		.DW		WORD2-$
WORD1	.DW		TIB
		.DW		AT		; ENDIF
WORD2	.DW		INN
		.DW		AT
		.DW		PLUS
		.DW		SWAP
		.DW		ENCL
		.DW		HERE
		.DW		LIT
		.DW		22H
		.DW		BLANK
		.DW		INN
		.DW		PSTOR
		.DW		OVER
		.DW		SUBB
		.DW		TOR
		.DW		RR
		.DW		HERE
		.DW		CSTOR
		.DW		PLUS
		.DW		HERE
		.DW		ONEP
		.DW		FROMR
		.DW		CMOVE
		.DW		SEMIS
;
		.DB		88H		; (NUMBER)
		.DB		"(NUMBER"
		.DB		')'+80H
		.DW		WORD-7
PNUMB	.DW		DOCOL
PNUM1	.DW		ONEP		; BEGIN
		.DW		DUP
		.DW		TOR
		.DW		CAT
		.DW		BASE
		.DW		AT
		.DW		DIGIT
		.DW		ZBRAN		; WHILE
		.DW		PNUM2-$
		.DW		SWAP
		.DW		BASE
		.DW		AT
		.DW		USTAR
		.DW		DROP
		.DW		ROT
		.DW		BASE
		.DW		AT
		.DW		USTAR
		.DW		DPLUS
		.DW		DPL
		.DW		AT
		.DW		ONEP
		.DW		ZBRAN		; IF
		.DW		PNUM3-$
		.DW		ONE
		.DW		DPL
		.DW		PSTOR		; ENDIF
PNUM3	.DW		FROMR
		.DW		BRAN		; REPEAT
		.DW		PNUM1-$
PNUM2	.DW		FROMR
		.DW		SEMIS
;
		.DB		86H		; NUMBER
		.DB		"NUMBE"
		.DB		'R'+80H
		.DW		PNUMB-0BH
NUMB	.DW		DOCOL
		.DW		ZERO
		.DW		ZERO
		.DW		ROT
		.DW		DUP
		.DW		ONEP
		.DW		CAT
		.DW		LIT
		.DW		2DH
		.DW		EQUAL
		.DW		DUP
		.DW		TOR
		.DW		PLUS
		.DW		LIT
		.DW		-1
NUMB1	.DW		DPL		; BEGIN
		.DW		STORE
		.DW		PNUMB
		.DW		DUP
		.DW		CAT
		.DW		BL
		.DW		SUBB
		.DW		ZBRAN		; WHILE
		.DW		NUMB2-$
		.DW		DUP
		.DW		CAT
		.DW		LIT
		.DW		2EH
		.DW		SUBB
		.DW		ZERO
		.DW		QERR
		.DW		ZERO
		.DW		BRAN		; REPEAT
		.DW		NUMB1-$
NUMB2	.DW		DROP
		.DW		FROMR
		.DW		ZBRAN		; IF
		.DW		NUMB3-$
		.DW		DMINU		; ENDIF
NUMB3	.DW		SEMIS
;
		.DB		85H		; -FIND		(0-3) SUCCESS
		.DB		"-FIN"		; (0-1) FAILURE
		.DB		'D'+80H
		.DW		NUMB-9
DFIND	.DW		DOCOL
		.DW		BL
		.DW		WORD
		.DW		HERE
		.DW		CONT
		.DW		AT
		.DW		AT
		.DW		PFIND
		.DW		DUP
		.DW		ZEQU
		.DW		ZBRAN		; IF
		.DW		DFIN1-$
		.DW		DROP
		.DW		HERE
		.DW		LATES
		.DW		PFIND		; ENDIF
DFIN1	.DW		SEMIS
;
		.DB		87H		; (ABORT)
		.DB		"(ABORT"
		.DB		')'+80H
		.DW		DFIND-8
PABOR	.DW		DOCOL
		.DW		ABORT
		.DW		SEMIS
;		
		.DB		85H		; ERROR
		.DB		"ERRO"
		.DB		'R'+80H
		.DW		PABOR-0AH
ERROR	.DW		DOCOL
		.DW		WARN
		.DW		AT
		.DW		ZLESS
		.DW		ZBRAN		; IF
		.DW		ERRO1-$
		.DW		PABOR		; ENDIF
ERRO1	.DW		HERE
		.DW		COUNT
		.DW		TYPE
		.DW		PDOTQ
		.DB		2
		.DB		"? "
		.DW		MESS
		.DW		SPSTO
;		CHANGE FROM FIG MODEL
;		.DW		INN,AT,BLK,AT
		.DW		BLK,AT
		.DW		DDUP
		.DW		ZBRAN
		.DW		ERRO2-$		; IF
		.DW		INN,AT
		.DW		SWAP
		.DW		WHERE				; THEN
ERRO2		.DW		QUIT
;
		.DB		83H		; ID.
		.DB		"ID"
		.DB		'.'+80H
		.DW		ERROR-8
IDDOT		.DW		DOCOL
		.DW		PAD
		.DW		LIT
		.DW		20H
		.DW		LIT
		.DW		5FH
		.DW		FILL
		.DW		DUP
		.DW		PFA
		.DW		LFA
		.DW		OVER
		.DW		SUBB
		.DW		PAD
		.DW		SWAP
		.DW		CMOVE
		.DW		PAD
		.DW		COUNT
		.DW		LIT
		.DW		1FH
		.DW		ANDD
		.DW		TYPE
		.DW		SPACE
		.DW		SEMIS
;
		.DB		86H		; CREATE
		.DB		"CREAT"
		.DB		'E'+80H
		.DW		IDDOT-6
CREAT	.DW		DOCOL
		.DW		DFIND
		.DW		ZBRAN		; IF
		.DW		CREA1-$
		.DW		DROP
		.DW		NFA
		.DW		IDDOT
		.DW		LIT
		.DW		4
		.DW		MESS
		.DW		SPACE		; ENDIF
CREA1	.DW		HERE
		.DW		DUP
		.DW		CAT
		.DW		WIDTH
		.DW		AT
		.DW		MIN
		.DW		ONEP
		.DW		ALLOT
		.DW		DUP
		.DW		LIT
		.DW		0A0H
		.DW		TOGGL
		.DW		HERE
		.DW		ONE
		.DW		SUBB
		.DW		LIT
		.DW		80H
		.DW		TOGGL
		.DW		LATES
		.DW		COMMA
		.DW		CURR
		.DW		AT
		.DW		STORE
		.DW		HERE
		.DW		TWOP
		.DW		COMMA
		.DW		SEMIS
;
		.DB		0C9H		; [COMPILE]
		.DB		"[COMPILE"
		.DB		']'+80H
		.DW		CREAT-9
BCOMP	.DW		DOCOL
		.DW		DFIND
		.DW		ZEQU
		.DW		ZERO
		.DW		QERR
		.DW		DROP
		.DW		CFA
		.DW		COMMA
		.DW		SEMIS
;
		.DB		0C7H		; LITERAL
		.DB		"LITERA"
		.DB		'L'+80H
		.DW		BCOMP-0CH
LITER	.DW		DOCOL
		.DW		STATE
		.DW		AT
		.DW		ZBRAN		; IF
		.DW		LITE1-$
		.DW		COMP
		.DW		LIT
		.DW		COMMA		; ENDIF
LITE1	.DW		SEMIS
;
		.DB		0C8H		; DLITERAL
		.DB		"DLITERA"
		.DB		'L'+80H
		.DW		LITER-0AH
DLITE	.DW		DOCOL
		.DW		STATE
		.DW		AT
		.DW		ZBRAN		; IF
		.DW		DLIT1-$
		.DW		SWAP
		.DW		LITER
		.DW		LITER		; ENDIF
DLIT1	.DW		SEMIS
;
		.DB		86H		; ?STACK
		.DB		"?STAC"
		.DB		'K'+80H
		.DW		DLITE-0BH
QSTAC	.DW		DOCOL  				; : ?STACK SP@ /0
		.DW		SPAT
		.DW		SZERO
		.DW		AT
		.DW		SWAP
		.DW		ULESS
		.DW		ONE
		.DW		QERR
		.DW		SPAT
		.DW		HERE
		.DW		LIT
		.DW		80H
		.DW		PLUS
		.DW		ULESS
		.DW		LIT
		.DW		7
		.DW		QERR
		.DW		SEMIS
;
		.DB		89H		; INTERPRET
		.DB		"INTERPRE"
		.DB		'T'+80H
		.DW		QSTAC-9
INTER	.DW		DOCOL
INTE1	.DW		DFIND		; BEGIN
		.DW		ZBRAN		; IF
		.DW		INTE2-$
		.if 0
		.DW		CR, DTOS
		.endif
		.DW		STATE
		.if 0
		.DW		CR, DTOS
		.endif
		.DW		AT
		.if 0
		.DW		CR, DTOS
		.endif
		.DW		LESS
		.DW		ZBRAN		; IF
		.DW		INTE3-$
		.if 0
		.DW		CR, DTOS
		.endif
		.DW		CFA
		.DW		COMMA
		.DW		BRAN		; ELSE
		.DW		INTE4-$
INTE3	.DW		CFA
		.DW		EXEC		; ENDIF
INTE4	.DW		QSTAC
		.DW		BRAN		; ELSE
		.DW		INTE5-$
INTE2	.DW		HERE
		.DW		NUMB
		.DW		DPL
		.DW		AT
		.DW		ONEP
		.DW		ZBRAN		; IF
		.DW		INTE6-$
		.DW		DLITE
		.DW		BRAN		; ELSE
		.DW		INTE7-$
INTE6	.DW		DROP
		.DW		LITER		; ENDIF
INTE7	.DW		QSTAC		; ENDIF
INTE5	.DW		BRAN		; AGAIN
		.DW		INTE1-$
;
		.DB		89H		; IMMEDIATE
		.DB		"IMMEDIAT"
		.DB		'E'+80H
		.DW		INTER-0CH
IMMED	.DW		DOCOL
		.DW		LATES
		.DW		LIT
		.DW		40H
		.DW		TOGGL
		.DW		SEMIS
;
		.DB		8AH		; VOCABULARY
		.DB		"VOCABULAR"
		.DB		'Y'+80H
		.DW		IMMED-0CH
VOCAB	.DW		DOCOL
		.DW		BUILD
		.DW		LIT
		.DW		0A081H
		.DW		COMMA
		.DW		CURR
		.DW		AT
		.DW		CFA
		.DW		COMMA
		.DW		HERE
		.DW		VOCL
		.DW		AT
		.DW		COMMA
		.DW		VOCL
		.DW		STORE
		.DW		DOES
DOVOC	.DW		TWOP
		.DW		CONT
		.DW		STORE
		.DW		SEMIS
;
		.DB		0C5H		; FORTH
		.DB		"FORT"
		.DB		'H'+80H
		.DW		VOCAB-0DH
FORTH	.DW		DODOE
		.DW		DOVOC
		.DW		0A081H
FORTHP	.DW		FLAST		; COLD START VALUE ONLY
;						  CHANGED EACH TIME A DEF IS APPENDED
;						  TO THE FORTH VOCABULARY
		.DW		0		; END OF VOCABULARY LIST
;
		.DB		8BH		; DEFINITIONS
		.DB		"DEFINITION"
		.DB		'S'+80H
		.DW		FORTH-8
DEFIN	.DW		DOCOL
		.DW		CONT
		.DW		AT
		.DW		CURR
		.DW		STORE
		.DW		SEMIS
;
		.DB		0C1H		; (
		.DB		'('+80H
		.DW		DEFIN-0EH
PAREN	.DW		DOCOL
		.DW		LIT
		.DW		29H
		.DW		WORD
		.DW		SEMIS
;
		.DB		84H		; QUIT
		.DB		"QUI"
		.DB		'T'+80H
		.DW		PAREN-4
QUIT	.DW		DOCOL
 .if 0
		.DW		DTOS, CR
 .endif
		.DW		ZERO
		.DW		BLK
		.DW		STORE
		.DW		LBRAC
QUIT1	.DW		RPSTO		; BEGIN
		.DW		CR
		.DW		QUERY
 .if 0		
		.DW		CR, DTOS, CR
 .endif
		.DW		INTER
 .if 0		
		.DW		CR, DTOS, CR
 .endif
		.DW		STATE
		.DW		AT
		.DW		ZEQU
		.DW		ZBRAN		; IF
		.DW		QUIT2-$
		.DW		PDOTQ
		.DB		2
		.DB		"OK"		; ENDIF
  .if DEBUG
		.DW		DTOS
  .endif
QUIT2	.DW		BRAN		; AGAIN
		.DW		QUIT1-$
;
		.DB		85H		; ABORT
		.DB		"ABOR"
		.DB		'T'+80H
		.DW		QUIT-7
ABORT	.DW		DOCOL
		.DW		SPSTO
		.DW		DEC
		.DW		QSTAC
		.DW		CR
		.DW		DOTCPU
		.DW		PDOTQ
		.DB		0DH
		.DB		"fig-FORTH "
		.DB		FIGREL+30H,ADOT,FIGREV+30H
		.DW		FORTH
		.DW		DEFIN
 .if 0		
		.DW		CR
		.DW		CSLL, DOT, CR
		.DW		ZERO, DOT, CR
 .endif
 .if 0
		.DW		traceon
		.DW		SZERO, DUP, DOT, SPACE, AT, DOT, CR
		.DW		RZERO, DUP, DOT, SPACE, AT, DOT, CR
		.DW		TIB,   DUP, DOT, SPACE, AT, DOT, CR
		.DW		WARN,  DUP, DOT, SPACE, AT, DOT, CR
		.DW		OUTT,  DUP, DOT, SPACE, AT, DOT, CR
 .endif
		.DW		QUIT
;
WRM		LXI		B,WRM1
		JMP		NEXT
WRM1	.DW		WARM
;
		.DB		84H		; WARM
		.DB		"WAR"
		.DB		'M'+80H
		.DW		ABORT-8
WARM	.DW		DOCOL
		.DW		MTBUF
		.DW		ABORT
;
CLD:	DI
		LXI		B,CLD1
		LHLD		ORIG+12H
		SPHL
 .if DEBUG		
		.if 0
		mvi		A,-1
		sta		trace
		.endif
		call PRINTI
		.db "Initial Stack Pointer:",0
		call HEXHL
 .endif
		JMP		NEXT
CLD1	.DW		COLD
;
		.DB		84H		; COLD				1.3
		.DB		"COL"
		.DB		'D'+80H
		.DW		WARM-7
COLD	.DW		DOCOL
		.DW		MTBUF
		.DW		ZERO,DENSTY
		.DW		STORE
		.DW		FIRST
		.DW		USE,STORE
		.DW		FIRST
		.DW		PREV,STORE
		.DW		DRZER
		.DW		LIT,0
		.DW		LIT,EPRINT
		.DW		STORE
;
;						INIT SOME USER VARIABLES
		.DW		LIT
		.DW		OCLD0
		.DW		LIT
		.DW		UP
		.DW		AT
		.DW		LIT
		.DW		6
		.DW		PLUS
		.DW		LIT
		.DW		OCLD1-OCLD0
		.DW		CMOVE
;
;						INIT VOCAB POINTERS
		.DW		LIT
		.DW		OFOR
		.DW		AT
		.DW		LIT
		.DW		FORTHP
		.DW		STORE
		.DW		LIT,OED
		.DW		AT
		.DW		LIT,EDITP
		.DW		STORE
		
;		SAME FOR ASSEMBLER IF RESIDENT
;
		.DW		ABORT
;
		.DB		84H		; S->D
		.DB		"S->"
		.DB		'D'+80H
		.DW		COLD-7
STOD	.DW		$+2
		POP		D
		LXI		H,0
		MOV		A,D
		ANI		80H
		JZ		STOD1
		DCX		H
STOD1	JMP		DPUSH
;
		.DB		82H		; +-
		.DB		"+"
		.DB		'-'+80H
		.DW		STOD-7
PM		.DW		DOCOL
		.DW		ZLESS
		.DW		ZBRAN		; IF
		.DW		PM1-$
		.DW		MINUS		; ENDIF
PM1		.DW		SEMIS
;
		.DB		83H		; D+-
		.DB		"D+"
		.DB		'-'+80H
		.DW		PM-5
DPM		.DW		DOCOL
		.DW		ZLESS
		.DW		ZBRAN		; IF
		.DW		DPM1-$
		.DW		DMINU		; ENDIF
DPM1	.DW		SEMIS
;
		.DB		83H		; ABS
		.DB		"AB"
		.DB		'S'+80H
		.DW		DPM-6
ABS		.DW		DOCOL
		.DW		DUP
		.DW		PM
		.DW		SEMIS
;
		.DB		84H		; DABS
		.DB		"DAB"
		.DB		'S'+80H
		.DW		ABS-6
DABS	.DW		DOCOL
		.DW		DUP
		.DW		DPM
		.DW		SEMIS
;
		.DB		83H		; MIN
		.DB		"MI"
		.DB		'N'+80H
		.DW		DABS-7
MIN		.DW		DOCOL,TDUP
		.DW		GREAT
		.DW		ZBRAN		; IF
		.DW		MIN1-$
		.DW		SWAP		; ENDIF
MIN1	.DW		DROP
		.DW		SEMIS
;
		.DB		83H		; MAX
		.DB		"MA"
		.DB		'X'+80H
		.DW		MIN-6
MAX		.DW		DOCOL,TDUP
		.DW		LESS
		.DW		ZBRAN		; IF
		.DW		MAX1-$
		.DW		SWAP		; ENDIF
MAX1	.DW		DROP
		.DW		SEMIS
;
		.DB		82H		; M*
		.DB		"M"
		.DB		'*'+80H
		.DW		MAX-6
MSTAR	.DW		DOCOL,TDUP
		.DW		XORR
		.DW		TOR
		.DW		ABS
		.DW		SWAP
		.DW		ABS
		.DW		USTAR
		.DW		FROMR
		.DW		DPM
		.DW		SEMIS
;
		.DB		82H		; M/
		.DB		"M"
		.DB		'/'+80H
		.DW		MSTAR-5
MSLAS	.DW		DOCOL
		.DW		OVER
		.DW		TOR
		.DW		TOR
		.DW		DABS
		.DW		RR
		.DW		ABS
		.DW		USLAS
		.DW		FROMR
		.DW		RR
		.DW		XORR
		.DW		PM
		.DW		SWAP
		.DW		FROMR
		.DW		PM
		.DW		SWAP
		.DW		SEMIS
;
		.DB		81H		; *
		.DB		'*'+80H
		.DW		MSLAS-5
STAR	.DW		DOCOL
		.DW		MSTAR
		.DW		DROP
		.DW		SEMIS
;
		.DB		84H		; /MOD
		.DB		"/MO"
		.DB		'D'+80H
		.DW		STAR-4
SLMOD	.DW		DOCOL
		.DW		TOR
		.DW		STOD
		.DW		FROMR
		.DW		MSLAS
		.DW		SEMIS
;
		.DB		81H		; /
		.DB		'/'+80H
		.DW		SLMOD-7
SLASH	.DW		DOCOL
		.DW		SLMOD
		.DW		SWAP
		.DW		DROP
		.DW		SEMIS
;
		.DB		83H		; MOD
		.DB		"MO"
		.DB		'D'+80H
		.DW		SLASH-4
MODD	.DW		DOCOL
		.DW		SLMOD
		.DW		DROP
		.DW		SEMIS
;
		.DB		85H		; */MOD
		.DB		"*/MO"
		.DB		'D'+80H
		.DW		MODD-6
SSMOD	.DW		DOCOL
		.DW		TOR
		.DW		MSTAR
		.DW		FROMR
		.DW		MSLAS
		.DW		SEMIS
;
		.DB		82H		; */
		.DB		"*"
		.DB		'/'+80H
		.DW		SSMOD-8
SSLA	.DW		DOCOL
		.DW		SSMOD
		.DW		SWAP
		.DW		DROP
		.DW		SEMIS
;
		.DB		85H		; M/MOD
		.DB		"M/MO"
		.DB		'D'+80H
		.DW		SSLA-5
MSMOD	.DW		DOCOL
		.DW		TOR
		.DW		ZERO
		.DW		RR
		.DW		USLAS
		.DW		FROMR
		.DW		SWAP
		.DW		TOR
		.DW		USLAS
		.DW		FROMR
		.DW		SEMIS
;
;		BLOCK MOVED DOWN 2 PAGES
;
;
		.DB		86H		; (LINE)
		.DB		"(LINE"
		.DB		')'+80H
		.DW		MSMOD-8
PLINE	.DW		DOCOL
		.DW		TOR
		.DW		LIT
		.DW		40H
		.DW		BBUF
		.DW		SSMOD
		.DW		FROMR
		.DW		BSCR
		.DW		STAR
		.DW		PLUS
		.DW		BLOCK
		.DW		PLUS
		.DW		LIT
		.DW		40H
		.DW		SEMIS
;
		.DB		85H		; .LINE
		.DB		".LIN"
		.DB		'E'+80H
		.DW		PLINE-9
DLINE	.DW		DOCOL
		.DW		PLINE
		.DW		DTRAI
		.DW		TYPE
		.DW		SEMIS
;
		.DB		87H		; MESSAGE
		.DB		"MESSAG"
		.DB		'E'+80H
		.DW		DLINE-8
MESS	.DW		DOCOL
		.DW		WARN
		.DW		AT
		.DW		ZBRAN		; IF
		.DW		MESS1-$
		.DW		DDUP
		.DW		ZBRAN		; IF
		.DW		MESS2-$
		.DW		LIT
		.DW		4
		.DW		OFSET
		.DW		AT
		.DW		BSCR
		.DW		SLASH
		.DW		SUBB
		.DW		DLINE
		.DW		SPACE		; ENDIF
MESS2	.DW		BRAN		; ELSE
		.DW		MESS3-$
MESS1	.DW		PDOTQ
		.DB		6
		.DB		"MSG # "
		.DW		DOT		; ENDI"
MESS3	.DW		SEMIS
		.PAGE
;------------------------------------------
;
;		8080 PORT FETCH AND STORE
;		( SELF MODIFYING CODE, NOT REENTRANT
;				OR ROM-ABLE )
;
		.DB		82H		; P@ "PORT @"
		.DB		"P"
		.DB		'@'+80H
		.DW		MESS-0AH
PTAT	.DW		$+2
		POP		D		;E <- PORT#
		LXI		H,$+5
		MOV		M,E
		.IF ~ APPLE
		IN		0		;( PORT# MODIFIED )
		.ENDIF
		.IF APPLE
		LDA		0E000H
		.ENDIF
		MOV		L,A		;L <- (PORT#)
		MVI		H,0
		JMP		HPUSH
;
		.DB		82H		; "PORT STORE"
		.DB		"P"
		.DB		'!'+80H
		.DW		PTAT-5
PTSTO	.DW		$+2
		POP		D		;E <- PORT#
		LXI		H,$+7
		MOV		M,E
		POP		H		;H <- CDATA
		MOV		A,L
		.IF ~ APPLE
		OUT		0		;( PORT# MODIFIED )
		.ENDIF
		.IF APPLE
		STA		0E010H
		.ENDIF
		JMP		NEXT
		.PAGE
		
;------------------------------------------------------
;		FORTH DISK INTERFACE
;
;		MAPPING DISK SECTORS ONTO FORTH BUFFERS & SCREENS
;		   ( THE FOLLOWING DIAGRAM IS ONLY AN EXAMPLE )
;
;		DISK										   MEMORY
;
;  =============+   ----^-------^-------^-------+============
;		 SECTOR I		I		I		I	    I
;		     ===+		I		I  SECTORS/BUF	I      BUFFER
;      TRACK	I		I		I		I		I
;		     ===+   ----I-------I-------V-------+====  SCREEN
;				I		I		I				I
;		========+		I  SECTORS/SCREEN		I
;				I		I		I				I
;		     ===+-------I----------V------------+============
;  D			I		I						I
;  R		 ===+    SCREENS					I
;  I			I    -------					I
;  V	========+     DRIVE						+====
;  E			I		I						I
;		     ===+		I						I
;				I		I						I
;		     ===+   ----V--------------------   +============
;    ////////// I   <----- NOT USED BY FORTH
;  =============+
;
;----------------------------------------------------
BPS		.EQU		128				; BYTES PER SECTOR
MXDRV		.EQU		2				; MAX # DRIVES
;
		.IF ~ APPLE
;		SINGLE DENSITY 8" FLOPPY CAPACITIES
SEPTR1		.EQU		26				; SECTORS/TRACK
TRPDR1		.EQU		77				; TRACKS/DRIVE
		.ENDIF
		.IF APPLE		;5-1/4
SEPTR1		.EQU		16
TRPDR1		.EQU		35
		.ENDIF
SEPDR1		.EQU		SEPTR1*TRPDR1		; SECTORS/DRIVE
SEPBU1		.EQU		KBBUF/BPS		    ; SECTORS/BUFFER
BUPSC1		.EQU		1024/KBBUF		    ; BUFFERS/SCREEN
SEPSC1		.EQU		SEPBU1*BUPSC1		; SECTORS/SCREEN
SCPDR1		.EQU		SEPDR1/SEPSC1		; SCREENS/DRIVE
BUPDR1		.EQU		BUPSC1*SCPDR1		; BUFFERS/DRIVE
USPDR1		.EQU		SCPDR1*SEPSC1		; USABLE SEC/DRV
;
;		DOUBLE DENSITY 8" FLOPPY CAPACITIES
		.IF ~ APPLE
SEPTR2		.EQU		52				; SECTORS/TRACK
TRPDR2		.EQU		77				; TRACKS/DRIVE
		.ENDIF
		.IF APPLE
SEPTR2		.EQU		16
TRPDR2		.EQU		35
		.ENDIF
SEPDR2		.EQU		SEPTR2*TRPDR2		; SECTORS/DRIVE
SEPBU2		.EQU		KBBUF/BPS		; SECTORS/BUFFER
BUPSC2		.EQU		1024/KBBUF		; BUFFERS/SCREEN
SEPSC2		.EQU		SEPBU2*BUPSC2		; SECTORS/SCREEN
SCPDR2		.EQU		SEPDR2/SEPSC2		; SCREENS/DRIVE
BUPDR2		.EQU		BUPSC2*SCPDR2		; BUFFERS/DRIVE
USPDR2		.EQU		SCPDR2*SEPSC2		; USABLE SEC/DRV
		.PAGE
;-------------------------------------------------------
;		CP/M DISK INTERFACE
;
;		CP/M BIOS CALLS USED
;		( NOTE EQU'S ARE 3 LOWER THAN DOCUMENTED OFFSETS
;		  BECAUSE BASE ADDR IS BIOS+3 )
;
RITSEC		.EQU		39
RDSEC		.EQU		36
SETDMA		.EQU		33
SETSEC		.EQU		30
SETTRK		.EQU		27
SETDSK		.EQU		24
;
;
;		FORTH VARIABLES AND CONSTANTS USED IN DISK INTERFACE
;
		.DB		85H		; DRIVE ( CURRENT DRIVE # )
		.DB		"DRIV"
		.DB		'E'+80H
		.DW		PTSTO-5
DRIVE	.DW		DOVAR,0
;
		.DB		83H		; SEC		( SECTOR # )
		.DB		"SE"
		.DB		'C'+80H
		.DW		DRIVE-8
SEC:	.DW		DOVAR
		.DW		0
;
		.DB		85H		; TRACK		( TRACK # )
		.DB		"TRAC"
		.DB		'K'+80H
		.DW		SEC-6
TRACK:	.DW		DOVAR,0
;
		.DB		83H		; USE		( ADDR OF NEXT BUFFER
;								TO BE REPLACED )
		.DB		"US"
		.DB		'E'+80H
		.DW		TRACK-8
USE:	.DW		DOVAR
		.DW		BUF1
;
		.DB		84H		; PREV
;		( ADDR OF BUFFER PREVIOUSLY ACCESSED BY CPU )
		.DB		"PRE"
		.DB		'V'+80H
		.DW		USE-6
PREV	.DW		DOVAR
		.DW		BUF1
;
		.DB		87H		; SEC/BLK ( # SECTORS/BLOCK )
		.DB		"SEC/BL"
		.DB		'K'+80H
		.DW		PREV-7
SPBLK	.DW		DOCON
		.DW		KBBUF/BPS
;
		.DB		85H		; #BUFF  ( NUMBER OF BUFFERS )
		.DB		"#BUF"
		.DB		'F'+80H
		.DW		SPBLK-10
NOBUF	.DW		DOCON,NBUF
;
		.DB		88H		; #SCR/DRV   ( # SCREENS/DRIVE )   1.3
		.DB		"#SCR/DR"
		.DB		'V'+80H
		.DW		NOBUF-8
NSCRD		.DW		DOCOL
		.DW		DENSTY,AT
		.DW		ZBRAN
		.DW		NSCR1-$
		.DW		LIT,SCPDR2
		.DW		BRAN
		.DW		NSCR2-$
NSCR1	.DW		LIT,SCPDR1
NSCR2	.DW		SEMIS
;
		.DB		87H		; DENSITY ( 0 = SINGLE , 1 = DOUBLE )
		.DB		"DENSIT"
		.DB		'Y'+80H
		.DW		NSCRD-11
DENSTY	.DW		DOVAR
		.DW		0
;
		.DB		8AH		; DISK-ERROR  ( DISK ERROR STATUS )
		.DB		"DISK-ERRO"
		.DB		'R'+80H
		.DW		DENSTY-10
DSKERR	.DW		DOVAR,0
;
;		DISK INTERFACE HIGH-LEVEL ROUTINES
;
		.DB		84H		; +BUF		( ADVANCE BUFFER )
		.DB		"+BU"
		.DB		'F'+80H
		.DW		DSKERR-13
PBUF	.DW		DOCOL
		.DW		LIT,CO
		.DW		PLUS,DUP
		.DW		LIMIT,EQUAL
		.DW		ZBRAN
		.DW		PBUF1-$
		.DW		DROP,FIRST
PBUF1:	.DW		DUP,PREV
		.DW		AT,SUBB
		.DW		SEMIS
;
		.DB		86H					; UPDATE
		.DB		"UPDAT"
		.DB		'E'+80H
		.DW		PBUF-7
UPDAT	.DW		DOCOL,PREV
		.DW		AT,AT
		.DW		LIT,8000H
		.DW		ORR
		.DW		PREV,AT
		.DW		STORE,SEMIS
;
		.DB		8DH					; EMPTY-BUFFERS
		.DB		"EMPTY-BUFFER"
		.DB		'S'+80H
		.DW		UPDAT-9
MTBUF	.DW		DOCOL,FIRST
		.DW		LIMIT,OVER
		.DW		SUBB,ERASEE
		.DW		SEMIS
;
		.DB		83H					; DR0
		.DB		"DR"
		.DB		'0'+80H
		.DW		MTBUF-16
DRZER	.DW		DOCOL,ZERO
		.DW		OFSET,STORE
		.DW		SEMIS
;
		.DB		83H					; DR1
		.DB		"DR"
		.DB		'1'+80H
		.DW		DRZER-6
DRONE	.DW		DOCOL
		.DW		DENSTY,AT
		.DW		ZBRAN
		.DW		DRON1-$
		.DW		LIT,BUPDR2
		.DW		BRAN
		.DW		DRON2-$
DRON1	.DW		LIT,BUPDR1
DRON2	.DW		OFSET,STORE
		.DW		SEMIS
;
		.DB		86H					; BUFFER
		.DB		"BUFFE"
		.DB		'R'+80H
		.DW		DRONE-6
BUFFE:	.DW		DOCOL,USE
		.DW		AT,DUP
		.DW		TOR
BUFF1	.DW		PBUF				; WON'T WORK IF SINGLE BUFFER
		.DW		ZBRAN
		.DW		BUFF1-$
		.DW		USE,STORE
		.DW		RR,AT
		.DW		ZLESS
		.DW		ZBRAN
		.DW		BUFF2-$
		.DW		RR,TWOP
		.DW		RR,AT
		.DW		LIT,7FFFH
		.DW		ANDD,ZERO
		.DW		RSLW
BUFF2	.DW		RR,STORE
		.DW		RR,PREV
		.DW		STORE,FROMR
		.DW		TWOP,SEMIS
;
		.DB		85H					; BLOCK
		.DB		"BLOC"
		.DB		'K'+80H
		.DW		BUFFE-9
BLOCK	.DW		DOCOL,OFSET
		.DW		AT,PLUS
		.DW		TOR,PREV
		.DW		AT,DUP
		.DW		AT,RR
		.DW		SUBB
		.DW		DUP,PLUS
		.DW		ZBRAN
		.DW		BLOC1-$
BLOC2	.DW		PBUF,ZEQU
		.DW		ZBRAN
		.DW		BLOC3-$
		.DW		DROP,RR
		.DW		BUFFE,DUP
		.DW		RR,ONE
		.DW		RSLW
		.DW		TWO,SUBB
BLOC3	.DW		DUP,AT
		.DW		RR,SUBB
		.DW		DUP,PLUS
		.DW		ZEQU
		.DW		ZBRAN
		.DW		BLOC2-$
		.DW		DUP,PREV
		.DW		STORE
BLOC1	.DW		FROMR,DROP
		.DW		TWOP,SEMIS

IOS:	LXI		D,BUF1
		LXI		H,DEMO
		LXI		B,1024
IOS0:	MOV		A,M
		STAX	D
		INX		D
		INX		H
		DCX		B
		MOV		A,C
		CPI		0
		JNZ		IOS0
		MOV		A,B
		CPI		0
		JNZ		IOS0
		RET				; Dummy return
;
		LHLD	1		; (HL) <- BIOS TABLE ADDR+3
		DAD		D		; + SERVICE REQUEST OFFSET
		PCHL			; EXECUTE REQUEST
;
;		RET FUNCTION PROVIDED BY CP/M
;
;
		.DB		86H		; SET-IO ( ASSIGN SECTOR, TRACK FOR BDOS )
		.DB		"SET-I"
		.DB		'O'+80H
		.DW		BLOCK-8
SETIO:	.DW		$+2
		PUSH	B		; SAVE (IP)
		LHLD	USE+2	; (BC) <- ADDR BUFFER
		MOV		B,H
		MOV		C,L
		LXI		D,SETDMA ; SEND BUFFER ADDR TO CP/M
		CALL	IOS
;
		LHLD	SEC+2		; (BC) <- (SEC) = SECTOR #
		MOV		C,L
		LXI		D,SETSEC	; SEND SECTOR # TO CP/M
		CALL	IOS
;
		LHLD	TRACK+2		; (BC) <- (TRACK) = TRACK #
		MOV		B,H
		MOV		C,L
		LXI		D,SETTRK
		CALL	IOS
;
		POP		B		; RESTORE (IP)
		JMP		NEXT
;
		.DB		89H		; SET-DRIVE
		.DB		"SET-DRIV"
		.DB		'E'+80H
		.DW		SETIO-9
SETDRV:	.DW		$+2
		PUSH	B		; SAVE (IP)
		LDA		DRIVE+2		; (C) <- (DRIVE) = DRIVE #
		MOV		C,A
		LXI		D,SETDSK		; SEND DRIVE # TO CP/M
		CALL	IOS
		POP		B		; RESTORE (IP)
		JMP		NEXT
;
;		T&SCALC				( CALCULATES DRIVE#, TRACK#, & SECTOR# )
;		STACK INPUT: SECTOR-DISPLACEMENT = BLK# * SEC/BLK
;		OUTPUT: VARIABLES DRIVE, TRACK, & SEC
;
		.DB		87H		; T&SCALC
		.DB		"T&SCAL"
		.DB		'C'+80H
		.DW		SETDRV-12
TSCALC:		.DW		DOCOL,DENSTY
		.DW		AT
		.DW		ZBRAN
		.DW		TSCALS-$
;		DOUBLE DENSITY
		.DW		LIT,USPDR2
		.DW		SLMOD
		.DW		LIT,MXDRV-1
		.DW		MIN
		.DW		DUP,DRIVE
		.DW		AT,EQUAL
		.DW		ZBRAN
		.DW		TSCAL1-$
		.DW		DROP
		.DW		BRAN
		.DW		TSCAL2-$
TSCAL1		.DW		DRIVE,STORE
		.DW		SETDRV
TSCAL2		.DW		LIT,SEPTR2
		.DW		SLMOD,TRACK
		.IF ~ APPLE
		.DW		STORE,ONEP
		.ENDIF
		.IF APPLE
		.DW		STORE
		.ENDIF
		.DW		SEC,STORE
		.DW		SEMIS
;		SINGLE DENSITY
TSCALS		.DW		LIT,USPDR1
		.DW		SLMOD
		.DW		LIT,MXDRV-1
		.DW		MIN
		.DW		DUP,DRIVE
		.DW		AT,EQUAL
		.DW		ZBRAN
		.DW		TSCAL3-$
		.DW		DROP
		.DW		BRAN
		.DW		TSCAL4-$
TSCAL3	.DW		DRIVE,STORE
		.DW		SETDRV
TSCAL4	.DW		LIT,SEPTR1
		.DW		SLMOD,TRACK
		.IF ~ APPLE
		.DW		STORE,ONEP
		.ENDIF
		.IF APPLE
		.DW		STORE
		.ENDIF
		.DW		SEC,STORE
		.DW		SEMIS
;
;		SEC-READ
;		( READ A SECTOR SETUP BY 'SET-DRIVE' & 'SETIO' )
;
		.DB		88H		; SEC-READ
		.DB		"SEC-REA"
		.DB		'D'+80H
		.DW		TSCALC-10
SECRD	.DW		$+2
		PUSH		B		; SAVE (IP)
		LXI		D,RDSEC		; ASK CP/M TO READ SECTOR
		CALL	IOS
		STA		DSKERR+2		; (DSKERR) <- ERROR STATUS
		POP		B		; RESTORE (IP)
		JMP		NEXT
;
;		SEC-WRITE
;		( WRITE A SECTOR SETUP BY 'SET-DRIVE' & 'SETIO' )
;
		.DB		89H		; SEC-WRITE
		.DB		"SEC-WRIT"
		.DB		'E'+80H
		.DW		SECRD-11
SECWT	.DW		$+2
		PUSH	B		; SAVE (IP)
		LXI		D,RITSEC		; ASK CP/M TO WRITE SECTOR
		CALL	IOS
		STA		DSKERR+2		; (DSKERR) <- ERROR STATUS
		POP		B		; RESTORE (IP)
		JMP		NEXT
;
		.DB		86H		; +TRACK  ( ADVANCE TRACK )		1.3
		.DB		"+TRAC"
		.DB		'K'+80H
		.DW		SECWT-12
PTRAC	.DW		$+2
		LDA		DENSTY+2		; GET #SECTORS/DRIVE
		ORA		A				; IF DENSITY = 0
		MVI		A,SEPTR1+1		; THEN SINGLE DENSITY
		JZ		PTRA1
		MVI		A,SEPTR2+1		; ELSE DOUBLE
PTRA1	LHLD	SEC+2				; IF NOT AT END OF TRACK
		CMP		L
		JNZ		NEXT				; THEN DONE
		MVI		A,1				; ELSE RESET SECTOR #
		STA		SEC+2
		LDA		TRACK+2				;   AND INCR TRACK #
		INR		A
		STA		TRACK+2
		JMP		NEXT
;
		.DB		87H		; +SECTOR ( ADVANCE SECTOR )		1.3
		.DB		"+SECTO"
		.DB		'R'+80H
		.DW		PTRAC-9
PSEC	.DW		$+2
		LDA		SEC+2		; INCR SECTOR #
		INR		A
		STA		SEC+2
		PUSH	D		; SAVE W
		LHLD	USE+2		; INCR USE
		LXI		D,BPS
		DAD		D
		SHLD	USE+2
		POP		D		; RESTORE W
		JMP		NEXT
;
		.DB		84H		; SELECT READ OR WRITE				1.3
		.DB		"?R/"		;   ( F --- F )
		.DB		'W'+80H
		.DW		PSEC-10
QRW		.DW		$+2
		XTHL				; (HL) <- (S1) = R/W FLAG
		MOV		A,L		; IF FLAG = 1
		ORA		H
		XTHL
		JZ		QRW1
;						  THEN READ SECTOR
		PUSH	B		; SAVE IP
		LXI		D,RDSEC
		CALL	IOS
		STA		DSKERR+2
		POP		B		; RESTORE IP
		JMP		NEXT
;						  ELSE WRITE SECTOR
QRW1	PUSH	B		; SAVE IP
		LXI		D,RITSEC
		CALL	IOS
		STA		DSKERR+2
		POP		B		; RESTORE IP
		JMP		NEXT
;
		.DB		83H		; R/W		( FORTH DISK PRIMITIVE ) 1.3
		.DB		"R/"
		.DB		'W'+80H
		.DW		QRW-7
		.IF		HAVEDISK
RSLW	.DW		DOCOL
		.DW		USE,AT
		.DW		TOR
		.DW		SWAP,SPBLK
		.DW		STAR,ROT
		.DW		USE,STORE
		.DW		TSCALC
		.DW		SPBLK,ZERO
		.DW		XDO				; DO
RSLW1	.DW		SETIO				; SET-IO
		.DW		QRW				; ?R/W
		.DW		PTRAC				; +TRACK
		.DW		PSEC				; +SECTOR
		.DW		XLOOP
		.DW		RSLW1-$				; LOOP
		.DW		DROP
		.DW		FROMR,USE
		.DW		STORE,SEMIS
		.ENDIF		
;
;--------------------------------------------------------
;
;		ALTERNATIVE R/W FOR NO DISK INTERFACE
;

;RSLW		.DW		DOCOL,DROP,DROP,DROP,SEMIS

;
;--------------------------------------------------------
;
		.DB		85H		; FLUSH
		.DB		"FLUS"
		.DB		'H'+80H
		.DW		RSLW-6
FLUSH	.DW		DOCOL
		.DW		NOBUF,ONEP
		.DW		ZERO,XDO
FLUS1	.DW		ZERO,BUFFE
		.DW		DROP
		.DW		XLOOP
		.DW		FLUS1-$
		.DW		SEMIS
;
		.DB		84H		; SAVE				1.3
		.DB		"SAV"
		.DB		'E'+80H
		.DW		FLUSH-8
SAVE	.DW		DOCOL
		.DW		FLUSH
		.DW		SEMIS
;
		.DB		84H		; LOAD
		.DB		"LOA"
		.DB		'D'+80H
		.DW		SAVE-7
LOAD	.DW		DOCOL,BLK
		.DW		AT,TOR
		.DW		INN,AT
		.DW		TOR,ZERO
		.DW		INN,STORE
		.DW		BSCR,STAR
		.DW		BLK,STORE		    ; BLK <- SCR * B/SCR
		.DW		INTER				; INTERPRET FROM OTHER SCREEN
		.DW		FROMR,INN
		.DW		STORE
		.DW		FROMR,BLK
		.DW		STORE
		.DW		SEMIS
;
		.DB		0C3H		; -->
		.DB		"--"
		.DB		'>'+80H
		.DW		LOAD-7
ARROW	.DW		DOCOL
		.DW		QLOAD
		.DW		ZERO
		.DW		INN
		.DW		STORE
		.DW		BSCR
		.DW		BLK
		.DW		AT
		.DW		OVER
		.DW		MODD
		.DW		SUBB
		.DW		BLK
		.DW		PSTOR
		.DW		SEMIS
;
		.DB		84H		        ; THRU				1.3
		.DB		"THR"
		.DB		'U'+80H
		.DW		ARROW-6
THRU	.DW		DOCOL
		.DW		ONEP,SWAP
		.DW		XDO				; DO
THRU1	.DW		IDO,LOAD
		.DW		XLOOP
		.DW		THRU1-$		   ; LOOP
		.DW		SEMIS
;
		.PAGE

		.if HAVEZ80MBC2
;
;		FORTH TO Z80-MBC2 "SERIAL IO INTERFACE"
;
;;; Print text immediate
		
PUTCHAR: PUSH		PSW
PCLOP1:	IN			11H
		ANI			00000001B		;transmitter ready?
		JZ			PCLOP1
		POP			PSW
		OUT			10H				;transmit
		RET
;				
PRINTI:	XTHL						;GET RETURN ADDRESS
		PUSH PSW
PIL:	MOV A,M						;GET A CHARACTER
		INX H						; NEXT SPOT
		ORA A
		JZ PIX						;NULL TERMINATION
		CALL PUTCHAR
		JMP PIL						;LOOP
PIX:	POP PSW
		XTHL						;RESTORE HL, UPDATE RETURN ADDR
		RET
;
TRACEON:
		.DW $+2
		MVI A,-1
		STA TRACE
		JMP NEXT
		
TRACEOFF:
		.DW $+2
		XRA A						;ZERO
		STA TRACE
		JMP NEXT

;;; Print String pointed to by HL
PRINT:	PUSH H
		PUSH PSW
PL:		MOV A,M						;GET A CHARACTER
		INX H						; NEXT SPOT
		ORA A
		JZ PX						;NULL TERMINATION
		JM PX						; OR HIGH-BIT TERMINATION
		CALL PUTCHAR
		JMP PL						;LOOP
PX:		POP PSW
		POP H
		RET
		
;;; Print HL as two characters
PRINTHL:
		PUSH H
		PUSH PSW
PHLLP:	MOV A,M
		ORA A						;SET FLAGS
		INX H
		CALL PUTCHAR
		JP PHLLP
		MVI A,13
		CALL PUTCHAR
		MVI A,10
		CALL PUTCHAR
		POP PSW
		POP H
		RET
		
;;; print two hex digits from A
PRINTHEX:
		PUSH PSW
		PUSH PSW
		ORA A 						;CLEAR CARRY
		RRC
		RRC
		RRC
		RRC						;HIGH NYBBLE
		ANI 0FH
		CPI 10
		JM PH1
		adi (('A'-'0') - 10)
PH1:	adi '0'
		call putchar
;;; Second digit
		pop PSW
		ani 0FH
		cpi 10
		jm PH2
		adi (('A'-'0') - 10)
PH2:	adi '0'
		;out m32ser
		call putchar
		pop PSW
		ret

		;; output HL in hex
HEXHL:
		push PSW
		mov A,H
		call PRINTHEX
		mov A,L
		call PRINTHEX
		pop PSW
		ret
		
		
PRINTNFA:	PUSH D
		INX D
		JMP PBC0
PRINTDE:
		PUSH D
PBC0:	PUSH PSW
PBCLP:	LDAX D
		ORA A						;SET FLAGS
		INX D
		CALL PUTCHAR
		JP PBCLP
		MVI A,13
		CALL PUTCHAR
		MVI A,10
		CALL PUTCHAR
		POP PSW
		POP D
		RET

EPRINT	.DW		0			; ENABLE PRINTER VARIABLE
LSTCHR  .DB 0				; Last character read.
m32ser	.EQU 1				; IO port with pseudo-serial interface
		
PQTER	LXI		H,0		;assume false
		.if 0
		LDA		LSTCHR		;Get last character input (if any)
		ORA		A		; Was there one already?
		JNZ		PQTRUE		;  return True
		IN		m32ser		;Read from IO processor
		ORA		A		;Check this result
		JZ		PQFALSE		;   no data either place
;;; We Have a char that we don't want yet in A.
		STA		LSTCHR		; Save it for now
PQTRUE:	INR		L		; THEN (S1) <- TRUE
		.endif
PQFALSE: JMP	HPUSH		; ELSE (S1) <- FALSE
;
PKEY
		.if 0
		LDA		LSTCHR
		ORA		A						; Check for saved character
		JZ		PKEY1				;  there isn't one!
		MOV		H,A						; return the saved char
		XRA		A						;  clear saved character
		STA		LSTCHR
		JMP		HPUSH				; next
		.endif
				
PKEY1:	IN      11H
		ANI		00000010B		;receiver ready?
		JZ		PKEY1
		NOP
		IN 		10H
			
		CPI		'T' & 31		; Turn on Trace?
		JNZ     KEYREAL
		LDA		trace
		CMA
		STA		trace
		jmp		PKEY1
KEYREAL: LXI		H,0
		MOV		L,A
		JMP		HPUSH		; (S1)LB <- CHR
		
;
PEMIT	.DW		$+2		; (EMIT)		ORPHAN
		POP		H		; () <- (S1)LB = CHR
		MOV		A,L
		CALL PUTCHAR
		JMP		NEXT

TOS1:	.DW 0						;temp vars
TOS2:	.DW 0
TOS3:	.DW 0
DTOS	.DW $+2						;Dump Top of Stack (ORPHAN)
		LXI H,0
		DAD SP						;GET SP
		CALL HEXHL
		MVI A,':'
		CALL PUTCHAR
		POP H						;GET TOP OF STACK
		SHLD TOS1
		POP H
		SHLD TOS2
		POP H
		SHLD TOS3
		PUSH H						;TOS3 BACK ON STACK
		LHLD TOS2
		PUSH H						;TOS2 BACK ON STACK
		LHLD TOS1
		PUSH H						;TOS1 BACK ON STACK
		CALL HEXHL				;PRINT TOS1
		MVI A,' '
		CALL PUTCHAR
		LHLD TOS2				;PRINT TOS2
		CALL HEXHL
		MVI A,' '
		CALL PUTCHAR
		LHLD TOS3				;PRINT TOS3
		CALL HEXHL
		JMP NEXT
		
;
PCR		MVI		A,ACR		; OUTPUT (CR) TO CONSOLE
		CALL 	PUTCHAR
		MVI		A,LF		; OUTPUT (LF) TO CONSOLE
		CALL 	PUTCHAR
		JMP		NEXT
;;;
		.endif 						; HAVEZ80MBC2
;
;----------------------------------------------------
		.PAGE
;
		.DB		0C1H		; '		( TICK )
		.DB		0A7H
		.DW		THRU-7
TICK	.DW		DOCOL
		.DW		DFIND
		.DW		ZEQU
		.DW		ZERO
		.DW		QERR
		.DW		DROP
		.DW		LITER
		.DW		SEMIS
;
		.DB		86H		     ; FORGET		1.3
		.DB		"FORGE"
		.DB		'T'+80H
		.DW		TICK-4
FORG	.DW		DOCOL
		.DW		CURR
		.DW		AT
		.DW		CONT
		.DW		AT
		.DW		SUBB
		.DW		LIT
		.DW		18H
		.DW		QERR
		.DW		TICK
		.DW		DUP
		.DW		FENCE
		.DW		AT
		.DW		ULESS
		.DW		LIT
		.DW		15H
		.DW		QERR
		.DW		DUP
		.DW		NFA
		.DW		DP
		.DW		STORE
		.DW		LFA
		.DW		AT
		.DW		CONT
		.DW		AT
		.DW		STORE
		.DW		SEMIS
;
		.DB		84H		; BACK
		.DB		"BAC"
		.DB		'K'+80H
		.DW		FORG-9
BACK	.DW		DOCOL
		.DW		HERE
		.DW		SUBB
		.DW		COMMA
		.DW		SEMIS
;
		.DB		0C5H		; BEGIN
		.DB		"BEGI"
		.DB		'N'+80H
		.DW		BACK-7
BEGIN	.DW		DOCOL
		.DW		QCOMP
		.DW		HERE
		.DW		ONE
		.DW		SEMIS
;
		.DB		0C5H		; ENDIF
		.DB		"ENDI"
		.DB		'F'+80H
		.DW		BEGIN-8
ENDIFF	.DW		DOCOL
		.DW		QCOMP
		.DW		TWO
		.DW		QPAIR
		.DW		HERE
		.DW		OVER
		.DW		SUBB
		.DW		SWAP
		.DW		STORE
		.DW		SEMIS
;
		.DB		0C4H		; THEN
		.DB		"THE"
		.DB		'N'+80H
		.DW		ENDIFF-8
THEN	.DW		DOCOL
		.DW		ENDIFF
		.DW		SEMIS
;
		.DB		0C2H		; DO
		.DB		"D"
		.DB		'O'+80H
		.DW		THEN-7
DO		.DW		DOCOL
		.DW		COMP
		.DW		XDO
		.DW		HERE
		.DW		THREE
		.DW		SEMIS
;
		.DB		0C4H		; LOOP
		.DB		"LOO"
		.DB		'P'+80H
		.DW		DO-5
LOOP		.DW		DOCOL
		.DW		THREE
		.DW		QPAIR
		.DW		COMP
		.DW		XLOOP
		.DW		BACK
		.DW		SEMIS
;
		.DB		0C5H		; +LOOP
		.DB		"+LOO"
		.DB		'P'+80H
		.DW		LOOP-7
PLOOP	.DW		DOCOL
		.DW		THREE
		.DW		QPAIR
		.DW		COMP
		.DW		XPLOO
		.DW		BACK
		.DW		SEMIS
;
		.DB		0C5H		; UNTIL
		.DB		"UNTI"
		.DB		'L'+80H
		.DW		PLOOP-8
UNTIL	.DW		DOCOL
		.DW		ONE
		.DW		QPAIR
		.DW		COMP
		.DW		ZBRAN
		.DW		BACK
		.DW		SEMIS
;
		.DB		0C3H		; END
		.DB		"EN"
		.DB		'D'+80H
		.DW		UNTIL-8
ENDD	.DW		DOCOL
		.DW		UNTIL
		.DW		SEMIS
;
		.DB		0C5H		; AGAIN
		.DB		"AGAI"
		.DB		'N'+80H
		.DW		ENDD-6
AGAIN	.DW		DOCOL
		.DW		ONE
		.DW		QPAIR
		.DW		COMP
		.DW		BRAN
		.DW		BACK
		.DW		SEMIS
;
		.DB		0C6H		; REPEAT
		.DB		"REPEA"
		.DB		'T'+80H
		.DW		AGAIN-8
REPEA	.DW		DOCOL
		.DW		TOR
		.DW		TOR
		.DW		AGAIN
		.DW		FROMR
		.DW		FROMR
		.DW		TWO
		.DW		SUBB
		.DW		ENDIFF
		.DW		SEMIS
;
		.DB		0C2H		; IF
		.DB		"I"
		.DB		'F'+80H
		.DW		REPEA-9
IFF		.DW		DOCOL
		.DW		COMP
		.DW		ZBRAN
		.DW		HERE
		.DW		ZERO
		.DW		COMMA
		.DW		TWO
		.DW		SEMIS
;
		.DB		0C4H		; ELSE
		.DB		"ELS"
		.DB		'E'+80H
		.DW		IFF-5
ELSEE	.DW		DOCOL
		.DW		TWO
		.DW		QPAIR
		.DW		COMP
		.DW		BRAN
		.DW		HERE
		.DW		ZERO
		.DW		COMMA
		.DW		SWAP
		.DW		TWO
		.DW		ENDIFF
		.DW		TWO
		.DW		SEMIS
;
		.DB		0C5H		; WHILE
		.DB		"WHIL"
		.DB		'E'+80H
		.DW		ELSEE-7
WHILE	.DW		DOCOL
		.DW		IFF
		.DW		TWOP
		.DW		SEMIS
;
		.DB		86H		; SPACES
		.DB		"SPACE"
		.DB		'S'+80H
		.DW		WHILE-8
SPACS	.DW		DOCOL
		.DW		ZERO
		.DW		MAX
		.DW		DDUP
		.DW		ZBRAN		; IF
		.DW		SPAX1-$
		.DW		ZERO
		.DW		XDO		; DO
SPAX2	.DW		SPACE
		.DW		XLOOP		; LOOP		ENDIF
		.DW		SPAX2-$
SPAX1	.DW		SEMIS
;
		.DB		82H		; <#
		.DB		"<"
		.DB		'#'+80H
		.DW		SPACS-9
BDIGS	.DW		DOCOL
		.DW		PAD
		.DW		HLD
		.DW		STORE
		.DW		SEMIS
;
		.DB		82H		; #>
		.DB		"#"
		.DB		'>'+80H
		.DW		BDIGS-5
EDIGS	.DW		DOCOL
		.DW		DROP
		.DW		DROP
		.DW		HLD
		.DW		AT
		.DW		PAD
		.DW		OVER
		.DW		SUBB
		.DW		SEMIS
;
		.DB		84H		; SIGN
		.DB		"SIG"
		.DB		'N'+80H
		.DW		EDIGS-5
SIGN		.DW		DOCOL
		.DW		ROT
		.DW		ZLESS
		.DW		ZBRAN		; IF
		.DW		SIGN1-$
		.DW		LIT
		.DW		2DH
		.DW		HOLD		; ENDIF
SIGN1	.DW		SEMIS
;
		.DB		81H		; #
		.DB		'#'+80H
		.DW		SIGN-7
DIG		.DW		DOCOL
		.DW		BASE
		.DW		AT
		.DW		MSMOD
		.DW		ROT
		.DW		LIT
		.DW		9
		.DW		OVER
		.DW		LESS
		.DW		ZBRAN		; IF
		.DW		DIG1-$
		.DW		LIT
		.DW		7
		.DW		PLUS		; ENDIF
DIG1	.DW		LIT
		.DW		30H
		.DW		PLUS
		.DW		HOLD
		.DW		SEMIS
;
		.DB		82H		; #S
		.DB		"#"
		.DB		'S'+80H
		.DW		DIG-4
DIGS	.DW		DOCOL
		.if 0
		.DW		PDOTQ
		.DB		4, "DIGS"
		.DW		DTOS, CR
		.endif
DIGS1	.DW		DIG		; BEGIN
		.DW		TDUP
		.DW		ORR
		.DW		ZEQU
		.DW		ZBRAN		; UNTIL
		.DW		DIGS1-$
		.if 0
		.DW		PDOTQ
		.DB		5, "DIGSX"
		.DW		DTOS, CR
		.endif
		.DW		SEMIS
;
		.DB		83H		; D.R
		.DB		"D."
		.DB		'R'+80H
		.DW		DIGS-5
DDOTR	.DW		DOCOL
		.if 0
		.DW		PDOTQ
		.DB		5, "DDOTR"
		.DW		DTOS, CR
		.endif
		.DW		TOR
		.DW		SWAP
		.DW		OVER
		.DW		DABS
		.DW		BDIGS
		.DW		DIGS
		.DW		SIGN
		.DW		EDIGS
		.DW		FROMR
		.DW		OVER
		.DW		SUBB
		.DW		SPACS
		.DW		TYPE
		.DW		SEMIS
;
		.DB		82H		; .R
		.DB		"."
		.DB		'R'+80H
		.DW		DDOTR-6
DOTR		.DW		DOCOL
		.DW		TOR
		.DW		STOD
		.DW		FROMR
		.DW		DDOTR
		.DW		SEMIS
;
		.DB		82H		; D.
		.DB		"D"
		.DB		'.'+80H
		.DW		DOTR-5
DDOT	.DW		DOCOL
		.if 0
		.DW		PDOTQ
		.DB		4, "DDOT"
		.DW		DTOS
		.DW		CR
		.endif
		.DW		ZERO
		.DW		DDOTR
		.DW		SPACE
		.DW		SEMIS
;
		.DB		81H		; .
		.DB		'.'+80H
		.DW		DDOT-5
		.if 1
DOT		.DW		DOCOL
		.DW		STOD
		.DW		DDOT
		.DW		SEMIS
		.else
;;; Assembler version that I know works
DOT		.DW 		$+2
		pop		H
		call		HEXHL
		jmp		NEXT
		.endif
;
		.DB		81H		; ?
		.DB		'?'+80H
		.DW		DOT-4
QUES		.DW		DOCOL
		.DW		AT
		.DW		DOT
		.DW		SEMIS
;
		.DB		82H		; U.
		.DB		"U"
		.DB		'.'+80H
		.DW		QUES-4
UDOT	.DW		DOCOL
		.DW		ZERO
		.DW		DDOT
		.DW		SEMIS
;
		.DB		85H		; VLIST
		.DB		"VLIS"
		.DB		'T'+80H
		.DW		UDOT-5
VLIST	.DW		DOCOL
		.DW		LIT
		.DW		80H
		.DW		OUTT
		.DW		STORE
		.DW		CONT
		.DW		AT
		.DW		AT
VLIS1	.DW		OUTT		; BEGIN
		.DW		AT
		.DW		CSLL
		.DW		GREAT
		.DW		ZBRAN		; IF
		.DW		VLIS2-$
		.DW		CR
		.DW		ZERO
		.DW		OUTT
		.DW		STORE		; ENDIF
VLIS2	.DW		DUP
		.DW		IDDOT
		.DW		SPACE
		.DW		SPACE
		.DW		PFA
		.DW		LFA
		.DW		AT
		.DW		DUP
		.DW		QTERM
		.DW		ZBRAN		; IF
		.DW		VLIS3-$
		.DW		KEY
		.DW		LIT
		.DW		13H
		.DW		EQUAL
		.DW		ZBRAN		; IF
		.DW		VLIS9-$
		.DW		KEY
		.DW		LIT
		.DW		11H
		.DW		EQUAL
		.DW		ZEQU
		.DW		ZBRAN		; IF
		.DW		VLIS3-$
		.DW		SPSTO
		.DW		QUIT		;THEN
		.DW		BRAN
		.DW		VLIS3-$		; ELSE
VLIS9	.DW		SPSTO
		.DW		QUIT		; THEN
VLIS3	.DW		ZEQU
		.DW		ZBRAN		; UNTIL
		.DW		VLIS1-$
		.DW		DROP
		.DW		SEMIS
;
;------ EXIT CP/M  -----------------------
;
		.DB		83H		; BYE
		.DB		"BY"
		.DB		'E'+80H
		.DW		VLIST-8
BYE		.DW		$+2
		JMP		0E000H
;-----------------------------------------------
;
		.DB		84H		; PAGE				1.3
		.DB		"PAG"
		.DB		'E'+80H
		.DW		BYE-6
PAG		.DW		DOCOL
		.DW		LIT,FF
		.DW		EMIT,CR
		.DW		SEMIS
;
		.DB		84H		; LIST				1.3
		.DB		"LIS"
		.DB		'T'+80H
		.DW		PAG-7
LIST	.DW		DOCOL
		.DW		CR,DUP
		.DW		SCR,STORE
		.DW		PDOTQ
		.DB		6,"SCR # "
		.DW		DOT
		.DW		LIT,10H
		.DW		ZERO,XDO
LIST1	.DW		CR,IDO
		.DW		LIT,3
		.DW		DOTR,SPACE
		.DW		IDO,SCR
		.DW		AT,DLINE
		.DW		QTERM				; ?TERMINAL
		.DW		ZBRAN
		.DW		LIST2-$		; IF
		.DW		LEAVE				; LEAVE
LIST2	.DW		XLOOP
		.DW		LIST1-$		; ENDIF
		.DW		CR,SEMIS
;
		.DB		85H		; INDEX				1.3
		.DB		"INDE"
		.DB		'X'+80H
		.DW		LIST-7
INDEX	.DW		DOCOL
		.DW		PAG
		.DW		ONEP,SWAP
		.DW		XDO
INDE1	.DW		CR,IDO
		.DW		LIT,3
		.DW		DOTR,SPACE
		.DW		ZERO,IDO
		.DW		DLINE,QTERM
		.DW		ZBRAN
		.DW		INDE2-$
		.DW		LEAVE
INDE2	.DW		XLOOP
		.DW		INDE1-$
		.DW		SEMIS
;
		.DB		85H		; TRIAD				1.3
		.DB		"TRIA"
		.DB		'D'+80H
		.DW		INDEX-8
TRIAD	.DW		DOCOL
		.DW		PAG
		.DW		LIT,3
		.DW		SLASH
		.DW		LIT,3
		.DW		STAR
		.DW		LIT,3
		.DW		OVER,PLUS
		.DW		SWAP,XDO
TRIA1	.DW		CR,IDO
		.DW		LIST
		.DW		QTERM				; ?TERMINAL
		.DW		ZBRAN
		.DW		TRIA2-$		; IF
		.DW		LEAVE				; LEAVE
TRIA2	.DW		XLOOP
		.DW		TRIA1-$		; ENDIF
		.DW		CR
		.DW		LIT,15
		.DW		MESS,CR
		.DW		SEMIS
;
		.DB		84H		; SHOW				1.3
		.DB		"SHO"
		.DB		'W'+80H
		.DW		TRIAD-8
SHOW	.DW		DOCOL
		.DW		ONEP,SWAP
		.DW		XDO
SHOW1	.DW		PAG,IDO
		.DW		TRIAD
		.DW		LIT,3
		.DW		XPLOO
		.DW		SHOW1-$
		.DW		SEMIS
;
		.DB		84H		; .CPU
		.DB		".CP"
		.DB		'U'+80H
		.DW		SHOW-7
DOTCPU	.DW		DOCOL
		.DW		BASE,AT
		.DW		LIT,36
		.DW		BASE,STORE
		.DW		LIT,22H
		.DW		PORIG,TAT
		.DW		DDOT
		.DW		BASE,STORE
		.DW		SEMIS
;
		.DB		85H		; MATCH
		.DB		"MATC"
		.DB		'H'+80H
		.DW		DOTCPU-7
MATCH:	.DW		$+2
		MOV		L,C		; (HL) <-- (BC)
		MOV		H,B
		POP		B		; (BC) <-- (0,N)
		MOV		A,C
		POP		D		; (DE) <-- PAD
		POP		B		; (BC) <-- (0,LENGTH)
		MOV		B,A		; (BC) <-- (N,LEN)
		XTHL				; (S1) <-- (IP)
						; (HL) <-- (CURSOR)
		PUSH	H		; SAVE CURSOR OVER IP
		PUSH	B		; SAVE N,LEN
		INR		C
		DCX		H
MATCH1:	DCR		C
		MOV		A,C
		CMP		B		; LEN < N ?
		JM		MATCH4		; FAIL
		INX		H
		LDAX	D
		XRA		M		; (PAD) = (CURSAD) ?
		JNZ		MATCH1		; TRY AGAIN
		PUSH	H		; SAVE CURSOR+I
		PUSH	D		; SAVE PAD
		PUSH	B		; SAVE N,LEN-I
		MVI		C,1		; J=MATCH COUNT=1
MATCH2:	INR		C
		MOV		A,B
		CMP		C		; J > N ?
		JM		MATCH5		; SUCCEED
		INX		D
		INX		H
		LDAX	D
		XRA		M		; MATCH ?
		JZ		MATCH2		; NEXT CHAR
		POP		B		; RESTORE PARAMS
		POP		D
		POP		H
		JMP		MATCH1
;
MATCH4:	POP		D		; (DE) <-- N,LEN
		POP		H		; CURSAD
		POP		B		; IP
		MVI		D,0
		XCHG
		LXI		D,0		; FAIL
		JMP		DPUSH
;
MATCH5:	POP		B		; N,LEN-I
		POP		D		; PAD
		POP		H		; CURSAD+I
		POP		B		; N,LEN
		POP		D		; CURSAD
		MOV		A,L
		SUB		E
		MOV		L,A
		MOV		A,H
		SBB		D
		MOV		H,A		; (HL) <-- I
		MOV		E,B
		MVI		D,0
		DAD		D		; (HL) <-- I+N
		POP		B		; IP
		LXI		D,1		; SUCCEED
		JMP		DPUSH
;
		.DB		85H				; DEPTH = NUMBER
		.DB		"DEPT"				; OF WORDS
		.DB		'H'+80H				; ON STACK
		.DW		MATCH-8
DEPTH	.DW		DOCOL
		.DW		SPAT
		.DW		SZERO
		.DW		AT
		.DW		SWAP
		.DW		SUBB
		.DW		TWO
		.DW		SLASH
		.DW		SEMIS
;
		.DB		84H		; TEXT
		.DB		"TEX"
		.DB		'T'+80H
		.DW		DEPTH-8
TEXT:	.DW		DOCOL
		.DW		HERE
		.DW		CSLL
		.DW		ONEP
		.DW		BLANK
		.DW		WORD
		.DW		HERE
		.DW		PAD
		.DW		CSLL
		.DW		ONEP
		.DW		CMOVE
		.DW		SEMIS
;
		.DB		84H		; LINE
		.DB		"LIN"
		.DB		'E'+80H
		.DW		TEXT-7
LINE:	.DW		DOCOL
		.DW		DUP
		.DW		LIT
		.DW		0FFF0H
		.DW		ANDD
		.DW		LIT
		.DW		17H
		.DW		QERR
		.DW		SCR
		.DW		AT
		.DW		PLINE
		.DW		DROP
		.DW		SEMIS
;
		.DB		086H		; EDITOR
		.DB		"EDITO"
		.DB		'R'+80H
		.DW		LINE-7
EDITOR:	.DW		DODOE
		.DW		DOVOC
		.DW		0A081H
EDITP	.DW		ELAST		; COLD START VALUE ONLY
;							; CHANGED WHEN NEW EDITOR DEF ADDED

		.DW		0
;
		.DB		85H		; WHERE
		.DB		"WHER"
		.DB		'E'+80H
		.DW		EDITOR-9
WHERE:	.DW		DOCOL
		.DW		DUP
		.DW		BSCR
		.DW		SLASH
		.DW		DUP
		.DW		SCR
		.DW		STORE
		.DW		PDOTQ
		.DB		6
		.DB		"SCR # "
		.DW		DEC
		.DW		DOT
		.DW		SWAP
		.DW		CSLL
		.DW		SLMOD
		.DW		CSLL
		.DW		STAR
		.DW		ROT
		.DW		BLOCK
		.DW		PLUS
		.DW		CR
		.DW		CSLL
		.DW		TYPE
		.DW		CR
		.DW		HERE
		.DW		CAT
		.DW		SUBB
		.DW		SPACS
		.DW		LIT
		.DW		5EH
		.DW		EMIT
		.DW		BCOMP
		.DW		EDITOR
		.DW		QUIT
		.DW		SEMIS
		.PAGE
;
;		EDITOR DEFINITIONS
;
		.DB		83H		; TOP
		.DB		"TO"
		.DB		'P'+80H
		.DW		FORTH+4		; CHAIN EDITOR VOCAB TO FORTH VOCAB
TOP		.DW		DOCOL
		.DW		ZERO
		.DW		RNUM
		.DW		STORE
		.DW		SEMIS
;
		.DB		87H		; #LOCATE
		.DB		"#LOCAT"
		.DB		'E'+80H		; LEAVE CURSO
		.DW		TOP-6		; OFFSET,LINE
NLOCAT	.DW		DOCOL
		.DW		RNUM
		.DW		AT
		.DW		CSLL
		.DW		SLMOD
		.DW		SEMIS
;
		.DB		85H		; #LEAD
		.DB		"#LEA"
		.DB		"D'+80H		; LINE ADDR"
		.DW		NLOCAT-0AH
NLEAD	.DW		DOCOL		; OFFSET
		.DW		NLOCAT
		.DW		LINE
		.DW		SWAP
		.DW		SEMIS
;
		.DB		84H		; #LAG
		.DB		"#LA"		; CURSOR ADDR,
		.DB		'G'+80H		; COUNT AFTER
		.DW		NLEAD-8		; CURSOR
NLAG	.DW		DOCOL
		.DW		NLEAD
		.DW		DUP
		.DW		TOR
		.DW		PLUS
		.DW		CSLL
		.DW		FROMR
		.DW		SUBB
		.DW		SEMIS
;
		.DB		85H		; -MOVE
		.DB		"-MOV"
		.DB		'E'+80H
		.DW		NLAG-7
DMOVE	.DW		DOCOL
		.DW		LINE
		.DW		CSLL
		.DW		CMOVE
		.DW		UPDAT
		.DW		SEMIS
;
		.DB		81H		; H
		.DB		'H'+80H
		.DW		DMOVE-8
EDH		.DW		DOCOL
		.DW		LINE
		.DW		PAD
		.DW		ONEP
		.DW		CSLL
		.DW		DUP
		.DW		PAD
		.DW		CSTOR
		.DW		CMOVE
		.DW		SEMIS
;
		.DB		81H		; E
		.DB		'E'+80H
		.DW		EDH-4
EDE		.DW		DOCOL
		.DW		LINE
		.DW		CSLL
		.DW		BLANK
		.DW		UPDAT
		.DW		SEMIS
;
		.DB		81H		; S
		.DB		'S'+80H
		.DW		EDE-4
EDS		.DW		DOCOL
		.DW		DUP
		.DW		ONE
		.DW		SUBB
		.DW		LIT
		.DW		0EH
		.DW		XDO
EDS1	.DW		IDO
		.DW		LINE
		.DW		IDO
		.DW		ONEP
		.DW		DMOVE
		.DW		LIT
		.DW		-1H
		.DW		XPLOO
		.DW		EDS1-$
		.DW		EDE
		.DW		SEMIS
;
		.DB		81H		; D
		.DB		'D'+80H
		.DW		EDS-4
EDD		.DW		DOCOL
		.DW		DUP
		.DW		EDH
		.DW		LIT
		.DW		0FH
		.DW		DUP
		.DW		ROT
		.DW		XDO
EDD1	.DW		IDO
		.DW		ONEP
		.DW		LINE
		.DW		IDO
		.DW		DMOVE
		.DW		XLOOP
		.DW		EDD1-$
		.DW		EDE
		.DW		SEMIS
;
		.DB		81H		; M
		.DB		'M'+80H
		.DW		EDD-4
EDM		.DW		DOCOL
		.DW		RNUM
		.DW		PSTOR
		.DW		CR
		.DW		NLOCAT
		.DW		LIT
		.DW		3
		.DW		DOTR
		.DW		SPACE
		.DW		DROP
		.DW		NLEAD
		.DW		TYPE
		.DW		LIT
		.DW		5EH
		.DW		EMIT
		.DW		NLAG
		.DW		TYPE
		.DW		SEMIS
;
		.DB		81H		; T
		.DB		'T'+80H
		.DW		EDM-4
EDT		.DW		DOCOL
		.DW		DUP
		.DW		CSLL
		.DW		STAR
		.DW		RNUM
		.DW		STORE
		.DW		DUP
		.DW		EDH
		.DW		ZERO
		.DW		EDM
		.DW		SEMIS
;
		.DB		81H		; L
		.DB		'L'+80H
		.DW		EDT-4
EDL		.DW		DOCOL
		.DW		SCR
		.DW		AT
		.DW		LIST
		.DW		ZERO
		.DW		EDM
		.DW		SEMIS
;
		.DB		85H		; CLEAR
		.DB		"CLEA"
		.DB		'R'+80H
		.DW		EDL-4
CLEAR	.DW		DOCOL
		.DW		SCR
		.DW		STORE
		.DW		LIT
		.DW		10H
		.DW		ZERO
		.DW		XDO
CLEA1	.DW		IDO
		.DW		EDE
		.DW		XLOOP
		.DW		CLEA1-$
		.DW		SEMIS
;
		.DB		84H		; COPY
		.DB		"COP"
		.DB		'Y'+80H
		.DW		CLEAR-8
COPY	.DW		DOCOL
		.DW		BSCR
		.DW		STAR
		.DW		OFSET
		.DW		AT
		.DW		PLUS
		.DW		SWAP
		.DW		BSCR
		.DW		STAR
		.DW		BSCR
		.DW		OVER
		.DW		PLUS
		.DW		SWAP
		.DW		XDO
COP1	.DW		DUP
		.DW		IDO
		.DW		BLOCK
		.DW		TWO
		.DW		SUBB
		.DW		STORE
		.DW		ONEP
		.DW		UPDAT
		.DW		XLOOP
		.DW		COP1-$
		.DW		DROP
		.DW		FLUSH
		.DW		SEMIS
;
		.DB		85H		; 1LINE
		.DB		"1LIN"
		.DB		'E'+80H
		.DW		COPY-7
ONELN	.DW		DOCOL
		.DW		NLAG
		.DW		PAD
		.DW		COUNT
		.DW		MATCH
		.DW		RNUM
		.DW		PSTOR
		.DW		SEMIS
;
		.DB		84H		; FIND
		.DB		"FIN"
		.DB		'D'+80H
		.DW		ONELN-8
FIND	.DW		DOCOL		; BEGIN
FIN1	.DW		LIT
		.DW		3FFH
		.DW		RNUM
		.DW		AT
		.DW		LESS
		.DW		ZBRAN		; IF
		.DW		FIN2-$
		.DW		TOP
		.DW		PAD
		.DW		HERE
		.DW		CSLL
		.DW		ONEP
		.DW		CMOVE
		.DW		ZERO
		.DW		ERROR		; ENDIF
FIN2	.DW		ONELN
		.DW		ZBRAN		; UNTIL
		.DW		FIN1-$
		.DW		SEMIS
;
		.DB		86H		; DELETE
		.DB		"DELET"
		.DB		'E'+80H
		.DW		FIND-7
DELETE	.DW		DOCOL
		.DW		TOR
		.DW		NLAG
		.DW		PLUS
		.DW		RR
		.DW		SUBB
		.DW		NLAG
		.DW		RR
		.DW		MINUS
		.DW		RNUM
		.DW		PSTOR
		.DW		NLEAD
		.DW		PLUS
		.DW		SWAP
		.DW		CMOVE
		.DW		FROMR
		.DW		BLANK
		.DW		UPDAT
		.DW		SEMIS
;
		.DB		81H		; R
		.DB		'R'+80H
		.DW		DELETE-9
EDR		.DW		DOCOL
		.DW		PAD
		.DW		ONEP
		.DW		SWAP
		.DW		DMOVE
		.DW		SEMIS
;
		.DB		81H		; P
		.DB		'P'+80H
		.DW		EDR-4
EDP		.DW		DOCOL
		.DW		ONE
		.DW		TEXT
		.DW		EDR
		.DW		SEMIS
;
		.DB		81H		; I
		.DB		'I'+80H
		.DW		EDP-4
EDI		.DW		DOCOL
		.DW		DUP
		.DW		EDS
		.DW		EDR
		.DW		SEMIS
;
		.DB		81H		; N
		.DB		'N'+80H
		.DW		EDI-4
EDN		.DW		DOCOL
		.DW		FIND
		.DW		ZERO
		.DW		EDM
		.DW		SEMIS
;
		.DB		81H		; F
		.DB		'F'+80H
		.DW		EDN-4
EDF		.DW		DOCOL
		.DW		ONE
		.DW		TEXT
		.DW		EDN
		.DW		SEMIS
;
		.DB		81H		; B
		.DB		'B'+80H
		.DW		EDF-4
EDB		.DW		DOCOL
		.DW		PAD
		.DW		CAT
		.DW		MINUS
		.DW		EDM
		.DW		SEMIS
;
		.DB		81H		; X
		.DB		'X'+80H
		.DW		EDB-4
EDX		.DW		DOCOL
		.DW		ONE
		.DW		TEXT
		.DW		FIND
		.DW		PAD
		.DW		CAT
		.DW		DELETE
		.DW		ZERO
		.DW		EDM
		.DW		SEMIS
;
		.DB		84H		; TILL
		.DB		"TIL"
		.DB		'L'+80H
		.DW		EDX-4
TILL	.DW		DOCOL
		.DW		NLEAD
		.DW		PLUS
		.DW		ONE
		.DW		TEXT
		.DW		ONELN
		.DW		ZEQU
		.DW		ZERO
		.DW		QERR
		.DW		NLEAD
		.DW		PLUS
		.DW		SWAP
		.DW		SUBB
		.DW		DELETE
		.DW		ZERO
		.DW		EDM
		.DW		SEMIS
;
		.DB		83H		; PUT
		.DB		"PU"
		.DB		'T'+80H
		.DW		TILL-7
EPUT	.DW		DOCOL
		.DW		PAD
		.DW		COUNT
		.DW		NLAG
		.DW		ROT
		.DW		OVER
		.DW		MIN
		.DW		TOR
		.DW		RR
		.DW		RNUM
		.DW		PSTOR
		.DW		RR
		.DW		SUBB
		.DW		TOR
		.DW		DUP
		.DW		HERE
		.DW		RR
		.DW		CMOVE
		.DW		HERE
		.DW		NLEAD
		.DW		PLUS
		.DW		FROMR
		.DW		CMOVE
		.DW		FROMR
		.DW		CMOVE
		.DW		UPDAT
		.DW		ZERO
		.DW		EDM
		.DW		SEMIS
;
ELAST	.DB		81H		; C
		.DB		'C'+80H
		.DW		EPUT-6
EDC		.DW		DOCOL
		.DW		ONE
		.DW		TEXT
		.DW		EPUT
		.DW		SEMIS
;
;		FORTH DEFINITIONS ( CONTINUED )
;
FLAST	.DB		84H		; TASK
		.DB		"TAS"
		.DB		'K'+80H
		.DW		WHERE-8
TASK	.DW		DOCOL
		.DW		SEMIS
;
INITDP	.DS		EM-$-1024		;CONSUME MEMORY TO LIMIT
;
DEMO:	.DB 	"                                                                "
		.DB 	"  : MARC CR   22 7 / . CR ;                                     "
		.DB 	"                                                                "
		.DB 	"  : TEST CR MARC CR MARC CR MARC ;                              "
		.DB 	"                                                                "
		.DB 	"                                                                "
		.DB 	"                                                                "
		.DB 	"                                                                "
		.DB 	"                                                                "
		.DB 	"                                                                "
		.DB 	"                                                                "
		.DB 	"                                                                "
		.DB 	"                                                                "
		.DB 	"                                                                "
		.DB 	"                                                                "
		.DB 	"                                                                "
	
;
		.PAGE
;
;				MEMORY MAP
;		( THE FOLLOWING EQUATES ARE NOT REFERENCED ELSEWHERE )
;
;				LOCATION		CONTENTS
;				--------		--------
MCOLD	.EQU 		ORIG				;JMP TO COLD START
MWARM	.EQU		ORIG+4				;JMP TO WARM START
MA2		.EQU		ORIG+8				;COLD START PARAMETERS
MUP		.EQU		UP					;USER VARIABLES' BASE 'REG"
MRP		.EQU		RPP					;RETURN "TACK 'REGISTER"
;
MBIP	.EQU		BIP					;DEBUG SUPPORT
MDPUSH	.EQU		DPUSH				;ADDRESS INTERPRETER
MHPUSH	.EQU		HPUSH
MNEXT	.EQU		NEXT
;
MDP0	.EQU		DP0					;START FORTH DICTIONARY
MDIO	.EQU		DRIVE				;CP/M DISK INTERFACE
MCIO	.EQU		EPRINT				;CONSOLE & PRINTER INTERFACE
MIDP	.EQU		INITDP				;END INITIAL FORTH DICTIONARY
;								  = COLD (DP) VALUE
;								  = COLD (FENCE) VALUE
;								  |  NEW
;								  |  DEFINITIONS
;								  V
;
;								  ^
;								  |  DATA
;								  |  STACK
MIS0	.EQU		INITS0				;  = COLD (SP) VALUE = (S0)
;								   = (TIB)
;								  |  TERMINAL INPUT
;								  |  BUFFER
;								  V
;
;								  ^
;								  |  RETURN
;								  |  STACK
MIR0	.EQU		INITR0		;START USER VARIABLES
;								  = COLD (RP) VALUE = (R0)
;								  = (UP)
;								;END USER VARIABLES
MFIRST	.EQU		BUF1		;START DISK BUFFERS
;								  = FIRST
MEND	.EQU		EM-1		;END DISK BUFFERS
MLIMIT	.EQU		EM			;LAST MEMORY LOC USED + 1
;								  = LIMIT
;
;
		.END		ORIG
		.NOPAGE
