

.KL_ROM_SELECT		equ &b90f
.KL_ROM_RESTORE		equ &b90c
.KL_ROM_DESELECT	equ &b918
.KL_FIND_COMMAND	equ &bcd4
.KL_LOG_EXT			equ &bcd1
.TXT_OUTPUT			equ &bb5a
.TXT_GET_WINDOW		equ &bb69
.TXT_GET_CURSOR		equ &bb78
.ROM_TYPE			equ &c000
.ROM_VERSION_START	equ &c001
.ROM_COMMAND_TABLE	equ &c004

	org &8000

	ld hl,welcomemsg
	call PRINT_STRING

	ld hl,WORK_SPACE
	ld bc,RSX_TABLE
	jp KL_LOG_EXT
	

.WORK_SPACE
	DEFS 4

.RSX_TABLE
	defw NAME_TABLE
	jp PATCH
	jp RESTORE
	jp HELP

.NAME_TABLE
	defb "PATC","H"+&80
	defb "RESTOR","E"+&80
	defb "HEL","P"+&80
	defb 0


	;patch KL_FIND_COMMAND
.PATCH
	ld a,(PATCHED)
	cp 1
	jp z,dontpatch

	ld hl,(KL_FIND_COMMAND+1)
	ld (ORIG_KL_FIND_COMMAND),hl
	ld (rst1addr),hl
        
	ld hl,NEW_KL_FIND_COMMAND
	ld a,&c3
	ld (KL_FIND_COMMAND),a
	ld (KL_FIND_COMMAND+1),hl

	ld a,1
	ld (PATCHED),a

	ld hl,patchmsg
	call PRINT_STRING
    ret

.dontpatch
	ld hl,alreadypatched
	call PRINT_STRING
	ret

.RESTORE  
	ld a,(PATCHED)
	cp 0
	jp z,dontunpatch

	ld a,&cf
	ld (KL_FIND_COMMAND),a
	ld hl,(ORIG_KL_FIND_COMMAND)
	ld (KL_FIND_COMMAND+1),hl

	ld hl,unpatchmsg
	call PRINT_STRING
	ld a,0
	ld (PATCHED),a
    ret

.dontunpatch
	ld hl,notpatched
	call PRINT_STRING
	ret	

	;*******************************************************
	; New KL_FIND_COMMAND:
	; If RSX is prefixed with 0 to 9 the provided ROM is 
	; searched for the RSX. If found the ROM select address
	; and address of routine are returned. If the RSX is not
	; found in the specified ROM or the RSX does not start
	; with a ROM number, control is passed to the original
	; KL_FIND_COMMAND.
	;
	; On Entry:
	;	HL = Address of command to search
	;
	; On Exit:
	;   If found:
	;	Carry true
	;	C = ROM Select Address
	;	HL = Address of routine
	;   If not found:
	;	Carry false
	;	C and HL Corrupt
	;*******************************************************

.NEW_KL_FIND_COMMAND

	ld (TO_FIND),hl				;store pointer to command to find

	;does the RSX start 0 to 9
	ld a,(hl)
	cp &3a
	jp nc,call_original
	sub &30
	jp c,call_original

	;if here, first character must be a number between 0 and 9
	;so start searching the ROM
	
	;select the ROM to search
	ld (ROM_ADDRESS),a
	ld c,a
	call KL_ROM_SELECT

	push bc						;BC holds info for KL_ROM_DESELECT call, so store it
	call FIND_RSX
	pop bc

	;Deselect selected ROM
	call KL_ROM_DESELECT

	;decide what to do if the command wasn't found in the ROM
	
	jp nc,call_original			;not found so need to call original KL_FIND_COMMAND

	;as command found, set KL_FIND_COMMAND exit conditions
	;HL = Address of Command
	;C = ROM Address
	;carry flag already set from command search

	ex de,hl					;on return from command search DE held the command address, so put in HL
	ld a,(ROM_ADDRESS)
	ld c,a
	ret

.FIND_RSX

	ld hl,ROM_COMMAND_TABLE
        
	;get address of command table into HL
	ld e,(hl)
    inc hl
    ld d,(hl)
    inc hl
    ex de,hl 

	;HL = address of command table
    ;DE = jump block for commands
	;BC = Address of command to find

.find	
	ld bc,(TO_FIND) 	;get address of command to find
	inc bc				;move past the ROM number

.nxtchr 
	ld a,(bc)
    cp (hl)
    jr nz,nxtcmd           	;not the same, so next command
    inc hl
    inc bc
    add a,a                	;top bit set
    jr nc,nxtchr          	;no, go check next char
	
	;if we get here it has been found
    ;DE at this point will have jump block entry
    scf			;set carry as command found
	ret
	
.nxtcmd 
	ld a,(hl)
    inc hl
    add a,a                	;have we got to end of previous check?
    jr nc,nxtcmd           	;no, so loop and carry on

    ;move to next command in jump block
       
    inc de
    inc de
    inc de

    ;check not at end of command table
    ;end of command table is 0, if we have that
    ;the command hasn't been found

    ld a,(hl)
    or a
    jr nz,find  	;go back and seach the command again

	;reached the end of the ROM command table and not found     
	xor a		;clear carry as command not found
    ret

	;as the RSX wasn't found in the specified ROM pass to original KL_FIND_COMMAND
	;HL will contain the address of the command passed originally
	
.call_original
	
	;very hacky way of calling the original KL_FIND_COMMAND routine

	ld hl,(TO_FIND)
	rst 1
.rst1addr
	defw 0		
	ret

	;****************************************************************************
	;** HELP RSX
	;****************************************************************************

.HELP
	and a
	jp z,list_all_roms	;we have no parameter, so list all ROMS
	
	;if we are here, we have a ROM specified, so get the title and all the commands

	ld a,(ix+0)
	ld (TO_FIND),a
	ld c,a
	call KL_ROM_SELECT
	push bc			;BC holds info for KL_ROM_DESELECT, so store it

	call display_rom_name_version
	call CR_LF
	call display_rom_commands	

	pop bc
	call KL_ROM_DESELECT
	ret

.list_all_roms

	ld b,15			;iterate 15 ROMS
.romlp	
	ld a,15			;startimg at ROM 0
	sub b	
	ld c,a
	ld (TO_FIND),a
	push bc
	call KL_ROM_SELECT
	ld a,(TO_FIND)
	and a
	jr nz,check_rom_type
	call display_rom_name_version
	call CR_LF
.check_rom_type
	ld hl,ROM_TYPE
	ld a,(hl)
	cp 3
	jr nc,skiprom
	call display_rom_name_version
	call CR_LF
.skiprom
	call KL_ROM_DESELECT	
	pop bc
	djnz romlp
	ret

.display_rom_name_version

	ld hl,ROM_MSG
	call PRINT_STRING

	ld a,(TO_FIND)
	
	;now figure out if its double or single digit ROM number and print

	cp 10
	jp nc,double_digit
	push af
	ld a," "
	call TXT_OUTPUT
	ld a, " "
	call TXT_OUTPUT
	pop af
	add a,"0"
	call TXT_OUTPUT	
	jr get_name
.double_digit
	push af
	ld a," "
	call TXT_OUTPUT
	ld a,"1"
	call TXT_OUTPUT
	pop af
	add a,"0"
	sub 10
	call TXT_OUTPUT
	
.get_name
	ld a,":"
	call TXT_OUTPUT
	ld a," "
	call TXT_OUTPUT

	ld hl,ROM_COMMAND_TABLE

	;get address of command table and put into HL
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ex de,hl

	ld d,0

.loop_rom_name

	ld a,(hl)
	ld e,a
	and &7f
	call TXT_OUTPUT
	ld a,e
	inc d
	inc hl
	bit 7,e
	jr z,loop_rom_name
.padname
	ld a,d
	cp 18
	jr z,print_rom_version
	call PRINT_SPACE
	inc d
	jr padname

.print_rom_version

	;print the ROM version number

	push hl				;preserve HL as that has gone past the name in the command table
	ld a,32
	call TXT_OUTPUT
	ld a,"v"
	call TXT_OUTPUT
	ld hl,ROM_VERSION_START		;start of ROM Version
	ld a,(hl)
	add a,"0"
	call TXT_OUTPUT
	ld a,"."
	call TXT_OUTPUT
	inc hl
	ld a,(hl)
	add a,"0"
	call TXT_OUTPUT
	inc hl
	ld a,(hl)
	add a,"0"
	call TXT_OUTPUT
	pop hl
	ret

	;-------------------------------------------------

.display_rom_commands

	ld d,0			;loop count for character padding
.cmdlp
	ld a,(hl)
	ld e,a
	and a
	ret z
	and &7f
	call TXT_OUTPUT
	ld a,e
	inc d
	inc hl
	bit 7,e
	jr z,cmdlp
.padspace	
	ld a,d
	cp 20			;padded to 20 chars?
	jr z,space_or_newline	;yes, so see if we can fit more across the screen or go to the next line
	call PRINT_SPACE	;carry on padding with spaces
	inc d
	jr padspace
.space_or_newline
	call CHECK_CURSOR_POSITION
	jr display_rom_commands


	;****************************************************************************
	;** Helper Routines
	;****************************************************************************

.CHECK_CURSOR_POSITION
	push hl
	call TXT_GET_WINDOW
	ld a,d
	sub h
	ld e,a
	call TXT_GET_CURSOR
	ld a,e
	sub h
	pop hl
	cp 18
	call c,CR_LF
	ret

.PRINT_STRING
	ld a,(hl)
	and a
	ret z
	call TXT_OUTPUT
	inc hl
	jr PRINT_STRING

.CR_LF	ld a,13
	call TXT_OUTPUT
	ld a,10
	call TXT_OUTPUT
	ret

.PRINT_SPACE
	ld a," "
	call TXT_OUTPUT
	ret

.PATCHED				defb 0		;0 = unpatched, 1= patched
.ROM_ADDRESS			defb 0		;store ROM number (address) at beginning of passed RSX
.ORIG_KL_FIND_COMMAND	defw 0 
.TO_FIND				defw 0		;pointer to RSX passed to new KL_FIND_COMMAND
.patchmsg				defb "KL_FIND_COMMAND has been patched.",13,10,10,0
.unpatchmsg				defb "KL_FIND_COMMAND restored to original.",13,10,10,0
.alreadypatched			defb "KL_FIND_COMMAND is already patched.",13,10,10,0
.notpatched				defb "KL_FIND_COMMAND hasn't been pacthed.",13,10,10,0
.ROM_MSG				defb "ROM",0
.welcomemsg				defb "RomTool Version 1.4",13,10,10
                        defb "|PATCH   = Patch KL_FIND_COMMAND",13,10
                        defb "|RESTORE = Restore original KL_FIND_COMMAND",13,10
                        defb "|HELP,n  = List all commands in the specified ROM",13,10
                        defb "|HELP    = List all ROMS",13,10,10
                        defb "Once patched, use |[n]COMMAND where [n] is the ROM number.",13,10,10,0
