; This symbol is hardcoded into debugger to break
.globl _fgbc_debugger_break
_fgbc_debugger_break:
ret

.globl _fgbc_debugger_stop
_fgbc_debugger_stop:
stop

.globl _fgbc_print_int
_fgbc_print_int:
LDA HL,2(SP)
PUSH AF
LD A,(HL)
CALL #._fgbc_print_and_ret
POP AF
RET

.globl _fgbc_print_string
_fgbc_print_string:
LDA HL,2(SP)
PUSH BC		; Save BC
PUSH AF		; Save AF
LD C,(HL) 
INC HL
LD B,1(HL)
$LOOPENTRY:
LD A,(BC)
OR A,A		; Check for NULL terminator
JR Z,$ENDLOOP
CALL #._fgbc_printa_and_ret
INC BC
JR $LOOPENTRY
$ENDLOOP:
POP AF
POP BC		; Restore BC
RET

; Raw opcodes (FGBC_PRINTA & RET)
._fgbc_printa_and_ret:
	.byte 0xFC, 0xC9

; (FGBC_PRINT & RET)
._fgbc_print_and_ret:
	.byte 0xFD, 0xC9