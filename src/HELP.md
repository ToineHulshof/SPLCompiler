# SSM Help

## Table of Contents

- [General help](#General)
- [Instructions help](#Instructions)

## General

### Keywords [⭱](#ssm-help)

[registers](#registers-) [PC](#PC-) [programcounter](#programcounter-) [SP](#SP-) [stackpointer](#stackpointer-) [MP](#MP-) [markpointer](#markpointer-) [HP](#HP-) [heappointer](#heappointer-) [RR](#RR-) [return register](#return register-) [instruction](#instruction-) [memory](#memory-) [stack](#stack-) [heap](#heap-) [code](#code-) [help](#help-) [syntax](#syntax-) [labels](#labels-) [False](#False-) [True](#True-) 
### registers [⭱](#ssm-help)

Eight registers are available, some of which have a specific purpose. A register is private location in a processor, often faster accessible then external memory. Currently the programcounter (PC), stackpointer (SP), markpointer (MP), heappointer (HP), and return register (RR) as well as freely usable scratch registers are available, respectively identified by numbers 0..7. Registers are identified by the name R<x>, where <x> is the register number. Register with a specific purpose are also named with the name indicating their purpose.

### PC [⭱](#ssm-help)

The programcounter (PC) is used to remember what the current next instruction is. It contains the address (i.e. points to) the location of the next instruction. The machine fetches an instruction from the location pointed to by the PC. After each fetch it is automatically updated to point to the next instruction.

### programcounter [⭱](#ssm-help)

See [PC](#PC-) 

### SP [⭱](#ssm-help)

The stackpointer (SP) is used to push and pop values for usage in expression evaluation. The stack is also used to store variables. These are often accessed via the MP.

### stackpointer [⭱](#ssm-help)

See [SP](#SP-) 

### MP [⭱](#ssm-help)

The markpointer (MP) is used to access local variables, allocated on the stack. Each variable is accessed using a displacement relative to the MP.

### markpointer [⭱](#ssm-help)

See [MP](#MP-) 

### HP [⭱](#ssm-help)

The heappointer (HP) is used to remember the next free address of the heap. After every store to the heap, the heappointer is incremented with the size of stored values. 

### heappointer [⭱](#ssm-help)

See [HP](#HP-) 

### RR [⭱](#ssm-help)

The return register (RR) is used to return a value without placing it on a stack. Strictly seen this is not necessary but a convenience, since values also can be passed via the stack.

### return register [⭱](#ssm-help)

See [RR](#RR-) 

### instruction [⭱](#ssm-help)

An instruction is an encoding of some operation, executed in the machine. A set of instructions stored in memory is called the code. Some instructions have inline operands, that is, after their location in the code an extra operand is stored, a constant, e.g. in "ldc 1". In pre/post conditions this location is indicated by M[PCpre+1] since it can be found on that location. The behavior of an instruction is both informally described as well as using pre/postcondifitions.

### memory [⭱](#ssm-help)

Memory stores words. A word is an 32 bits integer. Currently only a limited amount of memory words is reserver (2000), this is rather arbitrary, in the future memory size will adapt automatically to the amount needed.

### stack [⭱](#ssm-help)

Stack is the part of memory used to store values needed for evaluating expressions. The stack is located after the code and grows from lower addresses to higher ones.

### heap [⭱](#ssm-help)

Heap is the part of memory used to store composite values. The heap is located after the stack and grows from lower addresses to higher ones.

### code [⭱](#ssm-help)

Code is the part of memory used to store instructions. It starts at address 0.

### help [⭱](#ssm-help)

The Simple Stack Machine Interpreter executes instructions for a hypothetical (and thus simple) machine. See memory, registers, syntax, instruction as starting points for help.

### syntax [⭱](#ssm-help)

Syntax of instructions (as loaded from file) is: (label:)? (instr arg*)?. In other words, an (optional) instruction preceded by an (optional) label and followed by an argument if required. Comment may start with ";" or "//" (Java/C++ style) and ends at the end of the line. This characters are interpreted as start of comment. A label may be used as an argument. Example: "l1: beq l1 ; comment".

### labels [⭱](#ssm-help)

A label is an identifier indicating a position in the code. When loading, the code location of a label is calculated (called resolution). This is done in the user interface of the program and after loading labels are not kept consistent (when adding new instructions for example).

### False [⭱](#ssm-help)

Value False is encoded by a 0.

### True [⭱](#ssm-help)

Value True is encoded by a -1 (all 1 bit pattern 0xFFFFFFFF). However, when testing in the context of a BRF instruction takes place, anything else than 0 is considered to be True.

## Instructions

### Keywords [⭱](#ssm-help)

[ldc](#ldc-) [lds](#lds-) [ldms](#ldms-) [sts](#sts-) [stms](#stms-) [ldsa](#ldsa-) [ldl](#ldl-) [ldml](#ldml-) [stl](#stl-) [stml](#stml-) [ldla](#ldla-) [lda](#lda-) [ldma](#ldma-) [ldaa](#ldaa-) [sta](#sta-) [stma](#stma-) [ldr](#ldr-) [ldrr](#ldrr-) [str](#str-) [swp](#swp-) [swpr](#swpr-) [swprr](#swprr-) [ajs](#ajs-) [add](#add-) [mul](#mul-) [sub](#sub-) [div](#div-) [mod](#mod-) [and](#and-) [or](#or-) [xor](#xor-) [eq](#eq-) [ne](#ne-) [lt](#lt-) [le](#le-) [gt](#gt-) [ge](#ge-) [neg](#neg-) [not](#not-) [bsr](#bsr-) [bra](#bra-) [brf](#brf-) [brt](#brt-) [jsr](#jsr-) [ret](#ret-) [link](#link-) [unlink](#unlink-) [nop](#nop-) [halt](#halt-) [trap](#trap-) [annote](#annote-) [ldh](#ldh-) [ldmh](#ldmh-) [sth](#sth-) [stmh](#stmh-) 

### ldc [⭱](#ssm-help)

#### Description

Load Constant. Pushes the inline constant on the stack.

#### Pre and postconditions

```
SP_post = SP_pre + 1
M_post[SP_post] = M_pre[PC_pre+1]
```

### lds [⭱](#ssm-help)

#### Description

Load from Stack. Pushes a value relative to the top of the stack.

#### Pre and postconditions

```
SP_post = SP_pre + 1
M_post[SP_post] = M_pre[SP_pre + M_pre[PC_pre+1]]
```

### ldms [⭱](#ssm-help)

#### Description

Load Multiple from Stack. Pushes values relative to the top of the stack. Same as single load variant but second inline parameter is size.

#### Pre and postconditions

```
displ = M_pre[PC_pre + 1]
size = M_pre[PC_pre + 2]
SP_post = SP_pre + size
M_post[SP_post - size + 1 .. SP_post] = M_pre[SP_pre + displ .. SP_pre + displ + size - 1]
```

### sts [⭱](#ssm-help)

#### Description

Store into Stack. Pops a value from the stack and stores it in a location relative to the top of the stack.

#### Pre and postconditions

```
SP_post = SP_pre - 1
M_post[SP_pre + M_pre[PC_pre+1]] = M_pre[SP_pre]
```

### stms [⭱](#ssm-help)

#### Description

Store Multiple into Stack. Pops values from the stack and stores it in a location relative to the top of the stack. Same as single store variant but second inline parameter is size.

#### Pre and postconditions

```
displ = M_pre[PC_pre + 1]
size = M_pre[PC_pre + 2]
SP_post = SP_pre - size
M_post[SP_pre + displ .. SP_pre + displ + size - 1] = M_pre[SP_post + 1 .. SP_post + size]
```

### ldsa [⭱](#ssm-help)

#### Description

Load Stack Address. Pushes the address of a value relative to the stackpointer.

#### Pre and postconditions

```
SP_post = SP_pre + 1
M_post[SP_post] = SP_pre + M_pre[PC_pre+1]
```

### ldl [⭱](#ssm-help)

#### Description

Load Local. Pushes a value relative to the markpointer.

#### Pre and postconditions

```
SP_post = SP_pre + 1
M_post[SP_post] = M_pre[MP_pre + M_pre[PC_pre+1]]
```

### ldml [⭱](#ssm-help)

#### Description

Load Multiple Local. Pushes values relative to the markpointer. Same as single load variant but second inline parameter is size.

#### Pre and postconditions

```
displ = M_pre[PC_pre + 1]
size = M_pre[PC_pre + 2]
SP_post = SP_pre + size
M_post[SP_post - size + 1 .. SP_post] = M_pre[MP_pre + displ .. MP_pre + displ + size - 1]
```

### stl [⭱](#ssm-help)

#### Description

Store Local. Pops a value from the stack and stores it in a location relative to the markpointer.

#### Pre and postconditions

```
SP_post = SP_pre - 1
M_post[MP_pre + M_pre[PC_pre+1]] = M_pre[SP_pre]
```

### stml [⭱](#ssm-help)

#### Description

Store Multiple Local. Pops values from the stack and stores it in a location relative to the markpointer. Same as single store variant but second inline parameter is size.

#### Pre and postconditions

```
displ = M_pre[PC_pre + 1]
size = M_pre[PC_pre + 2]
SP_post = SP_pre - size
M_post[MP_pre + displ .. MP_pre + displ + size - 1] = M_pre[SP_post + 1 .. SP_post + size]
```

### ldla [⭱](#ssm-help)

#### Description

Load Local Address. Pushes the address of a value relative to the markpointer.

#### Pre and postconditions

```
SP_post = SP_pre + 1
M_post[SP_post] = MP_pre + M_pre[PC_pre+1]
```

### lda [⭱](#ssm-help)

#### Description

Load via Address. Dereferencing. Pushes the value pointed to by the value at the top of the stack. The pointer value is offset by a constant offset.

#### Pre and postconditions

```
SP_post = SP_pre
M_post[SP_post] = M_pre[M_pre[SP_pre] + M_pre[PC_pre+1]]
```

### ldma [⭱](#ssm-help)

#### Description

Load Multiple via Address. Pushes values relative to by the value at the top of the stack. Same as single load variant but second inline parameter is size.

#### Pre and postconditions

```
displ = M_pre[PC_pre + 1]
size = M_pre[PC_pre + 2]
SP_post = SP_pre + size - 1
M_post[SP_post - size + 1 .. SP_post] = M_pre[M_pre[SP_pre] + displ .. M_pre[SP_pre] + displ + size - 1]
```

### ldaa [⭱](#ssm-help)

#### Description

Load Address of Address. Add a constant offset to the address on top of the stack.

#### Pre and postconditions

```
SP_post = SP_pre
M_post[SP_post] = M_pre[SP_pre] + M_pre[PC_pre+1]
```

### sta [⭱](#ssm-help)

#### Description

Store via Address. Pops 2 values from the stack and stores the second popped value in the location pointed to by the first. The pointer value is offset by a constant offset.

#### Pre and postconditions

```
SP_post = SP_pre - 2
M_post[M_pre[SP_pre] + M_pre[PC_pre+1]] = M_pre[SP_pre-1]
```

### stma [⭱](#ssm-help)

#### Description

Store Multiple via Address. Pops values from the stack and stores it in a location relative to the value at the top of the stack. Same as single store variant but second inline parameter is size.

#### Pre and postconditions

```
displ = M_pre[PC_pre + 1]
size = M_pre[PC_pre + 2]
SP_post = SP_pre - size - 1
M_post[M_pre[SP_pre] + displ .. M_pre[SP_pre] + displ + size - 1] = M_pre[SP_post + 1 .. SP_post + size]
```

### ldr [⭱](#ssm-help)

#### Description

Load Register. Pushes a value from a register. Registers 0, 1, 2 and 3 are called PC (programcounter), SP (stackpointer), MP (markpointer) and RR (return register) respectively.

#### Pre and postconditions

```
SP_post = SP_pre + 1
M_post[SP_post] = REG_pre[ M_pre[PC_pre+1] ]
```

### ldrr [⭱](#ssm-help)

#### Description

Load Register from Register. Copy the content of the second register to the first. Does not affect the stack.

#### Pre and postconditions

```
REG_post[ M_pre[PC_pre+1] ] = REG_pre[ M_pre[PC_pre+2] ]
```

### str [⭱](#ssm-help)

#### Description

Store Register. Pops a value from the stack and stores it in the specified register. See also ldr.

#### Pre and postconditions

```
SP_post = SP_pre - 1
REG_post[ M_pre[PC_pre+1] ] = M_pre[SP_pre]
```

### swp [⭱](#ssm-help)

#### Description

Swap values. Swaps the 2 topmost values on the stack.

#### Pre and postconditions

```
SP_post = SP_pre
M_post[SP_post] = M_pre[SP_pre-1]
M_post[SP_post-1] = M_pre[SP_pre]
```

### swpr [⭱](#ssm-help)

#### Description

Swap Register. Swaps the content of a register with the top of the stack.

#### Pre and postconditions

```
SP_post = SP_pre
M_post[SP_post] = REG_pre[ M_pre[PC_pre+1] ]
REG_post[ M_pre[PC_pre+1] ] = M_pre[SP_pre]
```

### swprr [⭱](#ssm-help)

#### Description

Swap 2 Registers. Swaps the content of a register with another register.

#### Pre and postconditions

```
REG_post[ M_pre[PC_pre+1] ] = REG_pre[ M_pre[PC_pre+2] ]
REG_post[ M_pre[PC_pre+2] ] = REG_pre[ M_pre[PC_pre+1] ]
```

### ajs [⭱](#ssm-help)

#### Description

Adjust Stack. Adjusts the stackpointer with fixed amount.

#### Pre and postconditions

```
SP_post = SP_pre + M_post[PC_pre+1]
```

### add [⭱](#ssm-help)

#### Description

Addition. Replaces 2 top stack values with the addition of those values.

#### Pre and postconditions

```
SP_post = SP_pre - 1
M_post[SP_post] = M_pre[SP_pre - 1] + M_pre[SP_pre]
```

### mul [⭱](#ssm-help)

#### Description

Multiplication. Replaces 2 top stack values with the multiplication of those values.

#### Pre and postconditions

```
SP_post = SP_pre - 1
M_post[SP_post] = M_pre[SP_pre - 1] * M_pre[SP_pre]
```

### sub [⭱](#ssm-help)

#### Description

Substraction. Replaces 2 top stack values with the subtraction of those values.

#### Pre and postconditions

```
SP_post = SP_pre - 1
M_post[SP_post] = M_pre[SP_pre - 1] - M_pre[SP_pre]
```

### div [⭱](#ssm-help)

#### Description

Division. Replaces 2 top stack values with the division of those values.

#### Pre and postconditions

```
SP_post = SP_pre - 1
M_post[SP_post] = M_pre[SP_pre - 1] / M_pre[SP_pre]
```

### mod [⭱](#ssm-help)

#### Description

Division. Replaces 2 top stack values with the modulo of those values.

#### Pre and postconditions

```
SP_post = SP_pre - 1
M_post[SP_post] = M_pre[SP_pre - 1] % M_pre[SP_pre]
```

### and [⭱](#ssm-help)

#### Description

And. Replaces 2 top stack values with the bitwise and of those values.

#### Pre and postconditions

```
SP_post = SP_pre - 1
M_post[SP_post] = M_pre[SP_pre - 1] & M_pre[SP_pre]
```

### or [⭱](#ssm-help)

#### Description

Or. Replaces 2 top stack values with the bitwise or of those values.

#### Pre and postconditions

```
SP_post = SP_pre - 1
M_post[SP_post] = M_pre[SP_pre - 1] | M_pre[SP_pre]
```

### xor [⭱](#ssm-help)

#### Description

Exclusive Or. Replaces 2 top stack values with the bitwise exclusive or of those values.

#### Pre and postconditions

```
SP_post = SP_pre - 1
M_post[SP_post] = M_pre[SP_pre - 1] ^ M_pre[SP_pre]
```

### eq [⭱](#ssm-help)

#### Description

Test for equal. Replaces 2 top stack values with boolean result of the test. False is encoded as 0, True as -1 (0xFFFFFFFF). Used in combination with brf. This is a variant of cmp combined with beq.

#### Pre and postconditions

```
SP_post = SP_pre - 1
M_post[SP_post] = M_pre[SP_pre - 1] == M_pre[SP_pre]
```

### ne [⭱](#ssm-help)

#### Description

Test for not equal. Replaces 2 top stack values with boolean result of the test. False is encoded as 0, True as -1 (0xFFFFFFFF). Used in combination with brf. This is a variant of cmp combined with bne.

#### Pre and postconditions

```
SP_post = SP_pre - 1
M_post[SP_post] = M_pre[SP_pre - 1] != M_pre[SP_pre]
```

### lt [⭱](#ssm-help)

#### Description

Test for less then. Replaces 2 top stack values with boolean result of the test. False is encoded as 0, True as -1 (0xFFFFFFFF). Used in combination with brf. This is a variant of cmp combined with blt.

#### Pre and postconditions

```
SP_post = SP_pre - 1
M_post[SP_post] = M_pre[SP_pre - 1] < M_pre[SP_pre]
```

### le [⭱](#ssm-help)

#### Description

Test for less or equal. Replaces 2 top stack values with boolean result of the test. False is encoded as 0, True as -1 (0xFFFFFFFF). Used in combination with brf. This is a variant of cmp combined with ble.

#### Pre and postconditions

```
SP_post = SP_pre - 1
M_post[SP_post] = M_pre[SP_pre - 1] <= M_pre[SP_pre]
```

### gt [⭱](#ssm-help)

#### Description

Test for greater then. Replaces 2 top stack values with boolean result of the test. False is encoded as 0, True as -1 (0xFFFFFFFF). Used in combination with brf. This is a variant of cmp combined with bgt.

#### Pre and postconditions

```
SP_post = SP_pre - 1
M_post[SP_post] = M_pre[SP_pre - 1] > M_pre[SP_pre]
```

### ge [⭱](#ssm-help)

#### Description

Test for greater or equal. Replaces 2 top stack values with boolean result of the test. False is encoded as 0, True as -1 (0xFFFFFFFF). Used in combination with brf. This is a variant of cmp combined with bge.

#### Pre and postconditions

```
SP_post = SP_pre - 1
M_post[SP_post] = M_pre[SP_pre - 1] >= M_pre[SP_pre]
```

### neg [⭱](#ssm-help)

#### Description

Negation. Replaces top stack values with the (integer) negative of the value.

#### Pre and postconditions

```
SP_post = SP_pre
M_post[SP_post] = - M_pre[SP_pre]
```

### not [⭱](#ssm-help)

#### Description

Not. Replaces top stack values with the bitwise complement of the value.

#### Pre and postconditions

```
SP_post = SP_pre
M_post[SP_post] = ~ M_pre[SP_pre]
```

### bsr [⭱](#ssm-help)

#### Description

Branch to subroutine. Pushes the PC on the stack and jumps to the subroutine.

#### Pre and postconditions

```
SP_post = SP_pre + 1
M_post[SP_post] = PC_pre + 2
PC_post = PC_pre + M_pre[PC_pre + 1] + 2
```

### bra [⭱](#ssm-help)

#### Description

Branch Allways. Jumps to the destination. Replaces the PC with the destination address.

#### Pre and postconditions

```
PC_post = PC_pre + M_pre[PC_pre + 1] + 2
```

### brf [⭱](#ssm-help)

#### Description

Branch on False. If a False value is on top of the stack, jump to the destination.

#### Pre and postconditions

```
SP_post = SP_pre - 1
PC_post = PC_pre + M_pre[PC_pre + 1] + 2 (if false on top of the stack)
```

### brt [⭱](#ssm-help)

#### Description

Branch on True. If a True value is on top of the stack, jump to the destination.

#### Pre and postconditions

```
SP_post = SP_pre - 1
PC_post = PC_pre + M_pre[PC_pre + 1] + 2 (if true on top of the stack)
```

### jsr [⭱](#ssm-help)

#### Description

Jump to subroutine. Pops a destination from the stack, pushes the PC on the stack and jumps to the destination.

#### Pre and postconditions

```
SP_post = SP_pre
PC_post = M_pre[SP_pre]
M_post[SP_post] = PC_pre + 1
```

### ret [⭱](#ssm-help)

#### Description

Return from subroutine. Pops a previously pushed PC from the stack and jumps to it.

#### Pre and postconditions

```
SP_post = SP_pre - 1
PC_post = M_pre[SP_pre]
```

### link [⭱](#ssm-help)

#### Description

Reserve memory for locals. Convenience instruction combining the push of MP and the adjustment of the SP.

#### Pre and postconditions

```
MP_post = SP_pre  + 1
M_post[MP_post] = MP_pre
SP_post = MP_post + M_pre[PC_pre+1]
```

### unlink [⭱](#ssm-help)

#### Description

Free memory for locals. Convenience instruction combining the push of MP and the adjustment of the SP.

#### Pre and postconditions

```
MP_post = M_pre[MP_pre]
SP_post = MP_pre - 1
```

### nop [⭱](#ssm-help)

#### Description

No operation. Well, guess what...

#### Examples

```
nop
```

### halt [⭱](#ssm-help)

#### Description

Halt execution. Machine stops executing instructions.

#### Examples

```
halt
```

### trap [⭱](#ssm-help)

#### Description

Trap to environment function. Trap invokes a systemcall determined by its argument. Currently, trap supports the following system calls: <ol> <li value="0">Pop the topmost element from the stack and print it as an integer.</li> <li value="1">Pop the topmost element from the stack and print it as a unicode character.</li> <li value="10">Ask the user for an integer input and push it on the stack.</li> <li value="11">Ask the user for a unicode character input and push it on the stack.</li> <li value="12">Ask the user for a sequence of unicode characters input and push the characters on the stack terminated by a null-character.</li> <li value="20">Pop a null-terminated file name from the stack, open the file for reading and push a file pointer on the stack.</li> <li value="21">Pop a null-terminated file name from the stack, open the file for writing and push a file pointer on the stack.</li> <li value="22">Pop a file pointer from the stack, read a character from the file pointed to by the file pointer and push the character on the stack.</li> <li value="23">Pop a character and a file pointer from the stack, write the character to the file pointed to by the file pointer.</li> <li value="24">Pop a file pointer from the stack and close the corresponding file.</li> </ol>

#### Examples

```
ldc 5
trap 0 ; print 5 on output
```

### annote [⭱](#ssm-help)

#### Description

Annotate. A meta instruction (not producing code), annotating the stack display in the user interface with text and color. Annote takes 5 arguments, (1) a register name, (2) a low offset w.r.t. the register (used as starting point for annotating), (3) a high offset, (4) a color, (5) text. Color can be one of {black, blue, cyan, darkGray, gray, green, lightGray, magenta, orange, pink, red, yellow}. Text including spaces need to be enclosed in double quotes. The annote instruction is tied to the preceding (non-meta) instruction and will be performed immediately after the execution of that instruction.

#### Examples

```
annote SP -1 0 red "Pushed constants" ; annote top 2 stack values
```

### ldh [⭱](#ssm-help)

#### Description

Load from Heap. Pushes a value pointed to by the value at the top of the stack. The pointer value is offset by a constant offset.

#### Examples

```
ldc 5
sth
ldh 0
```

### ldmh [⭱](#ssm-help)

#### Description

Load Multiple from Heap. Pushes values pointed to by the value at the top of the stack. The pointer value is offset by a constant offset. Same as single load variant but the second inline parameter is size.

#### Examples

```
ldc 1
ldc 2
ldc 3
stmh 3
ldmh 0 3
```

### sth [⭱](#ssm-help)

#### Description

Store into Heap. Pops 1 value from the stack and stores it into the heap. Pushes the heap address of that value on the stack.

#### Examples

```
ldc 5
sth
```

### stmh [⭱](#ssm-help)

#### Description

Store Multiple into Heap. Pops values from the stack and stores it into the heap, retaining the order of the values. Same as single store variant but the inline parameter is size. Pushes the heap address of the last value on the stack.

#### Examples

```
ldc 1
ldc 2
ldc 3
stmh 3
```
