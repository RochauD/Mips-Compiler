

datatype register = 
      ZERO
    | AT
    | V0
    | V1
    | A0
    | A1
    | A2
    | A3
    | T0
    | T1
    | T2
    | T3
    | T4
    | T5
    | T6
    | T7
    | S0
    | S1
    | S2
    | S3
    | S4
    | S5
    | S6
    | S7
    | T8
    | T9
    | K0
    | K1
    | GP
    | SP
    | FP
    | RA;

datatype asmInstr = 
      ADD of (register * register * register)
    | ADDI of (register * register * int)
    | ADDIU of (register * register * int)
    | ADDU of (register * register * int)
    | AND of (register * register * register)
    | ANDI of (register * register * int)
    | BEQ of (register * register * int)
    | BGEZ of (register * int)
    | BGEZAL of (register * int)
    | BLEZ of (register * int)
    | BLTZ of (register * int)
    | BLTZAL of (register * int)
    | BNE of (register * register * int)
    | DIV of (register * register)
    | J of int
    | JAL of int
    | JR of register
    | LB of (register * int * register)
    | LUI of (register * int)
    | LW of (register * int * register)
    | MFHI of register
    | MFLO of register
    | MULT of (register * register)
    | MULTU of (register * register)
    | NOOP
    | OR of (register * register * register)
    | ORI of (register * register * int)
    | SB of (register * int * register)
    | SLL of (register * register * int)
    | SLT of (register * register * register)
    | SLTI of (register * register * int)
    | SLTIU of (register * register * int)
    | SLTU of (register * register * register)
    | SRA of (register * register * int)
    | SRL of (register * register * int)
    | SRLV of (register * register * register)
    | SUB of (register * register * register)
    | SUBU of (register * register * register)
    | SW of (register * int * register)
    | SYSCALL
    | XOR of (register * register * register)
    | XORI of (register * register * int);

fun convertRegisterToString(ZERO : register) = "$zero" 
    | convertRegisterToString(AT : register) = "$at"
    | convertRegisterToString(V0 : register) = "$v0"
    | convertRegisterToString(V1 : register) = "$v1"
    | convertRegisterToString(A0 : register) = "$a0"
    | convertRegisterToString(A1 : register) = "$a1"
    | convertRegisterToString(A2 : register) = "$a2"
    | convertRegisterToString(A3 : register) = "$a3"
    | convertRegisterToString(T0 : register) = "$t0"
    | convertRegisterToString(T1 : register) = "$t1"
    | convertRegisterToString(T2 : register) = "$t2"
    | convertRegisterToString(T3 : register) = "$t3"
    | convertRegisterToString(T4 : register) = "$t4"
    | convertRegisterToString(T5 : register) = "$t5"
    | convertRegisterToString(T6 : register) = "$t6"
    | convertRegisterToString(T7 : register) = "$t7"
    | convertRegisterToString(S0 : register) = "$s0"
    | convertRegisterToString(S1 : register) = "$s1"
    | convertRegisterToString(S2 : register) = "$s2"
    | convertRegisterToString(S3 : register) = "$s3"
    | convertRegisterToString(S4 : register) = "$s4"
    | convertRegisterToString(S5 : register) = "$s5"
    | convertRegisterToString(S6 : register) = "$s6"
    | convertRegisterToString(S7 : register) = "$s7"
    | convertRegisterToString(T8 : register) = "$t8"
    | convertRegisterToString(T9 : register) = "$t9"
    | convertRegisterToString(K0 : register) = "$k0"
    | convertRegisterToString(K1 : register) = "$k1"
    | convertRegisterToString(GP : register) = "$gp"
    | convertRegisterToString(SP : register) = "$sp"
    | convertRegisterToString(FP : register) = "$fp"
    | convertRegisterToString(RA : register) = "$ra";
