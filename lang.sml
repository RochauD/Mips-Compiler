

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
    | ADDU of (register * register * register)
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
    | DIVU of (register * register)
    | J of string
    | JAL of string
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
    | SLLV of (register * register * register)
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
    | XORI of (register * register * int)
    | LABEL of string;

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


fun convertInstructionToString(ADD(out, in1, in2)) = "add " ^ convertRegisterToString(out) ^ ", " ^ convertRegisterToString(in1) ^ ", " ^ convertRegisterToString(in2) ^ "\r\n"
  | convertInstructionToString(ADDI(out, in1, imm)) = "addi " ^ convertRegisterToString(out) ^ ", " ^ convertRegisterToString(in1) ^ ", " ^ Int.toString(imm) ^ "\r\n"
  | convertInstructionToString(ADDIU(out, in1, imm)) = "addiu " ^ convertRegisterToString(out) ^ ", " ^ convertRegisterToString(in1) ^ ", " ^ Int.toString(imm) ^ "\r\n"
  | convertInstructionToString(ADDU(out, in1, in2)) = "addu " ^ convertRegisterToString(out) ^ ", " ^ convertRegisterToString(in1) ^ ", " ^ convertRegisterToString(in2) ^ "\r\n"
  | convertInstructionToString(AND(out, in1, in2)) = "and " ^ convertRegisterToString(out) ^ ", " ^ convertRegisterToString(in1) ^ ", " ^ convertRegisterToString(in2) ^ "\r\n"
  | convertInstructionToString(ANDI(out, in1, imm)) = "andi " ^ convertRegisterToString(out) ^ ", " ^ convertRegisterToString(in1) ^ ", " ^ Int.toString(imm) ^ "\r\n"
  | convertInstructionToString(BEQ(in1, in2, offset)) = "beq " ^ convertRegisterToString(in1) ^ ", " ^ convertRegisterToString(in2) ^ ", " ^ Int.toString(offset) ^ "\r\n"
  | convertInstructionToString(BGEZ(in1, offset)) = "bgez " ^ convertRegisterToString(in1) ^ ", " ^ Int.toString(offset) ^ "\r\n"
  | convertInstructionToString(BGEZAL(in1, offset)) = "bgezal " ^ convertRegisterToString(in1) ^ ", " ^ Int.toString(offset) ^ "\r\n"
  | convertInstructionToString(BLEZ(in1, offset)) = "blez " ^ convertRegisterToString(in1) ^ ", " ^ Int.toString(offset) ^ "\r\n"
  | convertInstructionToString(BLTZ(in1, offset)) = "bltz " ^ convertRegisterToString(in1) ^ ", " ^ Int.toString(offset) ^ "\r\n"
  | convertInstructionToString(BLTZAL(in1, offset)) = "bltzal " ^ convertRegisterToString(in1) ^ ", " ^ Int.toString(offset) ^ "\r\n"
  | convertInstructionToString(BNE(in1, in2, offset)) = "bne " ^ convertRegisterToString(in1) ^ ", " ^ convertRegisterToString(in2) ^ ", " ^ Int.toString(offset) ^ "\r\n"
  | convertInstructionToString(DIV(in1, in2)) = "div " ^ convertRegisterToString(in1) ^ ", " ^ convertRegisterToString(in2) ^ "\r\n"
  | convertInstructionToString(DIVU(in1, in2)) = "divu " ^ convertRegisterToString(in1) ^ ", " ^ convertRegisterToString(in2) ^ "\r\n"
  | convertInstructionToString(J(target)) = "j " ^ target ^ "\r\n"
  | convertInstructionToString(JAL(target)) = "jal " ^ target ^ "\r\n"
  | convertInstructionToString(JR(target)) = "jr " ^ convertRegisterToString(target) ^ "\r\n"
  | convertInstructionToString(LB(out, offset, target)) = "lb " ^ convertRegisterToString(out) ^ ", " ^ Int.toString(offset) ^ "(" ^ convertRegisterToString(target) ^ ")" ^ "\r\n"
  | convertInstructionToString(LUI(out, imm)) = "lui " ^ convertRegisterToString(out) ^ ", " ^ Int.toString(imm) ^ "\r\n"
  | convertInstructionToString(LW(out, offset, target)) = "lw " ^ convertRegisterToString(out) ^ ", " ^ Int.toString(offset) ^ "(" ^ convertRegisterToString(target) ^ ")" ^ "\r\n"
  | convertInstructionToString(MFHI(out)) = "mfhi " ^ convertRegisterToString(out) ^ "\r\n"
  | convertInstructionToString(MFLO(out)) = "mflo " ^ convertRegisterToString(out) ^ "\r\n"
  | convertInstructionToString(MULT(in1, in2)) = "mult " ^ convertRegisterToString(in1) ^ ", " ^ convertRegisterToString(in2) ^ "\r\n"
  | convertInstructionToString(MULTU(in1, in2)) = "multu " ^ convertRegisterToString(in1) ^ ", " ^ convertRegisterToString(in2) ^ "\r\n"
  | convertInstructionToString(NOOP) = "noop" ^ "\r\n"
  | convertInstructionToString(OR(out, in1, in2)) = "or " ^ convertRegisterToString(out) ^ ", " ^ convertRegisterToString(in1) ^ ", " ^ convertRegisterToString(in2) ^ "\r\n"
  | convertInstructionToString(ORI(out, in1, imm)) = "ori " ^ convertRegisterToString(out) ^ ", " ^ convertRegisterToString(in1) ^ ", " ^ Int.toString(imm) ^ "\r\n"
  | convertInstructionToString(SB(in1, offset, target)) = "sb " ^ convertRegisterToString(in1) ^ ", " ^ Int.toString(offset) ^ "(" ^ convertRegisterToString(target) ^ ")" ^ "\r\n"
  | convertInstructionToString(SLL(out, in1, imm)) = "sll " ^ convertRegisterToString(out) ^ ", " ^ convertRegisterToString(in1) ^ ", " ^ Int.toString(imm) ^ "\r\n"
  | convertInstructionToString(SLLV(out, in1, in2)) = "sllv " ^ convertRegisterToString(out) ^ ", " ^ convertRegisterToString(in1) ^ ", " ^ convertRegisterToString(in2) ^ "\r\n"
  | convertInstructionToString(SLT(out, in1, in2)) = "slt " ^ convertRegisterToString(out) ^ ", " ^ convertRegisterToString(in1) ^ ", " ^ convertRegisterToString(in2) ^ "\r\n"
  | convertInstructionToString(SLTI(out, in1, imm)) = "slti " ^ convertRegisterToString(out) ^ ", " ^ convertRegisterToString(in1) ^ ", " ^ Int.toString(imm) ^ "\r\n"
  | convertInstructionToString(SLTIU(out, in1, imm)) = "sltiu " ^ convertRegisterToString(out) ^ ", " ^ convertRegisterToString(in1) ^ ", " ^ Int.toString(imm) ^ "\r\n"
  | convertInstructionToString(SLTU(out, in1, in2)) = "sltu " ^ convertRegisterToString(out) ^ ", " ^ convertRegisterToString(in1) ^ ", " ^ convertRegisterToString(in2) ^ "\r\n"
  | convertInstructionToString(SRA(out, in1, imm)) = "sra " ^ convertRegisterToString(out) ^ ", " ^ convertRegisterToString(in1) ^ ", " ^ Int.toString(imm) ^ "\r\n"
  | convertInstructionToString(SRL(out, in1, imm)) = "srl " ^ convertRegisterToString(out) ^ ", " ^ convertRegisterToString(in1) ^ ", " ^ Int.toString(imm) ^ "\r\n"
  | convertInstructionToString(SRLV(out, in1, in2)) = "srlv " ^ convertRegisterToString(out) ^ ", " ^ convertRegisterToString(in1) ^ ", " ^ convertRegisterToString(in2) ^ "\r\n"
  | convertInstructionToString(SUB(out, in1, in2)) = "sub " ^ convertRegisterToString(out) ^ ", " ^ convertRegisterToString(in1) ^ ", " ^ convertRegisterToString(in2) ^ "\r\n"
  | convertInstructionToString(SUBU(out, in1, in2)) = "subu " ^ convertRegisterToString(out) ^ ", " ^ convertRegisterToString(in1) ^ ", " ^ convertRegisterToString(in2) ^ "\r\n"
  | convertInstructionToString(SW(in1, offset, target)) = "sw " ^ convertRegisterToString(in1) ^ ", " ^ Int.toString(offset) ^ "(" ^ convertRegisterToString(target) ^ ")" ^ "\r\n"
  | convertInstructionToString(SYSCALL) = "syscall" ^ "\r\n"
  | convertInstructionToString(XOR(out, in1, in2)) = "xor " ^ convertRegisterToString(out) ^ ", " ^ convertRegisterToString(in1) ^ ", " ^ convertRegisterToString(in2) ^ "\r\n"
  | convertInstructionToString(XORI(out, in1, imm)) = "xori " ^ convertRegisterToString(out) ^ ", " ^ convertRegisterToString(in1) ^ ", " ^ Int.toString(imm) ^ "\r\n"
  | convertInstructionToString(LABEL(str)) = str ^ ":" ^ "\r\n";


datatype expression = 
      INTEGER_EXP of int
    | ADD_EXP of expression * expression
    | SUB_EXP of expression * expression
    | MUL_EXP of expression * expression
    | MOD_EXP of expression * expression
    | DIV_EXP of expression * expression
    | NOT_EXP of expression
    | AND_EXP of expression * expression
    | OR_EXP of expression * expression
    | XOR_EXP of expression * expression
    | LEQ_EXP of expression * expression
    | GEQ_EXP of expression * expression
    | NEQ_EXP of expression * expression
    | EQ_EXP of expression * expression
    | LT_EXP of expression * expression
    | GT_EXP of expression * expression
    | TO_BOOL_EXP of expression
    | CALL_EXP of string * expression list;

datatype statement =
      EMPTY_STATEMENT
    | WHILE_STATEMENT of expression * statement
    | IF_STATEMENT of expression * statement * statement
    | SEQUENCE_STATEMENT of statement list
    | RETURN_STATEMENT of expression
    | ASSIGNMENT_STATEMENT of string * expression
    | FUNCTION of string * string list * statement;


fun compileE(INTEGER_EXP(value), env) = ([ADDI(SP, SP, ~4), ADDI(T0, ZERO, value), SW(T0, 0, SP)], env)
  | compileE(ADD_EXP(exp1, exp2), env) = let
                                            val (res1, env1) = compileE(exp1,env)
                                            val (res2, env2) = compileE(exp2,env1)
                                         in
                                            (res2@res1@[LW(T0, 0, SP), LW(T1, 4, SP), ADDI(SP, SP, 4), ADD(T0, T0, T1), SW(T0, 0, SP)], env2)
                                         end
  | compileE(SUB_EXP(exp1, exp2), env) = let
                                            val (res1, env1) = compileE(exp1,env)
                                            val (res2, env2) = compileE(exp2,env1)
                                         in
                                            (res2@res1@[LW(T0, 0, SP), LW(T1, 4, SP), ADDI(SP, SP, 4), SUB(T0, T0, T1), SW(T0, 0, SP)], env2)
                                         end
  | compileE(MUL_EXP(exp1, exp2), env) = let
                                            val (res1, env1) = compileE(exp1,env)
                                            val (res2, env2) = compileE(exp2,env1)
                                         in
                                            (res2@res1@[LW(T0, 0, SP), LW(T1, 4, SP), ADDI(SP, SP, 4), MULT(T0, T1), MFLO(T0), SW(T0, 0, SP)], env2)
                                         end
  | compileE(MOD_EXP(exp1, exp2), env) = let
                                            val (res1, env1) = compileE(exp1,env)
                                            val (res2, env2) = compileE(exp2,env1)
                                         in
                                            (res2@res1@[LW(T0, 0, SP), LW(T1, 4, SP), ADDI(SP, SP, 4), DIV(T0, T1), MFHI(T0), SW(T0, 0, SP)], env2)
                                         end
  | compileE(DIV_EXP(exp1, exp2), env) = let
                                            val (res1, env1) = compileE(exp1,env)
                                            val (res2, env2) = compileE(exp2,env1)
                                         in
                                            (res2@res1@[LW(T0, 0, SP), LW(T1, 4, SP), ADDI(SP, SP, 4), DIV(T0, T1), MFLO(T0), SW(T0, 0, SP)], env2)
                                         end
  | compileE(NOT_EXP(exp), env) = let
                                      val (res1, env1) = compileE(exp,env)
                                  in
                                     (res1@[LW(T0, 0, SP), XORI(T0, T0, 1), SW(T0, 0, SP)], env1)
                                  end
  | compileE(AND_EXP(exp1, exp2), env) = let
                                            val (res1, env1) = compileE(exp1,env)
                                            val (res2, env2) = compileE(exp2,env1)
                                         in
                                            (res2@res1@[LW(T0, 0, SP), LW(T1, 4, SP), ADDI(SP, SP, 4), AND(T0, T0, T1), SW(T0, 0, SP)], env2)
                                         end
  | compileE(OR_EXP(exp1, exp2), env) = let
                                            val (res1, env1) = compileE(exp1,env)
                                            val (res2, env2) = compileE(exp2,env1)
                                         in
                                            (res2@res1@[LW(T0, 0, SP), LW(T1, 4, SP), ADDI(SP, SP, 4), OR(T0, T0, T1), SW(T0, 0, SP)], env2)
                                         end
  | compileE(XOR_EXP(exp1, exp2), env) = let
                                            val (res1, env1) = compileE(exp1,env)
                                            val (res2, env2) = compileE(exp2,env1)
                                         in
                                            (res2@res1@[LW(T0, 0, SP), LW(T1, 4, SP), ADDI(SP, SP, 4), XOR(T0, T0, T1), SW(T0, 0, SP)], env2)
                                         end
  | compileE(LEQ_EXP(exp1, exp2), env) = let
                                            val (res1, env1) = compileE(exp1,env)
                                            val (res2, env2) = compileE(exp2,env1)
                                         in
                                            (res2@res1@[LW(T0, 0, SP), LW(T1, 4, SP), ADDI(SP, SP, 4), SLT(T4, T0, T1), SLT(T2, T0, T1), SLT(T3, T1, T0), XOR(T0, T2, T3), XORI(T0, T0, 1), OR(T0, T0, T4), SW(T0, 0, SP)], env2)
                                         end
  | compileE(GEQ_EXP(exp1, exp2), env) = let
                                            val (res1, env1) = compileE(exp1,env)
                                            val (res2, env2) = compileE(exp2,env1)
                                         in
                                            (res2@res1@[LW(T0, 0, SP), LW(T1, 4, SP), ADDI(SP, SP, 4), SLT(T0, T1, T0), SW(T0, 0, SP)], env2)
                                         end
  | compileE(NEQ_EXP(exp1, exp2), env) = let
                                            val (res1, env1) = compileE(exp1,env)
                                            val (res2, env2) = compileE(exp2,env1)
                                        in
                                            (res2@res1@[LW(T0, 0, SP), LW(T1, 4, SP), ADDI(SP, SP, 4), SLT(T2, T0, T1), SLT(T3, T1, T0), XOR(T0, T2, T3), SW(T0, 0, SP)], env2)
                                        end
  | compileE(EQ_EXP(exp1, exp2), env) = let
                                            val (res1, env1) = compileE(exp1,env)
                                            val (res2, env2) = compileE(exp2,env1)
                                        in
                                            (res2@res1@[LW(T0, 0, SP), LW(T1, 4, SP), ADDI(SP, SP, 4), SLT(T2, T0, T1), SLT(T3, T1, T0), XOR(T0, T2, T3), XORI(T0, T0, 1), SW(T0, 0, SP)], env2)
                                        end
  | compileE(LT_EXP(exp1, exp2), env) = let
                                            val (res1, env1) = compileE(exp1,env)
                                            val (res2, env2) = compileE(exp2,env1)
                                         in
                                            (res2@res1@[LW(T0, 0, SP), LW(T1, 4, SP), ADDI(SP, SP, 4), SLT(T0, T0, T1), SW(T0, 0, SP)], env2)
                                         end
  | compileE(GT_EXP(exp1, exp2), env) = let
                                            val (res1, env1) = compileE(exp1,env)
                                            val (res2, env2) = compileE(exp2,env1)
                                         in
                                            (res2@res1@[LW(T0, 0, SP), LW(T1, 4, SP), ADDI(SP, SP, 4), SLT(T4, T0, T1), SLT(T2, T0, T1), SLT(T3, T1, T0), XOR(T0, T2, T3), XORI(T0, T0, 1), OR(T0, T0, T4), XORI(T0, T0, 1), SW(T0, 0, SP)], env2)
                                         end
  | compileE(TO_BOOL_EXP(exp), env) = compileE(NEQ_EXP(exp, INTEGER_EXP(0)), env)
  | compileE(CALL_EXP(id, args), env) =
        let 
            fun handleArgs(args, env) = 
                let 
                    fun createArgList(nil, env) = (nil, env)
                      | createArgList(h::t, env) = let
                                                       val (res1, env1) = compileE(h, env)
                                                       val (res2, env2) = createArgList(t, env1)
                                                   in
                                                       (res1@[LW(T0, 0, SP), ADDI(SP, SP, 4), ADD(A0, ZERO, T0)]@res2, env2)
                                                   end
                    fun pushArgsToStack(nil) = nil
                      | pushArgsToStack(h::t) = [ADDI(SP, SP, ~4), SW(T0, 0, SP)]
                in
                    if (List.length(args) > 4) then
                        let 
                            val (res1, env1) = createArgList(List.take(args, 4), env)
                        in
                            (res1 @ [ADDI(SP, SP, ~16)] @ pushArgsToStack(List.drop(args, 4)), env1)
                        end
                    else
                        createArgList(args, env)
                end
            val (resRes, envRes) = handleArgs(args, env)
        in
            (JAL(id)::resRes, envRes)
        end;

fun compileS(EMPTY_STATEMENT) = [NOOP]
  | compileS(WHILE_STATEMENT(expression, statement)) = [NOOP]
  | compileS(IF_STATEMENT(expression, statement1, statement2)) = [NOOP]
  | compileS(SEQUENCE_STATEMENT(statementList)) = [NOOP]
  | compileS(RETURN_STATEMENT(expression)) = [NOOP]
  | compileS(ASSIGNMENT_STATEMENT(id, expression)) = [NOOP]
  | compileS(FUNCTION(id, args, statement)) = [LABEL(id)];







(*)
main:
put on stackframe
do statement instruction here


setup stackframe
do statement
RETURN_STATEMENT*)



Control.Print.printDepth := 100000;
Control.Print.printLength := 10000;
Control.Print.stringDepth := 10000;

print (convertInstructionToString(ADD(S1, ZERO, S7)));
val test1 = FUNCTION("main", ["as"], RETURN_STATEMENT(CALL_EXP("main", [INTEGER_EXP(1)])));
compileS(test1);





val testExp = MUL_EXP(MUL_EXP(ADD_EXP(INTEGER_EXP(10), ADD_EXP(INTEGER_EXP(1), ADD_EXP(INTEGER_EXP(100), ADD_EXP(INTEGER_EXP(2), INTEGER_EXP(3))))), ADD_EXP(INTEGER_EXP(5), INTEGER_EXP(8))), INTEGER_EXP(7));

val adderExp = XOR_EXP(XOR_EXP(INTEGER_EXP(1), INTEGER_EXP(1)), INTEGER_EXP(0));

val (x,_) = compileE(adderExp, 0);
print (concat (map convertInstructionToString (x)));



