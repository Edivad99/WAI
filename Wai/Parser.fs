// Implementation file for parser generated by fsyacc
module Parser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open FSharp.Text.Lexing
open FSharp.Text.Parsing.ParseHelpers
# 1 "Parser.fsy"

open Ast

# 10 "Parser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | RCURLPAREN
  | LCURLPAREN
  | RPAREN
  | LPAREN
  | NEQ
  | GT
  | GEQ
  | EQ
  | LEQ
  | LT
  | OR
  | AND
  | NOT
  | FALSE
  | TRUE
  | DIV
  | MULT
  | MINUS
  | PLUS
  | VAR
  | WHILE
  | ELSE
  | IF
  | SKIP
  | SEMICOLON
  | EOF
  | Identifier of (string)
  | Number of (int)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_RCURLPAREN
    | TOKEN_LCURLPAREN
    | TOKEN_RPAREN
    | TOKEN_LPAREN
    | TOKEN_NEQ
    | TOKEN_GT
    | TOKEN_GEQ
    | TOKEN_EQ
    | TOKEN_LEQ
    | TOKEN_LT
    | TOKEN_OR
    | TOKEN_AND
    | TOKEN_NOT
    | TOKEN_FALSE
    | TOKEN_TRUE
    | TOKEN_DIV
    | TOKEN_MULT
    | TOKEN_MINUS
    | TOKEN_PLUS
    | TOKEN_VAR
    | TOKEN_WHILE
    | TOKEN_ELSE
    | TOKEN_IF
    | TOKEN_SKIP
    | TOKEN_SEMICOLON
    | TOKEN_EOF
    | TOKEN_Identifier
    | TOKEN_Number
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startprog
    | NONTERM_prog
    | NONTERM_seqStatement
    | NONTERM_statement
    | NONTERM_block
    | NONTERM_ifStmt
    | NONTERM_whileStmt
    | NONTERM_arithmExpr
    | NONTERM_boolExpr

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | RCURLPAREN  -> 0 
  | LCURLPAREN  -> 1 
  | RPAREN  -> 2 
  | LPAREN  -> 3 
  | NEQ  -> 4 
  | GT  -> 5 
  | GEQ  -> 6 
  | EQ  -> 7 
  | LEQ  -> 8 
  | LT  -> 9 
  | OR  -> 10 
  | AND  -> 11 
  | NOT  -> 12 
  | FALSE  -> 13 
  | TRUE  -> 14 
  | DIV  -> 15 
  | MULT  -> 16 
  | MINUS  -> 17 
  | PLUS  -> 18 
  | VAR  -> 19 
  | WHILE  -> 20 
  | ELSE  -> 21 
  | IF  -> 22 
  | SKIP  -> 23 
  | SEMICOLON  -> 24 
  | EOF  -> 25 
  | Identifier _ -> 26 
  | Number _ -> 27 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_RCURLPAREN 
  | 1 -> TOKEN_LCURLPAREN 
  | 2 -> TOKEN_RPAREN 
  | 3 -> TOKEN_LPAREN 
  | 4 -> TOKEN_NEQ 
  | 5 -> TOKEN_GT 
  | 6 -> TOKEN_GEQ 
  | 7 -> TOKEN_EQ 
  | 8 -> TOKEN_LEQ 
  | 9 -> TOKEN_LT 
  | 10 -> TOKEN_OR 
  | 11 -> TOKEN_AND 
  | 12 -> TOKEN_NOT 
  | 13 -> TOKEN_FALSE 
  | 14 -> TOKEN_TRUE 
  | 15 -> TOKEN_DIV 
  | 16 -> TOKEN_MULT 
  | 17 -> TOKEN_MINUS 
  | 18 -> TOKEN_PLUS 
  | 19 -> TOKEN_VAR 
  | 20 -> TOKEN_WHILE 
  | 21 -> TOKEN_ELSE 
  | 22 -> TOKEN_IF 
  | 23 -> TOKEN_SKIP 
  | 24 -> TOKEN_SEMICOLON 
  | 25 -> TOKEN_EOF 
  | 26 -> TOKEN_Identifier 
  | 27 -> TOKEN_Number 
  | 30 -> TOKEN_end_of_input
  | 28 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startprog 
    | 1 -> NONTERM_prog 
    | 2 -> NONTERM_seqStatement 
    | 3 -> NONTERM_seqStatement 
    | 4 -> NONTERM_statement 
    | 5 -> NONTERM_statement 
    | 6 -> NONTERM_statement 
    | 7 -> NONTERM_statement 
    | 8 -> NONTERM_statement 
    | 9 -> NONTERM_statement 
    | 10 -> NONTERM_block 
    | 11 -> NONTERM_ifStmt 
    | 12 -> NONTERM_ifStmt 
    | 13 -> NONTERM_whileStmt 
    | 14 -> NONTERM_arithmExpr 
    | 15 -> NONTERM_arithmExpr 
    | 16 -> NONTERM_arithmExpr 
    | 17 -> NONTERM_arithmExpr 
    | 18 -> NONTERM_arithmExpr 
    | 19 -> NONTERM_arithmExpr 
    | 20 -> NONTERM_arithmExpr 
    | 21 -> NONTERM_arithmExpr 
    | 22 -> NONTERM_boolExpr 
    | 23 -> NONTERM_boolExpr 
    | 24 -> NONTERM_boolExpr 
    | 25 -> NONTERM_boolExpr 
    | 26 -> NONTERM_boolExpr 
    | 27 -> NONTERM_boolExpr 
    | 28 -> NONTERM_boolExpr 
    | 29 -> NONTERM_boolExpr 
    | 30 -> NONTERM_boolExpr 
    | 31 -> NONTERM_boolExpr 
    | 32 -> NONTERM_boolExpr 
    | 33 -> NONTERM_boolExpr 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 30 
let _fsyacc_tagOfErrorTerminal = 28

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | RCURLPAREN  -> "RCURLPAREN" 
  | LCURLPAREN  -> "LCURLPAREN" 
  | RPAREN  -> "RPAREN" 
  | LPAREN  -> "LPAREN" 
  | NEQ  -> "NEQ" 
  | GT  -> "GT" 
  | GEQ  -> "GEQ" 
  | EQ  -> "EQ" 
  | LEQ  -> "LEQ" 
  | LT  -> "LT" 
  | OR  -> "OR" 
  | AND  -> "AND" 
  | NOT  -> "NOT" 
  | FALSE  -> "FALSE" 
  | TRUE  -> "TRUE" 
  | DIV  -> "DIV" 
  | MULT  -> "MULT" 
  | MINUS  -> "MINUS" 
  | PLUS  -> "PLUS" 
  | VAR  -> "VAR" 
  | WHILE  -> "WHILE" 
  | ELSE  -> "ELSE" 
  | IF  -> "IF" 
  | SKIP  -> "SKIP" 
  | SEMICOLON  -> "SEMICOLON" 
  | EOF  -> "EOF" 
  | Identifier _ -> "Identifier" 
  | Number _ -> "Number" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | RCURLPAREN  -> (null : System.Object) 
  | LCURLPAREN  -> (null : System.Object) 
  | RPAREN  -> (null : System.Object) 
  | LPAREN  -> (null : System.Object) 
  | NEQ  -> (null : System.Object) 
  | GT  -> (null : System.Object) 
  | GEQ  -> (null : System.Object) 
  | EQ  -> (null : System.Object) 
  | LEQ  -> (null : System.Object) 
  | LT  -> (null : System.Object) 
  | OR  -> (null : System.Object) 
  | AND  -> (null : System.Object) 
  | NOT  -> (null : System.Object) 
  | FALSE  -> (null : System.Object) 
  | TRUE  -> (null : System.Object) 
  | DIV  -> (null : System.Object) 
  | MULT  -> (null : System.Object) 
  | MINUS  -> (null : System.Object) 
  | PLUS  -> (null : System.Object) 
  | VAR  -> (null : System.Object) 
  | WHILE  -> (null : System.Object) 
  | ELSE  -> (null : System.Object) 
  | IF  -> (null : System.Object) 
  | SKIP  -> (null : System.Object) 
  | SEMICOLON  -> (null : System.Object) 
  | EOF  -> (null : System.Object) 
  | Identifier _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | Number _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us;65535us;1us;65535us;0us;1us;2us;65535us;0us;2us;21us;5us;4us;65535us;0us;4us;2us;6us;5us;6us;21us;4us;7us;65535us;0us;20us;2us;20us;5us;20us;21us;20us;26us;27us;28us;29us;33us;34us;4us;65535us;0us;18us;2us;18us;5us;18us;21us;18us;4us;65535us;0us;19us;2us;19us;5us;19us;21us;19us;20us;65535us;9us;10us;13us;14us;24us;45us;31us;45us;36us;37us;52us;39us;53us;40us;54us;41us;55us;42us;56us;43us;57us;44us;61us;45us;66us;45us;67us;45us;68us;46us;69us;47us;70us;48us;71us;49us;72us;50us;73us;51us;6us;65535us;24us;25us;31us;32us;57us;65us;61us;62us;66us;63us;67us;64us;|]
let _fsyacc_sparseGotoTableRowOffsets = [|0us;1us;3us;6us;11us;19us;24us;29us;50us;|]
let _fsyacc_stateToProdIdxsTableElements = [| 1us;0us;1us;0us;2us;1us;3us;1us;1us;1us;2us;2us;3us;10us;1us;3us;1us;4us;1us;4us;1us;4us;5us;4us;17us;18us;19us;20us;1us;4us;1us;5us;1us;5us;5us;5us;17us;18us;19us;20us;1us;5us;1us;6us;1us;6us;1us;7us;1us;8us;1us;9us;1us;10us;1us;10us;2us;11us;12us;2us;11us;12us;4us;11us;12us;25us;26us;2us;11us;12us;2us;11us;12us;1us;11us;1us;11us;1us;13us;1us;13us;3us;13us;25us;26us;1us;13us;1us;13us;1us;14us;1us;15us;5us;15us;17us;18us;19us;20us;1us;16us;5us;17us;17us;18us;19us;20us;5us;17us;18us;18us;19us;20us;5us;17us;18us;19us;19us;20us;5us;17us;18us;19us;20us;20us;5us;17us;18us;19us;20us;21us;11us;17us;18us;19us;20us;21us;27us;28us;29us;30us;31us;32us;10us;17us;18us;19us;20us;27us;28us;29us;30us;31us;32us;5us;17us;18us;19us;20us;27us;5us;17us;18us;19us;20us;28us;5us;17us;18us;19us;20us;29us;5us;17us;18us;19us;20us;30us;5us;17us;18us;19us;20us;31us;5us;17us;18us;19us;20us;32us;1us;17us;1us;18us;1us;19us;1us;20us;1us;21us;2us;21us;33us;1us;21us;1us;22us;1us;23us;1us;24us;3us;24us;25us;26us;3us;25us;25us;26us;3us;25us;26us;26us;3us;25us;26us;33us;1us;25us;1us;26us;1us;27us;1us;28us;1us;29us;1us;30us;1us;31us;1us;32us;1us;33us;|]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us;2us;4us;7us;9us;11us;14us;16us;18us;20us;22us;28us;30us;32us;34us;40us;42us;44us;46us;48us;50us;52us;54us;56us;59us;62us;67us;70us;73us;75us;77us;79us;81us;85us;87us;89us;91us;93us;99us;101us;107us;113us;119us;125us;131us;143us;154us;160us;166us;172us;178us;184us;190us;192us;194us;196us;198us;200us;203us;205us;207us;209us;211us;215us;219us;223us;227us;229us;231us;233us;235us;237us;239us;241us;243us;|]
let _fsyacc_action_rows = 75
let _fsyacc_actionTableElements = [|6us;32768us;1us;21us;19us;7us;20us;30us;22us;23us;23us;16us;26us;12us;0us;49152us;7us;32768us;1us;21us;19us;7us;20us;30us;22us;23us;23us;16us;25us;3us;26us;12us;0us;16385us;0us;16386us;7us;32768us;0us;22us;1us;21us;19us;7us;20us;30us;22us;23us;23us;16us;26us;12us;0us;16387us;1us;32768us;26us;8us;1us;32768us;7us;9us;4us;32768us;3us;56us;17us;36us;26us;38us;27us;35us;5us;32768us;15us;55us;16us;54us;17us;53us;18us;52us;24us;11us;0us;16388us;1us;32768us;7us;13us;4us;32768us;3us;56us;17us;36us;26us;38us;27us;35us;5us;32768us;15us;55us;16us;54us;17us;53us;18us;52us;24us;15us;0us;16389us;1us;32768us;24us;17us;0us;16390us;0us;16391us;0us;16392us;0us;16393us;6us;32768us;1us;21us;19us;7us;20us;30us;22us;23us;23us;16us;26us;12us;0us;16394us;1us;32768us;3us;24us;7us;32768us;3us;57us;12us;61us;13us;60us;14us;59us;17us;36us;26us;38us;27us;35us;3us;32768us;2us;26us;10us;67us;11us;66us;1us;32768us;1us;21us;1us;16396us;21us;28us;1us;32768us;1us;21us;0us;16395us;1us;32768us;3us;31us;7us;32768us;3us;57us;12us;61us;13us;60us;14us;59us;17us;36us;26us;38us;27us;35us;3us;32768us;2us;33us;10us;67us;11us;66us;1us;32768us;1us;21us;0us;16397us;0us;16398us;4us;32768us;3us;56us;17us;36us;26us;38us;27us;35us;2us;16399us;15us;55us;16us;54us;0us;16400us;2us;16401us;15us;55us;16us;54us;2us;16402us;15us;55us;16us;54us;0us;16403us;0us;16404us;5us;32768us;2us;58us;15us;55us;16us;54us;17us;53us;18us;52us;11us;32768us;2us;58us;4us;73us;5us;71us;6us;70us;7us;72us;8us;69us;9us;68us;15us;55us;16us;54us;17us;53us;18us;52us;10us;32768us;4us;73us;5us;71us;6us;70us;7us;72us;8us;69us;9us;68us;15us;55us;16us;54us;17us;53us;18us;52us;4us;16411us;15us;55us;16us;54us;17us;53us;18us;52us;4us;16412us;15us;55us;16us;54us;17us;53us;18us;52us;4us;16413us;15us;55us;16us;54us;17us;53us;18us;52us;4us;16414us;15us;55us;16us;54us;17us;53us;18us;52us;4us;16415us;15us;55us;16us;54us;17us;53us;18us;52us;4us;16416us;15us;55us;16us;54us;17us;53us;18us;52us;4us;32768us;3us;56us;17us;36us;26us;38us;27us;35us;4us;32768us;3us;56us;17us;36us;26us;38us;27us;35us;4us;32768us;3us;56us;17us;36us;26us;38us;27us;35us;4us;32768us;3us;56us;17us;36us;26us;38us;27us;35us;4us;32768us;3us;56us;17us;36us;26us;38us;27us;35us;7us;32768us;3us;57us;12us;61us;13us;60us;14us;59us;17us;36us;26us;38us;27us;35us;0us;16405us;0us;16406us;0us;16407us;7us;32768us;3us;57us;12us;61us;13us;60us;14us;59us;17us;36us;26us;38us;27us;35us;2us;16408us;10us;67us;11us;66us;0us;16409us;1us;16410us;11us;66us;3us;32768us;2us;74us;10us;67us;11us;66us;7us;32768us;3us;57us;12us;61us;13us;60us;14us;59us;17us;36us;26us;38us;27us;35us;7us;32768us;3us;57us;12us;61us;13us;60us;14us;59us;17us;36us;26us;38us;27us;35us;4us;32768us;3us;56us;17us;36us;26us;38us;27us;35us;4us;32768us;3us;56us;17us;36us;26us;38us;27us;35us;4us;32768us;3us;56us;17us;36us;26us;38us;27us;35us;4us;32768us;3us;56us;17us;36us;26us;38us;27us;35us;4us;32768us;3us;56us;17us;36us;26us;38us;27us;35us;4us;32768us;3us;56us;17us;36us;26us;38us;27us;35us;0us;16417us;|]
let _fsyacc_actionTableRowOffsets = [|0us;7us;8us;16us;17us;18us;26us;27us;29us;31us;36us;42us;43us;45us;50us;56us;57us;59us;60us;61us;62us;63us;70us;71us;73us;81us;85us;87us;89us;91us;92us;94us;102us;106us;108us;109us;110us;115us;118us;119us;122us;125us;126us;127us;133us;145us;156us;161us;166us;171us;176us;181us;186us;191us;196us;201us;206us;211us;219us;220us;221us;222us;230us;233us;234us;236us;240us;248us;256us;261us;266us;271us;276us;281us;286us;|]
let _fsyacc_reductionSymbolCounts = [|1us;2us;1us;2us;5us;4us;2us;1us;1us;1us;3us;7us;5us;5us;1us;2us;1us;3us;3us;3us;3us;3us;1us;1us;2us;3us;3us;3us;3us;3us;3us;3us;3us;3us;|]
let _fsyacc_productionToNonTerminalTable = [|0us;1us;2us;2us;3us;3us;3us;3us;3us;3us;4us;5us;5us;6us;7us;7us;7us;7us;7us;7us;7us;7us;8us;8us;8us;8us;8us;8us;8us;8us;8us;8us;8us;8us;|]
let _fsyacc_immediateActions = [|65535us;49152us;65535us;16385us;16386us;65535us;16387us;65535us;65535us;65535us;65535us;16388us;65535us;65535us;65535us;16389us;65535us;16390us;16391us;16392us;16393us;65535us;16394us;65535us;65535us;65535us;65535us;65535us;65535us;16395us;65535us;65535us;65535us;65535us;16397us;16398us;65535us;65535us;16400us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;16405us;16406us;16407us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;65535us;16417us;|]
let _fsyacc_reductions = lazy [|
# 268 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> Ast.Stm in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : 'gentype__startprog));
# 277 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_seqStatement in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 53 "Parser.fsy"
                                                                         Seq(_1) 
                   )
# 53 "Parser.fsy"
                 : Ast.Stm));
# 288 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_statement in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 56 "Parser.fsy"
                                                                         [_1] 
                   )
# 56 "Parser.fsy"
                 : 'gentype_seqStatement));
# 299 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_seqStatement in
            let _2 = parseState.GetInput(2) :?> 'gentype_statement in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 57 "Parser.fsy"
                                                                         _1 @ [_2] 
                   )
# 57 "Parser.fsy"
                 : 'gentype_seqStatement));
# 311 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> string in
            let _4 = parseState.GetInput(4) :?> 'gentype_arithmExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 60 "Parser.fsy"
                                                                         VarDec (_2, _4) 
                   )
# 60 "Parser.fsy"
                 : 'gentype_statement));
# 323 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            let _3 = parseState.GetInput(3) :?> 'gentype_arithmExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 61 "Parser.fsy"
                                                                         VarAss (_1, _3) 
                   )
# 61 "Parser.fsy"
                 : 'gentype_statement));
# 335 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 62 "Parser.fsy"
                                                                         Skip 
                   )
# 62 "Parser.fsy"
                 : 'gentype_statement));
# 345 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_ifStmt in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 63 "Parser.fsy"
                                                                         _1 
                   )
# 63 "Parser.fsy"
                 : 'gentype_statement));
# 356 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_whileStmt in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 64 "Parser.fsy"
                                                                         _1 
                   )
# 64 "Parser.fsy"
                 : 'gentype_statement));
# 367 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_block in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 65 "Parser.fsy"
                                                                         _1 
                   )
# 65 "Parser.fsy"
                 : 'gentype_statement));
# 378 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> 'gentype_seqStatement in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 67 "Parser.fsy"
                                                                         Seq(_2) 
                   )
# 67 "Parser.fsy"
                 : 'gentype_block));
# 389 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _3 = parseState.GetInput(3) :?> 'gentype_boolExpr in
            let _5 = parseState.GetInput(5) :?> 'gentype_block in
            let _7 = parseState.GetInput(7) :?> 'gentype_block in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 70 "Parser.fsy"
                                                                         IfThenElse (_3, _5, Some _7) 
                   )
# 70 "Parser.fsy"
                 : 'gentype_ifStmt));
# 402 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _3 = parseState.GetInput(3) :?> 'gentype_boolExpr in
            let _5 = parseState.GetInput(5) :?> 'gentype_block in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 71 "Parser.fsy"
                                                                         IfThenElse (_3, _5, None) 
                   )
# 71 "Parser.fsy"
                 : 'gentype_ifStmt));
# 414 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _3 = parseState.GetInput(3) :?> 'gentype_boolExpr in
            let _5 = parseState.GetInput(5) :?> 'gentype_block in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 73 "Parser.fsy"
                                                                         While (_3, _5) 
                   )
# 73 "Parser.fsy"
                 : 'gentype_whileStmt));
# 426 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> int in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 76 "Parser.fsy"
                                                                         Constant (_1) 
                   )
# 76 "Parser.fsy"
                 : 'gentype_arithmExpr));
# 437 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> 'gentype_arithmExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 77 "Parser.fsy"
                                                                         UnOp (Minus, _2) 
                   )
# 77 "Parser.fsy"
                 : 'gentype_arithmExpr));
# 448 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> string in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 78 "Parser.fsy"
                                                                         Variable (_1) 
                   )
# 78 "Parser.fsy"
                 : 'gentype_arithmExpr));
# 459 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_arithmExpr in
            let _3 = parseState.GetInput(3) :?> 'gentype_arithmExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 79 "Parser.fsy"
                                                                         BinOp (_1, Sum, _3) 
                   )
# 79 "Parser.fsy"
                 : 'gentype_arithmExpr));
# 471 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_arithmExpr in
            let _3 = parseState.GetInput(3) :?> 'gentype_arithmExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 80 "Parser.fsy"
                                                                         BinOp (_1, Subtraction, _3) 
                   )
# 80 "Parser.fsy"
                 : 'gentype_arithmExpr));
# 483 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_arithmExpr in
            let _3 = parseState.GetInput(3) :?> 'gentype_arithmExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 81 "Parser.fsy"
                                                                         BinOp (_1, Multiplication, _3) 
                   )
# 81 "Parser.fsy"
                 : 'gentype_arithmExpr));
# 495 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_arithmExpr in
            let _3 = parseState.GetInput(3) :?> 'gentype_arithmExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 82 "Parser.fsy"
                                                                         BinOp (_1, Division, _3) 
                   )
# 82 "Parser.fsy"
                 : 'gentype_arithmExpr));
# 507 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> 'gentype_arithmExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 83 "Parser.fsy"
                                                                         _2 
                   )
# 83 "Parser.fsy"
                 : 'gentype_arithmExpr));
# 518 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 86 "Parser.fsy"
                                                                         Boolean (true) 
                   )
# 86 "Parser.fsy"
                 : 'gentype_boolExpr));
# 528 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 87 "Parser.fsy"
                                                                         Boolean (false) 
                   )
# 87 "Parser.fsy"
                 : 'gentype_boolExpr));
# 538 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> 'gentype_boolExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 88 "Parser.fsy"
                                                                         UnOp (Not, _2) 
                   )
# 88 "Parser.fsy"
                 : 'gentype_boolExpr));
# 549 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_boolExpr in
            let _3 = parseState.GetInput(3) :?> 'gentype_boolExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 89 "Parser.fsy"
                                                                         BinOp (_1, And, _3) 
                   )
# 89 "Parser.fsy"
                 : 'gentype_boolExpr));
# 561 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_boolExpr in
            let _3 = parseState.GetInput(3) :?> 'gentype_boolExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 90 "Parser.fsy"
                                                                         BinOp (_1, Or, _3) 
                   )
# 90 "Parser.fsy"
                 : 'gentype_boolExpr));
# 573 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_arithmExpr in
            let _3 = parseState.GetInput(3) :?> 'gentype_arithmExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 91 "Parser.fsy"
                                                                         BinOp (_1, LessThan, _3) 
                   )
# 91 "Parser.fsy"
                 : 'gentype_boolExpr));
# 585 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_arithmExpr in
            let _3 = parseState.GetInput(3) :?> 'gentype_arithmExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 92 "Parser.fsy"
                                                                         BinOp (_1, LessEqualThan, _3) 
                   )
# 92 "Parser.fsy"
                 : 'gentype_boolExpr));
# 597 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_arithmExpr in
            let _3 = parseState.GetInput(3) :?> 'gentype_arithmExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 93 "Parser.fsy"
                                                                         BinOp (_1, GreaterEqualThan, _3) 
                   )
# 93 "Parser.fsy"
                 : 'gentype_boolExpr));
# 609 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_arithmExpr in
            let _3 = parseState.GetInput(3) :?> 'gentype_arithmExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 94 "Parser.fsy"
                                                                         BinOp (_1, GreaterThan, _3) 
                   )
# 94 "Parser.fsy"
                 : 'gentype_boolExpr));
# 621 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_arithmExpr in
            let _3 = parseState.GetInput(3) :?> 'gentype_arithmExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 95 "Parser.fsy"
                                                                         BinOp (_1, Equal, _3) 
                   )
# 95 "Parser.fsy"
                 : 'gentype_boolExpr));
# 633 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = parseState.GetInput(1) :?> 'gentype_arithmExpr in
            let _3 = parseState.GetInput(3) :?> 'gentype_arithmExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 96 "Parser.fsy"
                                                                         BinOp (_1, NotEqual, _3) 
                   )
# 96 "Parser.fsy"
                 : 'gentype_boolExpr));
# 645 "Parser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = parseState.GetInput(2) :?> 'gentype_boolExpr in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 97 "Parser.fsy"
                                                                         _2 
                   )
# 97 "Parser.fsy"
                 : 'gentype_boolExpr));
|]
# 657 "Parser.fs"
let tables : FSharp.Text.Parsing.Tables<_> = 
  { reductions = _fsyacc_reductions.Value;
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 31;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = tables.Interpret(lexer, lexbuf, startState)
let prog lexer lexbuf : Ast.Stm =
    engine lexer lexbuf 0 :?> _
