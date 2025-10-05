#+feature dynamic-literals

package lehm
import "core:fmt"
import "core:os"

simple_token_t :: enum {
    NONE = 0,

    BUILTIN,

    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACKET,
    RIGHT_BRACKET,
    LEFT_CURLY,
    RIGHT_CURLY,

    EQ, // =
    EQ_EQ, // ==
    COLON,      // :
    
    DOT, // .
    COMMA,      // ,
    SEMI_COLON, // ;
    HASH,         // "#"
    
    STAR,       // *
    PLUS,       // +
    PLUS_PLUS, // ++ 
    MINUS,      // -
    MINUS_MINUS, // --
    DIV_FSLASH, // /
    BACK_SLASH, // \
    PERCENT,    // %

    EQ_ARROW,     // =>
    MINUS_ARROW, // ->

    AMP, // &
    AMP_AMP,   // &&
    PIPE,      // |
    PIPE_PIPE, // ||
    CARET,     // ^
    TILDE,     // ~

    LESS,      // <
    LESS_LESS,  // << 
    LESS_EQ,   // <=
    GREAT,     // >
    GREAT_GREAT, // >> 
    GREAT_EQ,  // >=

    BANG,      // !
    BANG_EQ,   // !=

    EOF
}

builtin_t :: enum {
    NONE = 0, 

    IF,
    WHILE,
    FOR,
    FN,
    STRUCT, 
    ENUM,
    RETURN,
}

identifier_t :: distinct string

char_literal_t :: distinct u8 
int_literal_t :: distinct uint 
float_literal_t :: distinct f64
string_literal_t :: distinct string 

token_t :: union{
    simple_token_t, 
    builtin_t,

    identifier_t,

    char_literal_t, 
    int_literal_t, 
    float_literal_t,
    string_literal_t
}

lexer_t :: struct{
    bytes: []u8,
    position: uint,
    begin_i: uint,
    len: uint 
}

main :: proc(){
    using binop_t

    test := "0b1111_1111"
    assert(parse_binary_literal(transmute([]u8)test) == 255)
    test2 := "0b111"
    assert(parse_binary_literal(transmute([]u8)test2) == 7)

    handle, err := os.open("./tests/one.lh")
    if err != nil{
        panic("cannot find file"); 
    }
    bytes: [4096 * 4]u8 = {};
    n, _ := os.read_full(handle, bytes[:]);

    tokens := tokenize(bytes[:])
    
    nodes := make([]expr_t, 4096)

    parser:= parser_t{
        tokens=tokens[:], position=0, nodes=nodes, next_node=0, ptable=[32]u8{}
    }
    init_precedence(&parser) 
    
    assert(parser.ptable[binop_t.MULT] == 50)

    stmt : stmt_t;
    for stmt := parse_stmt(&parser); stmt != nil; stmt = parse_stmt(&parser){
        buf := naive_ir(stmt)
        str := format_ir_buffer(buf)
        fmt.println(str)
    }

    assert(consume_token(&parser, simple_token_t.EOF))
}

init_precedence :: proc(parser: ^parser_t){
    using binop_t
    t := parser.ptable[:]
    t[MULT] = 50
    t[DIV]= 50
    t[REM] = 50

    t[ADD] = 45
    t[SUB] = 45

    t[LT] = 35
    t[LTE]= 35
    t[GT]= 35
    t[GTE]= 35

    t[EQ] = 30
    t[NEQ]= 30

    t[LAND]=10
    
    t[LOR] = 5

}

map_char :: proc (c: u8) -> simple_token_t {
    using simple_token_t;
    switch c {
        case '~' : return TILDE
        case '=' : return EQ
        case '|' : return PIPE
        case '%' : return PERCENT
        case '+' : return PLUS
        case '/' : return DIV_FSLASH
        case '\\' :return  BACK_SLASH
        case '*' :return STAR
        case '-' :return MINUS
        case '{' :return LEFT_CURLY
        case '}' :return RIGHT_CURLY
        case '[' :return LEFT_BRACKET
        case ']' :return RIGHT_BRACKET
        case '(' :return LEFT_PAREN
        case ')' :return RIGHT_PAREN
        case '!' :return BANG
        case '<' :return LESS
        case '>' :return GREAT
        case ':' :return COLON
        case ';' :return SEMI_COLON
        case ',' :return COMMA
        case '^' :return CARET
        case '.': return DOT
        case '#': return HASH
        
        case:
            panic("cannot parse char as token")
    };
}

can_double :: proc(c: u8) -> simple_token_t{
    using simple_token_t;

    switch c {
        case '=': return EQ_EQ;
        case '|': return PIPE_PIPE;
        case '+': return PLUS_PLUS;
        case '-': return MINUS_MINUS;
        case '<': return LESS_LESS;
        case '>': return GREAT_GREAT;
        case '&': return AMP_AMP;
        case: return nil
    };
}

// TODO(yousef): where do you want to do bounds-checking?

consume :: proc(lexer: ^lexer_t) -> u8 {
    pos := lexer.position 
    lexer.position += 1;
    return lexer.bytes[pos]
}

match_consume_identifier_CHARorDIGIT :: proc(lexer: ^lexer_t) -> bool{
    pos := lexer.position 
    switch lexer.bytes[pos]{
        case '0'..='9', 'a'..='z', 'A'..='Z', '_':
            lexer.position+=1
            return true
        case:
            return false
    }
}

match_consume :: proc(lexer : ^lexer_t, c: u8) -> bool {
    pos := lexer.position 
    if lexer.bytes[pos] == c{
        lexer.position+=1; 
        return true;
    }
    return false;
}

match_consume_either :: proc(lexer: ^lexer_t, c1: u8, c2: u8) -> bool {
    pos := lexer.position 
    switch lexer.bytes[pos]{
        case c1, c2: {
            lexer.position+=1; 
            return true
        }
        case: return false
    }
}

match_consume_digit :: proc(lexer: ^lexer_t) -> bool {
    pos := lexer.position 
    switch lexer.bytes[pos]{
        case '0'..='9': {
            lexer.position+=1; 
            return true
        } 
        case: return false
    }
}

match_consume_hex_dig :: proc(lexer: ^lexer_t) -> bool{
    pos := lexer.position 
    switch lexer.bytes[pos]{
        case '0'..='9', 'a'..='f', 'A'..='F': {
            lexer.position+=1; 
            return true
        } 
        case: return false
    }
}

parse_binary_literal :: proc(str: []u8) -> uint{
    res: uint = 0;
    exp: uint = 0; 
    i: int = len(str) -1
    
    // skip "0b":
    for i>=2 {
        c:= str[i] 
        switch c {
            case '0', '1':
                res |= uint(c - '0') << exp 
                exp+=1;
            case '_':

        }
        i-=1;
    } 
    return res; 
}

parse_hex_literal :: proc(str : []u8) -> uint {
    res: uint = 0;
    exp: uint = 0; 
    i: int = len(str) -1

     // skip "0x":
    for i>=2 {
        c := str[i] 
        switch c {
           case '0'..='9':
                res |= uint(c - '0') << (exp*4)
                exp+=1
           case 'a'..='f':
                res |= (uint(c-'a') + 10) << (exp*4)
                exp+=1
           case 'A'..='F':
                res |= (uint(c-'A') + 10 )<< (exp*4)
                exp+=1
            case '_':
        }
        i-=1;
    } 
    return res; 
}

parse_decimal_literal :: proc(str: []u8)->uint{
    res: uint = 0;
    factor: uint = 1; 
    i: int = len(str) -1

    for i>=0 {
        c := str[i] 
        switch c {
           case '0'..='9':
                res += uint(c - '0') * factor;
                factor*=10

           case '_':
        }
        i-=1;
    } 
    return res; 
}

tokenize :: proc(buf: []u8) -> [dynamic]token_t {
    tokens := make([dynamic]token_t);
    
    lexer: lexer_t = {position=0, begin_i= 0, bytes=buf, len = len(buf)};
    

    builtins: map[string] builtin_t;
    {
        using builtin_t
        builtins = map[string] builtin_t{
            "for" = FOR,
            "while" = WHILE, 
            "if"= IF,
            "struct" = STRUCT, 
            "fn" = FN,
            "enum"= ENUM,
            "return" = RETURN
        }
    } 

    c : u8;

    for lexer.position < lexer.len {
        using simple_token_t;
        c = consume(&lexer)
        
        lexer.begin_i = lexer.position-1;

        switch c {

            case '&', '#', '.', '~', '|','%','+','/','\\','*','{','}','[',']','(',')',':',';',',','^': {
                t := can_double(c)
                if t != nil && match_consume(&lexer, c){
                    append(&tokens, t)
                }else{
                    append(&tokens, map_char(c))
                }
            }

            case '-':{
                if match_consume(&lexer, '-'){
                    append(&tokens, simple_token_t.MINUS_MINUS)
                } else if match_consume(&lexer, '>'){
                    append(&tokens, simple_token_t.MINUS_ARROW)
                }else{
                    append(&tokens, simple_token_t.MINUS)
                }
            }
            case '=': {
                if match_consume(&lexer, '='){
                    append(&tokens, EQ_EQ)
                }else if match_consume(&lexer, '>'){
                    append(&tokens, EQ_ARROW)
                }else{
                    append(&tokens, EQ)
                }
            }
            case '<': {  
                if match_consume(&lexer, '='){
                    append(&tokens, LESS_EQ)
                }else if match_consume(&lexer, '<'){
                    append(&tokens, LESS_LESS)
                }else{
                    append(&tokens, LESS)
                }
            }
            case '>':{
                if match_consume(&lexer, '='){
                    append(&tokens, GREAT_EQ)
                }else if match_consume(&lexer, '<'){
                    append(&tokens, GREAT_GREAT)
                }else{
                    append(&tokens, GREAT)
                }
            }

            case '!':{
                if match_consume(&lexer, '='){
                    append(&tokens, BANG_EQ)
                }else{
                    append(&tokens, BANG)
                }
            }

            case 'a'..='z',  'A'..='Z': {
                for match_consume_identifier_CHARorDIGIT(&lexer){} 

                start_i := lexer.begin_i;
                end_i_excl := lexer.position;
                str := string(lexer.bytes[start_i:end_i_excl])
                reserved := builtins[str]
                if reserved != nil {
                    append(&tokens, reserved)
                }else{
                    ident := identifier_t(str);
                    append(&tokens, ident);
                }
            }
            
            case '0': {
                if match_consume(&lexer, 'b'){
                    for match_consume_either(&lexer, '1', '0') || match_consume(&lexer, '_'){}
                    
                    start_i := lexer.begin_i;
                    end_i_excl := lexer.position;
                    
                    value := int_literal_t(parse_binary_literal(lexer.bytes[start_i:end_i_excl]))
                    append(&tokens, value);
                } else if match_consume(&lexer, 'x'){
                    for match_consume_hex_dig(&lexer) || match_consume(&lexer, '_'){}
                    
                    start_i := lexer.begin_i;
                    end_i_excl := lexer.position;
                    
                    value := int_literal_t(parse_hex_literal(lexer.bytes[start_i:end_i_excl]))
                    append(&tokens, value);
                }else{
                    // ignore floats for now

                    // I allow a leading zero (because I can)
                    for match_consume_digit(&lexer) || match_consume(&lexer, '_'){}

                    start_i := lexer.begin_i;
                    end_i_excl := lexer.position;
                    
                    value := int_literal_t(parse_decimal_literal(lexer.bytes[start_i:end_i_excl]))
                    append(&tokens, value);
                }
            }

            case '1'..='9': {
                // ignore floats for now
                for match_consume_digit(&lexer) || match_consume(&lexer, '_'){}

                start_i := lexer.begin_i;
                end_i_excl := lexer.position;
                
                value := int_literal_t(parse_decimal_literal(lexer.bytes[start_i:end_i_excl]))
                append(&tokens, value);
            }

            case '"': {
                d :u8;
                loop: for {
                    d = consume(&lexer)
                    switch d {
                        case '"':
                            break loop
                        case '!'..='~':
                        // do whatever now
                        case: break loop;
                    }
                }

                if d != '"'{
                    fmt.printf("%c", d)
                    panic("malformed string literal")
                }

                start_i := lexer.begin_i;
                end_i_excl := lexer.position;
                
                str_lit := string_literal_t(lexer.bytes[start_i+1:end_i_excl-1])

                append(&tokens, str_lit)
            }

            case ' ' , '\n' , '\r' , '\t' : {}
        }
    }

    append(&tokens, simple_token_t.EOF)
    return tokens
}

parser_t :: struct {
    tokens: []token_t,
    position: uint,
    nodes: []expr_t,
    next_node: uint,
    ptable: [32]u8
}

consume_token :: proc(parser: ^parser_t, t: token_t) -> bool{
    pos := parser.position
    if parser.tokens[pos] == t {
        parser.position+=1
        return true
    }
    return false
}

peek_token :: proc(parser: ^parser_t, t: token_t, amt: uint) -> bool{
    return parser.tokens[parser.position + amt] == t 
}

consume_identifier :: proc(parser: ^parser_t, ident: ^identifier_t) -> bool{
    pos := parser.position
    #partial switch token in parser.tokens[pos]  {
        case identifier_t:
            parser.position+=1
            ident^ = token
            return true 
        case: 
            return false
    }
    
}

peek_identifier :: proc(parser: ^parser_t, ident: ^identifier_t) -> bool{
    pos := parser.position
    #partial switch token in parser.tokens[pos]  {
        case identifier_t:
            ident^ = token
            return true 
        case: 
            return false
    }
}

expr_t :: union {
    ifexpr_t,
    assign_t,
    binexpr_t,
    unexpr_t,
    int_literal_t,
    char_literal_t,
    float_literal_t,
    string_literal_t,
    identifier_t,
    struct_info_t,
    func_info_t,
    proto_t,
    enum_info_t
}

assign_t :: struct{
    varname: identifier_t,
    lexpr: ^expr_t
}

struct_info_t :: struct{
    // layout (alignment, ...) later
    self: map[identifier_t]identifier_t
}

proto_t :: struct{
    args: map[identifier_t]identifier_t,
    ret_type: identifier_t
}

func_info_t :: struct{
    proto: proto_t,
    body: []stmt_t
}

enum_info_t :: struct{

}

stmt_t :: union {
    cdecl_t,
    vdecl_t,
    return_t,
    exprstmt_t
}

cdecl_t :: struct{
    name: identifier_t,
    value: expr_t
}

vdecl_t :: struct{
    name: identifier_t,
    value: expr_t
}

return_t :: struct{
    inner: ^expr_t
}

exprstmt_t :: struct {inner: expr_t}

ifexpr_t :: struct {
    cond: ^expr_t,
    then: ^expr_t,
    otherwise: ^expr_t
}

binexpr_t :: struct{
    op: binop_t,
    left: ^expr_t,
    right: ^expr_t
}

unexpr_t :: struct{
    op: unop_t ,
    inner: ^expr_t
}

unop_t :: enum{
    NONE = 0,
    NOT, COMPLEMENT
}

precedence :: [32]uint{}

binop_t :: enum{
    NONE = 0,
    LT, LTE, GT, GTE, NEQ, EQ, LAND, LOR,
    
    LSHIFT, RSHIFT, BAND, BOR,
    ADD, SUB, MULT, DIV, REM
}

ir_var_t :: struct {name: u32, ver: u32};

ir_value_t :: union{ir_var_t, int_literal_t}

ir_binary_t :: struct {op: binop_t, left: ir_value_t, right: ir_value_t, dest: ir_var_t}

ir_unary_t :: struct{op: unop_t, src: ir_value_t, dest: ir_var_t}

ir_emit_t :: struct{value: ir_value_t}

ir_instruction_t :: union{
    ir_unary_t,
    ir_binary_t,
    ir_emit_t
}

parse_stmt :: proc(parser: ^parser_t) -> stmt_t {
    varname: identifier_t;
    typename: identifier_t;

    current := parser.tokens[parser.position]

    #partial switch token in current {
        case identifier_t: {
            varname = token
            if peek_token(parser, simple_token_t.COLON, 1){
                parser.position+=2;
                if consume_token(parser, simple_token_t.COLON){
                    // ConstDecl
                    value := parse_expr(parser)
                    
                    if !consume_token(parser, simple_token_t.SEMI_COLON){
                        panic("malformed const declaration")
                    }

                    return cdecl_t{
                        varname, value
                    }
                }else if consume_token(parser, simple_token_t.EQ){
                    // VarDecl
                    value := parse_expr(parser)
                    
                    if !consume_token(parser, simple_token_t.SEMI_COLON){
                        panic("malformed var declaration")
                    }
                    
                    return vdecl_t{
                        name=varname, value=value
                    }

                }else if consume_identifier(parser, &typename){
                    if consume_token(parser, simple_token_t.EQ){
                        // VarDecl
                    }
                }

                
            }
        }

        case builtin_t:{
            if current==builtin_t.RETURN{
                advance(parser)
                inner :=boxed_node(parser, parse_expr(parser))
                if consume_token(parser, simple_token_t.SEMI_COLON){
                    return return_t{
                        inner
                    }
                }else{
                    panic("malformed return stmt.")
                }
            }
        }
    }

    expr := parse_expr(parser)
    if  expr != nil && consume_token(parser, simple_token_t.SEMI_COLON){
        return exprstmt_t {expr}
    }
    
    return nil
}


boxed_node :: proc(parser: ^parser_t, node: expr_t) -> ^expr_t{
    at :=parser.next_node 
    parser.nodes[at] = node
    parser.next_node+=1;

    return &parser.nodes[at]
}

parse_expr :: proc(parser: ^parser_t) -> expr_t{
    some_ident: identifier_t;
    if peek_identifier(parser, &some_ident){
        if peek_token(parser, simple_token_t.EQ, 1){
            parser.position+=2;
            lexpr := boxed_node(parser, parse_expr(parser))
            if !consume_token(parser, simple_token_t.SEMI_COLON){
                panic("malformed assignment")
            }
            return assign_t{
                some_ident, lexpr
            }
        }
    }

    logexpr := parse_logexpr(parser, DEF_MINPREC);
    if consume_token(parser, builtin_t.IF){
        then := parse_expr(parser)
        if then == nil {panic("malformed if-expression")}
        if consume_token(parser, simple_token_t.COMMA){   
            otherwise := parse_expr(parser)
            return ifexpr_t{
                cond=boxed_node(parser, logexpr),
                then=boxed_node(parser, then),
                otherwise=boxed_node(parser, otherwise)
            }
        }
        panic("malformed if-expression")
    }
    

    
    return logexpr
}

peek_connective :: proc(parser:^parser_t) -> binop_t {
    using binop_t
    
    switch parser.tokens[parser.position]{
        case simple_token_t.LESS:
            return LT 
        case simple_token_t.LESS_EQ:
            return LTE
        case simple_token_t.GREAT:
            return GT 
        case simple_token_t.GREAT_EQ:
            return GTE 
        case simple_token_t.EQ_EQ:
            return EQ 
        case simple_token_t.BANG_EQ:
            return NEQ
        case simple_token_t.AMP_AMP:
            return LAND 
        case simple_token_t.PIPE_PIPE:
            return LOR
    }

    return binop_t.NONE
}

DEF_MINPREC :u8 : 0

advance :: proc(parser: ^parser_t){
    parser.position+=1;
}

parse_logexpr :: proc(parser: ^parser_t, min_prec: u8) -> expr_t{
    left := parse_binexpr(parser, DEF_MINPREC);
    if left == nil{return nil}
    binop := peek_connective(parser)
    for binop != binop_t.NONE{
        prec :=  parser.ptable[binop]
        
        if prec >= min_prec{
            advance(parser)

            right := parse_logexpr(parser, prec+1)

            left = binexpr_t{
                op=binop, left=boxed_node(parser, left), right=boxed_node(parser, right)
            }
            binop=peek_connective(parser)
        }else{
            break;
        }
    }
    
    return left
}

peek_arithop :: proc(parser: ^parser_t) -> binop_t{
    using binop_t
    
    switch parser.tokens[parser.position]{
        case simple_token_t.PLUS:
            return ADD 
        case simple_token_t.MINUS:
            return SUB 
        case simple_token_t.DIV_FSLASH:
            return DIV 
        case simple_token_t.STAR:
            return MULT 
        case simple_token_t.PERCENT:
            return REM
    }

    return binop_t.NONE
}

parse_binexpr :: proc(parser: ^parser_t, min_prec: u8) -> expr_t{
    left := parse_unexpr(parser); 
    if left == nil{return nil}

    binop := peek_arithop(parser)
    for binop != binop_t.NONE{
        prec :=  parser.ptable[binop]
        
        if prec >= min_prec{
            advance(parser)

            right := parse_binexpr(parser, prec+1)

            left = binexpr_t{
                op=binop, left=boxed_node(parser, left), right=boxed_node(parser, right)
            }
            binop=peek_arithop(parser)
        }else{
            break;
        }
    }
    
    return left
    
}

peek_unop :: proc(parser: ^parser_t)->unop_t{
    using unop_t 

     switch parser.tokens[parser.position]{
        case simple_token_t.TILDE:
            return COMPLEMENT
        case simple_token_t.BANG:
            return NOT
    }

    return unop_t.NONE
}

parse_unexpr :: proc(parser: ^parser_t) -> expr_t{
    unop := peek_unop(parser)
    if unop != unop_t.NONE{
        advance(parser)
        inner := parse_unexpr(parser)
        return unexpr_t{
            op=unop, inner=boxed_node(parser, inner)
        }
    }else{
        inner := parse_prim(parser)
        return inner
    }
    
}

parse_prim :: proc(parser: ^parser_t) -> expr_t{
    // TODO(yousef): identifiers and parentheses
    next := parser.tokens[parser.position]
    #partial switch token in next{
        case int_literal_t:
            advance(parser)
            return token
        case string_literal_t:
            advance(parser)
            return token
        case char_literal_t:
            advance(parser)
            return token
        case identifier_t:
            advance(parser)
            return token
    }
    
    switch next{
        case builtin_t.ENUM:
            panic("unimplemented!")
        
        case builtin_t.FN:
            advance(parser)
            args : map[identifier_t]identifier_t;
            if consume_token(parser, simple_token_t.LEFT_PAREN){
                key: identifier_t;
                for consume_identifier(parser, &key){
                    if consume_token(parser, simple_token_t.COLON){
                        type : identifier_t;
                        if consume_identifier(parser, &type){
                            args[key]=type
                            if consume_token(parser, simple_token_t.COMMA){
                                if peek_token(parser, simple_token_t.RIGHT_PAREN, 0){
                                    break;
                                }
                            }else{
                                if peek_token(parser, simple_token_t.RIGHT_PAREN, 0 ){
                                    break;
                                }else{
                                    panic("malformed arg list")
                                }
                            }
                        }else{
                            panic("malformed arg list")    
                        }
                    }else{
                        panic("malformed struct decl")
                    }
                }
                if !consume_token(parser, simple_token_t.RIGHT_PAREN){
                    panic("malformed arg list")
                }

                body := make_dynamic_array_len([dynamic]stmt_t, 20);
                prototype: proto_t = {args, identifier_t("void")}
                
                if consume_token(parser, simple_token_t.MINUS_ARROW){
                    ident: identifier_t;
                    if consume_identifier(parser, &ident){
                        prototype.ret_type = ident
                    }else{
                        panic("malformed function declaration")
                    }
                }
                
                if consume_token(parser, simple_token_t.LEFT_CURLY){
                    s: stmt_t = parse_stmt(parser)
                    for {
                        if s == nil{
                            if consume_token(parser, simple_token_t.RIGHT_CURLY){
                                break;
                            }else{
                                panic("malformed function declaration")
                            }
                            
                        }else{
                            append(&body, s)
                            s= parse_stmt(parser)
                        }
                    }

                    return func_info_t{
                        prototype, body[:]
                    }
                }else{
                    return prototype;
                }
            }
            
        case builtin_t.STRUCT:{
            advance(parser)
            info: map[identifier_t]identifier_t
            if consume_token(parser, simple_token_t.LEFT_CURLY){
                key: identifier_t;
                for consume_identifier(parser, &key){
                    if consume_token(parser, simple_token_t.COLON){
                        type : identifier_t;
                        if consume_identifier(parser, &type){
                            info[key]=type
                            if consume_token(parser, simple_token_t.COMMA){
                                if peek_token(parser, simple_token_t.RIGHT_CURLY, 0){
                                    break;
                                }
                            }else{
                                if peek_token(parser, simple_token_t.RIGHT_CURLY, 0 ){
                                    break;
                                }else{
                                    panic("malformed struct decl")
                                }
                            }
                        }else{
                            panic("malformed struct decl")    
                        }
                    }else{
                        panic("malformed struct decl")
                    }
                }
                if consume_token(parser, simple_token_t.RIGHT_CURLY){
                    return struct_info_t{info}
                }
            }
            panic("malformed struct decl")    
        }
    }
    if consume_token(parser, simple_token_t.LEFT_PAREN){
        inner := parse_expr(parser)
        if consume_token(parser, simple_token_t.RIGHT_PAREN){
            if inner == nil{
                panic("malformed expression (paren)")
            }
            return inner
        }
    }

    return nil
}

naive_ir :: proc(stmt: stmt_t) -> []ir_instruction_t {
    instructions := make_dynamic_array([dynamic]ir_instruction_t);


    transform_stmt(stmt, &instructions)
    

    return instructions[:len(instructions)];
}

transform_stmt :: proc(stmt: stmt_t, ir_buf: ^[dynamic]ir_instruction_t) {
    #partial switch type in stmt{
        case return_t: {
            counter : u32 = 0; 
            val := transform_expr(stmt.(return_t).inner, &counter, ir_buf)
            append(ir_buf, ir_emit_t {value=val })
        }
        case: panic("unimplemented!")
    }
}

transform_expr :: proc(expr: ^expr_t, counter: ^u32, ir_buf: ^[dynamic]ir_instruction_t)-> ir_value_t {
    #partial switch type in expr{
        case int_literal_t: {
            return expr.(int_literal_t)
        }

        case binexpr_t:{
            this :=expr.(binexpr_t); 
            temp := counter^
            counter^+=1
            src_left := transform_expr(this.left, counter, ir_buf);
            dest := ir_var_t{name=temp, ver=0}

            src_right := transform_expr(this.right, counter, ir_buf);
            instruction : ir_instruction_t = ir_binary_t{
                op=this.op,
                left=src_left,
                right=src_right,
                dest=dest
            };

            append(ir_buf, instruction)
            return dest
        }

        case: panic("unimplemented!")
    }
}

import "core:strings"

format_value :: proc(sbuilder: ^strings.Builder, val: ir_value_t){
    switch type in val{
        case ir_var_t:{           
            fmt.sbprintf(sbuilder, "tmp%d.%d", val.(ir_var_t).name, val.(ir_var_t).ver)    
        }
    
        case int_literal_t:{
            fmt.sbprintf(sbuilder, "CONST(%d)", val.(int_literal_t))
        }
    }
}

// for debugging (whatever)
format_ir_buffer :: proc(ir_buf: []ir_instruction_t) -> string{
    string_builder := strings.builder_make()
    for instruction in ir_buf {
        switch type in instruction{
            case ir_emit_t:{
                fmt.sbprint(&string_builder, "EMIT ")
                format_value(&string_builder, instruction.(ir_emit_t).value);
                fmt.sbprint(&string_builder, "\n")
            }
            case ir_binary_t:{
                bin := instruction.(ir_binary_t)
                format_value(&string_builder, bin.dest)
                fmt.sbprint(&string_builder, " = ")                
                fmt.sbprint(&string_builder, bin.op)
                fmt.sbprint(&string_builder, " ")
                format_value(&string_builder, bin.left)
                fmt.sbprint(&string_builder, ", ")
                format_value(&string_builder, bin.right)
                
                fmt.sbprint(&string_builder, "\n")
            }

            case ir_unary_t:{
                panic("unimplemented");
            }
        }
    }

    return strings.to_string(string_builder)
}