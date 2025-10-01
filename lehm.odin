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
    STRUCT, 
    ENUM,
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
    fmt.println(tokens)
    nodes := [4096]expr_t{}
    parser:= parser_t{
        tokens=tokens[:], position=0, nodes=nodes[:], next_node=0, ptable=[32]u8{}
    }
    init_precedence(&parser) 
    
    assert(parser.ptable[binop_t.MULT] == 50)

    stmt : stmt_t;
    for stmt := parse_stmt(&parser); stmt != nil; stmt = parse_stmt(&parser){
        fmt.println(stmt)
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
            "enum"= ENUM,
        }
    } 

    c : u8;

    for lexer.position < lexer.len {
        using simple_token_t;
        c = consume(&lexer)
        
        lexer.begin_i = lexer.position-1;

        switch c {

            case '&', '#', '.', '~', '|','%','+','/','\\','*','-','{','}','[',']','(',')',':',';',',','^': {
                t := can_double(c)
                if t != nil && match_consume(&lexer, c){
                    append(&tokens, t)
                }else{
                    append(&tokens, map_char(c))
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

expr_t :: union {
    ifexpr_t, 
    binexpr_t,
    unexpr_t,
    int_literal_t,
    char_literal_t,
    float_literal_t,
    string_literal_t,
    identifier_t,
}

stmt_t :: union {
    cdecl_t,
    vdecl_t,
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


parse_stmt :: proc(parser: ^parser_t) -> stmt_t {
    varname: identifier_t;
    typename: identifier_t;

    #partial switch token in parser.tokens[parser.position] {
        case identifier_t: {
            varname = token
            if peek_token(parser, simple_token_t.COLON, 1){
                parser.position+=2;
                if consume_token(parser, simple_token_t.COLON){
                    // ConstDecl
                    decl : cdecl_t;
                    #partial switch expr in parser.tokens[parser.position]{
	                  case  int_literal_t:    
                        decl =  cdecl_t {
                            name=varname, value=expr
                        }
                      case  string_literal_t:
                        decl = cdecl_t {
                            name=varname, value=expr
                        }
                        case: panic("unexpected constant")
                    }
                    parser.position+=1;
                    if !consume_token(parser, simple_token_t.SEMI_COLON){
                        panic("malformed const declaration")
                    }
                    return decl
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

parse_unexpr :: proc(parser: ^parser_t) -> expr_t{
    primexpr := parse_prim(parser)
    return primexpr
}

parse_prim :: proc(parser: ^parser_t) -> expr_t{
    #partial switch token in parser.tokens[parser.position]{
        case int_literal_t:
            advance(parser)
            return token
        case string_literal_t:
            advance(parser)
            return token
        case char_literal_t:
            advance(parser)
            return token
    }

    return nil
}