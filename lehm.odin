#+feature dynamic-literals
#+vet unused

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
    ELSE,
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

    fmt.printfln("sizeof(parser_t): %d bytes", size_of(parser_t))
    fmt.printfln("sizeof(ir_state_t): %d bytes", size_of(ir_state_t))

    test := "0b1111_1111"
    assert(parse_binary_literal(transmute([]u8)test) == 255)
    test2 := "0b111"
    assert(parse_binary_literal(transmute([]u8)test2) == 7)

    handle, err := os.open("tests/two.lh")
    if err != nil{
        fmt.eprintln(err);
        panic("cannot find file"); 
    }
    bytes: [4096 * 4]u8 = {};
    os.read_full(handle, bytes[:]);

    tokens := tokenize(bytes[:])
    
    nodes := make([]expr_t, 4096)
    tracker := [0xFF]uint{};

    parser:= parser_t{
        tokens=tokens[:], position=0, nodes=nodes, next_node=0, ptable=[32]u8{},
        scope=0, global_symbols=make(map[identifier_t]bool), 
        locals_stack=new([4096]^local_t)[:], next_local=0, scope_ptrs=tracker[:], 
        in_proc=false, scope_ident_tracker=map[identifier_t]^scope_t{}, maybe_globals=make([dynamic]identifier_t)
    }

    init_precedence(&parser) 
    
    assert(parser.ptable[binop_t.MULT] == 50)

    all_stmts := make([dynamic]stmt_t)

    for stmt := parse_stmt(&parser); stmt!=nil ; stmt=parse_stmt(&parser){
        append(&all_stmts, stmt)
    }

    for var in parser.maybe_globals{
        ref := parser.global_symbols[var]
        if !ref {
            fmt.eprintln("undeclared symbol ", var)
            panic("")
        }else{
            // fmt.eprintln("symbol is ", ref^)
        }
    }

    ir_buf := naive_ir(&parser, all_stmts)
    
    str := format_ir_buffer(ir_buf)

    fmt.println(str)
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
            "else"=ELSE,
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

scope_t :: struct{
    self: u16,
    next: ^scope_t
}

unit_t :: struct{}

parser_t :: struct {
    tokens: []token_t,
    position: uint,

    nodes: []expr_t,
    next_node: uint,
    
    ptable: [32]u8,

    global_symbols: map[identifier_t]bool,
    immutable_int_vars: map[identifier_t]int_literal_t,
    function_pool: map[identifier_t]func_info_t,

    scope: u16,
    locals_stack: []^local_t,
    scope_ident_tracker: map[identifier_t]^scope_t,
    next_local: uint,
    scope_ptrs: []uint,
    maybe_globals: [dynamic]identifier_t,

    in_proc: bool,
    var_id: u32
}

next_var_id :: proc(parser:^parser_t) -> u32{
    parser.var_id+=1
    return parser.var_id-1
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
    if_expr_t,
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
    enum_info_t,
    ref_local_t,
    func_call_t
}

assign_t :: struct{
    left: ^expr_t,
    right: ^expr_t
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
    body: block_t
}

func_call_t :: struct{
    name: identifier_t,
    args: []expr_t
}

enum_info_t :: struct{}

if_stmt_t :: struct{
    cond: ^expr_t,
    then: ^block_t,
    otherwise: ^block_t
}

stmt_t :: union {
    local_const_t,
    vdecl_stmt_t,
    return_t,
    exprstmt_t,
    block_t,
    if_stmt_t,
    empty_stmt_t,
}

empty_stmt_t:: struct{}

block_t :: struct{
    list: ^[dynamic]stmt_t
}

new_block :: proc() -> block_t{
    return block_t{
        list=new([dynamic]stmt_t)
    }
}


local_const_t :: struct{
    ref: ref_local_t
}

ref_local_t :: ^local_t

local_t :: struct{
    is_var: bool,
    name: identifier_t,
    id: u32,
    expr: ^expr_t
}

vdecl_stmt_t :: struct{
    is: ref_local_t
}

return_t :: struct{
    inner: ^expr_t
}

exprstmt_t :: struct {inner: ^expr_t}

if_expr_t :: struct {
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

note_scope :: proc(parser: ^parser_t, ident: identifier_t){
    s := new_clone(scope_t{
        self = parser.scope, 
        next = nil
    })
    
    if parser.scope_ident_tracker[ident] != nil {
        h := parser.scope_ident_tracker[ident]
        s.next = h 
    }

    parser.scope_ident_tracker[ident] = s
} 

scope_mhave_ident :: proc(parser: ^parser_t, scope: u16, ident: identifier_t) -> bool{
    for e :=  parser.scope_ident_tracker[ident]; e!= nil ; e=e.next{
        if e.self == scope{
            return true
        }
    }

    return false
}

push_local :: proc(parser: ^parser_t, var: local_t)-> ref_local_t{
    at := new_clone(var)
    parser.locals_stack[parser.next_local]=at

    parser.next_local+=1
    
    note_scope(parser, var.name)
    // fmt.eprintln("generated: ", uintptr(at), "= ", at^)
    return ref_local_t(at)
}

begin_scope :: proc(parser: ^parser_t){
    parser.scope+=1
    parser.scope_ptrs[parser.scope] = parser.next_local
}

end_scope :: proc(parser: ^parser_t){
    parser.next_local = parser.scope_ptrs[parser.scope]
    parser.scope-=1
}

global_symbol :: proc(parser: ^parser_t, name: identifier_t){
    map_insert(&parser.global_symbols, name, true)
}

parse_stmt :: proc(parser: ^parser_t) -> stmt_t {
    ident: identifier_t;
    typename: identifier_t;

    current := parser.tokens[parser.position]
    
    if peek_token(parser, simple_token_t.LEFT_CURLY, 0){
        if !parser.in_proc{
            panic("block statements disallowed outside procedures")
        }
        advance(parser)

        begin_scope(parser)
        defer end_scope(parser)

        list := new([dynamic]stmt_t);
        s: stmt_t = parse_stmt(parser)
        for {
            if s == nil{
                if consume_token(parser, simple_token_t.RIGHT_CURLY){
                    break;
                }else{
                    fmt.eprintln(parser.tokens[parser.position])
                    panic("malformed block statement")
                }
            }else{
                append(list, s)
                s= parse_stmt(parser)
            }
        }

        return block_t{list};
    }

    #partial switch token in current {
        case identifier_t: {
            ident = token
            if peek_token(parser, simple_token_t.COLON, 1){
                parser.position+=2;
                if consume_token(parser, simple_token_t.COLON){
                    // ConstDecl
                    // assert value is const
                    rhs := parse_resolve_expr(parser)
                    
                    if !consume_token(parser, simple_token_t.SEMI_COLON){
                        panic("malformed const declaration")
                    }
                    
                    if parser.scope == 0{
                        #partial switch kind in rhs{
                            case int_literal_t:
                                global_symbol(parser, ident)
                                map_insert(&parser.immutable_int_vars, ident, rhs.(int_literal_t))
                            case func_info_t:
                                // FIXME(yousef): allocating for now
                                global_symbol(parser, ident)
                                map_insert(&parser.function_pool, ident, rhs.(func_info_t))
                            case:
                                panic("unimplemented constant expression")
                        }
                        
                        return empty_stmt_t{}
                    }else{
                        ref := push_local(parser, local_t{
                            is_var=false, name=ident, id=next_var_id(parser), expr=boxed_node(parser, rhs)
                        })
                        return local_const_t(local_const_t{ref})
                    }
                    
                    // unreachable: 
                    assert(false)
                }
                
                if consume_identifier(parser, &typename){}
                
                if consume_token(parser, simple_token_t.EQ){
                    // VarDecl

                    if !parser.in_proc {
                        panic("variable declarations disallowed outside procedures")
                    }
                    assert(parser.scope > 0)

                    rhs := parse_resolve_expr(parser)
                    
                    if !consume_token(parser, simple_token_t.SEMI_COLON){
                        panic("malformed var declaration")
                    }
                    
                    at := push_local(parser, local_t{
                        is_var=true, name=ident, id=next_var_id(parser), expr=boxed_node(parser, rhs)
                    })
                    // fmt.eprintln(at)

                    return vdecl_stmt_t{at}
                }
            }
        }

        case builtin_t:{
            switch current{
                case builtin_t.RETURN:{
                    advance(parser)
                    // FIXME(yousef): dynamic allocation 
                    inner:^expr_t=new_clone(parse_resolve_expr(parser))
                    
                    if consume_token(parser, simple_token_t.SEMI_COLON){
                        // fmt.eprintln("returning: ", uintptr(inner^.(ref_local_t)))
                        
                        return return_t{
                            inner
                        }
                    }else{
                        panic("malformed return stmt.")
                    }
                }
                case builtin_t.IF:{
                    advance(parser)

                    cond := parse_resolve_expr(parser)
                    if !peek_token(parser, simple_token_t.LEFT_CURLY, 0){
                        panic("expected if-condition body")
                    }
                    then_body := parse_stmt(parser)
                    
                    // FIXME(yousef): ALLOCATING FOR NOW
                    current_if := if_stmt_t{
                        cond=boxed_node(parser, cond),
                        then=new_clone(then_body.(block_t)),
                        otherwise=nil
                    }

                    if consume_token(parser, builtin_t.ELSE){
                        else_body := parse_stmt(parser)
                        current_if.otherwise=new_clone(else_body.(block_t))
                    }

                    return current_if
                }
                case builtin_t.WHILE:
            }
        }
    }

    expr := parse_resolve_expr(parser)
    if  expr != nil && consume_token(parser, simple_token_t.SEMI_COLON){
        return exprstmt_t {boxed_node(parser, expr)}
    }
    
    return nil
}


boxed_node :: proc(parser: ^parser_t, node: expr_t) -> ^expr_t{
    at :=parser.next_node 
    parser.nodes[at] = node
    parser.next_node+=1;

    return &parser.nodes[at]
}

parse_resolve_expr :: proc(parser: ^parser_t) -> expr_t{
    
    __parse_expr :: proc(parser: ^parser_t) -> expr_t{
            some_ident: identifier_t;
            if peek_identifier(parser, &some_ident){
                if peek_token(parser, simple_token_t.EQ, 1){
                    parser.position+=2;
                    rhs := boxed_node(parser, parse_resolve_expr(parser))
                    
                    assign_to := boxed_node(parser, some_ident)
                    
                    assert(rhs != assign_to)

                    assign :=  assign_t{
                        left=assign_to, right=rhs
                    }
                    return assign
                }
            }

            logexpr := parse_logexpr(parser, DEF_MINPREC);
            if consume_token(parser, builtin_t.IF){
                cond := parse_resolve_expr(parser)
                if cond == nil {panic("malformed if-expression")}
                if consume_token(parser, simple_token_t.COMMA){   
                    otherwise := parse_resolve_expr(parser)
                    return if_expr_t{
                        then=boxed_node(parser, logexpr),
                        cond=boxed_node(parser, cond),
                        otherwise=boxed_node(parser, otherwise)
                    }
                }
                panic("malformed if-expression")
            }
            
            return logexpr
    }

    __resolve_expr :: proc(parser: ^parser_t, expr: expr_t) -> expr_t  {
        switch kind in expr{
            case identifier_t:{
                ident := expr.(identifier_t)
                rightmost := parser.next_local-1
                for scope := parser.scope; scope>0; scope-=1{
                    leftmost := parser.scope_ptrs[scope]
                    
                    // fmt.printfln("scope=%d, leftmost=%d, rightmost=%d, ident=%s", scope, leftmost,  rightmost, ident)
                    
                    if scope_mhave_ident(parser, scope, ident){
                        // search all
                        for j := rightmost; j>=leftmost; j-=1{
                            local := parser.locals_stack[j]
                            
                            if local.name == ident{
                                // fmt.println("resolving: ", ident)
                                neu: expr_t = parser.locals_stack[j]
                                // fmt.println("is: ", expr)

                                return neu
                            }
                        }
                    }

                    rightmost=leftmost-1
                }
                
                // identifier is not a local
                append(&parser.maybe_globals, ident)
                return expr
            }

            case if_expr_t:
                _if := expr.(if_expr_t)
                _if.cond^ = __resolve_expr(parser, _if.cond^)
                _if.then^ = __resolve_expr(parser, _if.then^)
                _if.otherwise^  = __resolve_expr(parser, _if.otherwise^)
                return _if

            case assign_t:
                _a := expr.(assign_t)
                _a.right^ = __resolve_expr(parser, _a.right^)
                _a.left^ = __resolve_expr(parser, _a.left^)
            
                return _a

            case binexpr_t:
                _b := expr.(binexpr_t)
                _b.left^ = __resolve_expr(parser, _b.left^)
                _b.right^ = __resolve_expr(parser, _b.right^)
                
                return _b

            case unexpr_t:
                _u := expr.(unexpr_t)
                _u.inner^ = __resolve_expr(parser, _u.inner^)
                
                return _u

            case int_literal_t, char_literal_t, float_literal_t, string_literal_t:
                return expr

            case func_info_t:
                return expr

            case struct_info_t, enum_info_t, proto_t:
                fmt.eprintln("[warning] name resolution not implemented for this variant")
                return expr

            case ref_local_t:
                return expr
            
            case func_call_t:
                invoc := expr.(func_call_t)
                args := invoc.args
                for i in 0..<len(args){
                    some_arg := &args[i]
                    args[i] = __resolve_expr(parser, some_arg^)
                }

                panic("unimplemented")
            
        }
        return nil
    }

    expr := __parse_expr(parser)
    
    expr = __resolve_expr(parser, expr)

    return expr
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

current :: proc(parser: ^parser_t) -> token_t{
    return parser.tokens[parser.position]
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
            if consume_token(parser, simple_token_t.LEFT_PAREN){
                expr: expr_t;
                args := make([dynamic]expr_t)

                for {
                    expr = parse_resolve_expr(parser)
                    
                    // TODO(yousef): better error handling
                    assert(expr != nil)

                    append(&args, expr)
                    if !consume_token(parser, simple_token_t.COMMA){
                        if consume_token(parser, simple_token_t.RIGHT_PAREN){
                            break;  
                        }else{
                            fmt.eprintln("expected `(` found: ", current(parser))
                        }
                    }
                }

                return func_call_t{
                    name=token,
                    args=args[:len(args)]
                }
            }
            return token
    }
    
    switch next{
        case builtin_t.ENUM:
            panic("unimplemented!")
        
        case builtin_t.FN:{
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

                prototype: proto_t = {args, identifier_t("void")}
                
                if consume_token(parser, simple_token_t.MINUS_ARROW){
                    ident: identifier_t;
                    if consume_identifier(parser, &ident){
                        prototype.ret_type = ident
                    }else{
                        panic("malformed function declaration")
                    }
                }
                
                if peek_token(parser, simple_token_t.LEFT_CURLY, 0){
                    parser.in_proc = true
                    defer parser.in_proc = false
                    s := parse_stmt(parser)
                    if s == nil{
                        panic("expected block stmt")
                    }

                    return func_info_t{
                        prototype, s.(block_t)
                    }
                }else{
                    return prototype;
                }
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
        inner := parse_resolve_expr(parser)
        if consume_token(parser, simple_token_t.RIGHT_PAREN){
            if inner == nil{
                panic("malformed expression (paren)")
            }
            return inner
        }
    }

    return nil
}

/*---------------IR---------------*/
ir_varname_t :: distinct u32

ir_var_t :: struct {name: ir_varname_t, ver: u32};

ir_global_symbol :: distinct identifier_t

ir_value_t :: union{ir_var_t, ir_global_symbol, int_literal_t, ir_phony_t}

ir_binary_t :: struct {op: binop_t, left: ir_value_t, right: ir_value_t, dest: ir_var_t}

ir_unary_t :: struct{op: unop_t, src: ir_value_t, dest: ir_var_t}

ir_return_t :: struct{value: ir_value_t}

ir_label_t :: struct{id: u32, name: string}

ir_jump_label_t :: struct{to: ir_label_t}

ir_jz_label_t :: struct{test: ir_value_t, to: ir_label_t}

ir_emit_label_t :: struct{is: ir_label_t}

ir_define_gconst_t :: struct{dest: ir_global_symbol, constant: int_literal_t}

ir_copy_t :: struct{dest: ir_var_t, src:ir_value_t}

ir_instruction_t :: union{
    ir_unary_t,
    ir_binary_t,
    ir_return_t,
    ir_jump_label_t,
    ir_jz_label_t, 
    ir_emit_label_t,
    ir_copy_t,
    ir_define_gconst_t
}

ir_phony_t :: struct{}

branch_mutation_t :: distinct [2]u32;
/*
branch_mutation_t :: struct{
    [0] altbr: u32,    
    [1] mainbr: u32, // zero is uninitialised
    
}
*/

ir_state_t :: struct{
    ir_buf: [dynamic]ir_instruction_t,

    var_counter: u32,
    temp_counter: u32,
    label_counter: u32,

    // global_values: map[identifier_t]ir_globconst_t,
    
    locals: map[ref_local_t]ir_varname_t,

    branch_depth: u32,
    br_is_main: bool,
    br_stack: [256]bool,
    mutation_ledger: ^map[ir_varname_t][256]branch_mutation_t,
    mutated_vars: ^map[ref_local_t]bool,

    /*
        mutation ledger: 
        1) 
        refer to var in MAIN BRANCH: 
        lookup: ledger[var] => struct {mainbr_state | altbr_state}
        if mainbr_state != 0 { 
            correct reference is latest(branch_factor, var, mainbr)
        }else{
            correct reference is super(var) 
        }

        2) 
        mutate var in MAIN BRANCH: 
        lookup: ledger[var] => struct{} 
        mark var as mutated in current br_factor 

        if mainbr_state != 0 { 
            v_x :: latest(branch_factor, var, mainbr)
            
            emit copy: v_x <= new_value 
            ledger[var].main = v_x; 
        }else{
            correct reference is super(var) 
            v_y :: new_temp(branch_factor, var, mainbr)
            emit copy v_y <= new_value 
            ledger[var].main = v_y; 
        }

        in combine:         
        for each VARIABLE in mutated(br_factor)
            emit phi_copy NEXT(VARIABLE) <= phi_double {
                MAIN = main_br_state(br_factor) == 0? CURRENT(VARIABLE) : main_br_state,
                ALTER = alt_br_state(br_factor) == 0? CURRENT(VARIABLE) : alt_br_state,
            }
            mark VARIABLE mutation as false
    */
}

naive_ir :: proc(parser: ^parser_t, list: [dynamic]stmt_t) -> []ir_instruction_t {
    state := ir_state_t{
        ir_buf=make([dynamic]ir_instruction_t),
        label_counter=0,
        var_counter=0,
        // global_values=new(map[identifier_t]ir_globconst_t)^,

        locals = new(map[ref_local_t]ir_varname_t)^,
    
        branch_depth=0,
        br_is_main=false,
        mutation_ledger=new(map[ir_varname_t][256]branch_mutation_t),
        mutated_vars=new(map[ref_local_t]bool)
    }

    for name, some_int in parser.immutable_int_vars{
        transform_global_int_const(name, some_int, &state)
    }
    

    for name, info in parser.function_pool{
        transform_function(name, info, &state)
    }

    return state.ir_buf[:len(state.ir_buf)];
}


transform_local_const :: proc(local: local_const_t, state: ^ir_state_t){
    panic("unimplemented")
    /*
    src := transform_expr(local.ref.expr, state)
    
    dest := get_next_var(state)    
    ir_copy(dest.name, src, state)
    
    map_insert(&state.locals, local.ref, dest.name)
    */
}

make_const :: proc(const: ir_global_symbol, value: int_literal_t, state: ^ir_state_t){
    append(&state.ir_buf, ir_define_gconst_t{
        const, value
    })
}

// FIXME(yousef): rework this
transform_global_int_const :: proc(varname: identifier_t, some_int: int_literal_t, state: ^ir_state_t){
    make_const(ir_global_symbol(varname), some_int, state)
    // map_insert(&state.global_values, varname, dest)
}

transform_function :: proc(name: identifier_t, function: func_info_t, state: ^ir_state_t){
    transform_stmt(function.body, state)
}

transform_stmt :: proc(stmt: stmt_t, state: ^ir_state_t) {
    ir_buf := &state.ir_buf
    switch type in stmt{
        case return_t: {
            // fmt.eprintln("working return: ")
            val := transform_expr(stmt.(return_t).inner, state)
            append(ir_buf, ir_return_t {value=val })
        }

        case block_t:{
            for s in stmt.(block_t).list{
                transform_stmt(s, state)
            }
        }

        case vdecl_stmt_t:{
            var := stmt.(vdecl_stmt_t).is
            dest_name := get_next_varname(state)
            dest := ir_var_t{dest_name, 1}
            src := transform_expr(var.expr, state)
            append(ir_buf, ir_copy_t{dest=dest, src=src})
            map_insert(&state.locals, var, dest.name)
            // FIXME(yousef): think about your life choices
            arr := [256]branch_mutation_t{}
            arr[state.branch_depth][cast(u8)state.br_is_main] = 1;
            map_insert(state.mutation_ledger, dest.name, arr);
        }
        case local_const_t:
            transform_local_const(stmt.(local_const_t), state)
        
        case exprstmt_t:
            transform_expr(stmt.(exprstmt_t).inner, state)

        case if_stmt_t:{
            state.br_stack[state.branch_depth] = state.br_is_main;
            state^.branch_depth = state^.branch_depth + 1;
            
            _if := stmt.(if_stmt_t)

            _else := get_next_label(state, "else")
            combine := get_next_label(state, "combine")
            // here: begin_mainbr() 
            
            cond := transform_expr(_if.cond, state)
            jump_if_false := ir_jz_label_t{
                cond, _else
            }
            append(ir_buf, jump_if_false)

            transform_stmt(_if.then^, state)
            jump_combine := ir_jump_label_t{
                combine
            }
            append(ir_buf, jump_combine)

            if _if.otherwise != nil {
                emit_label(_else, ir_buf)
                // here: begin_altbr() 

                transform_stmt(_if.otherwise^, state)
            }
            
            emit_label(combine, ir_buf)
            
            // restore old value
            state^.br_is_main = state.br_stack[state.branch_depth - 1]
            
            // here: begin_combine() 

            state^.branch_depth = state^.branch_depth - 1;
        }
            
        case empty_stmt_t:
            return 

        case nil:
            panic("attempt to transform nil stmt!")

        case:
            panic("unimplemented!")

    }
}

SUPER :: proc(varname: ir_varname_t, br_depth:u32, state: ^ir_state_t) -> ir_var_t{
    depth := br_depth - 1; 
    for true {
        is_main := state.br_stack[depth];
        index := cast(u8) is_main; 
        version := state.mutation_ledger[varname][state.branch_depth][index]
        // NOTE(yousef): assumes that whenever a branch is terminated, this occurs:
        // ledger[var][br_factor] <- {[0] = 0 , [1] = 0}
        // THIS SHOULD ALWAYS BE TRUE

        if version != 0{
            return ir_var_t{name=varname, ver=version}
        }
        if depth == 0{
            break;
        }else{
            depth-=1;
        }
    }
    panic("[FATAL] variable not found in ledger.")
}

transform_expr :: proc(expr: ^expr_t, state: ^ir_state_t) -> ir_value_t {
    ir_buf := &state.ir_buf

    #partial switch type in expr^{
        case int_literal_t: {
            return expr.(int_literal_t)
        }

        case binexpr_t:{
            this :=expr.(binexpr_t); 
            
            src_left := transform_expr(this.left, state);
            dest := ir_var_t {get_next_varname(state), 1};

            src_right := transform_expr(this.right, state);
            instruction : ir_instruction_t = ir_binary_t{
                op=this.op,
                left=src_left,
                right=src_right,
                dest=dest
            };

            append(ir_buf, instruction)
            return dest
        }

        case if_expr_t:
            panic("unimplemented");

        case ref_local_t:{
            varname := state.locals[expr.(ref_local_t)];
            ledger := state.mutation_ledger;
            index := cast(u8) state.br_is_main;
            version := ledger[varname][state.branch_depth][index];
            if 0 == version {
                // this should never happen
                assert(state.branch_depth != 0);

                return SUPER(varname, state.branch_depth, state)
            }else{
                return ir_var_t {
                    name=varname,
                    ver=version
                };
            }
        }
        
        case identifier_t:
            // TODO(yousef): check this:
            return ir_global_symbol(expr.(identifier_t))
        
        case assign_t:
            assignment := expr.(assign_t)
            
            var_ref := assignment.left.(ref_local_t)
            varname := state.locals[var_ref]

            right := transform_expr(assignment.right, state)

            /*
            ir_copy(varname, right, state)
            map_insert(&state.locals, var_ref, varname)
            dest := latest_valid(state, varname)
            */ 
            _ = varname;
            _ = right;
            // assign to var
            panic("unimplemented!")
            

        case func_info_t:
            panic("unreachable: functions should not be handled as expressions in ir")
        
        case: 
            fmt.println("transforming: ", expr^)
            panic("unimplemented! ")
    }
}

emit_label :: proc(label: ir_label_t, ir_buf: ^[dynamic]ir_instruction_t){
    instr := ir_emit_label_t{is=label}
    append(ir_buf, instr)
}
/*
get_new_var :: proc(state: ^ir_state_t)->ir_var_t{
    new_var := ir_varname_t(state^.var_counter)
    var := ir_var_t{name=new_var, ver=1 }
    map_insert(&state.track_versions, var.name, var.ver)
    state^.var_counter+=1
    return var
}
*/
get_next_varname :: proc(state: ^ir_state_t)->ir_varname_t{
    new_name := ir_varname_t(state.var_counter)
    state^.var_counter+=1
    return new_name
}

// get_next_globconst :: proc(state: ^ir_state_t)->ir_globconst_t{
//     return {
//         get_next_varname(state)
//     }
// }

get_next_label :: proc(state: ^ir_state_t, name: string) -> ir_label_t{
    state^.label_counter+=1;
    return ir_label_t{
        id=state.label_counter-1,
        name=name
    }
}


// ************FORMATTING IR_BUFFER************

import "core:strings"

format_value :: proc(sbuilder: ^strings.Builder, val: ir_value_t){
    switch type in val{
        case ir_var_t:{           
            fmt.sbprintf(sbuilder, "tmp%d.%d", val.(ir_var_t).name, val.(ir_var_t).ver)    
        }

        case ir_global_symbol:{
            fmt.sbprintf(sbuilder, "GLOB(\"%s\")", val.(ir_global_symbol))    
        }
    
        case int_literal_t:{
            fmt.sbprintf(sbuilder, "LIT(%d)", val.(int_literal_t))
        }

        case ir_phony_t:
            fmt.sbprint(sbuilder, "phony(")
            phi := val.(ir_phony_t)
            _ = phi;
            /* for i in phi.fro..=phi.to{
                fmt.sbprintf(sbuilder, "tmp%d.%d", phi.src, i)
                
                if i != phi.to{
                    fmt.sbprint(sbuilder, ", ")
                }
            }
            */
            /*
            fmt.sbprintf(sbuilder, "tmp%d.%d, ", phi.src, phi.fro)
            fmt.sbprintf(sbuilder, "tmp%d.%d", phi.src, phi.to)
            */
            fmt.sbprint(sbuilder, ")")
            panic("unimplemented!")
    }
}

// for debugging (whatever)
format_ir_buffer :: proc(ir_buf: []ir_instruction_t) -> string{
    string_builder := strings.builder_make()
    fmt.sbprint(&string_builder, "\n")
    for instruction in ir_buf {
        switch type in instruction{
            case ir_return_t:{
                fmt.sbprint(&string_builder, "EMIT ")
                format_value(&string_builder, instruction.(ir_return_t).value);
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
            case ir_jump_label_t :{
                j := instruction.(ir_jump_label_t)
                fmt.sbprint(&string_builder, "JUMP $")
                fmt.sbprintf(&string_builder, "%d.", j.to.id)
                fmt.sbprint(&string_builder, j.to.name)
                fmt.sbprintf(&string_builder, "\n")
            }

            case ir_jz_label_t : {
                j := instruction.(ir_jz_label_t)
                fmt.sbprint(&string_builder, "JZ(")
                format_value(&string_builder, j.test)
                fmt.sbprint(&string_builder, ") $")
                fmt.sbprintf(&string_builder, "%d.", j.to.id)
                fmt.sbprint(&string_builder, j.to.name)
                fmt.sbprint(&string_builder, "\n")
            }

            case ir_emit_label_t:{
                is := instruction.(ir_emit_label_t).is
                fmt.sbprintf(&string_builder, "\n@%d.%s:\n", is.id, is.name)
            }
            case ir_copy_t: {
                copy := instruction.(ir_copy_t)
                format_value(&string_builder, copy.dest)
                fmt.sbprint(&string_builder, " = ")    
                format_value(&string_builder, copy.src)
                fmt.sbprint(&string_builder, "\n")
            }

            case ir_define_gconst_t:{
                gc := instruction.(ir_define_gconst_t)
                format_value(&string_builder, gc.dest)
                fmt.sbprint(&string_builder, " = ")    
                format_value(&string_builder, gc.constant)
                fmt.sbprint(&string_builder, "\n")
            }

            case ir_unary_t:{
                panic("unimplemented");
            }
        }
    }

    return strings.to_string(string_builder)
}