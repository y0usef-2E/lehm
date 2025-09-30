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

            case '#', '.', '~', '|','%','+','/','\\','*','-','{','}','[',']','(',')',':',';',',','^': {
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
                ident := identifier_t(lexer.bytes[start_i:end_i_excl]);

                append(&tokens, ident);
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

            case '"': {}

            case ' ' , '\n' , '\r' , '\t' : {}
        }
    }

    return tokens
}

parse :: proc(tokens : [dynamic]token_t ){
    token := tokens[0]
    #partial switch type in token {
        case identifier_t: {}
        case:
    }
}