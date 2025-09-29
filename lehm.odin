#+feature dynamic-literals

package lehm
import "core:fmt"
import "core:os"

simple_token_t :: enum {
    BUILTIN,

    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACKET,
    RIGHT_BRACKET,
    LEFT_CURLY,
    RIGHT_CURLY,

    EQ, // =
    EQ_EQ, // ==
    COLON,      // =
    COLON_EQ,   // :=
    COLON_COLON, // ::
    
    DOT, // .
    COMMA,      // ,
    SEMI_COLON, // ;
    QUESTION, // ?
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
    CARET,     // ^
    PIPE,      // |
    PIPE_PIPE, // ||

    LESS,      // <
    LESS_LESS,  // << 
    LESS_EQ,   // <=
    GREAT,     // >
    GREAT_GREAT, // >> 
    GREAT_EQ,  // >=

    BANG,      // !
    TILDE,     // ~
    BANG_EQ,   // !=
}

builtin_t :: enum {
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
        case '~' : return TILDE;
        case '=' : return EQ;
        case '|' : return PIPE;
        case '%' : return PERCENT;
        case '+' : return PLUS;
        case '/' : return DIV_FSLASH;
        case '\\' :return  BACK_SLASH;
        case '*' :return STAR;
        case '-' :return MINUS;
        case '{' :return LEFT_CURLY;
        case '}' :return RIGHT_CURLY;
        case '[' :return LEFT_BRACKET;
        case ']' :return RIGHT_BRACKET;
        case '(' :return LEFT_PAREN;
        case ')' :return RIGHT_PAREN;
        case '!' :return BANG;
        case '<' :return LESS;
        case '>' :return GREAT;
        case ':' :return COLON;
        case ';' :return SEMI_COLON;
        case ',' :return COMMA;
        case '^' :return CARET;
        
        case:
            panic("cannot parse char as token")
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
            case 'a'..='z',  'A'..='Z': {
                for match_consume_identifier_CHARorDIGIT(&lexer){} 

                start_i := lexer.begin_i;
                end_i_excl := lexer.position;
                ident := identifier_t(lexer.bytes[start_i:end_i_excl]);

                append(&tokens, ident);
            }
            
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