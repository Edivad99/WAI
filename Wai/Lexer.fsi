module Lexer

/// Rule linecomment
val linecomment: lexbuf: LexBuffer<char> -> token
/// Rule tokenize
val tokenize: lexbuf: LexBuffer<char> -> token
