module Lexer

/// Rule linecomment
val linecomment: lexbuf: LexBuffer<char> -> token
/// Rule tokenstream
val tokenstream: lexbuf: LexBuffer<char> -> token
