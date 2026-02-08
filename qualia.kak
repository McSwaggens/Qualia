# Kakoune syntax highlighting for Qualia (.q files)

# Detection
hook global BufCreate .*\.q %{
    set-option buffer filetype qualia
}

# Initialization
hook global WinSetOption filetype=qualia %{
    require-module qualia

    add-highlighter window/qualia ref qualia

    set-option window comment_line '//'
    set-option window indentwidth 0

    hook window InsertChar \n -group qualia-indent qualia-indent-on-newline
    hook -once -always window WinSetOption filetype=.* %{
        remove-hooks window qualia-.*
        remove-highlighter window/qualia
    }
}

provide-module qualia %§

# Highlighters
add-highlighter shared/qualia regions
add-highlighter shared/qualia/comment region '//' '$' fill comment
add-highlighter shared/qualia/string  region '"' (?<!\\)(\\\\)*" fill string

add-highlighter shared/qualia/code default-region group

# Attributes: [Qualia.Something(args)]
add-highlighter shared/qualia/code/attribute regex '\[[\w.()=]+\]' 0:attribute

# Keywords
add-highlighter shared/qualia/code/keyword regex \b(if|else|then|for|while|in|where|return|break|import|struct|enum|as|not|inc|dec|claim|defer|alias|asm)\b 0:keyword

# Built-in types
add-highlighter shared/qualia/code/type regex \b(byte|bool|int|int8|int16|int32|int64|uint|uint8|uint16|uint32|uint64|float32|float64)\b 0:type

# Literal values
add-highlighter shared/qualia/code/value regex \b(true|false|null)\b 0:value

# Hex literals: FFh, DEAD_BEEFh, FFh32
add-highlighter shared/qualia/code/hex_literal regex \b[0-9][0-9a-fA-F_]*h(?:[0-9]+)?\b 0:value

# Binary literals: 1010b, 1111_0000b, 10101010b32
add-highlighter shared/qualia/code/bin_literal regex \b[01][01_]*b(?:[0-9]+)?\b 0:value

# Float literals: 3.14, 3.14f32, 3.14f64
add-highlighter shared/qualia/code/float_literal regex \b[0-9][0-9_]*\.[0-9][0-9_]*(?:f(?:32|64))?\b 0:value

# Integer literals with qualifiers: 100s64, 255u8, 4k, 2m, 1g, 1t
add-highlighter shared/qualia/code/int_qualified regex \b[0-9][0-9_]*(?:s(?:8|16|32|64)|u(?:8|16|32|64)|[kmgt])\b 0:value

# Plain integer literals: 0, 42, 1_000_000
add-highlighter shared/qualia/code/int_literal regex \b[0-9][0-9_]*\b 0:value

# Multi-char operators
add-highlighter shared/qualia/code/operator regex (?::=|->|=>|&&|\|\||!=|<=|>=|<<|>>|\?\.|\.\.|\+=|-=|\*=|/=|\^=) 0:operator

# Single-char operators
add-highlighter shared/qualia/code/operator_single regex [+\-*/%&|^~!?<>=] 0:operator

# Function declarations and calls: FormalIdentifier(
add-highlighter shared/qualia/code/function regex \b([A-Z]\w*)\s*\( 1:function

# Module names after import
add-highlighter shared/qualia/code/module regex \b(import)\s+(\w[\w.]*) 2:module

# Indent
define-command -hidden qualia-indent-on-newline %{
    evaluate-commands -draft -itersel %{
        # Copy previous line indent
        try %{ execute-keys -draft <semicolon> K <a-&> }
        # Increase indent after lines ending with ':'
        try %{ execute-keys -draft k x <a-k> :\s*$ <ret> j <a-gt> }
    }
}

§
