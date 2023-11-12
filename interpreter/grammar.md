# {name} Grammar

This document describes the lexical grammar of {name}.

```
Separator ::= ';' | ',';

Body ::= $($Segment $Separator)* $Segment? ;;

Segment ::= $(Token)* ;;

Token ::= $Map | $Set | $Vec | $Tuple | $Symbol | $String | $Number | $Sigil | $Block ;;

Map ::= '#' '{' $MapBody '}' ;;

MapBody ::= $($MapEntry $Separator)* $MapEntry? ;;

MapEntry ::= $Token ':' $Segment ;;

Set ::= '#' '(' $Body ')' ;;

Vec ::= '[' $Body ']' ;;

Symbol ::= /[a-zA-Z_][a-zA-Z_$]*/ ;;

String ::= <enclosed sequence of characters, bounded by matching \" or \', allowing escape sequences> ;;

Number ::=
  | ('0' ('b' | 'o' | 'd' | 'x') /[0-9a-fA-F_]+/ $('.' /[0-9a-fA-F_]+/)?)
  | (/[0-9_]+/ $('.' /[0-9_]+/]) ;;

Sigil ::= '!' | '@' | '#' | '$' | '%' | '^' | '&' | '|' | '*' | '-' | '=' | '+' | ':' | '~'.

Block ::= '{' $Body '}' ;;
```
