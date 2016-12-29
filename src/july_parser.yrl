Terminals '+'
          '-' 
          '*' 
          '/'
          '('
          ')'
          number.

Nonterminals expr.

Rootsymbol expr.

Left 100 '+' '-'.
Left 110 '*' '/'.

expr -> expr '+' expr : {unpack('$2'), '$1', '$3'}.
expr -> expr '-' expr : {unpack('$2'), '$1', '$3'}.
expr -> expr '*' expr : {unpack('$2'), '$1', '$3'}.
expr -> expr '/' expr : {unpack('$2'), '$1', '$3'}.
expr -> '(' expr ')'  : '$2'.
expr -> number        : unpack('$1').

Erlang code.

unpack({_, _, Token}) -> Token;
unpack({Token, _})    -> Token.
