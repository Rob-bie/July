Terminals '+'
          '-' 
          '*' 
          '/'
          '<'
          '>'
          '<='
          '>='
          '=='
          '('
          ')'
          'and'
          'or'
          'not'
          number.

Nonterminals expr.

Rootsymbol expr.

Left  60   'and' 'or'.
Unary 70   'not'.
Left  80   '<' '>' '<=' '>=' '=='.
Left  100  '+' '-'.
Left  110  '*' '/'.

expr -> expr '+' expr   : {unpack('$2'), '$1', '$3'}.
expr -> expr '-' expr   : {unpack('$2'), '$1', '$3'}.
expr -> expr '*' expr   : {unpack('$2'), '$1', '$3'}.
expr -> expr '/' expr   : {unpack('$2'), '$1', '$3'}.
expr -> expr '<' expr   : {unpack('$2'), '$1', '$3'}.
expr -> expr '>' expr   : {unpack('$2'), '$1', '$3'}.
expr -> expr '<=' expr  : {unpack('$2'), '$1', '$3'}.
expr -> expr '>=' expr  : {unpack('$2'), '$1', '$3'}.
expr -> expr '==' expr  : {unpack('$2'), '$1', '$3'}.
expr -> expr 'and' expr : {unpack('$2'), '$1', '$3'}.
expr -> expr 'or' expr  : {unpack('$2'), '$1', '$3'}.
expr -> 'not' expr      : {unpack('$1'), '$2'}.
expr -> '(' expr ')'    : '$2'.
expr -> number          : unpack('$1').

Erlang code.

unpack({_, _, Token}) -> Token;
unpack({Token, _})    -> Token.
