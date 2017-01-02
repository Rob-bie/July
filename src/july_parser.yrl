Terminals '+'
          '-' 
          '*' 
          '/'
          '<'
          '>'
          '<='
          '>='
          '=='
          '='
          '('
          ')'
          '{'
          '}'
          'and'
          'or'
          'not'
          'fn'
          'if'
          ','
          '->'
          number
          l_ident.

Nonterminals expr 
             exprs
             block
             comma_sep_ident
             comma_sep_exprs
             param_list
             arg_list
             func_dec
             func_decs
             func_inv
             clause
             clauses
             if_stmt.

Rootsymbol func_decs.

Right 50   '='.
Left  60   'and' 'or'.
Unary 70   'not'.
Left  80   '<' '>' '<=' '>=' '=='.
Left  100  '+' '-'.
Left  110  '*' '/'.

if_stmt -> 'if' '{' clauses '}' : {if_stmt, '$3'}.

clauses -> clause clauses : ['$1'|'$2'].
clauses -> clause         : ['$1'].

clause -> expr '->' expr  : {'$1', '$3'}.
clause -> expr '->' block : {'$1', '$3'}.

func_decs -> func_dec func_decs : ['$1'|'$2'].
func_decs -> func_dec           : ['$1'].

func_dec -> 'fn' l_ident param_list block : {fn_dec, unpack('$2'), '$3', '$4'}.
func_dec -> expr                          : '$1'. % TEMP

func_inv -> l_ident arg_list : {fn_inv, unpack('$1'), '$2'}.

param_list -> '(' comma_sep_ident ')' : '$2'.

arg_list -> '(' comma_sep_exprs ')' : '$2'.

comma_sep_exprs -> expr ',' comma_sep_exprs    : ['$1'|'$3'].
comma_sep_exprs -> expr                        : ['$1'].

comma_sep_ident -> l_ident ',' comma_sep_ident : [unpack('$1')|'$3'].
comma_sep_ident -> l_ident                     : [unpack('$1')].

block -> '{' exprs '}' : {block, '$2'}.

exprs -> expr exprs : ['$1'|'$2'].
exprs -> expr       : ['$1'].

expr -> if_stmt          : '$1'.
expr -> func_inv         : '$1'.
expr -> expr '+' expr    : {unpack('$2'), '$1', '$3'}.
expr -> expr '-' expr    : {unpack('$2'), '$1', '$3'}.
expr -> expr '*' expr    : {unpack('$2'), '$1', '$3'}.
expr -> expr '/' expr    : {unpack('$2'), '$1', '$3'}.
expr -> expr '<' expr    : {unpack('$2'), '$1', '$3'}.
expr -> expr '>' expr    : {unpack('$2'), '$1', '$3'}.
expr -> expr '<=' expr   : {unpack('$2'), '$1', '$3'}.
expr -> expr '>=' expr   : {unpack('$2'), '$1', '$3'}.
expr -> expr '==' expr   : {unpack('$2'), '$1', '$3'}.
expr -> expr 'and' expr  : {unpack('$2'), '$1', '$3'}.
expr -> expr 'or' expr   : {unpack('$2'), '$1', '$3'}.
expr -> 'not' expr       : {unpack('$1'), '$2'}.
expr -> l_ident '=' expr : {unpack('$2'), unpack('$1'), '$3'}.
expr -> '(' expr ')'     : '$2'.
expr -> number           : unpack('$1').
expr -> l_ident          : unpack('$1').

Erlang code.

unpack({_, _, Token}) -> Token;
unpack({Token, _})    -> Token.
