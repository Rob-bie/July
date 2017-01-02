Definitions.

NUMBER          = \-?[0-9]+(\.[0-9]+)?
L_IDENT         = [a-z_]+
OPERATOR_INFIX  = (\+|\-|\*|\/|=|<|>|<=|>=|==|and|or|not)
PARENS          = (\(|\))
BRACES          = (\{|\})
COMMA           = ,
ARROW           = ->
WHITESPACE      = [\s\t\r\n]+
KEYWORDS        = (fn|if)

Rules.

{NUMBER}         : {token, {number, TokenLine, to_number(TokenChars)}}.
{OPERATOR_INFIX} : {token, {list_to_atom(TokenChars), TokenLine}}.
{PARENS}         : {token, {list_to_atom(TokenChars), TokenLine}}.
{BRACES}         : {token, {list_to_atom(TokenChars), TokenLine}}.
{COMMA}          : {token, {',', TokenLine}}.
{ARROW}          : {token, {'->', TokenLine}}.
{KEYWORDS}       : {token, {list_to_atom(TokenChars), TokenLine}}.
{L_IDENT}        : {token, {l_ident, TokenLine, TokenChars}}.
{WHITESPACE}     : skip_token.

Erlang code.

% Utility for converting a number to either an integer or a float.

to_number(N) ->
    case string:to_float(N) of
        {error, no_float} -> list_to_integer(N);
        {F, _Rest}        -> F
    end.
