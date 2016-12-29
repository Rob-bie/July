Definitions.

NUMBER          = [0-9]+(\.[0-9])?
OPERATOR_INFIX  = (\+|\-|\*|\/)
PARENS          = (\(|\))
WHITESPACE      = [\s\t\r\n]+

Rules.

{NUMBER}         : {token, {number, TokenLine, to_number(TokenChars)}}.
{OPERATOR_INFIX} : {token, {list_to_atom(TokenChars), TokenLine}}.
{PARENS}         : {token, {list_to_atom(TokenChars), TokenLine}}.
{WHITESPACE}     : skip_token.

Erlang code.

% Utility for converting a number to either an integer or a float.

to_number(N) ->
    case string:to_float(N) of
        {error, no_float} -> list_to_integer(N);
        {F, _Rest}        -> F
    end.
