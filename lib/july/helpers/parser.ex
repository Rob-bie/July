defmodule July.Helpers.Parser do

  alias July.Helpers.Lexer

  def parse(bin) do
    case Lexer.tokens(bin) do
      {:error, reason, _line_number} -> reason
      {:ok, tokens, _line_number}    -> :july_parser.parse(tokens)
    end
  end

end
