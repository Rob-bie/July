defmodule July.Helpers.Lexer do

  def tokens(bin) do
    bin_list = to_charlist(bin)
    :july_lexer.string(bin_list)
  end

end
