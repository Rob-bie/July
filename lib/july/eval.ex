defmodule July.Eval do

  alias July.Helpers.Parser

  def eval(bin) do
    case Parser.parse(bin) do
      {:ok, ast}       -> _eval(ast)
      {:error, reason} -> reason
    end
  end

  defp _eval({:+, node_l, node_r}), do: _eval(node_l) + _eval(node_r)
  defp _eval({:-, node_l, node_r}), do: _eval(node_l) - _eval(node_r)
  defp _eval({:*, node_l, node_r}), do: _eval(node_l) * _eval(node_r)
  defp _eval({:/, node_l, node_r}), do: _eval(node_l) / _eval(node_r)
  defp _eval(n) when is_number(n),  do: n

end
