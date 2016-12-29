defmodule July.Eval do

  alias July.Helpers.Parser
  alias July.Stdlib.Global

  def eval(bin) do
    case Parser.parse(bin) do
      {:ok, ast}       -> _eval(ast, Global.global_env)
      {:error, reason} -> reason
    end
  end

  defp _eval({op, node_l, node_r}, env) do
    env[op].(_eval(node_l, env), _eval(node_r, env))
  end

  defp _eval({op, node}, env) do
    env[op].(_eval(node, env))
  end

  defp _eval(n, _env) when is_number(n) do
    n
  end

end
