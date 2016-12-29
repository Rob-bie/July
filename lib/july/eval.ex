defmodule July.Eval do

  alias July.Helpers.Parser
  alias July.Stdlib.Global

  def eval(bin) do
    case Parser.parse(bin) do
      {:ok, ast}       -> _eval_all(ast, Global.global_env)
      {:error, reason} -> reason
    end
  end

  defp _eval_all(expr, env) when is_tuple(expr) do
    _eval(expr, env)
  end

  defp _eval_all(exprs, env) do
    Enum.reduce(exprs, {nil, env}, fn(expr, {_val, env}) ->
      case _eval(expr, env) do
        {result, new_env} -> {result, new_env}
        result            -> {result, env}
      end
    end)
  end

  defp _eval({:=, ident, node}, env) do
    result  = _eval(node, env)
    new_env = Map.put(env, ident, result)
    {result, new_env}
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

  defp _eval(ident, env) do
    Map.get(env, ident)
  end

end
