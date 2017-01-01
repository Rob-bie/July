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
    Enum.reduce(exprs, {nil, env}, fn(expr, {val, env}) ->
      case _eval(expr, env) do
        {:bind, result, new_env} -> {result, new_env}
        {:bind, new_env}         -> {val, new_env}
        result                   -> {result, env}
      end
    end)
  end

  defp _eval({:fn_dec, ident, params, body}, env) do
    new_env = Map.put(env, ident, %{params: params, body: body})
    {:bind, new_env}
  end

  defp _eval({:fn_inv, ident, args}, env) do
    %{params: params, body: body} = env[ident]
    args = Enum.map(args, fn(expr) -> _eval(expr, env) end)
    new_env = Enum.zip(params, args) |> Enum.into(%{}) |> Map.merge(env)
    _eval_block(body, new_env)
  end

  defp _eval({:=, ident, node}, env) do
    result  = _eval(node, env)
    new_env = Map.put(env, ident, result)
    {:bind, result, new_env}
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

  defp _eval_block(exprs, env) do
    {result, _new_env} = _eval_all(exprs, env)
    result
  end

end
