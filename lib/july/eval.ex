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
    evaluated_args = Enum.map(args, fn(expr) -> _eval(expr, env) end)
    case env[ident] do
      %{params: params, body: body} ->
        bounded_args = Enum.zip(params, args) |> Enum.into(%{})
        new_env = Map.merge(env, bounded_args)
        _eval(body, new_env)
      builtin_func                  ->
        apply(builtin_func, evaluated_args)
    end
  end

  defp _eval({:if_stmt, clauses}, env) do
    {_body, result} = Enum.find(clauses, fn({clause, _r}) ->
      _eval(clause, env)
    end)
    _eval(result, env)
  end

  defp _eval({:=, ident, node}, env) do
    result  = _eval(node, env)
    new_env = Map.put(env, ident, result)
    {:bind, result, new_env}
  end

  defp _eval({:block, body}, env) do
    {result, _env} = _eval_all(body, env)
    result
  end
  
  defp _eval({op, node_l, node_r}, env) do
    env[op].(_eval(node_l, env), _eval(node_r, env))
  end

  defp _eval({op, node}, env) do
    IO.inspect op
    env[op].(_eval(node, env))
  end

  defp _eval(n, _env) when is_number(n) do
    n
  end

  defp _eval(ident, env) do
    Map.get(env, ident)
  end

end
