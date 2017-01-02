defmodule July.Stdlib.Global do

  def global_env do
    %{
      :+        => fn(x, y) -> x + y end,
      :-        => fn(x, y) -> x - y end,
      :*        => fn(x, y) -> x * y end,
      :/        => fn(x, y) -> x / y end,
      :<        => fn(x, y) -> x < y end,
      :>        => fn(x, y) -> x > y end,
      :<=       => fn(x, y) -> x <= y end,
      :>=       => fn(x, y) -> x >= y end,
      :==       => fn(x, y) -> x == y end,
      :or       => fn(x, y) -> x or y end,
      :and      => fn(x, y) -> x and y end,
      :not      => fn(x) -> !x end,
      'inspect' => fn(x) -> IO.inspect(x) end,
     }
  end

end
