defmodule July.Stdlib.Global do

  def global_env do
    %{
      :+  => fn(x, y) -> x + y end,
      :-  => fn(x, y) -> x - y end,
      :*  => fn(x, y) -> x * y end,
      :/  => fn(x, y) -> x / y end,
      :<  => fn(x, y) -> x < y end,
      :>  => fn(x, y) -> x > y end,
      :<= => fn(x, y) -> x <= y end,
      :>= => fn(x, y) -> x >= y end,
      :== => fn(x, y) -> x == y end,
     }
  end

end
