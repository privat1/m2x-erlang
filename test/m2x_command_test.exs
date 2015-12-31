defmodule Test_m2x_command do
  use ExUnit.Case

  command = "00112233445566778899aabbccddeeff"

  for {func, arity, func_params, req_method, req_path, req_params} <- [
    { :view, "/2", :null, :get, "/commands/"<>command, :null },
  ] do
    test Atom.to_string(func) <> arity do
      MockEngine.check_normal(
        {unquote(req_method), unquote(req_path), unquote(req_params)},
        case unquote(func_params) do
          :null -> &(apply(:m2x_command, unquote(func), [&1, unquote(command)]))
          _     -> &(apply(:m2x_command, unquote(func), [&1, unquote(command), unquote(func_params)]))
        end
      )
    end
  end
end
