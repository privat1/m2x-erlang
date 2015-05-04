defmodule Test_m2x_distribution_stream do
  use ExUnit.Case

  dist   = "00112233445566778899aabbccddeeff"
  stream = "temperature"
  params = [{"param1", "value1"}, {"param2", "value2"}]

  for {func, arity, func_params, req_method, req_path, req_params} <- [
    { :view,          "/3", :null,  :get,    "/distributions/"<>dist<>"/streams/"<>stream,              :null  },
    { :create,        "/3", :null,  :put,    "/distributions/"<>dist<>"/streams/"<>stream,              :null  },
    { :create,        "/4", params, :put,    "/distributions/"<>dist<>"/streams/"<>stream,              params },
    { :update,        "/4", params, :put,    "/distributions/"<>dist<>"/streams/"<>stream,              params },
    { :delete,        "/3", :null,  :delete, "/distributions/"<>dist<>"/streams/"<>stream,              :null  },
  ] do
    test Atom.to_string(func) <> arity do
      MockEngine.check_normal(
        {unquote(req_method), unquote(req_path), unquote(req_params)},
        case unquote(func_params) do
          :null -> &(apply(:m2x_distribution_stream, unquote(func), [&1, unquote(dist), unquote(stream)]))
          _     -> &(apply(:m2x_distribution_stream, unquote(func), [&1, unquote(dist), unquote(stream), unquote(func_params)]))
        end
      )
    end
  end
end
