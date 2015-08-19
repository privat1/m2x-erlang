defmodule Test_m2x_collection do
  use ExUnit.Case

  collection = "00112233445566778899aabbccddeeff"
  params = [{"param1", "value1"}, {"param2", "value2"}]

  for {func, arity, func_params, req_method, req_path, req_params} <- [
    { :view,   "/2", :null,  :get,    "/collections/"<>collection, :null  },
    { :update, "/3", params, :put,    "/collections/"<>collection, params },
    { :delete, "/2", :null,  :delete, "/collections/"<>collection, :null  },
  ] do
    test Atom.to_string(func) <> arity do
      MockEngine.check_normal(
        {unquote(req_method), unquote(req_path), unquote(req_params)},
        case unquote(func_params) do
          :null -> &(apply(:m2x_collection, unquote(func), [&1, unquote(collection)]))
          _     -> &(apply(:m2x_collection, unquote(func), [&1, unquote(collection), unquote(func_params)]))
        end
      )
    end
  end
end
