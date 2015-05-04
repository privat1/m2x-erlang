defmodule Test_m2x_key do
  use ExUnit.Case

  key = "00112233445566778899aabbccddeeff"
  params = [{"param1", "value1"}, {"param2", "value2"}]

  for {func, arity, func_params, req_method, req_path, req_params} <- [
    { :view,       "/2", :null,  :get,    "/keys/"<>key,                :null  },
    { :update,     "/3", params, :put,    "/keys/"<>key,                params },
    { :delete,     "/2", :null,  :delete, "/keys/"<>key,                :null  },
    { :regenerate, "/2", :null,  :post,   "/keys/"<>key<>"/regenerate", :null  },
  ] do
    test Atom.to_string(func) <> arity do
      MockEngine.check_normal(
        {unquote(req_method), unquote(req_path), unquote(req_params)},
        case unquote(func_params) do
          :null -> &(apply(:m2x_key, unquote(func), [&1, unquote(key)]))
          _     -> &(apply(:m2x_key, unquote(func), [&1, unquote(key), unquote(func_params)]))
        end
      )
    end
  end
end
