defmodule Test_m2x_stream do
  use ExUnit.Case

  device = "00112233445566778899aabbccddeeff"
  stream = "temperature"
  params = [{"param1", "value1"}, {"param2", "value2"}]

  for {func, arity, func_params, req_method, req_path, req_params} <- [
    { :view,          "/3", :null,  :get,    "/devices/"<>device<>"/streams/"<>stream,              :null  },
    { :create,        "/3", :null,  :put,    "/devices/"<>device<>"/streams/"<>stream,              :null  },
    { :create,        "/4", params, :put,    "/devices/"<>device<>"/streams/"<>stream,              params },
    { :update,        "/4", params, :put,    "/devices/"<>device<>"/streams/"<>stream,              params },
    { :delete,        "/3", :null,  :delete, "/devices/"<>device<>"/streams/"<>stream,              :null  },
    { :update_value,  "/4", params, :put,    "/devices/"<>device<>"/streams/"<>stream<>"/value",    params },
    { :values,        "/3", :null,  :get,    "/devices/"<>device<>"/streams/"<>stream<>"/values",   :null  },
    { :values,        "/4", params, :get,    "/devices/"<>device<>"/streams/"<>stream<>"/values",   params },
    { :post_values,   "/4", params, :post,   "/devices/"<>device<>"/streams/"<>stream<>"/values",   params },
    { :delete_values, "/4", params, :delete, "/devices/"<>device<>"/streams/"<>stream<>"/values",   params },
    { :sampling,      "/4", params, :get,    "/devices/"<>device<>"/streams/"<>stream<>"/sampling", params },
    { :stats,         "/3", :null,  :get,    "/devices/"<>device<>"/streams/"<>stream<>"/stats",    :null  },
    { :stats,         "/4", params, :get,    "/devices/"<>device<>"/streams/"<>stream<>"/stats",    params },
  ] do
    test Atom.to_string(func) <> arity do
      MockEngine.check_normal(
        {unquote(req_method), unquote(req_path), unquote(req_params)},
        case unquote(func_params) do
          :null -> &(apply(:m2x_stream, unquote(func), [&1, unquote(device), unquote(stream)]))
          _     -> &(apply(:m2x_stream, unquote(func), [&1, unquote(device), unquote(stream), unquote(func_params)]))
        end
      )
    end
  end
end
