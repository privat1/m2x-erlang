defmodule Test_m2x_device do
  use ExUnit.Case

  device = "00112233445566778899aabbccddeeff"
  params = [{"param1", "value1"}, {"param2", "value2"}]

  for {func, arity, func_params, req_method, req_path, req_params} <- [
    { :view,            "/2", :null,  :get,    "/devices/"<>device,              :null  },
    { :update,          "/3", params, :put,    "/devices/"<>device,              params },
    { :delete,          "/2", :null,  :delete, "/devices/"<>device,              :null  },
    { :log,             "/2", :null,  :get,    "/devices/"<>device<>"/log",      :null  },
    { :location,        "/2", :null,  :get,    "/devices/"<>device<>"/location", :null  },
    { :update_location, "/3", params, :put,    "/devices/"<>device<>"/location", params },
    { :streams,         "/2", :null,  :get,    "/devices/"<>device<>"/streams",  :null  },
    { :values,          "/2", :null,  :get,    "/devices/"<>device<>"/values",   :null  },
    { :values,          "/3", params, :get,    "/devices/"<>device<>"/values",   params },
    { :post_updates,    "/3", params, :post,   "/devices/"<>device<>"/updates",  params },
    { :triggers,        "/2", :null,  :get,    "/devices/"<>device<>"/triggers", :null  },
    { :create_trigger,  "/3", params, :post,   "/devices/"<>device<>"/triggers", params },
  ] do
    test Atom.to_string(func) <> arity do
      MockEngine.check_normal(
        {unquote(req_method), unquote(req_path), unquote(req_params)},
        case unquote(func_params) do
          :null -> &(apply(:m2x_device, unquote(func), [&1, unquote(device)]))
          _     -> &(apply(:m2x_device, unquote(func), [&1, unquote(device), unquote(func_params)]))
        end
      )
    end
  end
end
