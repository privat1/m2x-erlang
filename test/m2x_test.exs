defmodule Test_m2x do
  use ExUnit.Case

  params = [{"param1", "value1"}, {"param2", "value2"}]

  for {func, arity, func_params, req_method, req_path, req_params} <- [
    { :devices,             "/1", :null,  :get,  "/devices",         :null  },
    { :devices,             "/2", params, :get,  "/devices",         params },
    { :search_devices,      "/1", :null,  :get,  "/devices/search",  :null  },
    { :search_devices,      "/2", params, :get,  "/devices/search",  params },
    { :devices_catalog,     "/1", :null,  :get,  "/devices/catalog", :null  },
    { :devices_catalog,     "/2", params, :get,  "/devices/catalog", params },
    { :devices_tags,        "/1", :null,  :get,  "/devices/tags",    :null  },
    { :create_device,       "/2", params, :post, "/devices",         params },
    { :commands,            "/1", :null,  :get,  "/commands",        :null  },
    { :commands,            "/2", params, :get,  "/commands",        params },
    { :send_command,        "/2", params, :post, "/commands",        params },
    { :distributions,       "/1", :null,  :get,  "/distributions",   :null  },
    { :create_distribution, "/2", params, :post, "/distributions",   params },
    { :collections,         "/1", :null,  :get,  "/collections",     :null  },
    { :create_collection,   "/2", params, :post, "/collections",     params },
    { :keys,                "/1", :null,  :get,  "/keys",            :null  },
    { :keys,                "/2", params, :get,  "/keys",            params },
    { :create_key,          "/2", params, :post, "/keys",            params },
  ] do
    test Atom.to_string(func) <> arity do
      MockEngine.check_normal(
        {unquote(req_method), unquote(req_path), unquote(req_params)},
        case unquote(func_params) do
          :null -> &(apply(:m2x, unquote(func), [&1]))
          _     -> &(apply(:m2x, unquote(func), [&1, unquote(func_params)]))
        end
      )
    end
  end
end
