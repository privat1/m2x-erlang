defmodule Test_m2x_device do
  use ExUnit.Case

  device = "00112233445566778899aabbccddeeff"
  params = [{"param1", "value1"}, {"param2", "value2"}]

  for {func, arity, func_params, req_method, req_path, req_params} <- [
    { :view,               "/2", :null,  :get,    "/devices/"<>device,                        :null  },
    { :update,             "/3", params, :put,    "/devices/"<>device,                        params },
    { :delete,             "/2", :null,  :delete, "/devices/"<>device,                        :null  },
    { :log,                "/2", :null,  :get,    "/devices/"<>device<>"/log",                :null  },
    { :location,           "/2", :null,  :get,    "/devices/"<>device<>"/location",           :null  },
    { :location_history,   "/2", :null,  :get,    "/devices/"<>device<>"/location/waypoints", :null  },
    { :location_history,   "/3", params, :get,    "/devices/"<>device<>"/location/waypoints", params },
    { :update_location,    "/3", params, :put,    "/devices/"<>device<>"/location",           params },
    { :metadata,           "/2", :null,  :get,    "/devices/"<>device<>"/metadata",           :null  },
    { :update_metadata,    "/3", params, :put,    "/devices/"<>device<>"/metadata",           params },
    { :streams,            "/2", :null,  :get,    "/devices/"<>device<>"/streams",            :null  },
    { :commands,           "/2", :null,  :get,    "/devices/"<>device<>"/commands",           :null  },
    { :values,             "/2", :null,  :get,    "/devices/"<>device<>"/values",             :null  },
    { :values,             "/3", params, :get,    "/devices/"<>device<>"/values",             params },
    { :values_search,      "/3", params, :get,    "/devices/"<>device<>"/values/search",      params },
    { :values_export_csv,  "/2", :null,  :get,    "/devices/"<>device<>"/values/export.csv",  :null  },
    { :values_export_csv,  "/3", params, :get,    "/devices/"<>device<>"/values/export.csv",  params },
    { :post_update,        "/3", params, :post,   "/devices/"<>device<>"/update",             params },
    { :post_updates,       "/3", params, :post,   "/devices/"<>device<>"/updates",            params },
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

  test "get_metadata_field/3" do
    MockEngine.check_normal(
      {:get, "/devices/"<>unquote(device)<>"/metadata/field_name", :null },
      &(:m2x_device.get_metadata_field(&1, unquote(device), "field_name"))
    )
  end

  test "set_metadata_field/4" do
    MockEngine.check_normal(
      {:put, "/devices/"<>unquote(device)<>"/metadata/field_name", [{"value", "field_value"}] },
      &(:m2x_device.set_metadata_field(&1, unquote(device), "field_name", "field_value"))
    )
  end
end
