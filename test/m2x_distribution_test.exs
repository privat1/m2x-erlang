defmodule Test_m2x_distribution do
  use ExUnit.Case

  distribution = "00112233445566778899aabbccddeeff"
  params = [{"param1", "value1"}, {"param2", "value2"}]

  for {func, arity, func_params, req_method, req_path, req_params} <- [
    { :view,            "/2", :null,  :get,    "/distributions/"<>distribution,              :null  },
    { :update,          "/3", params, :put,    "/distributions/"<>distribution,              params },
    { :delete,          "/2", :null,  :delete, "/distributions/"<>distribution,              :null  },
    { :metadata,        "/2", :null,  :get,    "/distributions/"<>distribution<>"/metadata", :null  },
    { :update_metadata, "/3", params, :put,    "/distributions/"<>distribution<>"/metadata", params },
    { :devices,         "/2", :null,  :get,    "/distributions/"<>distribution<>"/devices",  :null  },
    { :create_device,   "/3", params, :post,   "/distributions/"<>distribution<>"/devices",  params },
    { :streams,         "/2", :null,  :get,    "/distributions/"<>distribution<>"/streams",  :null  },
  ] do
    test Atom.to_string(func) <> arity do
      MockEngine.check_normal(
        {unquote(req_method), unquote(req_path), unquote(req_params)},
        case unquote(func_params) do
          :null -> &(apply(:m2x_distribution, unquote(func), [&1, unquote(distribution)]))
          _     -> &(apply(:m2x_distribution, unquote(func), [&1, unquote(distribution), unquote(func_params)]))
        end
      )
    end
  end

  test "get_metadata_field/3" do
    MockEngine.check_normal(
      {:get, "/distributions/"<>unquote(distribution)<>"/metadata/field_name", :null },
      &(:m2x_distribution.get_metadata_field(&1, unquote(distribution), "field_name"))
    )
  end

  test "set_metadata_field/4" do
    MockEngine.check_normal(
      {:put, "/distributions/"<>unquote(distribution)<>"/metadata/field_name", [{"value", "field_value"}] },
      &(:m2x_distribution.set_metadata_field(&1, unquote(distribution), "field_name", "field_value"))
    )
  end
end
