defmodule Test_m2x_collection do
  use ExUnit.Case

  collection = "00112233445566778899aabbccddeeff"
  params = [{"param1", "value1"}, {"param2", "value2"}]

  for {func, arity, func_params, req_method, req_path, req_params} <- [
    { :view,            "/2", :null,  :get,    "/collections/"<>collection,              :null  },
    { :update,          "/3", params, :put,    "/collections/"<>collection,              params },
    { :delete,          "/2", :null,  :delete, "/collections/"<>collection,              :null  },
    { :metadata,        "/2", :null,  :get,    "/collections/"<>collection<>"/metadata", :null  },
    { :update_metadata, "/3", params, :put,    "/collections/"<>collection<>"/metadata", params },
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

  test "get_metadata_field/3" do
    MockEngine.check_normal(
      {:get, "/collections/"<>unquote(collection)<>"/metadata/field_name", :null },
      &(:m2x_collection.get_metadata_field(&1, unquote(collection), "field_name"))
    )
  end

  test "set_metadata_field/4" do
    MockEngine.check_normal(
      {:put, "/collections/"<>unquote(collection)<>"/metadata/field_name", [{"value", "field_value"}] },
      &(:m2x_collection.set_metadata_field(&1, unquote(collection), "field_name", "field_value"))
    )
  end
end
