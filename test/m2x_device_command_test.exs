defmodule Test_m2x_device_command do
  use ExUnit.Case

  device = "00112233445566778899aabbccddeeff"
  command = "temperature"
  params = [{"param1", "value1"}, {"param2", "value2"}]

  for {func, arity, func_params, req_method, req_path, req_params} <- [
    { :view,    "/3", :null,  :get,  "/devices/"<>device<>"/commands/"<>command,             :null  },
    { :process, "/3", :null,  :post, "/devices/"<>device<>"/commands/"<>command<>"/process", :null  },
    { :process, "/4", params, :post, "/devices/"<>device<>"/commands/"<>command<>"/process", params },
    { :reject,  "/3", :null,  :post, "/devices/"<>device<>"/commands/"<>command<>"/reject",  :null  },
    { :reject,  "/4", params, :post, "/devices/"<>device<>"/commands/"<>command<>"/reject",  params },
  ] do
    test Atom.to_string(func) <> arity do
      MockEngine.check_normal(
        {unquote(req_method), unquote(req_path), unquote(req_params)},
        case unquote(func_params) do
          :null -> &(apply(:m2x_device_command, unquote(func), [&1, unquote(device), unquote(command)]))
          _     -> &(apply(:m2x_device_command, unquote(func), [&1, unquote(device), unquote(command), unquote(func_params)]))
        end
      )
    end
  end
end
