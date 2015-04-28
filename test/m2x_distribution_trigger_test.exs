defmodule Test_m2x_distribution_trigger do
  use ExUnit.Case

  dist    = "00112233445566778899aabbccddeeff"
  trigger = "abcdefabcdefabcdefabcdefabcdefff"
  params  = [{"param1", "value1"}, {"param2", "value2"}]

  for {func, arity, func_params, req_method, req_path, req_params} <- [
    { :view,   "/3", :null,  :get,    "/distributions/"<>dist<>"/triggers/"<>trigger,          :null  },
    { :update, "/4", params, :put,    "/distributions/"<>dist<>"/triggers/"<>trigger,          params },
    { :delete, "/3", :null,  :delete, "/distributions/"<>dist<>"/triggers/"<>trigger,          :null  },
    { :test,   "/3", :null,  :post,   "/distributions/"<>dist<>"/triggers/"<>trigger<>"/test", :null  },
  ] do
    test Atom.to_string(func) <> arity do
      MockEngine.check_normal(
        {unquote(req_method), unquote(req_path), unquote(req_params)},
        case unquote(func_params) do
          :null -> &(apply(:m2x_distribution_trigger, unquote(func), [&1, unquote(dist), unquote(trigger)]))
          _     -> &(apply(:m2x_distribution_trigger, unquote(func), [&1, unquote(dist), unquote(trigger), unquote(func_params)]))
        end
      )
    end
  end
end
