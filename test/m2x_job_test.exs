defmodule Test_m2x_job do
  use ExUnit.Case

  job = "00112233445566778899aabbccddeeff"
  params = [{"param1", "value1"}, {"param2", "value2"}]

  for {func, arity, func_params, req_method, req_path, req_params} <- [
    { :view, "/2", :null, :get, "/jobs/"<>job, :null  },
  ] do
    test Atom.to_string(func) <> arity do
      MockEngine.check_normal(
        {unquote(req_method), unquote(req_path), unquote(req_params)},
        case unquote(func_params) do
          :null -> &(apply(:m2x_job, unquote(func), [&1, unquote(job)]))
          _     -> &(apply(:m2x_job, unquote(func), [&1, unquote(job), unquote(func_params)]))
        end
      )
    end
  end
end
