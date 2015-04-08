defmodule M2XTest do
  use ExUnit.Case

  def real_client do
    :m2x.client("0123456789abcdef0123456789abcdef")
  end

  test "client user_agent" do
    assert Regex.match? ~r"\AM2X-Erlang/\S+ erlang/\S+ \(.*\)",
      :m2x_client.user_agent
  end

  test "real_client raw get /status" do
    {:ok, res} = real_client.({:raw, :get, "/status"})
    assert res[:status] == 200
    assert res[:json] == [{"api", "OK"}, {"triggers", "OK"}]
    assert res[:raw] |> is_binary
    assert res[:headers] |> is_list
  end

  test "real_client get /status" do
    {:ok, status, json} = real_client.({:get, "/status"})
    assert status == 200
    assert json == [{"api", "OK"}, {"triggers", "OK"}]
  end

  test "client get /status (verified by hand)" do
    subject = MockEngine.client(
      {:get, "/status", :null},
      {200, [{"api", "OK"}, {"triggers", "OK"}]}
    )
    {:ok, status, json} = subject.({:get, "/status"})
    assert status == 200
    assert json == [{"api", "OK"}, {"triggers", "OK"}]
  end

  test "client get /status => 200" do
    :ok = MockEngine.verify(
      {:get, "/status", :null},
      {200, [{"api", "OK"}, {"triggers", "OK"}]},
      fn(client) ->
        client.({:get, "/status"})
      end
    )
  end

  test "client_error with params" do
    :client_error = MockEngine.verify(
      {:get, "/foo/bar", [{"baz", "baz_value"}]},
      {404, [{"message", "not found"}]},
      fn(client) ->
        client.({:get, "/foo/bar", [{"baz", "baz_value"}]})
      end
    )
  end

  test "server_error with no return json" do
    :server_error = MockEngine.verify(
      {:get, "/foo/bar", :null},
      {555, :null},
      fn(client) ->
        client.({:get, "/foo/bar"})
      end
    )
  end
end
