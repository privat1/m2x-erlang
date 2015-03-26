defmodule M2XTest do
  use ExUnit.Case

  def api_key do
    "0123456789abcdef0123456789abcdef"
  end

  def client do
    :m2x.client(api_key)
  end

  test "client user_agent" do
    assert Regex.match? ~r"\AM2X-Erlang/\S+ erlang/\S+ \(.*\)",
      :m2x_client.user_agent
  end

  test "client get /status" do
    IO.inspect client.(:get, "/status", :null)
    IO.inspect client.(:get, "/status", [{"foo", "bar"}])
  end
end
