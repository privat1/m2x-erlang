
defmodule MockEngine do
  use GenServer

  defmodule State do
    defstruct \
      ref:         nil,
      api_key:     nil,
      api_base:    nil,
      api_version: nil,
      req_verb:    nil,
      req_path:    nil,
      req_body:    nil,
      res_status:  nil,
      res_body:    nil,
      res_headers: nil
  end

  def start do
    GenServer.start_link(MockEngine, %State{}, name: MockEngineProcess)
  end

  def api_key do
    "0123456789abcdef0123456789abcdef"
  end

  def api_base do
    "http://api-m2x.att.com" # TODO: use SSL
  end

  def api_version do
    :v2
  end

  def verify(request, response, block) when is_function(block) do
    subject = client(request, response)
    {is_ok, status, json} = block.(subject)
    ^response = {status, json}
    is_ok
  end

  def client(request, response) do
    client = :m2x.client(
      "0123456789abcdef0123456789abcdef",
      api_base,
      api_version,
      MockEngine
    )
    setup(request, response)
    client
  end

  def setup(request, response) do
    start
    GenServer.call MockEngineProcess, {:setup, request, response}
  end

  def request(verb, url, header_list, body, _) do
    GenServer.call MockEngineProcess, {:request, verb, url, header_list, body}
  end

  def body(ref) do
    GenServer.call MockEngineProcess, {:body, ref}
  end

  defp to_json(:null) do "" end
  defp to_json(params) do :jsx.encode(params) end

  def handle_call({:setup, {req_verb, req_path, req_params},
                           {res_status, res_params}}, _, _state) do
    {:reply, :ok, %State {
      ref:         make_ref,
      api_key:     api_key,
      api_base:    api_base,
      api_version: api_version,
      req_verb:    req_verb,
      req_path:    req_path,
      req_body:    to_json(req_params),
      res_status:  res_status,
      res_body:    to_json(res_params),
      res_headers: res_params==:null && [] || [{"Content-Type", "application/json"}]
    }}
  end

  def handle_call({:request, verb, url, header_list, body}, _, state) do
    headers = Enum.into(header_list, %{})
    {^verb, ^url} = {state.req_verb, state.api_base <> "/" <> Atom.to_string(state.api_version) <> state.req_path}
    case state.req_body do
    "" -> ^body = ""
    _  ->
      {:ok, "application/json"} = Map.fetch(headers, "Content-Type")
      ^body = state.req_body
    end
    api_key = state.api_key
    user_agent = :m2x_client.user_agent
    {:ok, ^api_key}    = Map.fetch(headers, "X-M2X-KEY")
    {:ok, ^user_agent} = Map.fetch(headers, "User-Agent")
    {:reply, {:ok, state.res_status, state.res_headers, state.ref}, state}
  end

  def handle_call({:body, given_ref}, _, state) do
    ^given_ref = state.ref
    {:reply, {:ok, state.res_body}, state}
  end
end


ExUnit.start()
