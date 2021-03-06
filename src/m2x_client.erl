
-module(m2x_client).

%% API
-export([create/1, create/2, create/3, create/4]).
-export([user_agent/0]).

%% Macros
-define(DEFAULT_API_BASE, <<"https://api-m2x.att.com">>).
-define(DEFAULT_API_VERSION, v2).

% Return the user agent string (as a binary)
user_agent() ->
  LibVersion = m2x:version(),
  ErlVersion = list_to_binary(erlang:system_info(otp_release)),
  {OsFamilyA, OsNameA} = os:type(),
  {OsFamily,  OsName}  = {list_to_binary(atom_to_list(OsFamilyA)),
                          list_to_binary(atom_to_list(OsNameA))},
  OsVersion = case os:version() of
    {MajA,MinA,RelA} ->
      {Maj,Min,Rel} = {integer_to_binary(MajA),integer_to_binary(MinA),integer_to_binary(RelA)},
      <<Maj/binary, ".", Min/binary, ".", Rel/binary>>;
    String           -> list_to_binary(String)
  end,
  <<"M2X-Erlang/", LibVersion/binary, " erlang/", ErlVersion/binary,
    " (", OsFamily/binary, ":", OsName/binary, " ", OsVersion/binary, ")">>.

%% Return an anonymous function that can make M2X API requests.
create(ApiKey) -> create(ApiKey, ?DEFAULT_API_BASE, ?DEFAULT_API_VERSION).
create(ApiKey, ApiBase) -> create(ApiKey, ApiBase, ?DEFAULT_API_VERSION).
create(ApiKey, ApiBase, ApiVersion) -> create(ApiKey, ApiBase, ApiVersion, hackney).
create(ApiKey, ApiBase, ApiVersion, HttpEngine) when is_binary(ApiKey) andalso is_binary(ApiBase) andalso is_atom(ApiVersion) ->
  HttpEngine:start(),
  fun(Args) ->
    request({ApiKey, ApiBase, ApiVersion, HttpEngine}, process_args(Args))
  end.

process_args(Args) ->
  case Args of
    {raw, Method, Path}                  -> {Method, Path, null, [], true};
    {raw, Method, Path, Params}          -> {Method, Path, Params, [], true};
    {raw, Method, Path, Params, Headers} -> {Method, Path, Params, Headers, true};
    {Method, Path}                       -> {Method, Path, null, [], false};
    {Method, Path, Params}               -> {Method, Path, Params, [], false};
    {Method, Path, Params, Headers}      -> {Method, Path, Params, Headers, false}
  end.

%% Make an API request with the given REST method, path and parameters
request({ApiKey, ApiBase, ApiVersion, HttpEngine}, {Method, Path, Params, Headers, UseRaw}) ->
  Url              = make_url(ApiBase, ApiVersion, Path),
  {Body, Headers2} = make_body(Params, Headers),
  HeaderList       = Headers2 ++ [
    {<<"X-M2X-KEY">>, ApiKey},
    {<<"User-Agent">>, user_agent()}
  ],
  OptionList       = [{cacertfile, filename:dirname(?FILE) ++ "/cacert.pem"}],
  make_response(HttpEngine:request(Method, Url, HeaderList, Body, OptionList), HttpEngine, UseRaw).

%% Convert the given HttpEngine response into an m2x-style response
make_response({ok, Status, Headers, BodyRef}, HttpEngine, UseRaw) ->
  {ok, Body} = HttpEngine:body(BodyRef),
  Json = case proplists:get_value(<<"Content-Type">>, Headers) of
           <<"application/json", _/binary>> -> jsx:decode(Body);
           _                                -> null
         end,
  IsOk = case Status div 100 of
           2 -> ok;
           4 -> client_error;
           5 -> server_error;
           _ -> other
         end,
  if
    UseRaw -> {IsOk, [ { raw,     Body },
                       { json,    Json },
                       { status,  Status },
                       { headers, Headers } ]};
    true   -> {IsOk, Status, Json}
  end.

%% Construct the URL to use in the HTTP request
make_url(ApiBase, ApiVersion, Path) ->
  BinaryPath = case Path of
    <<_/binary>> -> Path;
    _            -> list_to_binary(Path)
  end,
  SlashPath = case BinaryPath of
    <<"/", _/binary>> -> BinaryPath;
    _                 -> <<"/", BinaryPath>>
  end,
  ApiVersionString = atom_to_binary(ApiVersion, utf8),
  <<ApiBase/binary, "/", ApiVersionString/binary, SlashPath/binary>>.

%% Construct the body binary to use in the HTTP request
make_body(null, Headers) -> {<<>>, Headers};
make_body(Body, Headers) when is_binary(Body) -> {Body, Headers};
make_body(Params, Headers) when is_list(Params) ->
  case proplists:get_value(<<"Content-Type">>, Headers) of
    <<"application/json">> ->
      {jsx:encode(Params), Headers};
    undefined ->
      make_body(Params, [{<<"Content-Type">>, <<"application/json">>} | Headers])
  end.
