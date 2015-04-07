
-module(m2x_client).

%% API
-export([create/1, create/2, create/3]).
-export([user_agent/0]).

%% Macros
-define(DEFAULT_API_BASE, <<"http://api-m2x.att.com">>). % TODO: use SSL
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
create(ApiKey, ApiBase, ApiVersion) ->
  hackney:start(),
  fun(Method, Path, Params) ->
    request({ApiKey, ApiBase, ApiVersion},
      Method, Path, Params, [])
  end.

%% Make an API request with the given REST method, path and parameters
request({ApiKey, ApiBase, ApiVersion}, Method, Path, Params, Headers) ->
  Url              = make_url(ApiBase, ApiVersion, Path),
  {Body, Headers2} = make_body(Params, Headers),
  HeaderList       = Headers2 ++ [
    {<<"X-M2X-KEY">>, ApiKey},
    {<<"User-Agent">>, user_agent()}
  ],
  OptionList      = [], % TODO: use SSL
  make_response(hackney:request(Method, Url, HeaderList, Body, OptionList)).

%% Convert the given hackney response into an m2x-style response
make_response({ok, Status, Headers, BodyRef}) ->
  {ok, Body}  = hackney:body(BodyRef),
  Json        = case proplists:get_value(<<"Content-Type">>, Headers) of
                  <<"application/json">> -> jsx:decode(Body);
                  _                      -> null
                end,
  [ { raw,     Body },
    { json,    Json },
    { status,  Status },
    { headers, Headers } ].

%% Construct the URL to use in the HTTP request
make_url(ApiBase, ApiVersion, Path) ->
  SlashPath = case Path of
    <<"/", _/binary>> -> Path;
    _                 -> <<"/", Path>>
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
