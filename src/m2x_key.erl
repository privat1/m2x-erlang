
-module(m2x_key).

%% API
-export([view/2               ]).
-export([             update/3]).
-export([delete/2             ]).
-export([regenerate/2         ]).

%% @doc Get details of the specified key.
%% https://m2x.att.com/developer/documentation/v2/keys#View-Key-Details
view(Client, Key)           -> Client({get, <<"/keys/", Key/binary>>}).

%% @doc Update the specified key's information.
%% https://m2x.att.com/developer/documentation/v2/keys#Update-Key
update(Client, Key, Params) -> Client({put, <<"/keys/", Key/binary>>, Params}).

%% @doc Delete the specified key.
%% https://m2x.att.com/developer/documentation/v2/keys#Delete-Key
delete(Client, Key)         -> Client({delete, <<"/keys/", Key/binary>>}).

%% @doc Regenerate the specified key.
%% https://m2x.att.com/developer/documentation/v2/keys#Regenerate-Key
regenerate(Client, Key)     -> Client({post, <<"/keys/", Key/binary, "/regenerate">>}).
