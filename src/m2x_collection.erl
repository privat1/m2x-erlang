
-module(m2x_collection).

%% API
-export([view/2           ]).
-export([         update/3]).
-export([delete/2         ]).

%% @doc Get details of an existing collection.
%% https://m2x.att.com/developer/documentation/v2/collections#View-Collection-Details
view(Client, Coll)                    -> Client({get, <<"/collections/", Coll/binary>>}).

%% @doc Update an existing collection's information.
%% https://m2x.att.com/developer/documentation/v2/collections#Update-Collection-Details
update(Client, Coll, Params)          -> Client({put, <<"/collections/", Coll/binary>>, Params}).

%% @doc Delete an existing device collection.
%% https://m2x.att.com/developer/documentation/v2/collections#Delete-Collection
delete(Client, Coll)                  -> Client({delete, <<"/collections/", Coll/binary>>}).
