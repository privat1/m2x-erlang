
-module(m2x_collection).

%% API
-export([view/2                                   ]).
-export([                     update/3            ]).
-export([delete/2                                 ]).
-export([metadata/2                               ]).
-export([                     update_metadata/3   ]).
-export([get_metadata_field/3                     ]).
-export([                     set_metadata_field/4]).

%% @doc Get details of an existing collection.
%% https://m2x.att.com/developer/documentation/v2/collections#View-Collection-Details
view(Client, Coll)                    -> Client({get, <<"/collections/", Coll/binary>>}).

%% @doc Update an existing collection's information.
%% https://m2x.att.com/developer/documentation/v2/collections#Update-Collection-Details
update(Client, Coll, Params)          -> Client({put, <<"/collections/", Coll/binary>>, Params}).

%% @doc Delete an existing device collection.
%% https://m2x.att.com/developer/documentation/v2/collections#Delete-Collection
delete(Client, Coll)                  -> Client({delete, <<"/collections/", Coll/binary>>}).

%% @doc Get the custom metadata of the specified collection.
%% https://m2x.att.com/developer/documentation/v2/collections#Read-Collection-Metadata
metadata(Client, Dist)                 -> Client({get, <<"/collections/", Dist/binary, "/metadata">>}).

%% @doc Update the custom metadata of the specified collection.
%% https://m2x.att.com/developer/documentation/v2/collections#Update-Collection-Metadata
update_metadata(Client, Dist, Params)  -> Client({put, <<"/collections/", Dist/binary, "/metadata">>, Params}).

%% @doc Get the value of a single field of the custom metadata.
%% https://m2x.att.com/developer/documentation/v2/collections#Read-Collection-Metadata-Field
get_metadata_field(Client, Dist, K)    -> Client({get, <<"/collections/", Dist/binary, "/metadata/", K/binary>>}).

%% @doc Update the value of a single field of the custom metadata.
%% https://m2x.att.com/developer/documentation/v2/collections#Update-Collection-Metadata-Field
set_metadata_field(Client, Dist, K, V) -> Client({put, <<"/collections/", Dist/binary, "/metadata/", K/binary>>, [{<<"value">>, V}]}).
