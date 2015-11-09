
-module(m2x).
-define(VERSION, <<"1.1.0">>).
-vsn(?VERSION).

%% API
-export([version/0]).
-export([client/1, client/2, client/3, client/4]).

-export([devices/1,         devices/2            ]).
-export([search_devices/1,  search_devices/2     ]).
-export([devices_catalog/1, devices_catalog/2    ]).
-export([devices_tags/1                          ]).
-export([                   create_device/2      ]).
-export([distributions/1                         ]).
-export([                   create_distribution/2]).
-export([collections/1                           ]).
-export([                   create_collection/2  ]).
-export([keys/1,            keys/2               ]).
-export([                   create_key/2         ]).

%% Return the version number of this module, as a binary string
version() -> ?VERSION.

%% Convenience wrappers for m2x_client:create function.
client(A)          -> m2x_client:create(A).
client(A, B)       -> m2x_client:create(A, B).
client(A, B, C)    -> m2x_client:create(A, B, C).
client(A, B, C, D) -> m2x_client:create(A, B, C, D).

%% @doc Retrieve the list of devices accessible by the authenticated API key.
%% https://m2x.att.com/developer/documentation/v2/device#List-Devices
devices(Client)                     -> Client({get, <<"/devices">>}).
devices(Client, Params)             -> Client({get, <<"/devices">>, Params}).

%% @doc Retrieve the list of devices accessible by the authenticated API key that meet the search criteria.
%% https://m2x.att.com/developer/documentation/v2/device#Search-Devices
search_devices(Client)              -> Client({get, <<"/devices/search">>}).
search_devices(Client, Params)      -> Client({get, <<"/devices/search">>, Params}).

%% @doc Search the catalog of public devices.
%% https://m2x.att.com/developer/documentation/v2/device#List-Search-Public-Devices-Catalog
devices_catalog(Client)             -> Client({get, <<"/devices/catalog">>}).
devices_catalog(Client, Params)     -> Client({get, <<"/devices/catalog">>, Params}).

%% @doc Retrieve the list of device tags for the authenticated user.
%% https://m2x.att.com/developer/documentation/v2/device#List-Device-Tags
devices_tags(Client)                -> Client({get, <<"/devices/tags">>}).

%% @doc Create a new device.
%% https://m2x.att.com/developer/documentation/v2/device#Create-Device
create_device(Client, Params)       -> Client({post, <<"/devices">>, Params}).

%% @doc Retrieve list of device distributions accessible by the authenticated API key.
%% https://m2x.att.com/developer/documentation/v2/distribution#List-Distributions
distributions(Client)               -> Client({get, <<"/distributions">>}).

%% @doc Create a new device distribution.
%% https://m2x.att.com/developer/documentation/v2/distribution#Create-Distribution
create_distribution(Client, Params) -> Client({post, <<"/distributions">>, Params}).

%% @doc Retrieve list of device collections accessible by the authenticated API key.
%% https://m2x.att.com/developer/documentation/v2/collections#List-Collections
collections(Client)                 -> Client({get, <<"/collections">>}).

%% @doc Create a new device collection.
%% https://m2x.att.com/developer/documentation/v2/collections#Create-Collection
create_collection(Client, Params)   -> Client({post, <<"/collections">>, Params}).

%% @doc Retrieve list of keys associated with the specified account.
%% https://m2x.att.com/developer/documentation/v2/keys#List-Search-Keys
keys(Client)                        -> Client({get, <<"/keys">>}).
keys(Client, Params)                -> Client({get, <<"/keys">>, Params}).

%% @doc Create a new key associated with the specified account.
%% https://m2x.att.com/developer/documentation/v2/keys#Create-Key
create_key(Client, Params)          -> Client({post, <<"/keys">>, Params}).
