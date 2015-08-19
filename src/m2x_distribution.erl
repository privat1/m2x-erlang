
-module(m2x_distribution).

%% API
-export([view/2                     ]).
-export([           update/3        ]).
-export([delete/2                   ]).
-export([devices/2                  ]).
-export([           create_device/3 ]).
-export([streams/2                  ]).

%% @doc Get details of an existing distribution.
%% https://m2x.att.com/developer/documentation/v2/distribution#View-Distribution-Details
view(Client, Dist)                    -> Client({get, <<"/distributions/", Dist/binary>>}).

%% @doc Update an existing distribution's information.
%% https://m2x.att.com/developer/documentation/v2/distribution#Update-Distribution-Details
update(Client, Dist, Params)          -> Client({put, <<"/distributions/", Dist/binary>>, Params}).

%% @doc Delete an existing device distribution.
%% https://m2x.att.com/developer/documentation/v2/distribution#Delete-Distribution
delete(Client, Dist)                  -> Client({delete, <<"/distributions/", Dist/binary>>}).

%% @doc Retrieve list of devices added to the specified distribution.
%% https://m2x.att.com/developer/documentation/v2/distribution#List-Devices-from-an-existing-Distribution
devices(Client, Dist)                 -> Client({get, <<"/distributions/", Dist/binary, "/devices">>}).

%% @doc Add a new device to an existing distribution.
%% https://m2x.att.com/developer/documentation/v2/distribution#Add-Device-to-an-existing-Distribution
create_device(Client, Dist, Params)  -> Client({post, <<"/distributions/", Dist/binary, "/devices">>, Params}).

%% @doc Retrieve list of data streams associated with the specified distribution.
%% https://m2x.att.com/developer/documentation/v2/distribution#List-Data-Streams
streams(Client, Dist)                 -> Client({get, <<"/distributions/", Dist/binary, "/streams">>}).
