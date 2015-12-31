
-module(m2x_device).

%% API
-export([view/2                                   ]).
-export([                     update/3            ]).
-export([delete/2                                 ]).
-export([log/2                                    ]).
-export([location/2                               ]).
-export([location_history/2                       ]).
-export([                     location_history/3  ]).
-export([                     update_location/3   ]).
-export([metadata/2                               ]).
-export([                     update_metadata/3   ]).
-export([get_metadata_field/3                     ]).
-export([                     set_metadata_field/4]).
-export([streams/2                                ]).
-export([commands/2                               ]).
-export([values/2,            values/3            ]).
-export([                     values_search/3     ]).
-export([values_export_csv/2, values_export_csv/3 ]).
-export([                     post_update/3       ]).
-export([                     post_updates/3      ]).

%% @doc Get details of an existing device.
%% https://m2x.att.com/developer/documentation/v2/device#View-Device-Details
view(Client, Device)                      -> Client({get, <<"/devices/", Device/binary>>}).

%% @doc Update an existing device's information.
%% https://m2x.att.com/developer/documentation/v2/device#Update-Device-Details
update(Client, Device, Params)            -> Client({put, <<"/devices/", Device/binary>>, Params}).

%% @doc Delete an existing device.
%% https://m2x.att.com/developer/documentation/v2/device#Delete-Device
delete(Client, Device)                    -> Client({delete, <<"/devices/", Device/binary>>}).

%% @doc Retrieve list of HTTP requests received lately by the specified device.
%% https://m2x.att.com/developer/documentation/v2/device#View-Request-Log
log(Client, Device)                       -> Client({get, <<"/devices/", Device/binary, "/log">>}).

%% @doc Get location details of an existing device.
%% https://m2x.att.com/developer/documentation/v2/device#Read-Device-Location
location(Client, Device)                  -> Client({get, <<"/devices/", Device/binary, "/location">>}).

%% @doc Get location history details of an existing device.
%% https://m2x.att.com/developer/documentation/v2/device#Read-Device-Location-History
location_history(Client, Device)          -> Client({get, <<"/devices/", Device/binary, "/location/waypoints">>}).
location_history(Client, Device, Params)  -> Client({get, <<"/devices/", Device/binary, "/location/waypoints">>, Params}).

%% @doc Update the current location of the specified device.
%% https://m2x.att.com/developer/documentation/v2/device#Update-Device-Location
update_location(Client, Device, Params)   -> Client({put, <<"/devices/", Device/binary, "/location">>, Params}).

%% @doc Get the custom metadata of the specified device.
%% https://m2x.att.com/developer/documentation/v2/device#Read-Device-Metadata
metadata(Client, Device)                  -> Client({get, <<"/devices/", Device/binary, "/metadata">>}).

%% @doc Update the custom metadata of the specified device.
%% https://m2x.att.com/developer/documentation/v2/device#Update-Device-Metadata
update_metadata(Client, Device, Params)   -> Client({put, <<"/devices/", Device/binary, "/metadata">>, Params}).

%% @doc Get the value of a single field of the custom metadata.
%% https://m2x.att.com/developer/documentation/v2/device#Read-Device-Metadata-Field
get_metadata_field(Client, Device, K)     -> Client({get, <<"/devices/", Device/binary, "/metadata/", K/binary>>}).

%% @doc Update the value of a single field of the custom metadata.
%% https://m2x.att.com/developer/documentation/v2/device#Update-Device-Metadata-Field
set_metadata_field(Client, Device, K, V)  -> Client({put, <<"/devices/", Device/binary, "/metadata/", K/binary>>, [{<<"value">>, V}]}).

%% @doc Retrieve list of data streams associated with the specified device.
%% https://m2x.att.com/developer/documentation/v2/device#List-Data-Streams
streams(Client, Device)                   -> Client({get, <<"/devices/", Device/binary, "/streams">>}).

%% @doc Retrieve list of recent commands sent to the specified device.
%% https://m2x.att.com/developer/documentation/v2/device#Device-s-List-of-Received-Commands
commands(Client, Device)                  -> Client({get, <<"/devices/", Device/binary, "/commands">>}).

%% @doc List values from all data streams associated with a specific device.
%% https://m2x.att.com/developer/documentation/v2/device#List-Values-from-all-Data-Streams-of-a-Device
values(Client, Device)                    -> Client({get, <<"/devices/", Device/binary, "/values">>}).
values(Client, Device, Params)            -> Client({get, <<"/devices/", Device/binary, "/values">>, Params}).

%% @doc Search values from all data streams associated with a specific device.
%% https://m2x.att.com/developer/documentation/v2/device#Search-Values-from-all-Data-Streams-of-a-Device
values_search(Client, Device, Params)     -> Client({get, <<"/devices/", Device/binary, "/values/search">>, Params}).

%% @doc Export values from all data streams associated with a specific device.
%%  https://m2x.att.com/developer/documentation/v2/device#Export-Values-from-all-Data-Streams-of-a-Device
values_export_csv(Client, Device)         -> Client({get, <<"/devices/", Device/binary, "/values/export.csv">>}).
values_export_csv(Client, Device, Params) -> Client({get, <<"/devices/", Device/binary, "/values/export.csv">>, Params}).

%% @doc Post single values to multiple streams.
%% https://m2x.att.com/developer/documentation/v2/device#Post-Device-Updates--Single-Values-to-Multiple-Streams-
post_update(Client, Device, Params)       -> Client({post, <<"/devices/", Device/binary, "/update">>, Params}).

%% @doc Post multiple values to multiple streams.
%% https://m2x.att.com/developer/documentation/v2/device#Post-Device-Updates--Multiple-Values-to-Multiple-Streams-
post_updates(Client, Device, Params)      -> Client({post, <<"/devices/", Device/binary, "/updates">>, Params}).
