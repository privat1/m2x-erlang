
-module(m2x_device).

%% API
-export([view/2                                  ]).
-export([                     update/3           ]).
-export([delete/2                                ]).
-export([log/2                                   ]).
-export([location/2                              ]).
-export([                     update_location/3  ]).
-export([streams/2                               ]).
-export([values/2,            values/3           ]).
-export([                     values_search/3    ]).
-export([values_export_csv/2, values_export_csv/3]).
-export([                     post_update/3      ]).
-export([                     post_updates/3     ]).

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

%% @doc Update the current location of the specified device.
%% https://m2x.att.com/developer/documentation/v2/device#Update-Device-Location
update_location(Client, Device, Params)   -> Client({put, <<"/devices/", Device/binary, "/location">>, Params}).

%% @doc Retrieve list of data streams associated with the specified device.
%% https://m2x.att.com/developer/documentation/v2/device#List-Data-Streams
streams(Client, Device)                   -> Client({get, <<"/devices/", Device/binary, "/streams">>}).

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
