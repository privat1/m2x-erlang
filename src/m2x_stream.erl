
-module(m2x_stream).

%% API
-export([view/3                   ]).
-export([create/3, create/4       ]).
-export([          update/4       ]).
-export([delete/3                 ]).
-export([          update_value/4 ]).
-export([values/3, values/4       ]).
-export([          post_values/4  ]).
-export([          delete_values/4]).
-export([          sampling/4     ]).
-export([stats/3,  stats/4        ]).

%% @doc Get details of a specific data stream associated with an existing device.
%% https://m2x.att.com/developer/documentation/v2/device#View-Data-Stream
view(Client, Device, Stream)                  -> Client({get, <<"/devices/", Device/binary, "/streams/", Stream/binary>>}).

%% @doc Update or create a data stream associated with the specified device.
%% https://m2x.att.com/developer/documentation/v2/device#Create-Update-Data-Stream
create(Client, Device, Stream)                -> Client({put, <<"/devices/", Device/binary, "/streams/", Stream/binary>>}).
create(Client, Device, Stream, Params)        -> Client({put, <<"/devices/", Device/binary, "/streams/", Stream/binary>>, Params}).
update(Client, Device, Stream, Params)        -> Client({put, <<"/devices/", Device/binary, "/streams/", Stream/binary>>, Params}).

%% @doc Delete an existing data stream associated with a specific device.
%% https://m2x.att.com/developer/documentation/v2/device#Delete-Data-Stream
delete(Client, Device, Stream)                -> Client({delete, <<"/devices/", Device/binary, "/streams/", Stream/binary>>}).

%% @doc Update the current stream value of the specified stream.
%% https://m2x.att.com/developer/documentation/v2/device#Update-Data-Stream-Value
update_value(Client, Device, Stream, Params)  -> Client({put, <<"/devices/", Device/binary, "/streams/", Stream/binary, "/value">>, Params}).

%% @doc List values from an existing data stream associated with a specific device.
%% https://m2x.att.com/developer/documentation/v2/device#List-Data-Stream-Values
values(Client, Device, Stream)                -> Client({get, <<"/devices/", Device/binary, "/streams/", Stream/binary, "/values">>}).
values(Client, Device, Stream, Params)        -> Client({get, <<"/devices/", Device/binary, "/streams/", Stream/binary, "/values">>, Params}).

%% @doc Post timestamped values to an existing data stream associated with a specific device.
%% https://m2x.att.com/developer/documentation/v2/device#Post-Data-Stream-Values
post_values(Client, Device, Stream, Params)   -> Client({post, <<"/devices/", Device/binary, "/streams/", Stream/binary, "/values">>, Params}).

%% @doc Delete values in a stream by a date range.
%% https://m2x.att.com/developer/documentation/v2/device#Delete-Data-Stream-Values
delete_values(Client, Device, Stream, Params) -> Client({delete, <<"/devices/", Device/binary, "/streams/", Stream/binary, "/values">>, Params}).

%% @doc Sample values from an existing data stream associated with a specific device.
%% https://m2x.att.com/developer/documentation/v2/device#Data-Stream-Sampling
sampling(Client, Device, Stream, Params)      -> Client({get, <<"/devices/", Device/binary, "/streams/", Stream/binary, "/sampling">>, Params}).

%% @doc Return count, min, max, average and standard deviation stats for the values on an existing data stream.
%% https://m2x.att.com/developer/documentation/v2/device#Data-Stream-Sampling
stats(Client, Device, Stream)                 -> Client({get, <<"/devices/", Device/binary, "/streams/", Stream/binary, "/stats">>}).
stats(Client, Device, Stream, Params)         -> Client({get, <<"/devices/", Device/binary, "/streams/", Stream/binary, "/stats">>, Params}).
