
-module(m2x_device_command).

%% API
-export([view/3              ]).
-export([process/3, process/4]).
-export([reject/3,  reject/4 ]).

%% @doc Get details of a command as received by this device.
%% https://m2x.att.com/developer/documentation/v2/commands#Device-s-View-of-Command-Details
view(Client, Device, Command)            -> Client({get, <<"/devices/", Device/binary, "/commands/", Command/binary>>}).

%% @doc Mark a command as processed by this device.
%% https://m2x.att.com/developer/documentation/v2/device#Device-Marks-a-Command-as-Processed
process(Client, Device, Command)         -> Client({post, <<"/devices/", Device/binary, "/commands/", Command/binary, "/process">>}).
process(Client, Device, Command, Params) -> Client({post, <<"/devices/", Device/binary, "/commands/", Command/binary, "/process">>, Params}).

%% @doc Mark a command as rejected by this device.
%% https://m2x.att.com/developer/documentation/v2/device#Device-Marks-a-Command-as-Rejected
reject(Client, Device, Command)          -> Client({post, <<"/devices/", Device/binary, "/commands/", Command/binary, "/reject">>}).
reject(Client, Device, Command, Params)  -> Client({post, <<"/devices/", Device/binary, "/commands/", Command/binary, "/reject">>, Params}).
