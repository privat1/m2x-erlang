
-module(m2x_device_trigger).

%% API
-export([view/3           ]).
-export([         update/4]).
-export([delete/3         ]).
-export([test/3           ]).

%% @doc Get details of a specific trigger associated with an existing device.
%% https://m2x.att.com/developer/documentation/v2/device#View-Trigger
view(Client, Device, Trigger)           -> Client({get, <<"/devices/", Device/binary, "/triggers/", Trigger/binary>>}).

%% @doc Update an existing trigger associated with the specified device.
%% https://m2x.att.com/developer/documentation/v2/device#Update-Trigger
update(Client, Device, Trigger, Params) -> Client({put, <<"/devices/", Device/binary, "/triggers/", Trigger/binary>>, Params}).

%% @docDelete an existing trigger associated with a specific device.
%% https://m2x.att.com/developer/documentation/v2/device#Delete-Trigger
delete(Client, Device, Trigger)         -> Client({delete, <<"/devices/", Device/binary, "/triggers/", Trigger/binary>>}).

%% @doc Test the specified trigger by firing it with a fake value.
%% https://m2x.att.com/developer/documentation/v2/device#Test-Trigger
test(Client, Device, Trigger)           -> Client({post, <<"/devices/", Device/binary, "/triggers/", Trigger/binary, "/test">>}).
