
-module(m2x_distribution_trigger).

%% API
-export([view/3           ]).
-export([         update/4]).
-export([delete/3         ]).
-export([test/3           ]).

%% @doc Get details of a specific trigger associated with an existing distribution.
%% https://m2x.att.com/developer/documentation/v2/distribution#View-Trigger
view(Client, Dist, Trigger)           -> Client({get, <<"/distributions/", Dist/binary, "/triggers/", Trigger/binary>>}).

%% @doc Update an existing trigger associated with the specified distribution.
%% https://m2x.att.com/developer/documentation/v2/distribution#Update-Trigger
update(Client, Dist, Trigger, Params) -> Client({put, <<"/distributions/", Dist/binary, "/triggers/", Trigger/binary>>, Params}).

%% @docDelete an existing trigger associated with a specific distribution.
%% https://m2x.att.com/developer/documentation/v2/distribution#Delete-Trigger
delete(Client, Dist, Trigger)         -> Client({delete, <<"/distributions/", Dist/binary, "/triggers/", Trigger/binary>>}).

%% @doc Test the specified trigger by firing it with a fake value.
%% https://m2x.att.com/developer/documentation/v2/distribution#Test-Trigger
test(Client, Dist, Trigger)           -> Client({post, <<"/distributions/", Dist/binary, "/triggers/", Trigger/binary, "/test">>}).
