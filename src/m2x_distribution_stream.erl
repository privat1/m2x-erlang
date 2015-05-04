
-module(m2x_distribution_stream).

%% API
-export([view/3                   ]).
-export([create/3, create/4       ]).
-export([          update/4       ]).
-export([delete/3                 ]).

%% @doc Get details of a specific data stream associated with an existing distribution.
%% https://m2x.att.com/developer/documentation/v2/distribution#View-Data-Stream
view(Client, Dist, Stream)                  -> Client({get, <<"/distributions/", Dist/binary, "/streams/", Stream/binary>>}).

%% @doc Update or create a data stream associated with the specified distribution.
%% https://m2x.att.com/developer/documentation/v2/distribution#Create-Update-Data-Stream
create(Client, Dist, Stream)                -> Client({put, <<"/distributions/", Dist/binary, "/streams/", Stream/binary>>}).
create(Client, Dist, Stream, Params)        -> Client({put, <<"/distributions/", Dist/binary, "/streams/", Stream/binary>>, Params}).
update(Client, Dist, Stream, Params)        -> Client({put, <<"/distributions/", Dist/binary, "/streams/", Stream/binary>>, Params}).

%% @doc Delete an existing data stream associated with a specific distribution.
%% https://m2x.att.com/developer/documentation/v2/distribution#Delete-Data-Stream
delete(Client, Dist, Stream)                -> Client({delete, <<"/distributions/", Dist/binary, "/streams/", Stream/binary>>}).
