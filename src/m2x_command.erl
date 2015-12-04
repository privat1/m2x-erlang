
-module(m2x_command).

%% API
-export([view/2]).

%% @doc Get details of a sent command.
%% https://m2x.att.com/developer/documentation/v2/commands#View-Command-Details
view(Client, Command) -> Client({get, <<"/commands/", Command/binary>>}).
