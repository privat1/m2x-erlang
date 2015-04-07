
-module(m2x).
-define(VERSION, <<"0.0.1">>).
-vsn(?VERSION).

%% API
-export([version/0]).
-export([client/1, client/2, client/3, client/4]).

%% Return the version number of this module, as a binary string
version() -> ?VERSION.

%% Convenience wrappers for m2x_client:create function.
client(A)          -> m2x_client:create(A).
client(A, B)       -> m2x_client:create(A, B).
client(A, B, C)    -> m2x_client:create(A, B, C).
client(A, B, C, D) -> m2x_client:create(A, B, C, D).
