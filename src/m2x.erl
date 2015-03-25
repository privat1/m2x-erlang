
-module(m2x).

%% API
-export([client/1, client/2, client/3]).

%% Convenience wrappers for m2x_client:create function.
client(A)       -> m2x_client:create(A).
client(A, B)    -> m2x_client:create(A, B).
client(A, B, C) -> m2x_client:create(A, B, C).
