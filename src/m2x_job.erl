
-module(m2x_job).

%% API
-export([view/2]).

%% @doc Get details of the specified job.
%% https://m2x.att.com/developer/documentation/v2/jobs#View-Job-Details
view(Client, Job) -> Client({get, <<"/jobs/", Job/binary>>}).
