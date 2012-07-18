%% @doc Utility module for common functions.
-module(pgsql_util).

-export([get_value/2,
         get_value/3]).

%% @doc Get value from a proplist
-spec get_value(term(), list()) -> term() | false.
get_value(Key, Proplist) ->
    case lists:keyfind(Key, 1, Proplist) of
        {Key, Value} ->
            Value;
        false ->
            false
    end.

%% @doc Get value from a proplist
%% Returns the default value if the key doesn't exist.
-spec get_value(term(), list(), term()) -> term().
get_value(Key, Proplist, Default) ->
    case lists:keyfind(Key, 1, Proplist) of
        {Key, Value} ->
            Value;
        false ->
            Default
    end.
