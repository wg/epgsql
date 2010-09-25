%%% Copyright (C) 2010 - Anton Lebedevich.  All rights reserved.

-module(pgsql_text).

-export([decode/2]).

decode('_int4', Raw) ->
    Raw;

decode(_Other, Value) ->
    Value.
