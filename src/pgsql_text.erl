%%% Copyright (C) 2010 - Anton Lebedevich.  All rights reserved.

-module(pgsql_text).

-export([decode/2]).

decode('_int4', Raw) ->
    int4_array(Raw);

decode(_Other, Value) ->
    Value.

int4_array(<<${, Rest/binary>>) ->
    int4_array(Rest, [], []).

int4_array(<<$,, Rest/binary>>, Field, Accumulator) ->
    int4_array(Rest, [], [field_to_integer(Field) | Accumulator]);

int4_array(<<$}>>, Field, Accumulator) ->
    lists:reverse([field_to_integer(Field) | Accumulator]);
    
int4_array(<<Char, Rest/binary>>, Field, Accumulator) ->
    int4_array(Rest, [Char | Field], Accumulator).

field_to_integer(Field) ->
    list_to_integer(lists:reverse(Field)).
