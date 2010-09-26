%%% Copyright (C) 2010 - Anton Lebedevich.  All rights reserved.

-module(pgsql_text).

-export([decode/2]).

decode('_int4', Raw) ->
    int4_array(Raw);

decode(_Other, Value) ->
    Value.

int4_array(<<${, Rest/binary>>) ->
    int4_array(Rest, [], []).

int4_array(<<$,, Rest/binary>>, Element, Accumulator) ->
    int4_array(Rest, [], [field_to_integer(Element) | Accumulator]);

int4_array(<<$}>>, Element, Accumulator) ->
    lists:reverse([field_to_integer(Element) | Accumulator]);
    
int4_array(<<Char, Rest/binary>>, Element, Accumulator) ->
    int4_array(Rest, [Char | Element], Accumulator).

element_to_int4(Element) ->
    list_to_integer(lists:reverse(Element)).
