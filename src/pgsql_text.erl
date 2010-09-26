%%% Copyright (C) 2010 - Anton Lebedevich.  All rights reserved.

-module(pgsql_text).

-export([encode/1, encode/2, decode/2]).

-include("pgsql_protocol.hrl").

encode(_Type, Value) -> encode(Value).

encode(A) when is_atom(A)    -> encode(atom_to_list(A));
encode(B) when is_binary(B)  -> <<(byte_size(B)):?int32, B/binary>>;
encode(I) when is_integer(I) -> encode(integer_to_list(I));
encode(F) when is_float(F)   -> encode(float_to_list(F));
encode(L) when is_list(L)    -> 
    Bin = list_to_binary(L),
    <<(byte_size(Bin)):?int32, Bin/binary>>.

decode('_int4', Raw) ->
    int4_array(Raw);

decode(_Other, Value) ->
    Value.

int4_array(<<${, Rest/binary>>) ->
    int4_array(Rest, [], []).

int4_array(<<$,, Rest/binary>>, Element, Accumulator) ->
    int4_array(Rest, [], [element_to_int4(Element) | Accumulator]);

int4_array(<<$}>>, Element, Accumulator) ->
    lists:reverse([element_to_int4(Element) | Accumulator]);
    
int4_array(<<Char, Rest/binary>>, Element, Accumulator) ->
    int4_array(Rest, [Char | Element], Accumulator).

element_to_int4(Element) ->
    list_to_integer(lists:reverse(Element)).
