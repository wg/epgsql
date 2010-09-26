%%% Copyright (C) 2010 - Anton Lebedevich.  All rights reserved.

-module(pgsql_text).

-export([encode/1, encode/2, decode/2]).

-include("pgsql_protocol.hrl").

encode('_int4', List) when is_list(List) ->
    encode_int_array(lists:reverse(List), []);

encode(_Type, Value) -> encode(Value).

encode(A) when is_atom(A)    -> encode(atom_to_list(A));
encode(B) when is_binary(B)  -> <<(byte_size(B)):?int32, B/binary>>;
encode(I) when is_integer(I) -> encode(integer_to_list(I));
encode(F) when is_float(F)   -> encode(float_to_list(F));
encode(L) when is_list(L)    -> 
    Bin = list_to_binary(L),
    <<(byte_size(Bin)):?int32, Bin/binary>>.

encode_int_array([], []) ->
    encode_int_array([], [$}]);

encode_int_array([Int | Rest], []) ->
    encode_int_array(Rest, [int_to_element(Int), $}]);

encode_int_array([Int | Rest], Accumulator) when is_integer(Int) ->
    encode_int_array(Rest, [[int_to_element(Int), $,] | Accumulator]);

encode_int_array([], Accumulator) ->
    encode([${ | Accumulator]).

int_to_element(Int) when is_integer(Int) ->
    list_to_binary(integer_to_list(Int)).

decode('_int4', Raw) ->
    decode_int_array(Raw);

decode(_Other, Value) ->
    Value.

decode_int_array(<<${, Rest/binary>>) ->
    decode_int_array(Rest, [], []).

decode_int_array(<<$,, Rest/binary>>, Element, Accumulator) ->
    decode_int_array(Rest, [], [element_to_int(Element) | Accumulator]);

decode_int_array(<<$}>>, Element, Accumulator) ->
    lists:reverse([element_to_int(Element) | Accumulator]);
    
decode_int_array(<<Char, Rest/binary>>, Element, Accumulator) ->
    decode_int_array(Rest, [Char | Element], Accumulator).

element_to_int(Element) ->
    list_to_integer(lists:reverse(Element)).
