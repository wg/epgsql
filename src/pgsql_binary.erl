%%% Copyright (C) 2008 - Will Glozer.  All rights reserved.

-module(pgsql_binary).

-export([encode/2, decode/2, supports/1]).

-type cidr() :: {inet:ip_address(), integer()}.

-export_type([cidr/0]).

-define(int32, 1/big-signed-unit:32).
-define(datetime, (get(datetime_mod))).

encode(_Any, null)                          -> <<-1:?int32>>;
encode(bool, true)                          -> <<1:?int32, 1:1/big-signed-unit:8>>;
encode(bool, false)                         -> <<1:?int32, 0:1/big-signed-unit:8>>;
encode(boolean, true)                       -> <<1:?int32, 1:1/big-signed-unit:8>>;
encode(boolean, false)                      -> <<1:?int32, 0:1/big-signed-unit:8>>;
encode(int2, N)                             -> <<2:?int32, N:1/big-signed-unit:16>>;
encode(int4, N)                             -> <<4:?int32, N:1/big-signed-unit:32>>;
encode(integer, N)                          -> <<4:?int32, N:1/big-signed-unit:32>>;
encode(int8, N)                             -> <<8:?int32, N:1/big-signed-unit:64>>;
encode(bigint, N)                           -> <<8:?int32, N:1/big-signed-unit:64>>;
encode(float4, N)                           -> <<4:?int32, N:1/big-float-unit:32>>;
encode(float8, N)                           -> <<8:?int32, N:1/big-float-unit:64>>;
encode(point, {X, Y})                       -> <<16:?int32, X:1/big-float-unit:64, Y:1/big-float-unit:64>>;
encode(bpchar, C) when is_integer(C)        -> <<1:?int32, C:1/big-unsigned-unit:8>>;
encode(bpchar, B) when is_binary(B)         -> <<(byte_size(B)):?int32, B/binary>>;
encode(character, X)                        -> encode(bpchar, X);
encode(time = Type, B)                      -> ?datetime:encode(Type, B);
encode(timetz = Type, B)                    -> ?datetime:encode(Type, B);
encode(date = Type, B)                      -> ?datetime:encode(Type, B);
encode(timestamp = Type, B)                 -> ?datetime:encode(Type, B);
encode(timestamptz = Type, B)               -> ?datetime:encode(Type, B);
encode(interval = Type, B)                  -> ?datetime:encode(Type, B);
encode(bytea, B) when is_binary(B)          -> <<(byte_size(B)):?int32, B/binary>>;
encode(text, B) when is_binary(B)           -> <<(byte_size(B)):?int32, B/binary>>;
encode(varchar, B) when is_binary(B)        -> <<(byte_size(B)):?int32, B/binary>>;
encode(inet, B)                             -> encode(bytea, encode_net(B));
encode(cidr, B)                             -> encode(bytea, encode_net(B));
encode(boolarray, L) when is_list(L)        -> encode_array(bool, L);
encode(int2array, L) when is_list(L)        -> encode_array(int2, L);
encode(int4array, L) when is_list(L)        -> encode_array(int4, L);
encode(int8array, L) when is_list(L)        -> encode_array(int8, L);
encode(float4array, L) when is_list(L)      -> encode_array(float4, L);
encode(float8array, L) when is_list(L)      -> encode_array(float8, L);
encode(chararray, L) when is_list(L)        -> encode_array(bpchar, L);
encode(varchararray, L) when is_list(L)     -> encode_array(varchar, L);
encode(textarray, L) when is_list(L)        -> encode_array(text, L);
encode(Type, L) when is_list(L)             -> encode(Type, list_to_binary(L));
encode(_Type, _Value)                       -> {error, unsupported}.

decode(bool, <<1:1/big-signed-unit:8>>)     -> true;
decode(bool, <<0:1/big-signed-unit:8>>)     -> false;
decode(boolean, <<1:1/big-signed-unit:8>>)  -> true;
decode(boolean, <<0:1/big-signed-unit:8>>)  -> false;
decode(bpchar, <<C:1/big-unsigned-unit:8>>) -> C;
decode(character, X)                        -> decode(bpchar, X);
decode(int2, <<N:1/big-signed-unit:16>>)    -> N;
decode(int4, <<N:1/big-signed-unit:32>>)    -> N;
decode(integer, <<N:1/big-signed-unit:32>>) -> N;
decode(int8, <<N:1/big-signed-unit:64>>)    -> N;
decode(bigint, <<N:1/big-signed-unit:64>>)  -> N;
decode(float4, <<N:1/big-float-unit:32>>)   -> N;
decode(float8, <<N:1/big-float-unit:64>>)   -> N;
decode(point, <<X:1/big-float-unit:64,
                Y:1/big-float-unit:64>>)    -> {X, Y};
decode(record, <<_:?int32, Rest/binary>>)   -> list_to_tuple(decode_record(Rest, []));
decode(time = Type, B)                      -> ?datetime:decode(Type, B);
decode(timetz = Type, B)                    -> ?datetime:decode(Type, B);
decode(date = Type, B)                      -> ?datetime:decode(Type, B);
decode(timestamp = Type, B)                 -> ?datetime:decode(Type, B);
decode(timestamptz = Type, B)               -> ?datetime:decode(Type, B);
decode(interval = Type, B)                  -> ?datetime:decode(Type, B);
decode(boolarray, B)                        -> decode_array(B);
decode(int2array, B)                        -> decode_array(B);
decode(int4array, B)                        -> decode_array(B);
decode(int8array, B)                        -> decode_array(B);
decode(float4array, B)                      -> decode_array(B);
decode(float8array, B)                      -> decode_array(B);
decode(chararray, B)                        -> decode_array(B);
decode(varchararray, B)                     -> decode_array(B);
decode(textarray, B)                        -> decode_array(B);
decode(inet, B)                             -> decode_net(B);
decode(cidr, B)                             -> decode_net(B);
decode(_Other, Bin)                         -> Bin.

encode_array(Type, A) ->
    {Data, {NDims, Lengths}} = encode_array(Type, A, 0, []),
    Oid  = pgsql_types:type2oid(Type),
    Lens = [<<N:?int32, 1:?int32>> || N <- lists:reverse(Lengths)],
    Hdr  = <<NDims:?int32, 0:?int32, Oid:?int32>>,
    Bin  = iolist_to_binary([Hdr, Lens, Data]),
    <<(byte_size(Bin)):?int32, Bin/binary>>.

encode_array(_Type, [], NDims, Lengths) ->
    {<<>>, {NDims, Lengths}};
encode_array(Type, [H | _] = Array, NDims, Lengths) when not is_list(H) ->
    F = fun(E, Len) -> {encode(Type, E), Len + 1} end,
    {Data, Len} = lists:mapfoldl(F, 0, Array),
    {Data, {NDims + 1, [Len | Lengths]}};
encode_array(Type, Array, NDims, Lengths) ->
    Lengths2 = [length(Array) | Lengths],
    F = fun(A2, {_NDims, _Lengths}) -> encode_array(Type, A2, NDims, Lengths2) end,
    {Data, {NDims2, Lengths3}} = lists:mapfoldl(F, {NDims, Lengths2}, Array),
    {Data, {NDims2 + 1, Lengths3}}.

decode_array(<<NDims:?int32, _HasNull:?int32, Oid:?int32, Rest/binary>>) ->
    {Dims, Data} = erlang:split_binary(Rest, NDims * 2 * 4),
    Lengths = [Len || <<Len:?int32, _LBound:?int32>> <= Dims],
    Type = pgsql_types:oid2type(Oid),
    {Array, <<>>} = decode_array(Data, Type, Lengths),
    Array.

decode_array(Data, _Type, [])  ->
    {[], Data};
decode_array(Data, Type, [Len]) ->
    decode_elements(Data, Type, [], Len);
decode_array(Data, Type, [Len | T]) ->
    F = fun(_N, Rest) -> decode_array(Rest, Type, T) end,
    lists:mapfoldl(F, Data, lists:seq(1, Len)).

encode_net({{_, _, _, _} = Ip, Mask}) ->
    B = list_to_binary(tuple_to_list(Ip)),
    <<2, Mask, 1, (byte_size(B)), B/binary>>;
encode_net({{_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _} = Ip, Mask}) ->
    B = list_to_binary(tuple_to_list(Ip)),
    <<3, Mask, 1, (byte_size(B)), B/binary>>;
encode_net({_, _, _, _} = Ip) ->
    B = list_to_binary(tuple_to_list(Ip)),
    <<2, 32, 0, (byte_size(B)), B/binary>>;
encode_net({_, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _} = Ip) ->
    B = list_to_binary(tuple_to_list(Ip)),
    <<3, 128, 0, (byte_size(B)), B/binary>>.

decode_net(<<Family, Mask, IsCidr, Size, Bin/binary>>) ->
    {MaxMask, ProperSize} = case Family of
                                2 -> %% AF_INET
                                    {32, 4};
                                3 -> %% AF_INET6
                                    {128, 16}
                            end,
    IpTuple = list_to_tuple(binary_to_list(Bin)),
    case Size =:= ProperSize orelse Mask > MaxMask of
        false ->
            io:format("Ivalid value for inet type received"),
            {};
        true ->
            case IsCidr =:= 1 of
                true ->
                    {IpTuple, Mask};
                false ->
                    IpTuple
            end
    end.

decode_elements(Rest, _Type, Acc, 0) ->
    {lists:reverse(Acc), Rest};
decode_elements(<<-1:?int32, Rest/binary>>, Type, Acc, N) ->
    decode_elements(Rest, Type, [null | Acc], N - 1);
decode_elements(<<Len:?int32, Value:Len/binary, Rest/binary>>, Type, Acc, N) ->
    Value2 = decode(Type, Value),
    decode_elements(Rest, Type, [Value2 | Acc], N - 1).

decode_record(<<>>, Acc) ->
    lists:reverse(Acc);
decode_record(<<_Type:?int32, -1:?int32, Rest/binary>>, Acc) ->
    decode_record(Rest, [null | Acc]);
decode_record(<<Type:?int32, Len:?int32, Value:Len/binary, Rest/binary>>, Acc) ->
    Value2 = decode(pgsql_types:oid2type(Type), Value),
    decode_record(Rest, [Value2 | Acc]).

supports(bool)         -> true;
supports(boolean)      -> true;
supports(bpchar)       -> true;
supports(character)    -> true;
supports(int2)         -> true;
supports(int4)         -> true;
supports(int8)         -> true;
supports(float4)       -> true;
supports(float8)       -> true;
supports(bytea)        -> true;
supports(text)         -> true;
supports(varchar)      -> true;
supports(record)       -> true;
supports(date)         -> true;
supports(time)         -> true;
supports(timetz)       -> true;
supports(timestamp)    -> true;
supports(timestamptz)  -> true;
supports(interval)     -> true;
supports(boolarray)    -> true;
supports(int2array)    -> true;
supports(int4array)    -> true;
supports(int8array)    -> true;
supports(float4array)  -> true;
supports(float8array)  -> true;
supports(chararray)    -> true;
supports(varchararray) -> true;
supports(textarray)    -> true;
supports(inet)         -> true;
supports(cidr)         -> true;
supports(point)         -> true;
supports(_Type)        -> false.
