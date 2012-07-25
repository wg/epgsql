%%% Copyright (C) 2009 - Will Glozer.  All rights reserved.

-module(pgsql_sock).

-behavior(gen_server).

-export([start_link/4, send/2, send/3, cancel/3]).
-export([decode_string/1, lower_atom/1]).

-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([init/1, code_change/3, terminate/2]).

-include("pgsql.hrl").

-record(state, {c, mod, sock, tail}).

-define(int16, 1/big-signed-unit:16).
-define(int32, 1/big-signed-unit:32).

%% -- client interface --

start_link(C, Host, Username, Opts) ->
    gen_server:start_link(?MODULE, [C, Host, Username, Opts], []).

send(S, Type, Data) ->
    Bin = iolist_to_binary(Data),
    Msg = <<Type:8, (byte_size(Bin) + 4):?int32, Bin/binary>>,
    gen_server:cast(S, {send, Msg}).

send(S, Data) ->
    Bin = iolist_to_binary(Data),
    Msg = <<(byte_size(Bin) + 4):?int32, Bin/binary>>,
    gen_server:cast(S, {send, Msg}).

cancel(S, Pid, Key) ->
    gen_server:cast(S, {cancel, Pid, Key}).

%% -- gen_server implementation --

init([C, Host, Username, Opts]) ->
    process_flag(trap_exit, true),

    Opts2 = ["user", 0, Username, 0],
    Opts3 = 
    case proplists:get_value(database, Opts, undefined) of
        undefined -> Opts2;
        Database  -> [Opts2 | ["database", 0, Database, 0]]
    end,

    Port = proplists:get_value(port, Opts, 5432),
    SockOpts = [{active, false}, {packet, raw}, binary, {nodelay, true}],
    {ok, S} = gen_tcp:connect(Host, Port, SockOpts),

    State = #state{
      c    = C,
      mod  = gen_tcp,
      sock = S,
      tail = <<>>},

    State2 = 
    case proplists:get_value(ssl, Opts) of
        T when T == true; T == required ->
            ok = gen_tcp:send(S, <<8:?int32, 80877103:?int32>>),
            {ok, <<Code>>} = gen_tcp:recv(S, 1),
            start_ssl(Code, T, Opts, State);
        _ ->
            State
    end,

    setopts(State2, [{active, true}]),
    send(self(), [<<196608:32>>, Opts3, 0]),
    {ok, State2}.

handle_call(Call, _From, State) ->
    {stop, {unsupported_call, Call}, State}.

handle_cast({send, Data}, State) ->
    #state{mod = Mod, sock = Sock} = State,
    ok = Mod:send(Sock, Data),
    {noreply, State};

handle_cast({cancel, Pid, Key}, State) ->
    {ok, {Addr, Port}} = inet:peername(State#state.sock),
    SockOpts = [{active, false}, {packet, raw}, binary],
    {ok, Sock} = gen_tcp:connect(Addr, Port, SockOpts),
    Msg = <<16:?int32, 80877102:?int32, Pid:?int32, Key:?int32>>,
    ok = gen_tcp:send(Sock, Msg),
    gen_tcp:close(Sock),
    {noreply, State};

handle_cast(Cast, State) ->
    {stop, {unsupported_cast, Cast}, State}.

handle_info({_, _Sock, Data}, #state{tail = Tail} = State) ->
    State2 = decode(<<Tail/binary, Data/binary>>, State),
    {noreply, State2};

handle_info({Closed, _Sock}, State)
  when Closed == tcp_closed; Closed == ssl_closed ->
    {stop, sock_closed, State};

handle_info({Error, _Sock, Reason}, State)
  when Error == tcp_error; Error == ssl_error ->
    {stop, {sock_error, Reason}, State};

handle_info({'EXIT', _Pid, Reason}, State) ->
    {stop, Reason, State};

handle_info(Info, State) ->
    {stop, {unsupported_info, Info}, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% -- internal functions --

start_ssl($S, _Flag, Opts, State) ->
    #state{sock = S1} = State,
    case ssl:connect(S1, Opts) of
        {ok, S2}        -> State#state{mod = ssl, sock = S2};
        {error, Reason} -> exit({ssl_negotiation_failed, Reason})
    end;

start_ssl($N, Flag, _Opts, State) ->
    case Flag of
        true     -> State;
        required -> exit(ssl_not_available)
    end.

setopts(#state{mod = Mod, sock = Sock}, Opts) ->
    case Mod of
        gen_tcp -> inet:setopts(Sock, Opts);
        ssl     -> ssl:setopts(Sock, Opts)
    end.

decode(<<Type:8, Len:?int32, Rest/binary>>, #state{c = C} = State) ->
    Len2 = Len - 4,
    <<Data:Len2/binary, Tail/binary>> = Rest,
    case Type of
        $N ->
           gen_fsm:send_all_state_event(C, {notice, decode_error(Data)}),
           decode(Tail, State);
        $S ->
           [Name, Value] = decode_strings(Data),
           gen_fsm:send_all_state_event(C, {parameter_status, Name, Value}),
           decode(Tail, State);
        $E ->
           gen_fsm:send_event(C, {error, decode_error(Data)}),
           decode(Tail, State);
        $A ->
           <<Pid:?int32, Strings/binary>> = Data,
           [Channel, Payload] = 
           case decode_strings(Strings) of
               [Chan]          -> [Chan, <<>>];
               ChanWithPayload -> ChanWithPayload
           end,
           gen_fsm:send_all_state_event(C, {notification, Channel, Pid, Payload}),
           decode(Tail, State);
        _ ->
           gen_fsm:send_event(C, {Type, Data}),
           decode(Tail, State)
    end;
decode(Bin, State) ->
    State#state{tail = Bin}.


%% decode a single null-terminated string
decode_string(Bin) ->
    [H, T] = binary:split(Bin, <<0>>),
    {H, T}.

%% decode multiple null-terminated string
decode_strings(<<>>) ->                              
        [];                                                  
decode_strings(Bin) ->                               
        binary:split(trim_last_zero(Bin), <<0>>, [global]).  
                                                         
trim_last_zero(Bin) ->                                   
        binary:part(Bin, 0, byte_size(Bin)-1).               


%% decode field
%% Works, when Type =/= 0 only.
decode_fields(Bin) ->
    RawFields = binary:split(Bin, <<0>>, [global, trim]),
    [{Type, Str} || <<Type, Str/binary>> <- RawFields].


%% decode ErrorResponse
decode_error(Bin) ->
    Fields = decode_fields(Bin),
    Error = #error{
      severity = lower_atom(proplists:get_value($S, Fields)),
      code     = proplists:get_value($C, Fields),
      message  = proplists:get_value($M, Fields),
      extra    = decode_error_extra(Fields)},
    Error.

decode_error_extra(Fields) ->
    Types = [{$D, detail}, {$H, hint}, {$P, position}],
    decode_error_extra(Types, Fields, []).

decode_error_extra([], _Fields, Extra) ->
    Extra;
decode_error_extra([{Type, Name} | T], Fields, Extra) ->
    case proplists:get_value(Type, Fields) of
        undefined -> decode_error_extra(T, Fields, Extra);
        Value     -> decode_error_extra(T, Fields, [{Name, Value} | Extra])
    end.

lower_atom(Str) when is_binary(Str) ->
    lower_atom(binary_to_list(Str));
lower_atom(Str) when is_list(Str) ->
    list_to_atom(string:to_lower(Str)).
