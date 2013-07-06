-module(computer_server).
-include("mqtt.hrl").
%% API
-export([start_link/0, run/1]).
-record(state, {socket, client_id, keep_alive_timer, reconnect_wait_time, mqtt_broker, mqtt_port, username, password}).

-define(CONNECTION_TIMEOUT, 10000).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    {ok, ClientId} = application:get_env(client_id),
    {ok, KeepAliveTimer} = application:get_env(keep_alive_timer),
    {ok, ReconnectWaitTime} = application:get_env(reconnect_wait_time),
    {ok, ServerHost} = application:get_env(mqtt_broker_host),
    {ok, ServerPort} = application:get_env(mqtt_broker_port),
    {ok, UserName} = application:get_env(username),
    {ok, Password} = application:get_env(password),

    State = #state{
        client_id = ClientId,
        keep_alive_timer = KeepAliveTimer,
        reconnect_wait_time = ReconnectWaitTime,
        mqtt_broker = ServerHost,
        mqtt_port = ServerPort,
        username = UserName,
        password = Password
    },

    error_logger:info_msg("[~p] was started.~n", [?MODULE]),

    {ok, proc_lib:spawn_link(?MODULE, run, [State])}.


run(State) ->
    case gen_tcp:connect(State#state.mqtt_broker, State#state.mqtt_port, [binary, {active, true}]) of
        {ok, Socket} ->
            State2 = State#state{socket = Socket},

            %% send CONNECT
            ConnectData = mqtt:build_connect(State2#state.client_id, State2#state.keep_alive_timer div 1000, State2#state.username, State2#state.password),
            gen_tcp:send(Socket, ConnectData),
            %error_logger:info_msg("[~p] sent CONNECT: ~p~n", [?MODULE, ConnectData]),

            %% waiting for CONNACK
            receive
                {tcp, Socket, Msg} -> 
                    %error_logger:info_msg("[~p] received tcp data: ~p~n", [?MODULE, Msg]),
                    case mqtt_utils:is_connack_success(Msg) of
                        true ->
                            loop(State2, true);
                        false ->
                            gen_tcp:close(Socket),
                            Reason = "not a good CONNACK",
                            reconnect(State2, Reason)
                    end
            after
                ?CONNECTION_TIMEOUT ->
                    gen_tcp:close(Socket),
                    Reason = "no CONNACK",
                    reconnect(State2, Reason)
            end;
        _ ->
            Reason = "connection error",
            reconnect(State, Reason)
    end.


%% ===================================================================
%% Local Functions
%% ===================================================================

loop(State, SendHeartbeat) ->
    case SendHeartbeat of
        false ->
            do_nothing;
        true -> 
            %% do heartbeat (ping broker) by send Uptime
            send_uptime(State)
    end,

    receive
        {tcp, _Socket, Msg} -> 
	    %error_logger:info_msg("[~p] received tcp data: ~p~n", [?MODULE, Msg]),

            handle_packages(State, Msg);

        {tcp_closed, _Socket} ->
            %error_logger:info_msg("[~p] tcp_closed~n", [?MODULE]),

            Reason = "tcp_closed",
            reconnect(State, Reason);

        _AnyMsg ->
            %error_logger:info_msg("[~p] received any data ~p: ~p~n", [?MODULE, _AnyMsg]),
            loop(State, false)

    after
        State#state.keep_alive_timer ->
            loop(State, true)
    end.


handle_packages(State, <<>>) ->
    loop(State, false);
handle_packages(State, RawData) ->
    {FixedLength, RestLength} = mqtt_utils:get_msg_length(RawData),
    Data = binary:part(RawData, 0, FixedLength + RestLength), 
    <<TypeCode:4/integer, _:4/integer, _/binary>> = Data,

    Result = case TypeCode of
        ?PUBLISH ->     
            computer_handler:process_data_publish(erlang:self(), State#state.socket, Data),
            ok;

        ?PINGRESP ->
            ok;

        ?DISCONNECT ->
            stop
    end,

    case Result of
        stop ->
            Reason = "stoped",
            reconnect(State, Reason);
        ok ->
            RestRawData = binary:part(RawData, FixedLength + RestLength, erlang:byte_size(RawData) - FixedLength - RestLength),
            handle_packages(State, RestRawData)
    end.
    

send_uptime(State) ->
    {Uptime0, _} = erlang:statistics(wall_clock), % in millisecond
    Uptime = Uptime0 div 1000,

	{_, UptimeData} = mqtt_cmd:uptime(State#state.client_id, Uptime),
    %error_logger:info_msg("[~p] is sending Uptime: ~p~n", [?MODULE, UptimeData]),
    gen_tcp:send(State#state.socket, UptimeData),

    ok.


reconnect(State, Reason) ->
    timer:sleep(State#state.reconnect_wait_time),

    error_logger:info_msg("[~p] reconnecting because of ~p~n", [?MODULE, Reason]),
    run(State). %% reconnect
