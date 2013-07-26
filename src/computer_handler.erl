-module(computer_handler).
-include("mqtt.hrl").
-export([process_data_publish/3]).


%% ===================================================================
%% API functions
%% ===================================================================

process_data_publish(_SourcePid, _Socket, RawData) ->
    %error_logger:info_msg("[~p] received data: ~p~n", [?MODULE, RawData]),
    {Topic, Payload} = mqtt_utils:extract_publish_info(RawData),
    ClientId = string:substr(Topic, 2, 12),
    <<TypeCode:8/integer, _/binary>> = Payload,

    case TypeCode of
        ?CMD_SEND_COMMAND ->
            <<_:1/binary, Cmd0/binary>> = Payload,
            Cmd = erlang:binary_to_list(Cmd0),
            process_data_send_command(ClientId, Cmd)
    end.


%% ===================================================================
%% Local Functions
%% ===================================================================

process_data_send_command(ClientId, Cmd) ->
    error_logger:info_msg("[~p] received [send_command] data: ~p - ~p~n", [?MODULE, ClientId, Cmd]),

    case Cmd of
        "poweroff" ->
            shutdown();

        "restart" ->
            restart();

        _ ->
            ignore
    end,

    ok.


shutdown() ->
    case os:type() of
        {unix, _} ->
            os:cmd("poweroff"),
            ok;

        {win32, _} ->
            os:cmd("shutdown /s /t 0"),
            ok
    end.


restart() ->
    case os:type() of
        {unix, _} ->
            os:cmd("reboot"),
            ok;

        {win32, _} ->
            os:cmd("shutdown /r /t 0"),
            ok
    end.
