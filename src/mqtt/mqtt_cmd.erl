-module(mqtt_cmd).
-include("mqtt.hrl").
-export([online/2,
		offline/1,
        switch_control/3,
        send_command/2,
		switch_status/2,
        uptime/2]).


%% ===================================================================
%% API functions
%% ===================================================================

online(ClientId, UserName) ->
	Topic = lists:flatten(io_lib:format("/~s/online", [ClientId])),
    Payload = erlang:list_to_binary([?CMD_ONLINE | mqtt_utils:get_utf8_list(UserName)]),
	{Topic, mqtt:build_publish(Topic, Payload)}.


offline(ClientId) ->
	Topic = lists:flatten(io_lib:format("/~s/offline", [ClientId])),
    Payload = <<?CMD_OFFLINE>>,
	{Topic, mqtt:build_publish(Topic, Payload)}.


switch_control(ClientId, SwitchId, Status) ->
    Topic = lists:flatten(io_lib:format("/~s/cmd", [ClientId])),
    Payload = erlang:list_to_binary([?CMD_SWITCH_CONTROL, SwitchId, Status]),
	{Topic, mqtt:build_publish(Topic, Payload)}.


send_command(ClientId, Cmd) ->
    Topic = lists:flatten(io_lib:format("/~s/cmd", [ClientId])),
    Payload = erlang:list_to_binary([?CMD_SEND_COMMAND, Cmd]),
	{Topic, mqtt:build_publish(Topic, Payload)}.


switch_status(ClientId, Status) ->
    Topic = lists:flatten(io_lib:format("/~s/switch_status", [ClientId])),
    Payload = erlang:list_to_binary([?CMD_SWITCH_STATUS, Status]),
	{Topic, mqtt:build_publish(Topic, Payload)}.


uptime(ClientId, Uptime) ->
	Topic = lists:flatten(io_lib:format("/~s/uptime", [ClientId])),
    Uptime4 = Uptime div 16777216,
    Uptime3 = Uptime div 65536,
    Uptime2 = Uptime div 256,
    Uptime1 = Uptime rem 256,
    Payload = erlang:list_to_binary([?CMD_UPTIME, Uptime4, Uptime3, Uptime2, Uptime1]),
	{Topic, mqtt:build_publish(Topic, Payload)}.


%% ===================================================================
%% Local Functions
%% ===================================================================
