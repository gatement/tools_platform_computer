-module(mqtt_cmd).
-include("mqtt.hrl").
-export([online/4,
		offline/3,
        send_command/4,
        switch_control/5,
		switch_status/2,
        uptime/2,
		build_publish/4]).


%% ===================================================================
%% API functions 1 (Server side)
%% ===================================================================

online(ClientId, Topic, Qos, UserName) ->
    Payload = erlang:list_to_binary([?CMD_ONLINE | mqtt_utils:get_utf8_list(UserName)]),
	build_publish(ClientId, Topic, Payload, Qos).


offline(ClientId, Topic, Qos) ->
    Payload = <<?CMD_OFFLINE>>,
	build_publish(ClientId, Topic, Payload, Qos).


send_command(ClientId, Topic, Qos, Cmd) ->
    Payload = erlang:list_to_binary([?CMD_SEND_COMMAND, Cmd]),
	build_publish(ClientId, Topic, Payload, Qos).


switch_control(ClientId, Topic, Qos, SwitchId, Value) ->
    Payload = erlang:list_to_binary([?CMD_SWITCH_CONTROL, SwitchId, Value]),
	build_publish(ClientId, Topic, Payload, Qos).


build_publish(ClientId, Topic, Payload, Qos) ->
	case Qos of
		0 ->
			{0, mqtt:build_publish(Topic, Payload)};
		_ ->
			MsgId = model_mqtt_message_id:get_msg_id(ClientId),
			{MsgId, mqtt:build_publish(Topic, Payload, Qos, MsgId)}
	end.


%% ===================================================================
%% API functions 2 (Client side)
%% ===================================================================

switch_status(ClientId, Status) ->
    Topic = lists:flatten(io_lib:format("/~s/status", [ClientId])),
    Payload = erlang:list_to_binary([?CMD_SWITCH_STATUS, Status]),
	{Topic, mqtt:build_publish(Topic, Payload)}.


uptime(ClientId, Uptime) ->
	Topic = lists:flatten(io_lib:format("/~s/status", [ClientId])),
    Uptime4 = Uptime div 16777216,
    Uptime3 = Uptime div 65536,
    Uptime2 = Uptime div 256,
    Uptime1 = Uptime rem 256,
    Payload = erlang:list_to_binary([?CMD_UPTIME, Uptime4, Uptime3, Uptime2, Uptime1]),
	{Topic, mqtt:build_publish(Topic, Payload)}.


%% ===================================================================
%% Local Functions
%% ===================================================================
