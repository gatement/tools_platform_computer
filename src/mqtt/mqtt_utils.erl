-module(mqtt_utils).
-include("mqtt.hrl").
-export([get_utf8_bin/1,
		get_utf8_list/1,
		is_connack_success/1,
		strip_fixed_header/1,
		get_qos/1,
		get_msg_length/1,
		extract_connect_info/1,
		extract_publish_info/1,
		extract_publish_ack_info/1]).

-vsn("0.1.0").

%% implementation of MQTT_V3.1

%% ===================================================================
%% API functions
%% ===================================================================

get_utf8_bin(Content) ->
    erlang:list_to_binary(get_utf8_list(Content)).


get_utf8_list(Content) ->
    ContentBin = unicode:characters_to_binary(Content, latin1),

    ContentBinLen = erlang:size(ContentBin),
    ContentBinLenH = ContentBinLen div 256,
    ContentBinLenL = ContentBinLen rem 256,

    [ContentBinLenH, ContentBinLenL, ContentBin].


is_connack_success(Data) ->
	if 
		erlang:size(Data) < 4 ->
			false;
		true ->
			<<TypeCode:4/integer, _:4/integer, _/binary>> = Data,
			if
				TypeCode =/= ?CONNACK ->
					false;
				true ->
					ReturnCode = binary:at(Data, 3),
					if
						ReturnCode =/= 0 ->
							false;
						true ->
							true
					end
			end
	end.


strip_fixed_header(Msg) ->
	strip_fixed_header(Msg, true).


get_msg_length(Msg) ->
	{FixedHeaderLength, RestLength} = get_msg_length(Msg, 1, true, 0, 1),
	{FixedHeaderLength, RestLength}.


get_qos(Msg) ->
	Byte0 = binary:at(Msg, 0),
	(Byte0 bsr 1) band 2#00000011.


extract_connect_info(Data) ->
    RestData1 = mqtt_utils:strip_fixed_header(Data),

    <<_:9/binary, ConnectFlags:8/integer, KeepAliveTimerLenH:8/integer, KeepAliveTimerLenL:8/integer, RestData2/binary>> = RestData1,
    KeepAliveTimer = KeepAliveTimerLenH * 256 + KeepAliveTimerLenL,

    <<ClientIdLenH:8/integer, ClientIdLenL:8/integer, RestData3/binary>> = RestData2,
    ClientIdLen = ClientIdLenH * 256 + ClientIdLenL,
    ClientId = erlang:binary_to_list(binary:part(RestData3, 0, ClientIdLen)),
    RestData4 = binary:part(RestData3, ClientIdLen, erlang:byte_size(RestData3) - ClientIdLen),

    {UserName, RestData7} = if
    	ConnectFlags band 2#10000000 =:= 2#10000000 ->
    		<<UserNameLenH:8/integer, UserNameLenL:8/integer, RestData5/binary>> = RestData4,
    		UserNameLen = UserNameLenH * 256 + UserNameLenL,
		    UserName0 = erlang:binary_to_list(binary:part(RestData5, 0, UserNameLen)),
    		RestData6 = binary:part(RestData5, UserNameLen, erlang:byte_size(RestData5) - UserNameLen),
    		{UserName0, RestData6};
    	true ->
    		{undefined, RestData4}
    end,

    Password = if
    	ConnectFlags band 2#01000000 =:= 2#01000000 ->
    		<<PasswordLenH:8/integer, PasswordLenL:8/integer, RestData8/binary>> = RestData7,
    		PasswordLen = PasswordLenH * 256 + PasswordLenL,
		    Password0 = erlang:binary_to_list(binary:part(RestData8, 0, PasswordLen)),
    		Password0;
    	true ->
    		undefined
    end,

    {ClientId, KeepAliveTimer, UserName, Password}.


extract_publish_info(Msg) ->
	{FixedHeaderLength, _RestLength} = get_msg_length(Msg),

	TopicLengthH = binary:at(Msg, FixedHeaderLength),
	TopicLengthL = binary:at(Msg, FixedHeaderLength + 1),
	TopicLength = TopicLengthH * 256 + TopicLengthL,
	Topic = erlang:binary_to_list(binary:part(Msg, FixedHeaderLength + 2, TopicLength)),

	Qos = get_qos(Msg),

	BytesBeforePayload = if
		Qos > 0 ->
			FixedHeaderLength + TopicLength + 4;			
		true ->
			FixedHeaderLength + TopicLength + 2
	end,

	Payload = binary:part(Msg, BytesBeforePayload, erlang:byte_size(Msg) - BytesBeforePayload),

	{Topic, Payload}.


extract_publish_ack_info(Msg) ->
	{FixedHeaderLength, _RestLength} = get_msg_length(Msg),

	MsgIdH = binary:at(Msg, FixedHeaderLength),
	MsgIdL = binary:at(Msg, FixedHeaderLength + 1),
	MsgId = MsgIdH * 256 + MsgIdL,

	{MsgId}.


%% ===================================================================
%% Local Functions
%% ===================================================================

strip_fixed_header(Msg, Untouched) ->
	Msg2 = case Untouched of
		true ->
			<<_:1/binary, Msg0/binary>> = Msg,
			Msg0;
		false ->
			Msg
	end, 

	<<Len:8/integer, RestData/binary>> = Msg2,
	if 
		Len > 127 ->
			strip_fixed_header(RestData, false);
		true ->
			RestData
	end.


get_msg_length(Msg, Multiplier, Untouched, Length, Loop) ->
	Msg2 = case Untouched of
		true ->
			<<_:1/binary, Msg0/binary>> = Msg,
			Msg0;
		false ->
			Msg
	end,
	<<Len:8/integer, RestData/binary>> = Msg2,
	if
		Len > 127 ->
			Length2 = Length + (Len band 127) * Multiplier,
			Multiplier2 = Multiplier * 128,			
			get_msg_length(RestData, Multiplier2, false, Length2, Loop + 1);
		true ->
			Length2 = Length + (Len band 127) * Multiplier,
			{Loop + 1, Length2}
	end.
