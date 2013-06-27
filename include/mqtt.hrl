%% MQTT_V3.1
-define(CONNECT,     1).
-define(CONNACK,     2).
-define(PUBLISH,     3).
-define(PUBACK,      4).
-define(PUBREC,      5).
-define(PUBREL,      6).
-define(PUBCOMP,     7).
-define(SUBSCRIBE,   8).
-define(SUBACK,      9).
-define(UNSUBSCRIBE, 10).
-define(UNSUBACK,    11).
-define(PINGREQ,     12).
-define(PINGRESP,    13).
-define(DISCONNECT,  14).

%% COMMAND
-define(CMD_ONLINE,          1).
-define(CMD_OFFLINE,         2).
-define(CMD_SWITCH_STATUS,   3).
-define(CMD_SWITCH_CONTROL,  4).
-define(CMD_SEND_COMMAND,    5).
-define(CMD_UPTIME,          6).


%% CONNACK CODE
-define(ACCEPTED,                    0).
-define(UNACCEPTABLE_PROTOCOL_VER,   1).
-define(IDENTIFIER_REJECTED,         2).
-define(SERVER_UNAVAILABLE,          3).
-define(BAD_USERNAME_OR_PASSWORD,    4).
-define(NOT_AUTHORIZED,              5).
