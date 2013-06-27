tool_computer
============

tools_platform tool_computer

## install as windows service
erlsrv add tool_computer -w "C:\app\deployment\tools_platform" -c "tools_platform remote tool" -ar "-pa c:/app/deployment/tools_platform/platform_core/deps/uuid/ebin c:/app/deployment/tools_platform/platform_core/ebin c:/app/deployment/tools_platform/cleaner/ebin c:/app/deployment/tools_platform/db_backup/ebin c:/app/deployment/tools_platform/monitor/ebin c:/app/deployment/tools_platform/mqtt_broker/ebin c:/app/deployment/tools_platform/mqtt_client/ebin c:/app/deployment/tools_platform/interface_http/ebin c:/app/deployment/tools_platform/iptracker/ebin c:/app/deployment/tools_platform/remote/ebin -config c:/app/deployment/tools_platform/local_win_remote -eval \"remote:start()\""
