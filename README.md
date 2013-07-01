tool_computer
============

tools_platform tool_computer

## install as windows service
erlsrv add tool_computer -w "C:\app\tool_computer" -c "tool_computer for tools_platform" -ar "-pa c:/app/tool_computer/ebin -config c:/app/tool_computer/local_win -eval \"computer:start()\""
