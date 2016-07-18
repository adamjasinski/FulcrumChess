#/bin/sh
mono --debug --debugger-agent=transport=dt_socket,server=y,address=127.0.0.1:55555 build/bin/FenParser.exe
#mono --debug build/bin/FenParser.exe
