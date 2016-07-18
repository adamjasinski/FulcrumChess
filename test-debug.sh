#/bin/sh
mono --debug --debugger-agent=transport=dt_socket,server=y,address=127.0.0.1:55555 ./packages/xunit.runner.console.2.1.0/tools/xunit.console.exe ./build/test/FenParserTests.dll
#mono --debug build/bin/FenParser.exe

#mono --trace=N:FenParserTests  ./packages/xunit.runner.console.2.1.0/tools/xunit.console.exe ./test/bin/FenParserTests.dll -noshadowcopy -verbose
