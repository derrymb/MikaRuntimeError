-module(test).
-export([print_hello/0]).

print_hello()->
    io:fwrite("Hello world!\nThis is so cool\n").