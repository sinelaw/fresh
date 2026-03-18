-module(hello).
-export([greet/1, main/0]).

%% Greeting function
greet(Name) ->
    io_lib:format("Hello, ~s!", [Name]).

main() ->
    Message = greet("World"),
    io:format("~s~n", [Message]),
    lists:foreach(fun(I) ->
        io:format("Item: ~p~n", [I])
    end, [1, 2, 3]).
