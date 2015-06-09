-module(matrixcli).

-export([start/0]).

start() ->
    {ok, [Config]} = file:consult("matrix.config"),
    #{server := Homeserver, roomAlias := RoomAlias, user := User, password := Password} = Config,
    matrix:init(),
    {ok, AccessToken} = matrix:login(User, Password, Homeserver),
    {ok, RoomId} = matrix:joinRoom(RoomAlias, AccessToken, Homeserver),
    spawn(matrix, listen, [RoomId, AccessToken, Homeserver, self()]),
    cli_output(User, AccessToken, Homeserver, RoomId).
    %spawn(fun() -> cli_output(User, AccessToken, Homeserver, RoomId) end).
    %cli_input(User, AccessToken, Homeserver, RoomId).

cli_output(User, AccessToken, Homeserver, RoomId) ->
  %io:format("Waiting for messages... My pid is ~p~n", [self()]),
  receive
    {matrix, roomlistener, {RoomId, UserId, Content}} ->
      io:format("~p >> ~p~n", [UserId, Content]),
      cli_output(User, AccessToken, Homeserver, RoomId);
    _Other ->
      io:format("CLI Got a message, not sure what..(~p)~n", [_Other]),
      cli_output(User, AccessToken, Homeserver, RoomId)
  end.

cli_input(User, AccessToken, Homeserver, RoomId) ->
  {ok, [Input]} = io:fread("> ", "~s"),
  matrix:sendTextMessage(Input, RoomId, AccessToken, Homeserver),
  cli_input(User, AccessToken, Homeserver, RoomId).