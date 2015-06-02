%% @author andreas
%% @doc @todo Add description to 'matrix_cli_client'.


-module('matrix_cli_client').

%% ====================================================================
%% API functions
%% ====================================================================
-export([run/4]).

%A simple function that logs in, joins a room and sends a message. That's it.
run(Homeserver, Room, Username, Password) ->
	matrix:init(),
	Token = matrix:login(Username, Password, Homeserver),
	RoomID = matrix:joinRoom(Room, Token, Homeserver),
	matrix:sendTextMessage("Hello from Erlang!", RoomID, Token, Homeserver).

%% ====================================================================
%% Internal functions
%% ====================================================================


