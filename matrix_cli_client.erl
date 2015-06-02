%% Copyright 2015 Andreas Hallberg
%% 
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%% 
%%        http://www.apache.org/licenses/LICENSE-2.0
%% 
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.

%% @doc A very simple module that tests "matrix.erl".


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


