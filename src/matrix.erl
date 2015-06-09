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

%% @doc A small experimental Matrix client.

-module(matrix).
-export([init/0, login/3, joinRoom/3, sendTextMessage/4, listen/4, helloMatrix/4]).

-ifdef (TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(API_URL, "/_matrix/client/api/v1/").

-define(DEBUG, false).

%Initiate, login, join a room, send a "Hello from Erlang" message.
%Example: matrix:helloMatrix("https:myhomeserv.er:8008", "myusername", "mypassword", "#publicroom:myhomeserv.er").
helloMatrix(Homeserver, Username, Password, RoomAlias) ->
	init(),
	AccessToken = login(Username, Password, Homeserver),
	RoomID = joinRoom(RoomAlias, AccessToken, Homeserver),
	sendTextMessage("Hello from Erlang!", RoomID, AccessToken, Homeserver),
	"Success! Look at the chat to see the message.".

%Initiate by starting http client, ssl, listening processes etc.
init() ->
	inets:start(),
	ssl:start(),
	ok.

%Login using username/password. All parameters should be strings.
%Returns an access token needed for authenticated requests.
login(Username, Password, Server) ->
	UN = erlang:list_to_binary(Username),
	PW = erlang:list_to_binary(Password),
	Raw = #{type => 'm.login.password', user => UN, password => PW},
	Encoded = jiffy:encode(Raw),
	Credentials = api_post("login", Encoded, Server),
	#{<<"access_token">> := Token} = Credentials,
	{ok, erlang:binary_to_list(Token)}.

%Join a room by its' alias or ID. Make sure you have received an access token from login/3.
%Returns the room ID.
joinRoom(RoomIdOrAlias, AccessToken, Server) ->
	Resource = string:concat("join/", RoomIdOrAlias),
	Body = jiffy:encode(#{}),
	JoinResp = api_post(Resource, Body, AccessToken, Server),
	#{<<"room_id">> := RoomId} = JoinResp,
	{ok, erlang:binary_to_list(RoomId)}.

%Send a TEXT message to the given room.
sendTextMessage(Message, RoomId, AccessToken, Server) ->
	Resource = string:concat("rooms/", string:concat(RoomId, "/send/m.room.message")),
	Body = jiffy:encode(#{body => erlang:list_to_binary(Message), msgtype => <<"m.text">>}),
	Res = api_post(Resource, Body, AccessToken, Server),
	{ok, Res}.

%Listen for TEXT messages in the given room. This will essentially listen forever and prevent the program from terminating!
%TODO: Fix that.. This should probably run in a separate process.
listen(RoomId, AccessToken, Server, Listener) ->
	Resource = string:concat("rooms/", string:concat(RoomId, "/messages")),
	Sync = api_get(Resource, AccessToken, Server),
	#{<<"end">> := End, <<"start">>:= Start} = Sync,
	log("Start", Start),
	log("End", End),
	listen(RoomId, AccessToken, erlang:binary_to_list(End), Server).

%Listen for text messages (or any events) by polling.
%TODO: Run in separate process end send messages to whoever is listening..
listen(RoomId, AccessToken, _, Server, Listener) ->
	Resource = "events",
	Response = api_get(Resource, AccessToken, Server),
	#{<<"chunk">> := Chunk, <<"end">> := NEnd} = Response,
%% 	log("chunk", jiffy:encode(Chunk, [pretty])),
	case Chunk of
		#{content := Content, type := <<"m.room.message">>, <<"user_id">> := UserId} ->
			#{body := Body, msgtype := <<"m.text">>} = Content,
			Listener ! {matrix, roomlistener, {RoomId, UserId, Content}};
			%print([UserId, " >> ", Body]);
		_Else ->
			asd
	end,
	listen(RoomId, AccessToken, NEnd, Server).

%Helper function that performs a POST request to Server/API_URL/Resource, with a json-formatted Body.
%Returns the response body as JSON, will probably crash if anything goes wrong.
api_post(Resource, Body, Server) ->
	Method = post,
	URL = escape_url(string:concat(Server, string:concat(?API_URL, Resource))),
	log("POST_URL", URL),
	log("POST_REQUEST", Body),
	Header = [],
	Type = "application/json",
	HttpOptions = [],
	Options = [],
	Resp = httpc:request(Method, {URL, Header, Type, Body}, HttpOptions, Options),
	{ok, {{_, 200, _}, _, RBody}} = Resp,
	log("POST_RESPONSE", RBody),
	jiffy:decode(RBody, [return_maps]).

%Perform a GET request to Server/API_URL/Resource
%Returns the response body as JSON.
%TODO: Make it easier to insert qs.
api_get(Resource, Server) ->
	Method = get,
	URL = escape_url(string:concat(Server, string:concat(?API_URL, Resource))),
	log("GET_URL", URL),
	Header = [],
	HttpOptions = [],
	Options = [],
	Resp = httpc:request(Method, {URL, Header}, HttpOptions, Options),
	{ok, {{_, 200, _}, _, RBody}} = Resp,
	log("GET_RESPONSE", RBody),
	jiffy:decode(RBody, [return_maps]).

%Perform authenticated POST.
api_post(Resource, Body, AccessToken, Server) ->
	QS = string:concat("?access_token=", AccessToken),
	AuthedResource = string:concat(Resource, QS),
	api_post(AuthedResource, Body, Server).

%Perform authenticated GET.
api_get(Resource, AccessToken, Server) ->
	QS = string:concat("?access_token=", AccessToken),
	AuthedResource = string:concat(Resource, QS),
	api_get(AuthedResource, Server).

log(Tag, Message) ->
	if ?DEBUG ->
		io:fwrite(Tag),
		io:fwrite(": "),
		io:fwrite(Message),
		io:fwrite("\n");
	   true ->
		   ok
	end.

%Pretty prints......
print([M]) ->
	io:fwrite(M),
	io:fwrite("\n");

print([H|T]) ->
	io:fwrite(H),
	print(T);

print(Message) ->
	io:fwrite(Message),
	io:fwrite("\n").

%%TODO: Really, really should not do it this way. Temporary fix. Escape the whole URL properly instead.
escape_url(URL) ->
	log("ESCAPEURL", URL),
	re:replace(URL,"#","%23",[{return,list}]).


%%-------------------------------------------
%%                TESTS
%%-------------------------------------------
-ifdef(TEST).

init_test() ->
	?assert(ok == init()).

getAPI_test() ->
	?debugMsg("getAPI_test not implemented").

postAPI_test() ->
	?debugMsg("postAPI_test not implemented").

login_test() ->
	?debugMsg("login_test not implemented").

joinRoom_test() ->
	?debugMsg("joinRoom_test not implemented").

sendTextMessage_test() ->
	?debugMsg("sendTextMessage_test not implemented").

-endif.
