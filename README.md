# matrix-erlang-sdk

A simple REST wrapper for Matrix.org for Erlang development.

NOTE: ⚠️ Hasn't been maintained for a long while. Don't use this.

## Status
Early development. Essentially a "hello world" library at the moment.

## Usage (module: matrix)

Hello Matrix!
```erlang
matrix:helloMatrix("https://myhomeserv.er:port", "myusername", "mypassword", "#someroom:myhomeserv.er").
```

Initiate:
```erlang
Homeserver = "http://theserver.domain:port"
matrix:init().
```

Log in with password:
```erlang
AccessToken = matrix:login("username", "password", Homeserver).
```

Join room:
```erlang
RoomID = matrix:joinRoom("#room:domain.asd", AccessToken, Homeserver).
```

Send a message:
```erlang
matrix:sendTextMessage("The message", RoomID, AccessToken, Homeserver).
```

Listen for text messages (runs forever, will not terminate!)
```erlang
matrix:listen(RoomID, AccessToken, Homeserver).
```

## Note
Depends on **Jiffy** (https://github.com/davisp/jiffy) for JSON-handling.
Install with **rebar get-deps** or **apt-get install erlang-jiffy**.
