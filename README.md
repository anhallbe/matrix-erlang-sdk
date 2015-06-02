# matrix-erlang-sdk

A simple REST wrapper for Matrix.org for Erlang development.

## Usage (module: matrix)
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

Listen for messages (runs forever, will not let you terminate the program!)
```erlang
matrix:listen(RoomID, AccessToken, Homeserver).
```

## Usage (module: matrix_cli_client)
This will initiate, log in, join room, send a message and exit.
```erlang
matrix_cli_client:run("https://yourserver.asd", "#theroom:homeserver", "username", "password").
```
## Note
Depends on **Jiffy** (https://github.com/davisp/jiffy) for JSON-handling.
Install with **apt-get install erlang-jiffy**.
