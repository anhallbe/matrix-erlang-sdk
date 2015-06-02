# matrix-erlang-sdk

A simple REST wrapper for Matrix.org for Erlang development.

## Usage (module: matrix)
Initiate:
```
Homeserver = "http://theserver.domain:port"
matrix:init().
```

Log in with password:
```
AccessToken = matrix:login("username", "password", Homeserver).
```

Join room:
```
RoomID = matrix:joinRoom("#room:domain.asd", AccessToken, Homeserver).
```

Send a message:
```
matrix:sendTextMessage("The message", RoomID, AccessToken, Homeserver).
```

Listen for messages (runs forever, will not let you terminate the program!)
```
matrix:listen(RoomID, AccessToken, Homeserver).
```

## Usage (module: matrix_cli_client)
```
matrix_cli_client:run("https://yourserver.asd", "#theroom:homeserver", "username", "password").
```
