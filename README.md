# example-servant-partial-client

This project is a small example of how to create versioned APIs and partial
servant client implementations.

You can build and test the server and client with stack:

```
stack build
stack test example-servant-partial-client
```

You can also run the server with `stack exec example-servant-partial-client` and
test queries in a separate shell:

Version 1:

```
curl -H 'Content-type: application/json' localhost:3000/v1/user/add --data '{"userIdent": "alice", "name": "Alice", "age": 42}'
curl -H 'Content-type: application/json' localhost:3000/v1/user/get/alice
```

Version 2:

```
curl -H 'Content-type: application/json' localhost:3000/v2/user/add --data '{"userIdent": "jsmith", "name": "John Smith", "age": 30}'
curl -H 'Content-type: application/json' localhost:3000/v2/user/get/jsmith
```

Version 3:

```
curl -H 'Content-type: application/json' localhost:3000/v3/user/add --data '{"userIdent": "bob", "name": "Bob", "age": 20, "address" : "Washington", "phoneNumber" : "123-456-789"}'
curl -H 'Content-type: application/json' localhost:3000/v3/user/get/bob
curl -H 'Content-type: application/json' localhost:3000/v3/user/update/bob --data '{"userIdent": "bob", "name": "Bob Saget", "age": 60, "address" : "Washington", "phoneNumber" : "123-456-789"}'
curl -H 'Content-type: application/json' localhost:3000/v3/user/delete/bob
curl -H 'Content-type: application/json' localhost:3000/v3/user/exists/bob
```

## Versioning
