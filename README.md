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

## Data Model Versioning

Servant is neutral in regards to how users sets up their data models and
handle version changes in models. The data model versioning used in this project
is only intended to be an example and not necessarily the best choice for a
production project. There are three decisions users have to make:
representation (the data type),
functions to convert between representations, and storage.

### Representation

In this example, each version of the API is
paired with a data model module for each version. Version 1 of the API is paired
with Models.V1, Version 2 of the API is paired with Models.V2, etc. Even if the
models have not changed between API versions, we still provide a data model
module for that version to make the semantics of the API clear.

### Conversion between representations

There are no changes between V1 and V2 so the conversion functions move the data
from one type to the other. From V1 and V2 to V3 there are new selectors added.
The selectors that do not exist in V1 and V2 need to be initiated to a neutral
or empty value in V3. You may consider using [safecopy](http://hackage.haskell.org/package/safecopy)
for a more principled way to convert between data type versions.

### Storage

In this project all the data is stored in a Map of V3 in TVar that is shared
throughout the server. There may be cases where you want to store all versions
in the database. If you want to see how to use Persistent with Servant, refer
to this example: [example-servant-persistent](https://github.com/haskell-servant/example-servant-persistent).
