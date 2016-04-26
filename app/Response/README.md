Responses
=========

This directory contains all of the Haskell files responsible for creating server responses.
These modules should all be exported by `hs/Response.hs`.

All of the responses contain a value `???Response` of type `ServerPart Response`,
or a function that returns such a value.
These are the values which are the actual responses sent to the client.
Many use the [blaze-html](https://hackage.haskell.org/package/blaze-html)
library to generate an HTML response. Others use plain [happstack functions](https://hackage.haskell.org/package/happstack-server-7.4.4/docs/Happstack-Server-Response.html)
to serve responses of different types.
