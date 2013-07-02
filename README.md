# EventSafe

EventSafe is a small library to build type-safe event stores.

## How to use

You should probably by creating types for your events, resources and resource references.

Everything can be tied together by implementing the typeclasses, such as  `Resource`, `ResourceRef` and `StorableEvent`.

Once it's done, you will be able to create an `EventStorage` and start using `writeEvent` and `readResource` for instance.

## Examples

See `Examples` for examples of code (`Simple`, `WithTVar` and `Storage`).

To run them, you can type something like `runhaskell -isrc -i. Examples/Simple.hs`.
