# query-string

A [query-string](https://en.wikipedia.org/wiki/Query_string) parser with nesting support.

Most (if not all?) of the existing hackage libs for URI parsing only support simple 1D key-value query parameters. However, it is common for clients to communicate nested data structures (like JSON) encoded in the URL query-string for GET requests (where a request body isn't permitted or desired).

This package contains 0 deps (besides `base`) and requires 0 extensions, so it should be as flexible and portable as possible, without any baggage.

Inspired by [ljharb/qs](https://github.com/ljharb/qs).

NOTE: This is still under development.

### TODO

- [ ] Improve docs, add examples.
- [ ] Add a pretty instance/function so it's easier on the eyes when working in CLI with large inputs.
- [ ] Add hSpec test with a handful of common cases.
- [ ] Fix TODOs.
- [ ] Add support for encoding.
- [ ] Add quick-check test that arbitrarily generates, encodes and parses back.