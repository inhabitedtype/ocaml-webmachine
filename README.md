# ocaml-webmachine

ocaml-webmachine is a layer on top of cohttp library that implements the state
machines described in [RFC 7231 - Semantics and Content][7231], [RFC 7232 -
Conditional Requests][7232], [RFC 7234 - Cachcing][7234], and [RFC 7235 -
Authentication][7235]. This is inspired by Basho's [webmachine][] project.

[![Build Status](https://magnum.travis-ci.com/inhabitedtype/ocaml-webmachine.svg?token=XSg14w1MrphqpyipUNfk&branch=master)](https://magnum.travis-ci.com/inhabitedtype/ocaml-webmachine)

[7231]: http://tools.ietf.org/html/rfc7231
[7232]: http://tools.ietf.org/html/rfc7232
[7234]: http://tools.ietf.org/html/rfc7234
[7235]: http://tools.ietf.org/html/rfc7235

[webmachine]: https://github.com/basho/webmachine

## Installation

Install the core library and its dependencies by pinning:

```bash
opam pin add webmachine
```

During development, you can install the latest changes by commiting them to the
local git repository and running:

```bash
opam upgrade
```

For building and running the tests during development, you will need to install
the `oUnit` package and reconfigure the build process to enable tests:

```bash
opam install oUnit
./configure --enable-tests
make && make test
```

## Getting Started

webmachine implements [this decision diagram][diagram] to determine how an HTTP
request should be handled. This includes validation, authentication, content
negotiation, and caching. A resource specifies the decision that should be made
at each node in the diagram by defining the appropriate method in a `resource`
subclass. The correspondence is suggested by the name of the method for now.
This will be better-documented in the future.

[diagram]: https://raw.githubusercontent.com/basho/webmachine/develop/docs/http-headers-status-v3.png

## License

Proprietary and confidential.
