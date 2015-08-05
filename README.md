# ocaml-webmachine

ocaml-webmachine is a layer on top of [cohttp][] that implements a
state-machine-based HTTP request processor. It's particularly well-suited for
writing RESTful APIs. As the name suggests, this is an OCaml port of the
[webmachine][] project.

[cohttp]: https://github.com/mirage/ocaml-webmachine
[webmachine]: https://github.com/webmachine/webmachine

[![Build Status](https://magnum.travis-ci.com/inhabitedtype/ocaml-webmachine.svg?token=XSg14w1MrphqpyipUNfk&branch=master)](https://magnum.travis-ci.com/inhabitedtype/ocaml-webmachine)

## Installation

Install the library and its depenencies via [OPAM][opam]:

```bash
opam install webmachine
```

## Development

To install development versions of the library, pin the package:

```bash
opam pin add webmachine
```

You can install the latest changes by commiting them to the local git
repository and running:

```bash
opam upgrade webmachine
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

[diagram]: https://raw.githubusercontent.com/webmachine/webmachine/develop/docs/http-headers-status-v3.png

## License

BSD3, see LICENSE file for its text.
