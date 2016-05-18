# ocaml-webmachine

ocaml-webmachine is a layer on top of [cohttp][] that implements a
state-machine-based HTTP request processor. It's particularly well-suited for
writing RESTful APIs. As the name suggests, this is an OCaml port of the
[webmachine][] project.

[cohttp]: https://github.com/mirage/ocaml-webmachine
[webmachine]: https://github.com/webmachine/webmachine

[![Build Status](https://travis-ci.org/inhabitedtype/ocaml-webmachine.svg?branch=master)](https://travis-ci.org/inhabitedtype/ocaml-webmachine)

## Installation

Install the library and its depenencies via [OPAM][opam]:

[opam]: http://opam.ocaml.org/

```bash
opam install webmachine
```

## Getting Started

webmachine implements [this decision diagram][diagram] to determine how an HTTP
request should be handled. This includes validation, authentication, content
negotiation, and caching. A resource specifies the decision that should be made
at each node in the diagram by defining the appropriate method in a `resource`
subclass. The correspondence is suggested by the name of the method for now.
This will be better-documented in the future.

### Examples

To build the examples in the `examples/` subdirectory, reconfigure the build
process and recompile:

```bash
./configure --enable-examples
make clean && make
```

[diagram]: https://raw.githubusercontent.com/webmachine/webmachine/develop/docs/http-headers-status-v3.png


## Development

To install development dependencies, pin the package from the root of the
repository:

```bash
opam pin add -n webmachine .
opam install --deps-only webmachine
```

After this, you may install a development version of the library using the
install command as usual.

For building and running the tests during development, you will need to install
the `oUnit` package and reconfigure the build process to enable tests:

```bash
opam install oUnit
./configure --enable-tests
make && make test
```

## License

BSD3, see LICENSE file for its text.
