#!/bin/sh

opam install async lwt cohttp-lwt-unix cohttp-async

dune build examples/hello_lwt.exe
dune build examples/crud_lwt.exe
dune build examples/hello_async.exe
