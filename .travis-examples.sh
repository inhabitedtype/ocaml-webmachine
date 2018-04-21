#!/bin/sh

opam install async lwt cohttp-lwt-unix cohttp-async

jbuilder build _build/default/examples/hello_lwt.exe
jbuilder build _build/default/examples/crud_lwt.exe
jbuilder build _build/default/examples/hello_async.exe
