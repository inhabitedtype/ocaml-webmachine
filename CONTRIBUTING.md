# Goals

[erlang]: https://github.com/webmachine/webmachine
[lwt]: https://ocsigen.org/lwt
[async]: https://github.com/janestreet/async

The goals of this project are the following:

* implement a faithful port of the [Erlang webmachine project][erlang];
* support [lwt][] and [async][] equally well; and
* impose as few dependencies on the user as possible.

## Commits and Pull Requests

[commit-msg]: http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html

The best way to get a pull request accepted is for it to directly serve the
goals of the project, while being simple and straightforward for project
maintainers to evalute its functionality and maintainability. In service to the
these criteria, pull request and commit messages should clearly describe what
they include and why. Generally speaking, follow the guidelines of [this blog
post][commit-msg] when composing commit messages. Pull requests should pass
existing tests and include additional tests when appropriate. The code should
be clear rather than clever (two-liners over one-liners), and documented when
the code can't speak for itself.

[process]: https://twitter.com/davecheney/status/676645735647940608

If a part of the existing code is not relevant to the topic of your pull
request, please refrain from modifying it (see below). If a change or feature
will have a large effect on the codebase, bring it up for discussion in an
issue. To quote a tweet:

> People of github,
>
> 1. Raise issue
> 2. Discuss, propose solution
> 3. Implement
>
> Sometimes it's ok to skip a step, but pls don't change the order.

&mdash; [Dave Cheney][process]

## Whitespace

Two space indentation. No tabs. No trailing whitespace. If you find any
violations in or around code that you need to modify, feel free to fix it. If
you find violations in unrelated code, just leave it there.

## Updating Dependencies

In the event that a contribution requires changes to the project's
dependencies, here's how you should incorporate those changes into the
build/release process:

[oasis]: https://github.com/inhabitedtype/ocaml-webmachine/tree/master/_oasis
[OPAM]: http://http://opam.ocaml.org/

1. Modify the [`_oasis`][oasis] file to reflect changed dependencies;
2. Run `oasis setup` to regenerate files used in the build process; and
3. Run `oasis2opam --local` to regenerate the opam file used when publishing
   releases to [OPAM][OPAM].

Try to include these changes in a separate commit. That's not always possible,
so don't sweat it too much.
