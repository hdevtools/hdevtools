hdevtools
=========

Persistent GHC powered background server for FAST Haskell development tools


About
-----

`hdevtools` is a backend for text editor plugins, to allow for things such as
syntax and type checking of Haskell code, and retrieving type information, all
directly from within your text editor.

The advantage that `hdevtools` has over competitors, is that it runs silently
in a persistent background process, and therefore is able to keeps all of your
Haskell modules and dependent libraries loaded in memory. This way, when you
change only a single source file, only it needs to be reloaded and rechecked,
instead of having to reload everything.

This makes `hdevtools` very fast for checking syntax and type errors (runs just
as fast as the `:reload` command in GHCi).

In fact, syntax and type checking is so fast, that you can safely enable auto
checking on every save. Even for huge projects, checking is nearly instant.

Once you start using `hdevtools` and you get used to having your errors shown
to you instantly (without having to switch back and forth between GHCi and your
editor), and shown directly on your code, in your editor (without having to
wait forever for GHC to run) you will wonder how you ever lived without it.

In addition to checking Haskell source code for errors, `hdevtools` has tools
for getting info about identifiers, and getting type information for snippets
of code.

Text Editor Integration
-----------------------

`hdevtools` is designed to be integrated into text editors. The list of current
editor plugins that supply this integration is below.

But before you do anything, you must first install `hdevtools` itself. The
easiest way is from [Stackage][14] using [stack][15]:
```
$ stack --resolver lts-7.0 install hdevtools
```

**Note:** `hdevtools` automatically discovers compiler and libraries installed
via stack.

Alternatively one can install from [Hackage][1] via cabal install:
```
$ cabal install hdevtools
```

Then you should install one or more of the following editor plugins:

### Vim - [Syntastic][2] ###

[Syntastic][2] is a popular syntax checking plugin for Vim, and is the
recommended Vim integration of `hdevtools` syntax and type checking. Recent
versions of Syntastic(since Sep. 2012) have builtin support for `hdevtools`.

Simply install `hdevtools` (as above) and [Syntastic][2], and it will
automatically check your Haskell files.

[Syntastic][2] will respect the `g:hdevtools_options` variable (the same one as
used by [vim-hdevtools][3], see below). See the section "Specifying GHC
Options" below for details how to use it.

### Vim - [vim-hdevtools][3] ###

In addition to Syntastic, it is recommended that you also use
[`vim-hdevtools`][3] for additional functionality.

[`vim-hdevtools`][3] offers integration with the rest of the `hdevtools` tools,
including retrieving info about the identifier under the cursor, and getting
the type of the code under the cursor. Refer to its documentation for more
details.

### Emacs - [flycheck][5] ###

[Flycheck][5] is a modern batteries-included
syntax checker for Emacs, and there is a [flycheck-hdevtools][6] checker available.

### Atom - [linter][8] ###

There are *two* packages for the [Atom](https://atom.io) editor:

+ [linter-hdevtools][8] quickly finds and underlines type errors in Haskell files,
+ [hover-tooltips-hdevtools][9] displays the types of identifiers under the mouse.

### Sublime - [SublimeLinter][10]

[SublimeLinter][10] is a plugin for Sublime Text 3 that provides a framework
for linting code. The [SublimeLinter-contrib-hdevtools][11] plugin uses
`hdevtools` to typecheck Haskell code.

### Manual Editor Integration for any Editor ###

Most editors allow you to run a `make` command, and will then parse the output
for errors and show line numbers, allowing you to jump between errors.

The `hdevtools check` command is suitable for such usage.

For example, in Vim something like this will work:

    :let &makeprg='hdevtools check %'

(Vim will replace the `%` character with the name of the current file). Then
you can run

    :make

And Vim will invoke `hdevtools` to check the current file for errors, and then
show a list of them and allow jumping to them.

See the "Command Line Usage" section below for more information.

Command Line Usage
------------------

Note: When using one of the above editor plugins, you don't really need to know
this.

### Available Commands and Help ###

For the list of commands available, run:

    $ hdevtools --help

To get help for a specific command, run:

    $ hdevtools [COMMAND] --help

For example:

    $ hdevtools check --help

### The `hdevtools` background process ###

The first time `hdevtools` runs a command, it will spawn a background process
that will remain running forever. You can check the status of this background
process by running:

    $ hdevtools --status

You can shutdown the background process by running:

    $ hdevtools --stop-server

Communication with the background process is done through a unix socket file.
The default name is `.hdevtools.sock`, in the current directory. This allows
you to use `hdevtools` with multiple projects simultaneously, without the
background processes getting in the way of each other.

You can use a different socket file name with the `--socket` option, which
should be used for each invocation of `hdevtools`. Remember that when telling
`hdevtools` to check a Haskell file, paths are relative to the path of the
background process, not your current directory. This can cause problems, and
therefore it is recommended that you leave the socket file as the default, and
always run `hdevtools` from the same directory.

You can specify the path to a target file with the `--path` option. This is
useful for integration with IDEs that submit a *copy* of the original source
file (in a temporary directory) to `hdevtools` making it impossible to extract
the `.cabal` information for the file's project. In such cases, you can run as:

    $ hdevtools check -p /path/to/file.hs /tmp/file.hs

and `hdevtools` will use the given path to obtain the `.cabal` information.


### Specifying GHC Options ###

For most non-trivial projects, you will need to tell `hdevtools` about
additional GHC options that your project requires.

All `hdevtools` commands accept a `-g` flag for this purpose.

For example:

* Your project source code is in the directory `src`
* You want to use the GHC option `-Wall`
* You want to hide the package `transformers` to prevent conflicts

Invoke `hdevtools` with something like this:

    $ hdevtools check -g -isrc -g -Wall -g -hide-package -g transformers Foo.hs

Notice that a `-g` flag is inserted before each GHC option. Don't try to string
multiple GHC options together after a single `-g` flag:

This won't work:

    $ hdevtools check -g '-hide-package transformers' Foo.hs

The Vim plugins allow setting GHC options in the `g:hdevtools_options`
variable.  For example, for the above project, put the following in your
`.vimrc`:

    let g:hdevtools_options = '-g -isrc -g -Wall -g -hide-package -g transformers'

In general, you will need to pass to `hdevtools` the same GHC options that you
would pass to GHCi.

Credits
-------

* `hdevtools` was inspired by [ghcmod][4].
* development moved here from [bitc/hdevtools][12] and [schell/hdevtools][13].

[1]: http://hackage.haskell.org/package/hdevtools
[2]: https://github.com/scrooloose/syntastic
[3]: https://github.com/bitc/vim-hdevtools
[4]: http://www.mew.org/~kazu/proj/ghc-mod/en/
[5]: https://github.com/flycheck/flycheck
[6]: https://github.com/flycheck/flycheck-hdevtools
[7]: https://atom.io
[8]: https://atom.io/packages/linter-hdevtools
[9]: https://atom.io/packages/hover-tooltips-hdevtools
[10]: sublimelinter.com
[11]: https://packagecontrol.io/packages/SublimeLinter-contrib-hdevtools
[12]: https://github.com/bitc/hdevtools
[13]: https://github.com/schell/hdevtools
[14]: https://www.stackage.org/package/hdevtools
[15]: haskellstack.org
