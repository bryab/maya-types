A `maya.cmds` typings generator for Python.

Written in Rust, because why not. It's fun.

Inspired by this Python script:

https://gist.github.com/yamahigashi/da1876765de3c8e5f570a9c57baa45d6

# Status

This is a work-in-progress and does not yet generate full type definitions.
The goal of this project is to make use of `Typing.overload` to make many different
overloads. Each mode, along with the short and long-form flag names, and along with the various
return types in query mode, will be a different overload.

# Limitations

## Short and Long-form parameters

Mixing "short" and "long" form parameters is not allowed. For example, this will not work:

`maya.cmds.xform(q=True,matrix=True)`

Instead do one of these:

`maya.cmds.xform(q=True,m=True)`
`maya.cmds.xform(query=True,matrix=True)`
