A `maya.cmds` typings generator for Python.
Written in Rust, because why not. It's fun.

# Just give me the types

[maya.cmds typings for Maya 2023](./typings/maya/cmds/__init__.pyi)

# Usage

- Download the documentation for your Maya version, for example: https://download.autodesk.com/akn/2023/maya/autodesk-maya-user-guide-2023.3-en.zip
- Copy the "CommandsPython" folder from there and into the `./source_docs/2023` folder here.
- `cargo run`

# Status

This is a work-in-progress and does not yet generate full type definitions.

The main goal of this project is to make use of `Typing.overload` (Available in Python3.9+), such that the return values of functions in query mode return the correct type. This should make Pylance happier for you!

# Limitations

## Short and Long-form flags

Long and short-form flags cannot be mixed with these type definitions. This will produce an error:

`maya.cmds.xform(q=True,matrix=True)`

Instead do one of these:

`maya.cmds.xform(q=True,m=True)`

`maya.cmds.xform(query=True,matrix=True)`
