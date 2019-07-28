# relational-algebra-haskell

This project contains a shallow embedding of an extension of relational algebra
in Haskell. The extension allows relations that are not in first normal form,
i.e., nested relations.

## Instructions

We will assume that this project exists locally and we are logged into a shell
where the working directory is the root of the project.

This project uses [Stack][stack] to simplify dependency management.

### Build

Build the project with the command `stack build`.

### Test

Test the project with the command `stack test`.

[stack]: https://www.haskellstack.org
