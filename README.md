# Quasi 🔮

Quasi (*/ˈkwāˌzī/*) is an interpreted programming language that's designed to be largely similar to most languages, but with slightly off-putting syntactical differences.

## Quick links

- [Installation](#installation)
- [Usage](#usage)
- [Examples](#examples)
- [Syntax](#syntax)
  - [Comments](#comments)
  - [Variables](#variables)
  - [Control flow](#control-flow)

## Installation

The binary name for Quasi is `quasi`.

Currently, building from source is the only way to install Quasi:

```bash
$ git clone https://github.com/matteopolak/quasi
$ cd quasi
$ cargo build --release
$ ./target/release/quasi --version
quasi 0.1.0
```

## Usage

```bash
Usage: quasi <PATH>

Arguments:
  <PATH>  Path to the script to execute

Options:
  -h, --help     Print help
  -V, --version  Print version
```

## Examples

Examples can be found in the [examples](examples) directory.

## Syntax

Quasi is a dynamically typed language and not whitespace sensitive.

### Comments

```py
# This is a comment
```

### Variables

```rust
let x = 1;
let y = 2;
let z = x + y;
```

### Control flow

```rust
if x == 1 {
  print "x is 1";
} else if x == 2 {
  print "x is 2";
} else
  print "x is neither 1 nor 2";
```

```rust
while x < 10 {
  print x;
  x = x + 1;
}
```
