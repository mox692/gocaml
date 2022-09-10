# gocaml
gocaml is a REPL interpreter with syntax like [Go](https://go.dev/).

# Install

### Ocaml toolcain
OCaml toolchain needed.

* On macOS
```
$ brew install opam
```

* On Linux
```
# Ubuntu
$ add-apt-repository ppa:avsm/ppa
$ apt update
$ apt install opam

# Archlinux
$ pacman -S opam

# Debian (stable, testing and unstable)
$ apt-get install opam
```

[More detailed information](https://ocaml.org/docs/up-and-running)

### build & run
Just run make.
```
$ make

$ ./gocaml
```

# Supported syntax
Currently, only some simple syntax supported.  


*arithmetic operations*
```
4 + 3 * 2
=> 10
```
*variable*
```
foo := 42
=> 0

foo
=> 42
```
*if statement*
```
a := 0
=> 0

if 1 == 1 {
    a = 42
}
=> 0

a
=> 42
```
*struct declare*
```
type A struct {
  foo int          
}
=> 0

a := A {
  foo: 42
}
=> 0

a.foo
=> 42
```
