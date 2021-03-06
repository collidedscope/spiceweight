# Spiceweight

**Spiceweight** is an interpreter for the [Whitespace programming language](https://en.wikipedia.org/wiki/Whitespace_(programming_language)) written in Crystal. It provides a handful of very useful features, to include:

* negative heap addressing, which allows standard library routines (such as the ones in [libspw](https://github.com/collidedscope/spitewaste/tree/master/lib/spitewaste/libspw)) to perform their work without any risk of clobbering the "user" data in positive heap space. This breaks spec with the reference implementation, but it's a worthwhile trade-off.
* arbitrary-precision integers, but only when necessary; Spiceweight checks whether an arithmetic operation would overflow and only then promotes the result to `BigInt`, so no undue performance penalty is incurred. This feature is notably absent from some otherwise excellent interpreters, such as [Pavel Shub's `wspace`](http://pavelshub.com/blog/2010/10/wspace/).
* benchmarking calls to labels whose names begin with "bench", so long as a symbol file has been previously generated by a tool such as [`spw`](../../../spitewaste).

## Installation

Simply have [Crystal](https://crystal-lang.org/install/) installed and run `make` to build the executable (**`spwt`**). You can then copy it to a `$PATH` directory of your choosing or run the provided `./install` script to automate the process.

## Usage
```
$ spwt --help
Usage: spwt [OPTIONS] FILE
    -r, --report                     Display stack, heap, and instruction count after executing
    -b, --bench                      Benchmark calls to labels whose names begin with 'bench'
Labels in Whitespace code are purely numerical, of course, so this relies on
some other tool having previously generated a symbol table mapping label names
to their corresponding numbers. The Spitewaste assembler is one such tool.
    -h, --help                       Show this help
```
In most cases, you'll simply want to run `spwt foo.ws` to execute a Whitespace program. The `--report` switch is nice for ensuring that your program exits with a clean stack. The symbol file needed by `--bench` is automatically generated and stored somewhere Spiceweight knows to find it by the [Spitewaste assembler](../../../spitewaste) when passing `--symbol-file=%` to `spw exec|convert`.
