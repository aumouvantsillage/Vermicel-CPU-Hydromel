
Vermicel is a simple RISC processor core that implements most of the RISC-V base instruction set (RV32I).

This implementation is written in [Hydromel](https://github.com/aumouvantsillage/Hydromel-lang),
an experimental hardware description language built on top of the [Racket](https://racket-lang.org/) platform. 

Hydromel designs can be simulated but they cannot be synthesized yet.

A synthesizable version of Vermicel, written in SystemVerilog, is also available in the
[Vermicel-CPU-SystemVerilog repository](https://github.com/tiliosys/Vermicel-CPU-SystemVerilog).

## Running the demos

First install [Racket](https://racket-lang.org/) and [Hydromel](https://github.com/aumouvantsillage/Hydromel-lang)
package.

The demos are simple RISC-V programs written in a Racket-based assembly language.
They run on a minimal simulated computer with a Vermicel core, a text output peripheral, and a periodic interrupt generator.

### Hello World

Output a character string:

```
racket vermicel-demos/hello.rkt
```

### Fibonacci

Print the first terms of the Fibonacci sequence:

```
racket vermicel-demos/fibonacci.rkt
```

### Tick

Count and print the number of periodic interrupts:

```
racket vermicel-demos/tick.rkt
```

