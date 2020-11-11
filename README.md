# HOVALAAG Superoptimization

[HOVALAAG](http://silverspaceship.com/hovalaag/) is a fictional VILW processor and architecture, built for the video game of the same name.

But what's more fun than playing video games? Asking the finest SMT solvers to solve the challanges for you!

This project provides a Racket ISA simulator for HOVALAAG, supporting all features including IO, and an example of using Rosette to solve a basic program-design query. I may in uture extend this to try and solve the challanges, but making a query sounded like the hard part so there's no timeline on that.

## Usage

`racket solver.rkt` to query the solution to i*8=16, where i*8 is implimneted as the example program from the HOVALAAG referance manual.

To write/run HOVALAAG programs, you can use the `run` function. This returns a full trace of microarch state throughout the program. `run-output` takes the final output value after the specified number of clocks.

A program is a list of instructions - there is no assembler, you provide the control signal for each unit individually. You also need to provide a inital `reg`. `reg` is the full arch state, primerally register valies but including the input queues (as they have the same access latency as registers).
