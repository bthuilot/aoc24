# Advent of Code 2024 - Racket

### Index

1. [Building/Running](#Buidling/Running)
2. [Tests](#Tests)
3. [Completed Days](#Completed-Days)

This repositories stores my solutions in [Racket](https://racket-lang.org/) to [Advent of Code 2024](https://adventofcode.com/2024).

For more info on the approach to each day,
read the module header comment located at the top of each day's source file 
(an index is located below)


## Building/Running

This project leverages dune to compile and run the project, to build an executable simply run

```bash
racket main.rkt
# or
make run

# Optionally specify only a subset of days to run
racket main.rkt -d 1,8,23 # Run days 1, 8 and 23
```

## Tests

Tests are written using [RackUnit](https://docs.racket-lang.org/rackunit/)

To test functionaility of the project, invoke the following:

```bash
make test
# Or
raco test .
```

## Completed Days

Below is an index to every completed day's implementation source code (containing documentation of approach) and the challenge for the day

- [Day 0](days/00.rkt) : *This is a test day supposed to server a placeholder until the challenge starts*
<!-- - [Day 1](days/01.rkt) : [Problem](https://adventofcode.com/2024/day/1) -->
<!-- - [Day 2](days/02.rkt) : [Problem](https://adventofcode.com/2024/day/2) -->
<!-- - [Day 3](days/03.rkt) : [Problem](https://adventofcode.com/2024/day/3) -->
<!-- - [Day 4](days/04.rkt) : [Problem](https://adventofcode.com/2024/day/4) -->
<!-- - [Day 5](days/05.rkt) : [Problem](https://adventofcode.com/2024/day/5) -->
<!-- - [Day 6](days/06.rkt) : [Problem](https://adventofcode.com/2024/day/6) -->
<!-- - [Day 7](days/07.rkt) : [Problem](https://adventofcode.com/2024/day/7) -->
<!-- - [Day 8](days/08.rkt) : [Problem](https://adventofcode.com/2024/day/8) -->
<!-- - [Day 9](days/09.rkt) : [Problem](https://adventofcode.com/2024/day/9) -->
<!-- - [Day 10](days/10.rkt) : [Problem](https://adventofcode.com/2024/day/10) -->
<!-- - [Day 11](days/11.rkt) : [Problem](https://adventofcode.com/2024/day/11) -->
<!-- - [Day 12](days/12.rkt) : [Problem](https://adventofcode.com/2024/day/12) -->
<!-- - [Day 13](days/13.rkt) : [Problem](https://adventofcode.com/2024/day/13) -->
<!-- - [Day 14](days/14.rkt) : [Problem](https://adventofcode.com/2024/day/14) -->
<!-- - [Day 15](days/15.rkt) : [Problem](https://adventofcode.com/2024/day/15) -->
<!-- - [Day 16](days/16.rkt) : [Problem](https://adventofcode.com/2024/day/16) -->
<!-- - [Day 17](days/17.rkt) : [Problem](https://adventofcode.com/2024/day/17) -->
<!-- - [Day 18](days/18.rkt) : [Problem](https://adventofcode.com/2024/day/18) -->
<!-- - [Day 19](days/19.rkt) : [Problem](https://adventofcode.com/2024/day/19) -->
<!-- - [Day 20](days/20.rkt) : [Problem](https://adventofcode.com/2024/day/20) -->
<!-- - [Day 21](days/21.rkt) : [Problem](https://adventofcode.com/2024/day/21) -->
<!-- - [Day 22](days/22.rkt) : [Problem](https://adventofcode.com/2024/day/22) -->
