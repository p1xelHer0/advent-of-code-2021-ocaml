# ✨ Advent of Code 2021 ✨

## My solutions to [Advent of Code 2021](https://adventofcode.com/2021/) written in [OCaml](https://ocaml.org/)

### Setup

- Install [Node.js](https://nodejs.org/), I use [fnm](https://github.com/Schniz/fnmvv/)

- Install [esy](https://esy.sh/) locally using `npm`

```bash
npm i
```

- Build dependencies using `esy`

```bash
npx esy
```

### Run [inline tests](https://github.com/janestreet/ppx_inline_test/)

```bash
npx esy test
```

- or in watch mode

```bash
npx esy test:watch
```

### Running puzzles

Puzzles are numbered `day_{1..25}`

- For example, to run the puzzle of day 1

```bash
npx esy dune exec bin/day_1/puzzle.exe
Day 1A: <result>
Day 1B: <result>
```
