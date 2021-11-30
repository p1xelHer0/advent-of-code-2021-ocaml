# ✨ Advent of Code 2021 ✨

## My solutions to [Advent of Code 2021](https://adventofcode.com/2021/) written in [OCaml](https://ocaml.org/)

### Setup

- Install [Node.js](https://nodejs.org/), I use [fnm](https://github.com/Schniz/fnmvv/)

- Install [esy](https://esy.sh/) locally using `npm`

```
npm i
```

- Build dependencies using `esy`

```
npx esy
```

### Run [inline tests](https://github.com/janestreet/ppx_inline_test/)

```
npx esy test
```

or in watch mode

```
npx esy test:watch
```

### Running puzzles

Puzzles are numbered `{1..25}`

```
npx esy dune exec bin/day_1/puzzle.exe
```
