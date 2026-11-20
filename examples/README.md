# Examples

Each folder holds one L2 program twice:

- `program.l2`: the source, in the concrete syntax of the assignment
  (`let x : T = e1 in {e2}`, `while e1 do {e2}`, `if e1 then {e2} else {e3}`);
- `program.ml`: the AST the parser should produce for it, built with the
  constructors of [`lib/ast.ml`](../lib/ast.ml).

| Example                   | Shows                                   | Result       |
| ------------------------- | --------------------------------------- | ------------ |
| [`max`](max)              | `let`, `if` and comparison              | `7 : int`    |
| [`aliasing`](aliasing)    | two names for the same reference        | `42 : int`   |
| [`factorial`](factorial)  | `while`, assignment and sequencing      | `120 : int`  |
