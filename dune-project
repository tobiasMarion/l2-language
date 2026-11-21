(lang dune 3.25)

(name l2)

(version 0.1.0)

(generate_opam_files true)

(source
 (github tobiasMarion/l2-language))

(authors "Gabriel Richter" "João Kenji Suwa" "Tobias Cadoná Marion")

(maintainers "Gabriel Richter" "João Kenji Suwa" "Tobias Cadoná Marion")

(license MIT)

(package
 (name l2)
 (synopsis
  "Parser, type inference and big-step evaluator for L2, a functional language with imperative constructs")
 (description
  "An implementation of L2: arithmetic and boolean expressions, typed let bindings, conditionals, and imperative constructs (references with new, dereference and assignment, sequencing and while loops). The project contains a parser, a type inference algorithm and a big-step operational semantics evaluator. Final project for Programming Languages I (INF05029) at the Institute of Informatics, Federal University of Rio Grande do Sul (UFRGS), 2026/2.")
 (tags
  ("programming languages"
   "interpreter"
   "type inference"
   "type system"
   "operational semantics"
   "big-step semantics"
   "parser"
   "references"))
 (depends
  (ocaml
   (>= 5.4))
  dune
  ; Development tools: installed only with --with-dev-setup
  (ocaml-lsp-server :with-dev-setup)
  (ocamlformat
   (and
    :with-dev-setup
    (= 0.29.0)))
  (utop :with-dev-setup)))

; Reference: https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
