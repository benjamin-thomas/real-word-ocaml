(lang dune 3.15)

(; opam install . --deps-only [--with-doc --with-test]
 generate_opam_files)

(package
 (allow_empty)
 (name dev)
 (depends base))
