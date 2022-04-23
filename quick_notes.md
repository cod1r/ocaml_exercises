When trying to build executables using `ocamlfind ocamlopt -package <pkg> -linkpkg <filename>.ml`,
if the filename of the ocaml file is the same as the package name, errors might be thrown.
