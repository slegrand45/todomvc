#!/bin/sh
ocamlbuild -use-ocamlfind \
  -pkgs lwt.syntax,js_of_ocaml,js_of_ocaml.syntax,js_of_ocaml.tyxml,tyxml,js_of_ocaml.deriving,js_of_ocaml.deriving.syntax,deriving \
  -syntax camlp4o \
  todomvc.byte

js_of_ocaml +weak.js --opt 3 -o js/todomvc.js todomvc.byte

# To disable JS linters at a file level, you can for instance use sed:
# sed -i '' '1i\
# // jscs:disable
# ' js/todomvc.js
# sed -i '' '1i\
# /* jshint ignore:start */
# ' js/todomvc.js

