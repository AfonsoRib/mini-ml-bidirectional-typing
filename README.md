# A ml like language that implements a bidirectional typechecker

This is the implementation of "Bidirectional Typing Rules: A Tutorial"
by David Raymond Christiansen.

~~The tokenizer is made with parsec and the parser itself is made with
Earley. Couldn't find any other good examples of Earley code so I hope
this may serve a simple example for it.~~

The parser is made entirly with parsec. I initially went with Earley
because I was still a bit clueless on left recursion but overtime I
managed to fix it. If you want a simple example of Earley code there
is a branch for it but is unmaintained.

It's only meant to be a typechecker, it does not eval code (as of now).
