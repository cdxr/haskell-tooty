### tooty

`tooty` is a small experimental Haskell library for 2D drawing in OpenGL. The
goal is to create a functional-style DSL similar in spirit to
`graphics-drawingcombinators`, but larger in scope, and exposing the underlying
OpenGL concepts.

At present moment the library is defined in terms of effectful functions in IO.
This is a fragile but convenient and fast way for me to explore the design
space and prototype the API. Once I have the bulk of the functionality
organized to my satisfaction, I intend to reimplement the library in terms of
pure computations with well-defined semantics.
