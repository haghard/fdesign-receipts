

https://github.com/jdegoes/functional-design

https://medium.com/bigdatarepublic/writing-functional-dsls-for-business-domains-1bccc5d3f62b


### Encoding

* Executable (initial) encoding (a case class with a raw function to execute smth)

    +) Easy to add operators and constructors

    +) Simple to understand and compose, ready to run. No need to build an AST and then interprete it

    +) Performance, no extra layers

    +) Easy to start using in legacy codebases.

    -) Lack of introspection

* Declarative encoding instead of case classes we build an ADT

   +) Optimizations

   +) Auditing

   -) Can't easy add new operations without changing all existing code(interpreters)

### Design principles


There are many ways to factor a DSL, but some are better than others. These guiding principles help come to a good design. Our components should be
 * Composable, to permit a lot of power in a small, reasonable package
 * Expressive, to solve the full range of problems in the domain
 * Orthogonal, such that no primitive provides the capabilities of any other