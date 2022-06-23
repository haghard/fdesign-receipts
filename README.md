### Encodings

* Executable (initial) encoding (a case class with a raw function to execute smth)

    +) Easy to add operators and constructors

    +) Simple to understand and compose, ready to run. No need to build an AST and then interpret it

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


### Links


https://github.com/jdegoes/functional-design

Far more than you've ever wanted to know about ADTs by Nicolas Rinaudo at FP in the City Conference: https://youtu.be/MqGWb7OvVqs

https://medium.com/bigdatarepublic/writing-functional-dsls-for-business-domains-1bccc5d3f62b

Declarative vs Executable Encodings: https://youtu.be/OD1Yr48-0Js

Data Oriented Programming in Java: https://www.infoq.com/articles/data-oriented-programming-java/