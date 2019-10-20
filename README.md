# instant-lang

Assessment task for MIMUW course "Methods of Implementing Programming Languages"

## Dependencies

Following external libraries were used:
- process
- filepath
- megaparsec
- mtl
- containers
- transformers
- parser-combinators


## Task contents

A program in the Instant language consists of a sequence of statements separated by semicolons.

There are two kinds of statements:

- expression - prints its value on stdout,
- assignment of the form `variable = expression` - assigns value of the expression to he variable in the LHS; does not print anything.

Expressions are built from integer literals, variables and arithmetic operators. Evaluation order within an expression is not predefined (you can choose whatever order suits you best)

BNFC syntax:

```
Prog. Program ::= [Stmt] ;
SAss. Stmt ::= Ident "=" Exp;
SExp. Stmt ::= Exp ;
separator Stmt ";" ;

ExpAdd.            Exp1   ::= Exp2 "+"  Exp1 ;
ExpSub.            Exp2   ::= Exp2 "-"  Exp3 ;
ExpMul.            Exp3   ::= Exp3 "*"  Exp4 ;
ExpDiv.            Exp3   ::= Exp3 "/"  Exp4 ;
ExpLit.            Exp4   ::= Integer ;
ExpVar.            Exp4   ::= Ident ;
coercions Exp 4;
```

Note:

- addition binds to the **right**
- addition and multiplication are commutative but not associative

Your task is to write a compiler from Instant to JVM and LLVM.

In this assignment, the generated code should execute all the operations specified in the input program. Hence it is not allowed to replace the expression 2+3 by constant 5, omitting assignments to unused variables, etc. Improving generated code will be a subject of later assignments.

The only allowed, indeed desirable improvement is choosing an evaluation order so as to minimize the needed JVM stack size. In any case needed stack size must be computed and declared. (clever solutions like `.limit stack 1000` will not be appreciated). Similarly you should compute and declare the number of needed locals.

### Technical requirements
- Your solution should be submitted as a packed tar achive (`.tar.gz` or `.tgz`)
- The root directory of this archive should contain at least
- A text file README describing how to compile and run the program, used libraries and tools, project directory structure, possibly references to more detaild documentation.
- A Makefile to build the project
- src directory containing only source files of your solution (and possibly the `Instant.cf` supplied by us); auxiliary files should be placed in other directories.
- All used libraries (apart from the standard library of the programming language used) must be described in README
- Your submission must compile on students by running make
- After the build, the root of the project must contain executable files `insc_jvm` and `insc_llvm`
- Executing `insc_jvm foo/bar/baz.ins` for a correct program baz.ins should create files `baz.j` (Jasmin) and `baz.class` (JVM) in the directory `foo/bar` (running Jasmin with `-d` may be helpful here). Needed runtime library methods should be placed in `Runtime.class` in the lib subdirectory.
- Executing `insc_llvm foo/bar/baz.ins` for a correct program baz.ins should create files `baz.ll` (text LLVM) and `baz.bc` (lli-exectutable bitcode) in the directory `foo/bar`.

### Example programs
The archive [instant161024.tgz](https://www.mimuw.edu.pl/~ben/Zajecia/Mrj2019/instant161024.tgz) contains example programs with their expected output, as well as Instant.cf containing the BNFC grammar of Instant.
