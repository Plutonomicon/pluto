# `pluto`

[![Hercules-ci][Herc badge]][Herc link]
[![Cachix Cache][Cachix badge]][Cachix link]
[![Built with Nix][Built with Nix badge]][Built with Nix link]

[Herc badge]: https://img.shields.io/badge/ci--by--hercules-green.svg
[Herc link]: https://hercules-ci.com/github/Plutonomicon/pluto 
[Cachix badge]: https://img.shields.io/badge/cachix-plutonomicon--pluto-blue.svg
[Cachix link]: https://public-plutonomicon.cachix.org
[Built with Nix badge]: https://builtwithnix.org/badge.svg
[Built with Nix link]: https://builtwithnix.org


## The Plutus Core assembler

`pluto` is a programming language which is essentially [Untyped Plutus Core](https://iohk.io/en/blog/posts/2021/02/02/plutus-tx-compiling-haskell-into-plutus-core/) (UPLC) with a little bit of syntactic sugar. `pluto` is written in a Haskell-like human-friendly syntax. UPLC is an untyped lambda calculus with strict evaluation semantics.

`pluto` is really nothing more than a syntax for writing UPLC directly. Why would you do that, you ask? Probably, the reason is that the Haskell to Plutus compiler's output was too big when you gave it your program as input. UPLC is the language which the Cardano blockchain runs. It is the language in which on-chain code is delivered on the chain. The size of on-chain dapp code is critical to the overall throughput of the network. Therefore writing this code in hand-optimized UPLC makes sense, if the results are significantly smaller than the output of the Haskell to Plutus compiler for a solution to the same problem. `pluto` was written in part to test this hypothesis, that dapps can be optimized by rewriting their on-chain code directly in Plutus Core.

### Fair warning

`pluto` is experimental and almost entirely untested in practice. Expect bugs.

### System requirements

Your system must have Nix installed in order to build `pluto`. Currently, there are no binary distributions of `pluto`; you must build from source. See https://nixos.org/download.html for instructions to install Nix.

#### Nix flakes (optional)

To build `pluto` as a flake, you must have Nix 2.4. You can install this on NixOS adding the following
stanza to your configuration.nix.

```
nix = {
  package = pkgs.nixUnstable;
  extraOptions = ''
   experimental-features = nix-command flakes
  '';
};
```

Check that your `nix --version` is at least 2.4pre.

To install flakes on non-NixOS, run this to get the latest Nix 2.4:

```
curl -L https://nixos.org/nix/install | sh
```

Then edit (or create) `~/.config/nix/nix.conf` to include the following line:

```
experimental-features = nix-command flakes
```

### Build instructions

```
nix-build
```

### Usage instructions.

These usage instructions assume a `pluto` binary is on your shell `PATH`. If you ran `nix build`, then there is a `pluto` binary at `./result/bin/pluto`.

To print usage instructions:

```
pluto --help
```

To print usage instructions on a specific command:

```
pluto --help [command]
```

There are two commands: `assemble` and `run`.

 * `assemble` takes Pluto source code and turns it into binary object code.
 * `run` takes Pluto source code and executes it.

### Syntax

To get a feel for the syntax, take a look at some examples, in `./examples`.

Pluto has token-level (lexical) syntax and term-level syntax. First the text is lexically analyzed into a series of whitespace-separated tokens. Then the tokens are parsed into an AST.

The grammars below are written in semi-formal notation, a mixture of BNF-like notation and Perl-compatible regular expression notation.

#### Lexical grammar

```
Start ::= Whitespace* (Token Whitespace*)*

Whitespace ::= [\t\r\n] | OneLineComment | MultiLineComment

OneLineComment ::= "--" ([^\r\n]*[\r\n])

MultiLineComment ::= "{-" MultiLineCommentEnding

MultiLineCommentEnding ::= "-}" | . MultiLineCommentEnding

Token ::= "\\" | "->" | 
        | "!" | "#" | "(" | ")" | "Error"
        | Integer | ByteString | Text | "True" | "False"
        | "[" | "]" | "," | "." | "`" | "{" | "}"
        | "data" | "sigma" | "=" | Builtin | "let"
        | ";" | "in" | "if" | "then" | "else"
        | Var

Var ::= [a-z][a-zA-Z0-9]*

Integer ::= "-"? [0-9]+

ByteString ::= "0x" [0-9a-eA-E]+

Text ::= "\"" ( "\\" [\\rnt] | [^\r\n\t\\] )* "\""

InfixBuiltin ::= 
  "+i" | "-i" | "*i" | "/i" | "%i"
       | "==i" | "<i" | "<=i" | "+b" | ":b" | "!b"
       | "==b" | "<b" | "<=b" | "+s" | "==s" | "==d"

Builtin ::=
    "AddInteger" | "SubtractInteger" | "MultiplyInteger"
  | "DivideInteger" | "QuotientInteger" | "RemainderInteger"
  | "ModInteger" | "EqualsInteger" | "LessThanInteger"
  | "LessThanEqualsInteger" | "AppendByteString"
  | "ConsByteString" | "SliceByteString" | "LengthByteString"
  | "IndexByteString" | "EqualsByteString" | "LessThanByteString"
  | "LessThanEqualByteString" | "Sha2_256" | "Sha3_256" | "Blake2b_256"
  | "VerifySignature" | "AppendString" | "EqualsString"
  | "EncodeUtf8" | "DecodeUtf8" | "IfThenElse" | "ChooseUnit"
  | "MkCons" | "HeadList" | "TailList" | "NullList"
  | "ChooseData" | "ConstrData" | "MapData" | "ListData"
  | "IData" | "BData" | "UnConstrData" | "UnMapData"
  | "EqualsData" | "MkPairData" | "MkNilData" | "MkNilPairData"
```

#### Syntactical grammar

```
Start ::= Program

Program ::= Term

Term ::= Term0

Term0 ::= Lambda | Term1

Lambda ::= "\" Var+ "->" Term

Term1 ::= IfTerm | LetTerm | Term2

IfTerm ::= "if" Term2 "then" Term2 "else" Term1

LetTerm ::= "let" Binding ( ";" Binding )* "in" Term2

Term2 ::= InfixApply | Term3

InfixApply ::= Term3 InfixOp Term3

InfixOp ::= InfixBuiltin | BacktickInfix

BacktickInfix ::= "`" ( Var | Builtin ) "`"

Term3 ::= Term4+

Term4 ::= "!" Term5 | "#" Term5 | Term5

Term5 ::= Var | Builtin | "Error" | ParenTerm | Constant

ParenTerm ::= "(" Term ")"

Unit ::= "(" ")"

Constant ::= Bool | Integer | ByteString | Text | Data

Data ::= "data" ( Sigma | List | Map | Integer | ByteString )

Sigma ::= "sigma" Integer "." "[" Data* "]"

List ::= "[" Data* "]"

Map ::= "{" ( Data "=" Data )* "}"
```

### Development guidelines

Run `nix-shell` (or `nix develop`, if you prefer to use the flake and you have Nix 2.4) to drop yourself in the development shell. From here, you may launch your text-editor and get access to IDE support via Haskell Language Server, as well use `cabal` to build and run the project.

For rapid compilation feedback cycle, Ghcid can be run as follows,

```
ghcid -T PlutusCore.Assembler.EntryPoint.main --setup ":set args run examples/hello.pluto"
```

To run specific tests (eg: tests on example) in Ghcid,

```
ghcid -c 'cabal repl spec' -T Main.main --setup ':set args "-p" example'
```

#### Checks to do before submitting a PR

Does the style / lint check pass? With a clean checkout, run:

```
./ci/lint.sh
```

This will update the Haskell code with any changes from stylish-haskell. If you have a clean checkout and stylish-haskell produces no changes, then it will output any lint recommendations. Implement all lint recommendations, such that the above script produces no style changes no recommendations.

Do the tests pass?

```
cabal test
```

Do the examples compile?

```
nix-build && ./ci/examples.sh
```

Currently, you must do all of these steps manually, or run the `manual-ci` function inside a nix shell, as we do not have CI set up.

### Examples

#### `run`

To run the HelloWorld example,

```
cabal run pluto -- run examples/hello.pluto 0x$(echo -n "Charles" | od -A n -t x1 | sed 's/ *//g') -v
```

Note here that:

- The `-v` option dumps intermediate ASTs. Ignore it, to display only the script out.
- We pass a bytestring to the script, as a Pluto literal (which represents bytestrings beginning with `0x`). In general any Plutus `Data` value is accepted.

The echo example can be used to view the Plutus `Data` representation of a given Pluto value, for example:

```
$ cabal run pluto -- run examples/echo.pluto '{ 2 = 43 }'
Constant () (Some (ValueOf data (Map [(I 2,I 43)])))
$
```

#### `eval`

`eval` can be used to evaluate a top-level binding with (optional) arguments that represented as a Pluto expression. For example, this command evalutes the `greet` function from `hello.pluto` by applying it with the two given arguments.

```
cabal run pluto -- eval examples/hello.pluto greet '"Bonjour"' '"Charles"'
```

Top-level variables can also be accessed by ignoring the arguments:

```
cabal run pluto -- eval examples/hello.pluto defaultGreeting
```

Note the distinction between `run` and `eval` *in regards to the type of the arguments*.  `run` expects a Plutus `Data` value; `eval` expects a Pluto expression.

#### `assemble`

To only assemble the Pluto program into a Plutus bytecode:

```
cabal run pluto -- assemble examples/hello.pluto
```

#### Sample Contract

See `examples/contracts/sample` for the sample "gift" contract, written in both Haskell and Pluto. This example includes QuickCheck ContractModel tests to test the contract using both Haskell and Pluto version. It demonstrates how to unpack `ScriptContext` in Pluto.

To run the sample contract tests,

```
cabal run pluto-sample-contract
```

### Haskell FFI 

Pluto programs can be accessed and evaluated (via Plutus Core) from Haskell code as follows:

```haskell
import qualified PlutusCore.Assembler.Types.AST as AST
import qualified PlutusCore.Assembler.FFI as FFI

hello :: AST.Program ()
hello = $(FFI.load "examples/hello.pluto")

$(FFI.bind 'hello
  "defaultGreeting" [t|String|])

$(FFI.bind 'hello
  "greet" [t|String -> String -> String|])
```

The above exposes the two top-level bindings from the hello.pluto program, using the given type declaration. 

### Working with Plutus `Data`

The `data` keyword can be used to create lists, maps, and sigma types. For example:

```lisp
let 
  x = data [1, 2, 3]
in 
  x
```

Plutus provides a number of builtins to work with `Data`. For operating on lists, we have:

- `UnListData :: Data -> [Data]` (fails if the Data is not a list)
- `HeadList :: [a] -> a` (fails if list is empty)
- `TailList :: [a] -> [a]`
- `ChooseList` can be used to "case" on lists

Higher-level operations on lists can be written in Pluto itself. For example, `fibonacci.pluto` and `sum.pluto` defines "fold".

```
$ cabal run pluto -- run examples/fibonacci.pluto 5
Constant () (Some (ValueOf data (List [I 8,I 5,I 3,I 2,I 1,I 1,I 0])))
```
