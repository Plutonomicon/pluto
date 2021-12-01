# `pluto`

## The Plutus Core assembler

`pluto` is a programming language which is essentially Untyped Plutus Core (UPLC) with a little bit of syntactic sugar. `pluto` is written in a Haskell-like human-friendly syntax. UPLC is an untyped lambda calculus with strict evaluation semantics.

`pluto` is really nothing more than a syntax for writing UPLC directly. Why would you do that, you ask? Probably, the reason is that the Haskell to Plutus compiler's output was too big when you gave it your program as input. UPLC is the language which the Cardano blockchain runs. It is the language in which on-chain code is delivered on the chain. The size of on-chain dapp code is critical to the overall throughput of the network. Therefore writing this code in hand-optimized UPLC makes sense, if the results are significantly smaller than the output of the Haskell to Plutus compiler for a solution to the same problem. `pluto` was written in part to test this hypothesis, that dapps can be optimized by rewriting their on-chain code directly in Plutus Core.

### Fair warning

`pluto` is experimental and largely untested. Expect bugs.

### System requirements

To build `pluto`, you must have Nix 2.4. You can install this on NixOS adding the following
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

To build:

```
nix build
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

Run `nix develop` to drop yourself in the development shell. From here, you may launch your text-editor and get access to IDE support via Haskell Language Server, as well use `cabal` to build and run the project.

To run the HelloWorld example,

```
cabal run pluto -- run examples/hello.pluto
```

To only assemble the Pluto program into a Plutus bytecode:

```
cabal run pluto -- assemble examples/hello.pluto
```

For rapid compilation feedback cycle, Ghcid can be run as follows,

```
ghcid -T PlutusCore.Assembler.EntryPoint.main --setup ":set args run examples/hello.pluto"
```
