# `pluto`

## The Plutus Core assembler

`pluto` is a programming language which is essentially Untyped Plutus Core (UPLC) with a little bit of syntactic sugar. `pluto` is written in a Haskell-like human-friendly syntax. UPLC is an untyped lambda calculus with strict evaluation semantics.

`pluto` is really nothing more than a syntax for writing UPLC directly. Why would you do that, you ask? Probably, the reason is that the Haskell to Plutus compiler's output was too big when you gave it your program as input. UPLC is the language which the Cardano blockchain runs. It is the language in which on-chain code is delivered on the chain. The size of on-chain dapp code is critical to the overall throughput of the network. Therefore writing this code in hand-optimized UPLC makes sense, if the results are significantly smaller than the output of the Haskell to Plutus compiler for a solution to the same problem. `pluto` was written in part to test this hypothesis, that dapps can be optimized by rewriting their on-chain code directly in Plutus Core.

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

```
nix build
```

### Syntax

To get a feel for the syntax, take a look at some examples.

What follows is not a formal grammar for `pluto`, or even a wholly accurate description of the grammar, but a rough description, sufficient to get the idea, allowing for all valid syntax, while also allowing for some edge cases which are disallowed in practice to avoid parsing ambiguity, but which people are not likely to write in practice. The main thing to consider that is part of the grammar of `pluto` but not captured in the description below is the fact that `pluto` requires nested applications of binary operations to be fully parenthesized, lacking any notion of operator precedence. The grammar of `pluto` is actually substantially simpler than that of most languages.

For a more formal description of the grammar of `pluto`, take a look at the code, in `PlutusCore.Assembler.Tokenize` and `PlutusCore.Assembler.Parse`. 

```
Program ::= Term

Term ::= Var | Lambda | Apply | Force | Delay | Constant | Builtin | 'Error'
       | Let | IfThenElse | '(' Term ')' | InfixApply

Var ::= [a-z][a-zA-Z0-9_]*

Lambda ::= '\' Var+ '->' Term

Apply ::= Term Term

Force ::= '!' Term

Delay ::= '#' Term

Constant ::= Integer | ByteString | Text | '()' | 'True' | 'False' | Data

Data ::= 'data' DataConstant

DataConstant ::= DataConstr | DataMap | DataList | Integer | ByteString

DataConstr ::= 'sigma' NonNegativeInteger '.' '[' (DataConstant ( , DataConstant)*)? ']'

DataMap ::= '{' (DataMapEntry (',' DataMapEntry)*)? '}'

DataMapEntry ::= DataConstant '=' DataConstant

DataList ::= '[' (DataConstant (',' DataConstant)*)? ']'

Builtin ::= 'AddInteger' | 'SubtractInteger' | 'MultiplyInteger' | 'DivideInteger'
          | 'QuotientInteger' | 'RemainderInteger' | 'ModInteger' | 'EqualsInteger'
          | 'LessThanInteger' | 'LessThanEqualsInteger' | 'AppendByteString'
          | 'ConsByteString' | 'SliceByteString' | 'LengthByteString' | 'IndexByteString'
          | 'EqualsByteString' | 'LessThanByteString' | 'LessThanEqualByteString'
          | 'Sha2_256' | 'Sha3_256' | 'Blake2b_256' | 'VerifySignature' | 'AppendString'
          | 'EqualsString' | 'EncodeUtf8' | 'DecodeUtf8' | 'IfThenElse' | 'ChooseUnit'
          | 'Trace' | 'FstPair' | 'SndPair' | 'ChooseList' | 'MkCons' | 'HeadList'
          | 'TailList' | 'NullList' | 'ChooseData' | 'ConstrData' | 'MapData' | 'ListData'
          | 'IData' | 'BData' | 'UnConstrData' | 'UnMapData' | 'UNBData' | 'EqualsData'
          | 'MkPairData' | 'MkNilData' | 'MkNilPairData'

Let ::= 'let' LetClause (';' LetClause)* 'in' Term

LetClause ::= Var '=' Term

IfThenElse ::= 'if' Term 'then' Term 'else' Term

InfixApply ::= Term InfixBuiltin Term
             | Term '`' (Builtin | Var) '`' Term

InfixBuiltin ::= '+i' | '-i' | '*i' | '/i' | '%i' | '==i' | '<i' | '<=i'
               | '+b' | ':b' | '!b' | '==b' | '<b' | '<=b'
               | '+s' | '==s'
               | '==d'
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
