This repo will contain a Plutus Core assembler and associated tests and tooling. The purpose of this is to write Untyped Plutus Core in a human-readable, Haskell-like syntax which translates 1-to-1 into Plutus Core.

Syntax (rough and ambiguous)
============================

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
