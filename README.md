# Hask AI
First foray into functional programming & propositional logic.

**Dependencies**

- [GHCi](https://wiki.haskell.org/GHC/GHCi) 
- [Cabal](https://www.haskell.org/cabal/)

**Usage**

```cabal build``` to build the package.

```cabal repl``` to load the package into the GHCi.

**Parser**

There is a parser implemented enabling users to parse in formulas using a relatable, English form e.g. "A And B"

Example usage: ```let formula = parseString "A And B Not C Imply D"```

**Operations Accepted**

- Not 
- And
- Or
- Imply
- Iff

**Modules**

- *LogicTypes* : Stored types used for formalized logic.
- *Parser* : Parser implementation to parse English formulas into Haskell's types.
- *PropositionalLogic* : Functions related to propositional logic.
- *ModelTheory* : Functions related to model theory.

**Notes**

This repo is still very young in its development cycle. There is still much to be added.
