# Propositional Module

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

- *PT* (PropositionalTypes) : Stored types used for formalized logic.
- *PP* (PropositionalParser) : Parser implementation to parse English formulas into Haskell's types.
- *PP* (PropositionalHelpers) : Helper functions.
- *PL* (PropositionalLogic) : Functions related to propositional logic.
