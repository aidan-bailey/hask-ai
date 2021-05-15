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

- *PT* (PropositionalTypes) : Propositional types and datatypes.
- *PP* (PropositionalParser) : English to formula parser.
- *PP* (PropositionalHelpers) : Propositional helper functions.
- *PL* (PropositionalLogic) : Propositional logic functions.
