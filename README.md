# Hask AI
First foray into functional programming & propositional logic.

**Dependencies**

- [GHCi](https://wiki.haskell.org/GHC/GHCi) 

**Usage**

All the code is present within the ```propositional.hs``` Haskell file.
Just load it up in the GHCi to get started.

**Parser**

There is a parser implemented which enables the parsing of propositional formulas e.g. 'A And B Or C'. 

To use this, see method ```parseString```

example usage: ```let formula = parseString "A And B Or C Not D"```
