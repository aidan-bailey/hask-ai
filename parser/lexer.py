#!/usr/bin/env python3

import ply.lex as lex


class PropositionalLexer(object):

    # token list
    tokens = ("Atom", "UnaOp", "BinOp")

    literals = ["(", ")"]

    def t_UnaOp(self, t):
        r"Not"
        return t

    def t_BinOp(self, t):
        r"And|Or|Implies|Iff"
        return t

    def t_Atom(self, t):
        r"[A-Z]+"
        return t

    def t_ERR(self, t):
        r"[A-Z]+[a-z]*|0"
        return t

    # regex rule for lines
    def t_newline(self, t):
        r"\n+"
        t.lexer.lineno += len(t.value)

    # find horizontal position
    def find_column(self, input, token):
        line_start = input.rfind("\n", 0, token.lexpos) + 1
        return (token.lexpos - line_start) + 1

    # ignore string for spaces and tabs
    t_ignore = " "

    # error handling
    def t_error(self, t):
        # print("Illegal character '%s'" % t.value[0])
        t.lexer.skip(1)
        t.type = "ERR"
        t.value = "Illegal character '%s'" % t.value[0]
        return t

    # lexer builder
    def build(self, **kwargs):
        return lex.lex(module=self, debug=False, **kwargs)


if __name__ == "__main__":
    lexer = PropositionalLexer().build()
    input_buffer = []
    userin = input()
    while userin != "#":
        input_buffer.append(userin)
        userin = input()
        lexer.input("\n".join(input_buffer))

        while True:
            tok = lexer.token()
            if not tok:
                break
            print(tok)
