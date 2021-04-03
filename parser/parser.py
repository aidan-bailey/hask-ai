import ply.yacc as yacc

from lexer import PropositionalLexer


class PropositionalParser(object):
    tokens = PropositionalLexer.tokens

    def p_form_atom(self, p):
        "Form : Atom"
        p[0] = "Atom '%s'" % p[1]

    def p_form_paren(self, p):
        "Form : '(' Form ')'"
        p[0] = p[2]

    def p_form_una(self, p):
        "Form : UnaOp Form"
        p[0] = "%s (%s)" % (p[1], p[2])

    def p_form_bin(self, p):
        "Form : Form BinOp Form"
        p[0] = "%s (%s) (%s)" % (p[2], p[1], p[3])

    def p_error(self, p):
        raise Exception

    def build(self, **kwargs):
        return yacc.yacc(module=self, debug=False, **kwargs)


if __name__ == "__main__":
    lexer = PropositionalLexer().build()
    parser = PropositionalParser().build()
    userin = input()
    while userin != "#":
        try:
            result = parser.parse(input=userin, lexer=lexer)
            if result != None:
                print(result)
        except Exception:
            print("Error in formula")
        userin = input()
