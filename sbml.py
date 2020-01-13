#William Chen
#ID: 111062517

import ply.lex as lex
import ply.yacc as yacc
import sys

symbolTable = {}

#A language between SML and Python...

#Lexing Rules
################################################################
################################################################

tokens = [
    #TYPES
    'INT',
    'REAL',
    'BOOL',
    'STRING',

    #ARITHMETIC OPERATORS
    'PLUS',
    'MIN',
    'DIV',
    'MULT',
    'EXPONEN',
    'INTDIV',
    'MOD',
    
    #GROUPING
    'LPAREN',
    'RPAREN',
    'LBRAC',
    'RBRAC',
    'LCURL',
    'RCURL',
    'COMMA',
    'SINGLEQUOTE',
    'DOUBLEQUOTE',
    
    #STRUCTURES
    'MEMB',
    'CONS',
    'GET',

    #LOGIC
    'NOT',
    'AND',
    'OR',

    #EQUALITY
    'LT',
    'LEQ',
    'GEQ',
    'GT',
    'EQ',
    'NEQ',

    #END
    'END',

    'PRINT',

    #VAR
    'VARNAME',
    'ASSIGN',
    'IF',
    'ELSE',
    'WHILE'

]

reserved = {
    'print' :'PRINT',
    'in' : 'MEMB',
    'div' : 'INTDIV',
    'mod' : 'MOD',
    'andalso' : 'AND',
    'orelse' : 'OR',
    'not' : 'NOT',
    'if' : 'IF',
    'else' : 'ELSE',
    'while' : 'WHILE'
}

t_SINGLEQUOTE = r'\''
t_DOUBLEQUOTE = r'\"'
t_CONS = r'::'
t_END = r';'
t_GET = r'\#'
t_COMMA = r','

t_EXPONEN = r'\*\*'
t_PLUS = r'\+'
t_MIN = r'\-'
t_MULT = r'\*'
t_DIV = r'\/'

t_EQ = r'=='
t_LEQ = r'<='
t_GEQ = r'>='
t_NEQ = r'<>'
t_GT = r'>'
t_LT = r'<'

t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRAC = r'\['
t_RBRAC = r'\]'
t_LCURL = r'\{'
t_RCURL = r'\}'

t_ASSIGN = r'='

def t_BOOL(t):
    r'True|False'
    if(t.value == 'True'):
        t.value = True
    else:
        t.value = False
    return t

def t_ID(t):
    r'[a-zA-Z][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value,'VARNAME')
    if(t.type == 'VARNAME'):
        t.value = str(t.value)
    return t

def t_REAL(t):
    r'((\d+\.\d*)|(\d*\.\d+))(e\-?\d+)?'
    t.value = float(t.value)
    return t

def t_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_STRING(t):
    r'(\"[^\"\']*\")|(\'[^\"\']*\')'
    t.value = str(t.value)[1:-1]
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

t_ignore = ' \t'

def t_error(t):
    t.lexer.skip(1)

lexer = lex.lex()

#AST
##############################################################
##############################################################

#Create  an assignment node

symbolTable = {}
semanticError = False
syntaxError = False

def raiseSemanticError():
    global semanticError
    if(not semanticError):
        semanticError = True
        print("SEMANTIC ERROR")
        sys.exit()

class Node:
    def __init__(self,type,children=None,leaf=None):
        self.type = type
        if children:
            self.children = children
        else:
            self.children = [ ]
        self.leaf = leaf
    def eval(self):
        childValues = []
        global symbolTable
        for child in self.children:
            if type(child) == Node:
                childValues.append(child.eval())
            else:
                childValues.append(child)
        if(self.type == "binop"):
            if(self.leaf == "+"):
                if(
                isinstance(childValues[0],(int,float)) and isinstance(childValues[1],(int,float))
                or isinstance(childValues[0],(str)) and isinstance(childValues[1],(str))
                or isinstance(childValues[0],(list)) and isinstance(childValues[1],(list))
                ):
                    return childValues[0] + childValues[1]
                else:
                    raiseSemanticError()
                    return None
            elif(self.leaf == "-"):
                if(isinstance(childValues[0],(int,float)) and isinstance(childValues[1],(int,float))):
                    return childValues[0]-childValues[1]
                else:
                    raiseSemanticError()
                    return None
            elif(self.leaf == "*"):
                if(isinstance(childValues[0],(int,float)) and isinstance(childValues[1],(int,float))):
                    return childValues[0]*childValues[1]
                else:
                    raiseSemanticError()
                    return None
            elif(self.leaf == "/"):
                if(isinstance(childValues[0],(int,float)) and isinstance(childValues[1],(int,float))):
                    if(childValues[1] == 0):
                        raiseSemanticError()
                        return None
                    return childValues[0]/childValues[1]
                else:
                    raiseSemanticError()
                    return None
            elif(self.leaf == "**"):
                return childValues[0]**childValues[1]
            elif(self.leaf == "mod"):
                return childValues[0]%childValues[1]
            elif(self.leaf == "div"):
                if(childValues[1] != 0 and isinstance(childValues[0],(int)) and isinstance(childValues[1],(int))):
                    return childValues[0]//childValues[1]
                else:
                    raiseSemanticError()
                    return None
            elif(self.leaf == "andalso"):
                if(isinstance(childValues[0],(bool)) and isinstance(childValues[1],(bool))):
                    return childValues[0] and childValues[1]
                else:
                    raiseSemanticError()
                    return None
            elif(self.leaf == "orelse"):
                if(isinstance(childValues[0],(bool)) and isinstance(childValues[1],(bool))):
                    return childValues[0] or childValues[1]
                else:
                    raiseSemanticError()
                    return None
            elif(self.leaf == "<"):
                if((isinstance(childValues[0],(int,float)) and isinstance(childValues[1],(int,float))) or (type(childValues[0]) is str and type(childValues[1]) is str)):
                    return childValues[0]<childValues[1]
                else:
                    raiseSemanticError()
                    return None
            elif(self.leaf == "<="):
                if((isinstance(childValues[0],(int,float)) and isinstance(childValues[1],(int,float))) or (type(childValues[0]) is str and type(childValues[1]) is str)):
                    return childValues[0]<=childValues[1]
                else:
                    raiseSemanticError()
                    return None
            elif(self.leaf == ">"):
                if((isinstance(childValues[0],(int,float)) and isinstance(childValues[1],(int,float))) or (type(childValues[0]) is str and type(childValues[1]) is str)):    
                    return childValues[0]>childValues[1]
                else:
                    raiseSemanticError()
                    return None
            elif(self.leaf == ">="):
                if((isinstance(childValues[0],(int,float)) and isinstance(childValues[1],(int,float))) or (type(childValues[0]) is str and type(childValues[1]) is str)):
                    return childValues[0]>=childValues[1]
                else:
                    raiseSemanticError()
                    return None
            elif(self.leaf == "=="):
                if((isinstance(childValues[0],(int,float)) and isinstance(childValues[1],(int,float))) or (type(childValues[0]) is str and type(childValues[1]) is str)):
                    return childValues[0]==childValues[1]
                else:
                    raiseSemanticError()
                    return None
            elif(self.leaf == "<>"):
                if((isinstance(childValues[0],(int,float)) and isinstance(childValues[1],(int,float))) or (type(childValues[0]) is str and type(childValues[1]) is str)):
                    return childValues[0]!=childValues[1]
                else:
                    raiseSemanticError()
                    return None
            elif(self.leaf == "in"):
                try:
                    return childValues[0] in childValues[1]
                except:
                    raiseSemanticError()
                    return None
            elif(self.leaf == "="):
                try:
                    global symbolTable
                    symbolTable[childValues[0]] = childValues[1]
                except:
                    raiseSemanticError()
                    return None
        elif(self.type == "unop"):
            if(self.leaf == "-"):
                if(isinstance(childValues[0],(int,float))):
                    return -childValues[0]
                else:
                    raiseSemanticError()
                    return None
            if(self.leaf == "not"):
                if(type(childValues[0]) is bool):
                    return not childValues[0]
                else:
                    raiseSemanticError()
                    return None
        elif(self.type == "index"):
            if(type(childValues[1]) is int and isinstance(childValues[0],(str,list))):
                if(childValues[1] >= len(childValues[0]) or childValues[1] < 0):
                    raiseSemanticError()
                    return None
                return childValues[0][childValues[1]]
            else:
                raiseSemanticError()
                return None
        elif(self.type == "tindex"):
            if(type(childValues[0] is int and isinstance(childValues[1],(tuple)))):
                ind = childValues[0]-1
                if(ind >= len(childValues[1]) or ind < 0):
                    raiseSemanticError()
                    return None
                return childValues[1][ind]
            else:
                raiseSemanticError()
                return None
        elif(self.type == "varindex"):
            return "symbolTable" + "[\'" + childValues[0] + "\']" + childValues[1]
        elif(self.type == "varindextail"):
            if(type(childValues[0]) == int):
                return "[" + str(childValues[0]) + "]" + childValues[1]
            else:
                raiseSemanticError()
                return None
        elif(self.type == "assign"):
            if(not childValues[0] == None and not childValues[1] == None):
                if(type(childValues[1]) == str):
                    command = childValues[0] + "=" + "\'" +childValues[1]+"\'"
                else:
                    command = childValues[0] + "=" + str(childValues[1])
                exec(command,globals())
            else:
                raiseSemanticError()
            return None
        elif(self.type == "cons"):
            try:
                childValues[1].insert(0,childValues[0])
                return childValues[1]
            except:
                raiseSemanticError()
                return None
        elif(self.type == "list"):
            return [childValues[0]] + childValues[1]
        elif(self.type == "tuple"):
            return tuple(childValues[0])
        elif(self.type == "print"):
            if(not childValues[0] is None):
                global semanticError
                if(not semanticError):
                    print(childValues[0])
            return None
        elif(self.type == "var"):
            if childValues[0] in symbolTable:
                return symbolTable[childValues[0]]
            else:
                raiseSemanticError()
        elif(self.type == "if"):
            if(type(childValues[0]) == bool):
                if(childValues[0]):
                    for n in childValues[1]:
                        n.eval()
            else:
                raiseSemanticError()
        elif(self.type == "elif"):
            if(type(childValues[0]) == bool):
                if(childValues[0]):
                    for n in childValues[1]:
                        n.eval()
                else:
                    for n in childValues[2]:
                        n.eval()
            else:
                raiseSemanticError()
        elif(self.type == "while"):
            if(type(childValues[0]) == bool):
                while(self.children[0].eval()):
                    for n in childValues[1]:
                        n.eval()
            else:
                raiseSemanticError()

def p_block(p):
    'block : LCURL stmtlist RCURL'
    p[0] = p[2]

def p_stmtlist(p):
    'stmtlist : stmt stmtlist'
    if(type(p[1])==list):
        p[0] = p[1]+p[2]
    else:
        p[0] = [p[1]] + p[2]

def p_if(p):
    'if : IF LPAREN expression RPAREN block'
    p[0] = Node('if',[p[3],p[5]],)

def p_if_else(p):
    'if : IF LPAREN expression RPAREN block ELSE block'
    p[0] = Node('elif',[p[3],p[5],p[7]],)

def p_while(p):
    'while : WHILE LPAREN expression RPAREN block'
    p[0] = Node('while',[p[3],p[5]],)

def p_stmtlist_empty(p):
    'stmtlist : empty'
    p[0] = []
    
def p_statement(p):
    '''
    stmt : expression END
          | print END
          | assignment END
          | block
          | if
          | while
    '''
    p[0] = p[1]

def p_empty(p):
    'empty :'
    pass

def p_expr(p):
    '''
    expression : STRING
                | INT
                | REAL
                | tuple
                | BOOL
                | var
    '''
    p[0] = p[1]

def p_expr_list(p):
    'expression : list'
    p[0] = p[1]

def p_expression_binop(p):
    '''expression : expression PLUS expression
                | expression MIN expression
                | expression MULT expression
                | expression DIV expression
                | expression EXPONEN expression
                | expression INTDIV expression
                | expression MOD expression
                | expression AND expression
                | expression OR expression
                | expression LT expression
                | expression GT expression
                | expression LEQ expression
                | expression GEQ expression
                | expression EQ expression
                | expression NEQ expression
                | expression MEMB expression'''
    p[0] = Node("binop", [p[1],p[3]], p[2])

def p_assignment(p):
    'assignment : VARNAME ASSIGN expression'
    p[0] = Node("binop", [p[1],p[3]], p[2])

def p_assignment_varexpr(p):
    'assignment : varexpr ASSIGN expression'
    p[0] = Node("assign",[p[1],p[3]],None)

def p_varexpr_index(p):
    'varexpr : VARNAME varexprtail'
    p[0] = Node("varindex",[p[1],p[2 ]],None)

def p_varexprtail(p):
    'varexprtail : LBRAC expression RBRAC varexprtail'
    p[0] = Node("varindextail",[p[2],p[4]],None)

def p_varexprtail_empty(p):
    'varexprtail : empty'
    p[0] = ""

def p_print(p):
    'print : PRINT LPAREN expression RPAREN'
    p[0] = Node("print", [p[3]],)

def p_var(p):
    'var : VARNAME'
    p[0] = Node("var", [p[1]],)

def p_expression_paren(p):
    'expression : LPAREN expression RPAREN %prec PAREN'
    p[0] = p[2]

def p_expression_unary_minus(p):
    '''expression : MIN expression %prec UMINUS'''
    p[0] = Node("unop",[p[2]],p[1])

def p_expression_unary_not(p):
    '''expression : NOT expression'''
    p[0] = Node("unop",[p[2]],p[1])

def p_expression_index(p):
    'expression : expression LBRAC expression RBRAC'
    p[0] = Node("index",[p[1],p[3]],None)

def p_expression_tupleget(p):
    'expression : GET INT tuple'
    p[0] = Node("tindex",[p[2],p[3]],None)

def p_expression_cons(p):
    'expression : expression CONS expression'
    p[0] = Node("cons",[p[1],p[3]],None)

def p_list(p):
    'list : LBRAC list_element RBRAC'
    p[0] = p[2]

def p_list_empty(p):
    'list : LBRAC RBRAC'
    p[0] = []

def p_list_element(p):
    'list_element : expression list_tail'
    p[0] = Node("list",[p[1],p[2]], None)

def p_list_tail(p):
    'list_tail : COMMA expression list_tail'
    p[0] = Node("list",[p[2],p[3]], None)

def p_list_tail_null(p):
    'list_tail : empty'
    p[0] = []

def p_list_tuple(p):
    'tuple : LPAREN list_element RPAREN %prec TUPLE_CREATION'
    p[0] = Node("tuple",[p[2]],)

def p_error(p):
    global syntaxError
    syntaxError = True

precedence = (
    ('left','END'),
    ('left','OR'),
    ('left','AND'),
    ('left','NOT'),
    ('left','LT','GT','LEQ','GEQ','EQ','NEQ'),
    ('right','CONS'),
    ('left','MEMB'),
    ('left','PLUS','MIN'),
    ('left','MULT','DIV','MOD','INTDIV'),
    ('right','UMINUS'),
    ('right','EXPONEN'),
    ('left','GET'),
    ('left','TUPLE_CREATION'),
    ('left','LBRAC'),
    ('left','PAREN')
)

parser = yacc.yacc(debug=False,errorlog=yacc.NullLogger)

'''
# Set up a logging object
import logging
logging.basicConfig(
    level = logging.DEBUG,
    filename = "parselog.txt",
    filemode = "w",
    format = "%(filename)10s:%(lineno)4d:%(message)s"
)
log = logging.getLogger()
'''

fp = sys.argv[1]
with open(fp) as f:
    data = f.read()
    result = parser.parse(data)
    if(syntaxError):
        print("SYNTAX ERROR")
    else:
        for line in result:
            if (type(line) == Node):
                line.eval()