#GEROGIANNIS KONSTANTINOS 3196  cse63196
#PAPATHANASIOU ALEXANDROS 3308  cse63308

import sys


#GLOBALS

global comp_oper,symbols,bound_words,operators,ID,Variables,line_counter,subprograms_flag, filename,f,ret_flag,index
ret_flag = 0
subprograms_flag = 0
line_counter = 1
index = 0
nesting_level = 0
starting_offset = 12
comp_oper = ["<",">","=","<=",">=","<>"]
symbols = [",",";",";","+",":","-","*","/","=","[","]","(",")","{","}","."]
bound_words = ["declare","program",

"if", "else", "while",

"switchcase","case","forcase","incase", "default",

"not","and","or",

"function","procedure","call","return","in","inout",

"input","print"]

operators = ["+","-","*","/"]
ID = []
Variables = []
CString = []
ST = []
finalStr = []
def open_file(name):

    global prog
    prog = open(name, 'r')          

    prog.seek(0)

##################Lexycal analyzer#########################
def lex():
    global prog,line_counter,state,given_int,char,state
    state = 0
    word = ""
    while(state != 7):
        char = prog.read(1)
        if (state == 0):
            if (char == ' ' or char == '\t'):
                state = 0
            elif(char == '\n' or char == '\r'):
                state = 0
                line_counter += 1
            elif(char.isalpha()):
                word += char
                state = 1
            elif(char.isdigit()):
                word += char
                state = 2
            elif(char == '<'):
                word += char
                state = 3
            elif(char == '>'):
                word += char
                state = 4
            elif(char == ':'):
                word+= char
                state = 5
            elif(char == '#'):
                state = 6
            elif(char in symbols):
                word += char
                state = 7
            else:
                print("ERROR: Char not in syntax! Line:",char)
                sys.exit()
        elif(state == 1):
            if(char.isalpha() or char.isdigit()):
                if(len(word) <= 30):
                    word += char
                else:
                    print("ERROR:More characters than expected")
                    sys.exit()
            elif(char == ' ' or char == '\t'):
                state = 7
            elif(char == '\n'):
                state = 7
                line_counter += 1
            elif(char in symbols or char in comp_oper):
                state = 7
                prog.seek(prog.tell() - 1)
                
        elif(state ==2):
            if(char.isdigit()):
                word += char
            elif(char.isalpha()):
                print("ERROR: Character instead of expected integer, line: ",line_counter)
                sys.exit()
            elif(char == ' ' or char == '\t'):
                given_int = int(word)
                if(-((2^32)-1) <= given_int and given_int <= ((2^32)-1)):
                    state = 7
                else:
                    print("ERROR: Integer out of bounds!, line",line_counter)
                    sys.exit()
            elif(char == '\n'):
                line_counter += 1
                given_int = int(word)
                if(-((2^32)-1) <= given_int and given_int <= ((2^32)-1)):
                    state = 7
                else:
                    print("ERROR: Integer out of bounds!, line:",line_counter)
                    sys.exit()
            elif(char in symbols or char in comp_oper):
                state = 7
                prog.seek(prog.tell() - 1)
                
        elif(state == 3):
            if(char == '='):
                word += char
                state = 7
            elif(char == '>'):
                word += char
                state = 7
            elif(char ==' ' or char == '\t'):
                state = 7
            elif(char == '\n'):
                line_counter += 1
                state = 7
            elif(char.isalpha() or char.isdigit()):
                prog.seek(prog.tell() - 1)
                state = 7
            else:
                print("ERROR: Unexpected syntax in line:",line_counter)
                sys.exit()
        elif(state == 4):
            if(char == '='):
                word += char
                state = 7
            elif(char == ' ' or char == '\t'):
                state = 7
            elif(char == '\n'):
                line_counter += 1
                state = 7
            elif(char.isalpha() or char.isdigit()):
                prog.seek(prog.tell() - 1)
                state = 7
            else:
                print("ERROR: Unexpected syntax in line:",line_counter)
                sys.exit()
        elif(state == 5):
            if(char == '='):
                word += char
                state = 7
            elif(char == ' ' or char == '\t'):
                print("ERROR: Expected '=' in line:",line_counter)
                sys.exit()
            elif(char == '\n'):
                line_counter += 1
                print("ERROR: Expected '=' in line:",line_counter)
                sys.exit()
            else:
                print("ERROR: Unexpected syntax in line:",line_counter)
                sys.exit()
        elif(state == 6):
            if(char == '#'):
                state = 0
            else:
                pass
    return word



###################### START OF SYNTAX ALANYZER##############################
def program():
    global token,prg_name
    token = lex()
    if(token == "program"):
        token = lex()
        prg_name = token
        addScope()
        block(prg_name)
        #deleteScope()
    else:
        print("ERROR: Keyword Program was expected in line:",line_counter)
        sys.exit()

def block(block_name):
    global token,name
    declarations()
    subprograms()
    genquad("begin_block",block_name,"_","_")
    statements()
    if(block_name == prg_name):
        genquad("halt","_","_","_")
    genquad("end_block",block_name,"_","_")
    
            

def declarations():
    global token,counters
    counters = 0
    token = lex()
    while(token == 'declare'):
        counters = counters + 1
        varlist()
        
def varlist():
    global token
    token = lex()
    while(token != ';'):
        if(token == ','):
            token = lex()
        elif(token not in Variables):
            Variables.append(token)
            
            addVarEntity(token,ST[-1][0][1])
            token = lex()
        else:
            print("ERROR:Variable already defined!")
            sys.exit()
    token = lex()
            
def subprograms():
    global token,subprograms_flag
    while(token == "function" or token == "procedure"):
        subprograms_flag = 1
        subprogram()
        
    
def subprogram():
    global token,idtok,proc_flag,func_flag
    func_flag = 0
    proc_flag = 0
    if(token == 'function'):
        func_flag = 1
        token = lex()
        if(token not in ID):
            ID.append(token)
            idtok = token
            token = lex()
            addScope()
            addBlockEntity(idtok,'func')
            if(token == '('):
                formalparlist()
                token = lex()
                block(idtok)
                #deleteScope()
                if(ret_flag == 0):
                    print("ERROR: Expected atleast one return statement inside the function line:",line_counter)
                    sys.exit()
                func_flag = 0
            else:
                print("ERROR: Expected ( in line: ",line_counter)
                sys.exit()
        else:
            print("ERROR:Function already defined in line:",line_counter)
            sys.exit()
    elif(token == 'procedure'):
        proc_flag = 1
        token = lex()
        if(token not in ID):
            ID.append(token)
            idtok = token
            token = lex()
            addScope()
            addBlockEntity(idtok,'proc')
            if(token == '('):
                formalparlist()
                token = lex()
                block(idtok)
                proc_flag = 0
                #deleteScope()
            else:
                print("ERROR: Expected ( in line: ",line_counter)
                sys.exit()
        else:
            print("ERROR:Procedure already defined in line:",line_counter)
            sys.exit()
    else:
        print("ERROR: Expected keyword 'procedure' or 'function' in line: ",line_counter)
        sys.exit()

def formalparlist():
    global token
    token = lex()
    while(1):
        if(token == ','):
            token = lex()
            continue
        elif(token == 'in' or token == 'inout'):
            mode = token
            formalparitem(mode)
            token = lex()
        elif(token == ')'):
            break
        else:
            print("ERROR: Unexpected syntax in line: ",line_counter)
            sys.exit()
    
def formalparitem(mode):
    global token
    token = lex()
    addParEntity(mode,token,ST[-1][0][1])
    if(token not in Variables):
        Variables.append(token)
        
    else:
        pass

def statements():
    global token,prevToken,y
    while(1):
        #print("WELCOME TOKEN NAMED:",token)
        if(token in bound_words):
            #print("I found a statement!:",token)
            statement()
        elif(token == "{"):
            token = lex()
        elif(token in Variables):
            #print("I found an ID!:",token)
            prevToken = token
            token = lex()
            if(token == ':='):
                y = assingStat()
                genquad(":=",y,"_",prevToken)
                #print("Finished assignStat with:",token)
            else:
                print("ERROR:Expected assignment symbol (:=) in line: ",line_counter)
                sys.exit()
        elif(token == ';'):
            #print("I found a semicolon",token)
            token = lex()
        elif(token == '.'):
            print("ERROR:EOF,comment brackets did not close")
            sys.exit()
        elif(token == '}'):
            token = lex()
            if(token == ";"):
                break
            elif(token == "."):
                #print("Success!!!Program ended! EOF!")
                break
            else:
                break
        else:
            print("ERROR:Unexpected syntax in line: ",line_counter)
            sys.exit()
def statement():
    global token
    if(token == 'if'):
        ifStat()
        #print("Finished ifStat with:",token)
    elif(token == 'while'):
        whileStat()
        #print("Finished whileStat with:",token)
    elif(token == 'switchcase'):
        switchcaseStat()
        #print("Finished switchcaseStat with:",token)
    elif(token == 'forcase'):
        forcaseStat()
       # print("Finished forcaseStat with:",token)
    elif(token == 'incase'):
        incaseStat()
        #print("Finished incaseStat with:",token)
    elif(token == 'call'):
        callStat()
        #print("Finished callStat with:",token)
    elif(token == 'return'):
        returnStat()
        #print("Finished returnStat with:",token)
    elif(token == 'input'):
        inputStat()
        #print("Finished inputStat with:",token)
    elif(token == 'print'):
        printStat()
        #print("Finished printStat with:",token)
    else:
        pass

def assingStat():
    global token,prevToken,res
    token = lex()
    res = expression()
    return res

def ifStat():
    global token
    token = lex()
    if(token == '('):
        B = condition()
        B_true = B[0]
        B_false = B[1]
        if(token == ')'):
            token = lex()

            
            backpatch(B_true,nextquad())

            
            statements()


            
            ifList = makelist(nextquad())
            genquad("jump","_","_","_")
            backpatch(B_false,nextquad())
            
            
            elsepart()
            backpatch(ifList,nextquad())
            
        else:
            print("ERROR:Expected ) in line: ",line_counter)
            sys.exit()
    else:
        print("ERROR:Expected ( in line: ",line_counter)
        sys.exit()
        

def elsepart():
    global token
    if (token == "else"):
        token = lex()
        statements()
    

def whileStat():
    global token
    token = lex()
    Bquad = nextquad()
    if(token == '('):
        B = condition()
        B_true = B[0]
        B_false = B[1]
        
        if(token == ')'):
            backpatch(B_true,nextquad())
            
            token = lex()
            statements()
            
            genquad("jump","_","_",Bquad)
            backpatch(B_false,nextquad())
        else:
            print("ERROR:Expected ) in line: ",line_counter)
            sys.exit()
    else:
        print("ERROR:Expected ( in line: ",line_counter)
        sys.exit()


def switchcaseStat():
    global token
    token = lex()
    exitList = emptylist()
    while(1):
        if(token == "case"):
            token = lex()
            if(token == "("):

                
                Cond = condition()
                Cond_true = Cond[0]
                Cond_false = Cond[1]
                
                if(token == ")"):
                    token = lex()
                    backpatch(Cond_true,nextquad())

                    
                    statements()

                    
                    e = makelist(nextquad())
                    genquad("jump","_","_","_")
                    mergelist(exitList,e)
                    backpatch(Cond_false,nextquad())

                    
                else:
                    print("ERROR: Expected ) in line:",line_counter)
                    sys.exit()
            else:
                print("ERROR: Expected ( in line:",line_counter)
                sys.exit()
        elif(token == "default"):
            token = lex()
            statements()
            backpatch(exitList,nextquad())
            break
        else:
            print("ERROR: Unexpected syntax in line:",line_counter)
            sys.exit()
            break
    if(token == ";"):
        pass
    else:
        print("ERROR: Expected } in line :", line_counter)
        sys.exit()


def forcaseStat():
    global token
    token = lex()
    p1Quad = nextquad()
    while(1):
        if(token == "case"):
            token = lex()
            if(token == "("):
                Cond = condition()
                Cond_true = Cond[0]
                Cond_false = Cond[1]
                if(token == ")"):
                    token = lex()
                    backpatch(Cond_true,nextquad())
                    
                    statements()

                    genquad("jump","_","_",p1Quad)
                    backpatch(Cond_false,nextquad())
                else:
                    print("ERROR: Expected ) in line:",line_counter)
                    sys.exit()
            else:
                print("ERROR: Expected ( in line:",line_counter)
                sys.exit()
        elif(token == "default"):
            token = lex()
            statements()
            break
        else:
            print("ERROR: Unexpected syntax in line:",line_counter)
            sys.exit()
            break
    if(token == ";"):
        pass
    else:
        print("ERROR: Expected } in line :", line_counter)
        sys.exit()
            
def incaseStat():
    global token
    token = lex()
    w = newtemp()
    p1Quad = nextquad()
    genquad(":=",1,"_",w)
    while(1):
        if(token == "case"):
            token = lex()
            if(token == "("):
                Cond = condition()
                Cond_true = Cond[0]
                Cond_false = Cond[1]
                
                if(token == ")"):
                    token = lex()

                    backpatch(Cond_true,nextquad())
                    genquad(":=",0,"_",w)
                    
                    statements()

                    backpatch(Cond_false,nextquad())
                    
                else:
                    print("ERROR: Expected ) in line:",line_counter)
                    sys.exit()
            else:
                print("ERROR: Expected ( in line:",line_counter)
                sys.exit()
        elif(token == ";"):
            pass
        else:
            print("ERROR: Unexpected syntax in line:",line_counter)
            sys.exit()
            break
            
    genquad("=",w,0,p1Quad)

    
def returnStat():
    global token,ret_flag
    if(proc_flag == 1):
        print("ERROR:Illegal return inside a procedure!")
        sys.exit()
    elif(func_flag == 0):
        print("ERROR:Illegal return outside of a function in line:",line_counter)
        sys.exit()
    token = lex()
    if(token == '('):
        token = lex()
        ex = expression()
        
        if (token == ')'):
            token = lex()
            genquad("ret",ex,"_","_")
            
            ret_flag = 1
            pass
        else:
            print("ERROR:Expected ) in line: ",line_counter)
            sys.exit()
    else:
        print("ERROR:Expected ( in line: ",line_counter)
        sys.exit()

def callStat():
    global token,idtok1
    token = lex()
    idtok1 = token
    if (token not in ID):
        print("ERROR: Procedure not defined!")
        sys.exit()
    else:
        token = lex()
        if( token == '('):
            actualparlist()
            if(token == ')'):
                genquad("call",idtok1,"_","_")
                token = lex()
                pass
            else:
                print("ERROR:Expected ) in line: ",line_counter)
                sys.exit()
        else:
            print("ERROR:Expected ( in line: ",line_counter)
            sys.exit()
def printStat():
    global token
    token = lex()
    if(token == '('):
        token = lex()
        ex = expression()
       
        if(token == ')'):
            token = lex()
            genquad("out",ex,"_","_")
            pass
        else:
            print("ERROR:Expected ) in line: ",line_counter)
            sys.exit()
    else:
        print("ERROR:Expected ( in line: ",line_counter)
        sys.exit()

def inputStat():
    global token,idtok2
    token = lex()
    if(token == '('):
        token = lex()
        idtok2 = token
        if(token not in Variables):
            print("ERROR: Variable not defined!")
            sys.exit()
        else:
            token = lex()
            if(token == ')'):
                token = lex()
                genquad("inp",idtok2,"_","_")
                pass
            else:
                print("ERROR:Expected ) in line: ",line_counter)
                sys.exit()
    else:
        print("ERROR:Expected ( in line: ",line_counter)
        sys.exit()
        
def actualparlist():
    global token,actualitem
    token = lex()
    while(1):
        if(token == ','):
            token = lex()
            continue
        elif(token == 'in' or token == 'inout'):
            actualitem = actualparitem()
        elif(token == ')'):
            break
        else:
            print("ERROR: Unexpected syntax in line: ",line_counter)
            sys.exit()
    return actualitem
    
            
def actualparitem():
    global token,actualres,idtok3
    if(token == 'in'):
        actualres = token
        token = lex()
        x = expression()
        genquad("par",x,"CV","_")
        return actualres
    elif(token == 'inout'):
        actualres = token
        token = lex()
        if(token in Variables):
            idtok3 = token
            genquad("par",idtok3,"REF","_")
            token = lex()
            return actualres
        else:
            print("ERROR: Variable not defined!")
            sys.exit()
    else:
        print("ERROR:Keyword in or inout expected in line: ",line_counter)
        sys.exit()
    

            
def condition():
    global token
    
    Q1 = boolterm()
    B_true = Q1[0]
    B_false = Q1[1]
    
    while(token == 'or'):
        token = lex()
        
        bakcpatch(B_false,nextquad())
        
        Q2 = boolterm()

        Q2_true = Q2[0]
        Q2_false = Q2[1]
        B_true = mergelist(B_true,Q2_true)
        B_false = Q2_false
        
    if(token == ')' or token == ']'):
        pass
    else:   
        print("ERROR:Unexpected syntax!")
        sys.exit()
    B = [B_true,B_false]
    return B
                
def boolterm():
    global token
    R = boolfactor()
    Q_true = R[0]
    Q_false = R[1]
    
    while(token == 'and'):
        token = lex()

        backpatch(Q_true,nextQuad())

        R2 = boolfactor()
        R2_true = R2[0]
        R2_false = R2[1]

        Q_false = mergelist(Q_false,R2_false)
        Q_true = R2_true
        
        
    if(token == ')' or token == ']'):
        pass
    else:   
        print("ERROR:Unexpected syntax!")
        sys.exit()
    Q = [Q_true,Q_false]
    return Q
        
def boolfactor():
    global token
    token = lex()
    if(token == 'not'):
        token = lex()
        if(token == '['):

            
            B = condition()
            
            token = lex()
            if(token == ']'):
                R_true = B[1]
                R_false = B[0]              ##REVERSE RTRUE RFALSE BECAUSE OF NOT##
                R = [R_true,R_false]
                return R
                
            else:
                print("ERROR:Expected ] in line: ",line_counter)
                sys.exit()
        else:
            print("ERROR:Expected [ in line: ",line_counter)
            sys.exit()
    elif(token == '['):
        R = condition()
        token = lex()
        if(token == ']'):
            return R
        else:
            print("ERROR:Expected ] in line: ",line_counter)
            sys.exit()
    elif(token in ID or token in Variables):
        ex1 = expression()
        if(token in comp_oper):
            relop = token
            
            token = lex()
            ex2 = expression()

            R_true = makelist(nextquad())
            genquad(relop,ex1,ex2,"_")
            R_false = makelist(nextquad())
            genquad("jump","_","_","_")
            R = [R_true,R_false]
            return R

            
        else:
            print("ERROR:Unexpected syntax in line: ", line_counter)
            sys.exit()
    else:
        print("ERROR: Unexpected syntax!")
        sys.exit()

        
def expression():
    global token,op_res,term_res,op,interm
    op_res = optionalSign()
    term_res = term()
    if(op_res != None):
        q1 = newtemp()
        genquad(op_res,0,term_res,q1)
        term_res = q1
    while(token == '+' or token == '-'):
        op = token
        token = lex()
        interm = term()
        q2 = newtemp()
        genquad(op,term_res,interm,q2)
        term_res = q2
    return term_res


def term():
    global token,factor_res,op,infactor
    factor_res = factor()
    while(token == '*' or token == '/'):
        op = token
        token = lex()
        infactor = factor()
        q3 = newtemp()
        genquad(op,factor_res,infactor,q3)
        factor_res = q3
    return factor_res
      
        
def factor():
    global token,num,id_res,inexpression,id1
    if(token.isdigit()):
        num = token
        token = lex()
        return num
    elif(token == '('):
        token = lex()
        inexpression = expression()
        if(token == ')'):
            token = lex()
            return inexpression
        else:
            print("ERROR:Expected ) in line: ",line_counter)
            sys.exit()
    elif(token in ID):
        id1 = token
        id_res = idtail()
        if(id_res == 0):
            return id1
        else:
            q4 = newtemp()
            genquad("par",q4,"RET","_")
            genquad("call",id1,"_","_")
            return q4
    elif(token in Variables):
        tok = token
        token = lex()
        return tok
    else:
        print("ERROR:Unexptected syntax in line: ",line_counter)
        sys.exit()
        
def idtail():
    global token,actualpar
    token = lex()
    if(token == '('):
        actualpar = actualparlist()
        if(token == ')'):
            token = lex()
            return actualpar
        else:
            print("ERROR:Expected ) in line: ",line_counter)
            sys.exit()
    else:
        actualpar = ""
        return actualpar


def optionalSign():
    global token,oper
    if(token == '+' or token == '-'):
        oper = token
        token = lex()
        return oper
    else:
        pass
################################### END OF SYNTAX ALANYZER###############################################


    

################################## START OF INTERMEDIATE CODE ###########################################
global quad_num,temp_num,quad_list,gen_quad
temp_num = 0
quad_num = 0
quad_list = []
def emptylist():
    empty = []
    return empty

def nextquad():
    global quad_num
    return quad_num

def genquad(op,x,y,z):
    global quad_num
    gen_quad = [nextquad(),op,x,y,z]
    quad_list.append(gen_quad)
    quad_num = quad_num + 1

def newtemp():
    global quad_num,temp_num
    temp = "T_"+ str(temp_num)
    addTempEntity(temp,ST[-1][0][1])
    temp_num = temp_num + 1
    return temp

def makelist(quad_num):
    new_list = []
    new_list.append(quad_num)
    return new_list

def mergelist(list_1,list_2):
    new_merged_list = list_1 + list_2
    return new_merged_list

def backpatch(list_1,z):
    for i in list_1:
        quad_list[i][4] = z

def create_int_file():
    intFile = open( sp[0]+".int","w")
    #for i in quad_list:
    #    print(str(i))
    for i in quad_list:
        intFile.write(str(i[0])+" : "+ str(i[1])+" | "+ str(i[2])+" | "+ str(i[3])+" | "+str(i[4])+" | "+"\n")
    
  
    intFile.close


################################# END OF INTERMEDIATE CODE ##############################################


    
################################# START OF C FILE ####################################################

def createCFile():
    global CString,T0
    cFile = open(sp[0]+'.c' , 'w')
    T0 = "#include <stdio.h>\n"
    CString.append(T0)
    T0 = "int main() \n{\n"
    CString.append(T0)
    id_num = len(Variables)
    counter = 0
    if( id_num > 0):
        T0 = "\tint "
        CString.append(T0)
        T0 = ""
        while(id_num != 1):
            T0 = T0 + Variables[counter] + ", "
            id_num = id_num - 1
            counter = counter + 1
        T0 = T0 + Variables[counter] + ";\n"
        CString.append(T0)
   
    for quad in quad_list:
        #T0 = "\tL_"+str(quad[0]+1)+":"
        #CString.append(T0)
        op = quad[1]
        x = quad[2]
        y = quad[3]
        z = quad[4]
        if( op == "begin_block"):
             T0 ="\tL_0: \n"
             CString.append(T0)
             
        if( op == ":="):
            T0 = "\tL_"+str(quad[0])+": "
            T0 = T0 + str(z) + " = " + str(x)+ ";\n"
            CString.append(T0)
            
        if( op in operators):
            T0 = "\tL_"+str(quad[0])+": "
            T0 = T0 + str(z) + " = " + str(x) + str(op) + str(y) + ";\n"
            CString.append(T0)
            
        if(op in comp_oper):
            T0 = "\tL_"+str(quad[0])+": "  
            T0 = T0 + "if (" + str(x) + str(op) + str(y)+ ")" + " goto L_"+ str(z)+";\n"
            CString.append(T0)
           
        if(op == "jump"):
           T0 = "\tL_"+str(quad[0])+": "
           T0 = T0 + "goto L_" + str(z) + ";\n"
           CString.append(T0)

        if(op == "inp"):
            T0 = "\tL_"+str(quad[0])+": "
            T0 = T0 + "scanf(\"%d\"," + str(x) +");\n"
            CString.append(T0)

        if(op == "out"):
           T0 = "\tL_"+str(quad[0])+": "
           T0 = T0 + "printf(\"%d\"," + str(x) +");\n"
           CString.append(T0)

        if(op == "halt"):
            T0 = "\tL_"+str(quad[0])+": \n"
            CString.append(T0)
    T0 = "}"
    CString.append(T0)
    for i in CString:
        cFile.write(i)
    cFile.close


################################# END OF C FILE ##################################################

    
#################################START OF SYMBOL TABLE FUNCTIONS##################################
def addScope():
    global nesting_level,starting_offset
    ST.append([[nesting_level,starting_offset]])
    nesting_level = nesting_level + 1
    printScopes("new scope")
    

def addVarEntity(var,offset):
    ST[-1].append([var,'var',offset])   #add the new variable to the symbol table array
    ST[-1][0][1] += 4                    #previous offset + 4 for the next entity
    printScopes("new var")
    
def addParEntity(parMode,par_name,offset):
    call_type = ''
    if(parMode == 'in'):
        call_type = 'CV'
    elif(parMode == 'inout'):
        call_type = 'REF'
    ST[-1].append([par_name,'par',call_type,offset])
    ST[-1][0][1] += 4                   #previous offset + 4 for the next entity
    printScopes("new par")

def addTempEntity(tmp_name,offset):
    ST[-1].append([tmp_name,'temp',offset])
    ST[-1][0][1] += 4                   #previous offset + 4 for the next entity
    printScopes("new tmp")

def addBlockEntity(block_name,block_type):
    
    ST[-2].append([block_name,block_type,None]) #add procedure or function entity
    printScopes("new block")
    
def deleteScope():
    global nesting_level
    del ST[-1]
    printScopes("deleted scope")
    
def printScopes(scope_func):
    if(print_scopes):
        n = len(ST)
        print(scope_func)
        for i in range (n):
            print(ST[i])
        print("\n")
        
def symbolTableFile():
    STlen = len(ST)
    symbFile = open(sp[0]+"SymbolTable"+'.txt' , 'w')
    for i in range(STlen):
        symbFile.write(str(ST[i]))
        symbFile.write("\n")
        
def searchEntity(name):
    global index,entity_ar
    entity_ar = []      #array with [entity,scopelevel]
    index = -1
    n = 0
    for n in range(len(ST)):
        currScope = ST[index]
        for i in currScope:
            if(i[0] == name):
                #print("found",currScope[0][0])
                entity_ar = i
                entity_ar.append(currScope[0][0])
                return entity_ar
        index -= 1
    entity_ar[0] = 0
    return entity_ar


def searchEntityBlock(name):
    global index,entity_ar
    entity_ar = []      #array with [entity,scopelevel]
    index = -1
    n = 0
    for n in range(len(ST)):
        currScope = ST[index]
        for i in currScope:
            if(i[0] == name and (i[1] == "func" or i[1] == "proc")):
                #print("found",currScope[0][0])
                entity_ar = i
                entity_ar.append(currScope[0][0])
                return entity_ar
        index -= 1
    entity_ar[0] = 0
    return entity_ar


#################################END OF SYMBOL TABLE FUNCTIONS ###########################

        
################################# START OF FINAL CODE ####################################

    
def gnvlcode(name):
    global finalStr,tmp_ent
    n = 0
    tmp_ent = searchEntity(name)
    if(tmp_ent[0] != 0):
        offset = tmp_ent[-2]
        ent_scope = tmp_ent[-1]
        finalStr.append("\t"+"lw $t0,-4($sp)\n")
        for n in range(ent_scope - 1):
            finalStr.append("\t"+"lw $t0,-4($t0)\n")
        finalStr.append("\t"+"add $t0,$t0,-"+str(offset)+"\n")
    else:
        print("ERROR:Entity not found in scopes!")
        sys.exit()

        
def loadvr(name,r):
    global finalStr,tmp_ent
    if(str(name).isdigit()):
        finalStr.append("\t"+"li $t"+ str(r) +","+str(name)+"\n")
    else:                   
        tmp_ent = searchEntity(name)
        if(tmp_ent[0] != 0):
            ent_name = tmp_ent[0]
            ent_type = tmp_ent[1]
            ent_offset = tmp_ent[-2]
            ent_scope = tmp_ent[-1]
            if( ent_scope == 0 and (ent_type != 'func' and ent_type != 'proc')):#global
                finalStr.append("\t"+"lw $t"+str(r)+",-"+str(ent_offset)+"($s0)"+"\n")
            elif(ent_scope == len(ST)-1):
                if(ent_type == 'var' or (ent_type=='par' and tmp_ent[2] == 'CV') or ent_type == 'temp'):
                    finalStr.append("\t"+"lw $t"+str(r)+",-"+str(ent_offset)+"($sp)"+"\n")
                elif(ent_type =='par' and tmp_ent[2] == 'REF'):
                    finalStr.append("\t"+"lw $t0,-"+str(ent_offset)+"($sp)"+"\n")
                    finalStr.append("\t"+"lw $t"+str(r)+",($t0)"+"\n")
                else:
                    print("Error inside loadvr1")
                    sys.exit()
            elif(ent_scope < len(ST)-1):
                print("type,2:",ent_type,tmp_ent[2])
                if(ent_type == 'var' or (ent_type == 'par' and tmp_ent[2] == 'CV')):
                    gnvlcode(name)
                    finalStr.append("\t"+"lw $t"+str(r)+",($t0)"+"\n")
                elif(ent_type == 'par' and tmp_ent[2] == 'REF'):
                    gnvlcode(name)
                    finalStr.append("\t"+"lw $t0,($t0)"+"\n")
                    finalStr.append("\t"+"lw $t"+str(r)+",($t0)"+"\n")
                else:
                    print("Error inside loadvr2")
                    sys.exit()
            else:
                print("Error inside loadvr3")
                sys.exit()
        else:
            print("ERROR:Entity not found in scopes!")
            sys.exit()


def storerv(r,name):
    global finalStr,tmp_ent
    tmp_ent = searchEntity(name)
    if(tmp_ent[0] != 0):
        ent_name = tmp_ent[0]
        ent_type = tmp_ent[1]
        ent_offset = tmp_ent[-2]
        ent_scope = tmp_ent[-1]
        if( ent_scope == 0 and ent_type == 'var'):
            finalStr.append("\t"+"sw $t"+str(r)+",-"+str(ent_offset)+"($s0)"+"\n")
        elif(ent_type == 'temp' or (ent_type == 'par' and tmp_ent[2] == 'CV' and ent_scope == len(ST)-1)):
            finalStr.append("\t"+"sw $t"+str(r)+",-"+str(ent_offset)+"($sp)"+"\n")
        elif(ent_type == 'par' and tmp_ent[2] == 'REF' and ent_scope == len(ST)-1):
            finalStr.append("\t"+"lw $t0,-"+str(ent_offset)+"($sp)"+"\n")
            finalStr.append("\t"+"sw $t"+str(r)+",($t0)"+"\n")
        elif(ent_type == 'par' and tmp_ent[2] == 'CV' and ent_scope < len(ST)-1):
            gnvlcode(name)
            finalStr.append("\t"+"sw $t"+str(r)+",($t0)"+"\n")
        elif(ent_type == 'par' and tmp_ent[2] == 'REF' and ent_scope < len(ST)-1):
            gnvlcode(name)
            finalStr.append("\t"+"lw $t0,($t0)"+"\n")
            finalStr.append("\t"+"sw $t"+str(r)+",($t0)"+"\n")
        else:
            print("Error inside storerv")
            sys.exit()
    else:
        print("ERROR:Entity not found in scopes!")
        sys.exit()

def compBranch(rel):
    if(rel == "="):
        return "beq "
    elif(rel == ">"):
        return "bgt "
    elif(rel == "<"):
        return "blt "
    elif(rel == ">="):
        return "bge "
    elif(rel == "<="):
        return "ble "
    elif(rel == "<>"):
        return "bne "
def opMips(op):
    if(op == "+"):
        return "add "
    elif(op == "-"):
        return "sub "
    elif(op == "*"):
        return "mul "
    elif(op == "/"):
        return "div "
    
def finalCode(quad):
    global tmp_ent,prg_name,dec_ent,call_ent,index
    begin_offset = ST[-1][0][1]
    line = quad[0]+1
    op = quad[1]
    x = quad[2]
    y = quad[3]
    z = quad[4]
    finalStr.append("\n")
    finalStr.append("L"+ str(line)+":")
    if(op == "jump"):
        finalStr.append("\t"+"b L"+str(z+1)+"\n")
    elif(op in comp_oper):
        loadvr(x,"1")
        loadvr(y,"2")
        finalStr.append("\t"+compBranch(op)+",$t1,$t2,L"+str(z)+"\n")
    elif(op == ":="):
        loadvr(x,"1")
        storerv("1",z)
    elif(op in operators):
        loadvr(x,"1")
        loadvr(y,"2")
        finalStr.append("\t"+opMips(op)+",$t1,$t1,$t2"+"\n")
        storerv("1",z)
    elif(op == "out"):
        finalStr.append("\t"+"li $v0,1"+"\n")
        loadvr(x,"0")
        finalStr.append("\t"+"move $a0,$t0"+"\n")
        finalStr.append("\t"+"syscall"+"\n")
    elif(op == "inp"):
        finalStr.append("\t"+"li $v0,5"+"\n")
        finalStr.append("\t"+"syscall"+"\n")
        storerv("0",x)
        finalStr.append("\t"+"move $v0,$t0"+"\n")
    elif(op == "ret"):
        loadvr(x,"1")
        finalStr.append("\t"+"lw $t0,-8($sp)"+"\n")
        finalStr.append("\t"+"sw $t1,($t0)"+"\n")
    elif(op == "par"):
        tmp_ent = searchEntity(x)
        if(tmp_ent[0] != 0):
            ent_name = tmp_ent[0]
            ent_type = tmp_ent[1]
            ent_offset = tmp_ent[-2]
            ent_scope = tmp_ent[-1]
            framelength = ST[ent_scope][0][1]
            
            if(y == "CV"):
                if(index == 0):
                    finalStr.append("\t"+"add $fp,$sp,"+str(framelength)+"\n")
                loadvr(x,"0")
                finalStr.append("\t"+"sw $t0,-"+str(12+4*index)+"($fp)"+"\n")
                index += 1
            elif(y == "REF"):
                dec_ent = searchEntity(x)
                dec_name = dec_ent[0]
                dec_type = dec_ent[1]
                dec_offset = dec_ent[-2]
                dec_scope = dec_ent[-1]
                if(index == 0):
                    finalStr.append("\t"+"add $fp,$sp,"+str(framelength)+"\n")
                if(ent_scope == len(ST)-1): #same level
                    if(dec_type == "var" or dec_type == "temp" or (dec_type == "par" and dec_ent[2] == "CV")):
                       finalStr.append("\t"+"addi $t0,$sp,-"+str(dec_offset)+"\n")
                    elif(dec_type == "par" and dec_ent[2] == "REF"):
                        finalStr.append("\t"+"lw $t0,-"+str(dec_offset)+"($sp) \n")
                elif(ent_scope != len(ST)-1):   #different level
                    if(dec_type == "var" or (dec_type == "par" and dec_ent[2] == "CV")):
                        gnvlcode(x)
                    elif(dec_type == "par" and dec_ent[2] == "REF"):
                        gnvlcode(x)
                        finalStr.append("\t"+"lw $t0,($t0) \n")
                finalStr.append("\t"+"sw $t0,-"+str(12+4*index)+"($fp) \n")
                index += 1
            elif(y == "RET"):
                if(index == 0):
                    finalStr.append("\t"+"add $fp,$sp,"+str(framelength)+"\n")
                dec_ent = searchEntity(x)
                dec_name = dec_ent[0]
                dec_type = dec_ent[1]
                dec_offset = dec_ent[-2]
                dec_scope = dec_ent[-1]
                finalStr.append("\t"+"addi $t0,$sp,-"+str(dec_offset)+"\n")
                finalStr.append("\t"+"sw $t0,-8($fp) \n")
                index += 1
    elif(op == "call"):
        index = 0
        framelen = 0

        call_ent = searchEntityBlock(x)
        call_name = call_ent[0]
        call_type = call_ent[1]
        call_offset = call_ent[-2]
        call_scope = call_ent[-1]
        #call_stat not implemented for finalcode
        finalStr.append("\n")
    elif(op == "begin_block"):
        if(x == prg_name):#the program starts
            asmFile = open(sp[0]+".asm","a")
            asmFile.write("\nLmain:"+"\n")
            finalStr.append("\t"+"addi $sp,$sp,"+str(begin_offset)+"\n")
            finalStr.append("\t"+"move $s0,$sp \n")
        else:
            finalStr.append("\t"+"sw $ra,($sp) \n")
    elif(op == "end_block"):
        if(x != prg_name):
            finalStr.append("\t"+"lw $ra,($sp) \n")
            finalStr.append("\t"+"jr $ra \n")
    elif(op == "halt"):
        finalStr.append("\t"+"li $v0,10 \n")
        finalStr.append("\t"+"syscall \n")
    else:
        print("Error in finalcode could not find quad: ",op)
        sys.exit()
            
        
   
        
def FinalIterator():
    finalStr.append("L0:\tb Lmain \n")
    for quad in quad_list:
        index = quad[0]
        finalCode(quad)
    writeFinal()

def writeFinal():
    asmFile = open(sp[0]+".asm","w+")
    for i in finalStr:
        asmFile.write(i)
    
        
        
################################# END OF FINAL CODE ######################################
        
################################  MAIN   #################################################

print_scopes = True
filename = raw_input("Enter your file name:")
f = open_file(filename)
sp = filename.split('.')
program()
print("Syntactical analyzation was successful!")
create_int_file()
print("Int file complete")
if(subprograms_flag == 1 ):
    print("Cannot create C File because there is a function or a procedure in the program")
else:
    createCFile()
    print("C File complete")
symbolTableFile()
FinalIterator()
print("Asm file successfuly created!")

