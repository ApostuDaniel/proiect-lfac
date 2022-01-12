%{
    #include <stdio.h>
    #include <string.h>
    #include <stdlib.h>
    #include <stdbool.h>
    #include <strings.h>
    #include "restart.h"
    extern FILE* yyin;
    extern char* yytext;
    extern int yylineno;
    void yyerror(char *);
    int yylex(void);

    void lookupIdTable(identif** id);
    identif** lookupIdTableByName(char* s);
    // expression* create_int_expr(int value);
    // expression* create_bool_expr(bool value);
    // expression* create_char_expr(char value);
    // expression* create_float_expr(float value);
    // expression* create_str_expr(char* value);
    // expression* create_expr_expr(expression* value);
    // void createBinaryOperation(expression** expr1, expression** expr2, expression** expr3, char op);
    // void createCompareOperation(expression** expr1, expression** expr2, expression** expr3, Comparison op);
    // void free_expr(expression* expr);
    identif* create_id(bool isConst, bool isType, bool isAssigned, char* Name, type_struct type);
    value_struct* createValue(Type t, int ival, bool bval, float fval, char cval, char* sval);
    value_struct* executeArithmeticOp(AST* left, AST* right, Operation op);
    value_struct* executeLogicalOp(AST* left, AST* right, Operation op);
    value_struct* executeComparisonOp(AST* left, AST* right, Operation op);
    void id_assignment(char* name, Type type, int size, value_struct* value, bool isArray, bool isConst, bool isType, bool isAssigned );
    // void fill_id(identif* id, bool isConst, bool isType, bool isAssigned, int index, type_struct type, expression* expression);
    void free_id(identif* id);
    void free_tree(AST* root);
    void freeValue(value_struct* value);
    void freeIdTable();
    void copyValue(value_struct** dest, value_struct** source);
    AST* buildAST(ASTInput in, AST* child1, AST* child2);
    value_struct* evalAST(AST* root);
    
    

    void print(char* string, AST* root);

    identif* id_table[100] = {0};
    unsigned int fillAmount = 0;
    
    
    
    void addVar(int x);
    void addVarID(char* name, char* datatype, char* type1, char* value1, char* defined);
    void printSymbolTable();
    int inSymbolTable1(char*);
    void updateVarValue(char* name, char* val);
    char* scopeTxt(int scope);
    char* inttoStr(int x);
    char* floattoStr(float x);
    char* chartoStr(char x);
    int nopar=0;
    int funccount=0;
    int scope=1;
    int noSym1=0;
    int inTable;
    char buff[40];
    
%}

%union{
    int intval;
    char* strval;
    char charval;
    bool boolval;
    float floatval;
    AST* expr_tree;
    Type tip;
}

%token GLOBAL /*global*/ VAR /*var*/ CONST /*const*/ STRING/*string*/ BOOL/*bool*/ FLOAT/*float*/  NEWTYPE/*newtype*/ 
%token CHAR/*char*/ INT/*int*/ FUNC/*func*/ TAKES/*takes*/ RETURNS/*returns*/ START/*start*/ THEN
%token VOID/*null*/ IF/*if*/ ELSE/*else*/ WHILE/*while*/ RETURN/*return*/ FOR/*for*/
%token ASSGN/*<-*/ EQ/*==*/ NEQ/*!=*/ GEQ/*>=*/ LEQ/*<=*/ AND/*&&*/ OR/*||*/ PP/*++*/ MM/*--*/ PRINT /*Print()*/

%right THEN ELSE
%left ','
%right ASSGN
%left OR 
%left AND 
%left EQ NEQ 
%left LEQ GEQ '>' '<'
%left '+' '-'
%left '*' '/'
%right '!'
%right '.'
%right PP MM
%left '(' '['

%token <strval>STRINGCONST
%token <boolval>BOOLCONST
%token <floatval>FLOATCONST
%token <charval>CHARCONST
%token <intval>INTCONST
%token <strval> ID /*identifier*/ 
%type <expr_tree> expr
%type <expr_tree> constant
%type <tip> type;
%type <strval> id;

%start s
%%
s:  global_scope definitions entry_point;

global_scope: GLOBAL '{' var_defs '}'
|   /* EMPTY */ 
;

var_defs: var_defs var_def ';'
|   /* EMPTY */ 
;

var_def: VAR type ID { if(lookupIdTableByName($3)) yyerror("Nu se poate declara de mai multe ori aceeasi variabila"); id_assignment($3, $2, -1, NULL, false, false, false, false); addVar(1); }   
|   VAR type ID ASSGN expr  { 
    if(lookupIdTableByName($3)) yyerror("Nu se poate declara de mai multe ori aceeasi variabila");
    value_struct* value = evalAST($5);
    free_tree($5);
    id_assignment($3, $2, -1, value, false, false, false, true); addVar(2);
    }

|   CONST type ID ASSGN expr {
    if(lookupIdTableByName($3)) yyerror("Nu se poate declara de mai multe ori aceeasi variabila");
    value_struct* value = evalAST($5);
    free_tree($5);
    id_assignment($3, $2, -1, value, false, true, false, true); addVar(3);
    }
|   VAR type '['INTCONST']' ID { if(lookupIdTableByName($6)) yyerror("Nu se poate declara de mai multe ori aceeasi variabila"); id_assignment($6, $2, $4, NULL, true, false, false, false); addVar(1); }
|   VAR type'['INTCONST']' ID ASSGN '{'expr'}'{
if(lookupIdTableByName($6)) yyerror("Nu se poate declara de mai multe ori aceeasi variabila");
    value_struct* value = evalAST($9);
    free_tree($9); 
    id_assignment($6, $2, $4, value, true, false, false, true); addVar(1); 
    switch(id_table[fillAmount-1]->idType.type)
                {
                    case T_INT:
                        updateVarValue($6, inttoStr(value->intvalue));
                        break;
                    case T_FLOAT:
                        updateVarValue($6, floattoStr(value->floatvalue));
                        break;
                    case T_BOOL:
                        updateVarValue($6, value->boolvalue ? "true" : "false");
                        break;
                    case T_CHAR:
                        updateVarValue($6, chartoStr(value->charvalue));
                        break;
                    case T_STRING:
                        updateVarValue($6, value->strvalue);
                        break;
                    default:
                        yyerror("Can't use assignment on user defined types");
                } 
    }
|   CONST type'['INTCONST']' ID ASSGN '{'expr'}'{
    if(lookupIdTableByName($6)) yyerror("Nu se poate declara de mai multe ori aceeasi variabila");
    value_struct* value = evalAST($9);
    free_tree($9); 
    id_assignment($6, $2, $4, value, true, true, false, true); addVar(4);
    switch(id_table[fillAmount-1]->idType.type)
                {
                    case T_INT:
                        updateVarValue($6, inttoStr(value->intvalue));
                        break;
                    case T_FLOAT:
                        updateVarValue($6, floattoStr(value->floatvalue));
                        break;
                    case T_BOOL:
                        updateVarValue($6, value->boolvalue ? "true" : "false");
                        break;
                    case T_CHAR:
                        updateVarValue($6, chartoStr(value->charvalue));
                        break;
                    case T_STRING:
                        updateVarValue($6, value->strvalue);
                        break;
                    default:
                        yyerror("Can't use assignment on user defined types");
                } 
    }
|   VAR ID ID { if(lookupIdTableByName($3)) yyerror("Nu se poate declara de mai multe ori aceeasi variabila"); id_assignment($3, T_USER, -1, NULL, false, false, false, false ); addVar(1); }
|   VAR ID'['INTCONST']' ID { if(lookupIdTableByName($6)) yyerror("Nu se poate declara de mai multe ori aceeasi variabila"); id_assignment($6, T_USER, $4, NULL, true, false, false, false ); addVar(1); }
|   CONST ID ID { if(lookupIdTableByName($3)) yyerror("Nu se poate declara de mai multe ori aceeasi variabila"); id_assignment($3, T_USER, -1, NULL, false, true, false, false ); addVar(4); }
|   CONST ID'['INTCONST']' ID { if(lookupIdTableByName($6)) yyerror("Nu se poate declara de mai multe ori aceeasi variabila"); id_assignment($6, T_USER, $4, NULL, true, true, false, false ); addVar(4); }
;

id: ID {$$ = strdup($1); free($1);}
|   id '.' ID{$$ = calloc(1, sizeof(char) * (strlen($1) + strlen($3))); strcat($$, $1); strcat($$, "."); strcat($$, $3); free($1); free($3);}
;

type: STRING {$$ = T_STRING; }
|   FLOAT   {$$ = T_FLOAT; }
|   BOOL    {$$ = T_BOOL; }
|   CHAR    {$$ = T_CHAR; }
|   INT     {$$ = T_INT; }
;


constant: STRINGCONST {
        ASTInput input; bzero(&input, sizeof(ASTInput)); 
        input.role = N_STRING; 
        input.value = createValue(T_STRING, 0, false, 0.0, 0, $1);
        free($1);
        $$ = buildAST(input, NULL, NULL);
        addVarID(yytext, "string" , "Constanta", $1, scopeTxt(scope));
    }
|   BOOLCONST   {
        ASTInput input; bzero(&input, sizeof(ASTInput)); 
        input.role = N_BOOLEAN; 
        input.value = createValue(T_BOOL, 0, $1, 0.0, 0, NULL);
        $$ = buildAST(input, NULL, NULL);
        addVarID(yytext, "bool" , "Constanta", $1 ? "true" : "false" , scopeTxt(scope));
    }
|   INTCONST    {
        ASTInput input;  
        input.role = N_INTEGER; 
        input.value = createValue(T_INT, $1, false, 0.0, 0, NULL);
        $$ = buildAST(input, NULL, NULL);
        addVarID(yytext, "int" , "Constanta", inttoStr($1), scopeTxt(scope));
    }
|   CHARCONST   {
        ASTInput input; bzero(&input, sizeof(ASTInput)); 
        input.role = N_CHARACHTER; 
        input.value = createValue(T_CHAR, 0, false, 0.0, $1, NULL);
        $$ = buildAST(input, NULL, NULL);
        addVarID(yytext, "char" , "Constanta", chartoStr($1), scopeTxt(scope));
    }
|   FLOATCONST  {
        ASTInput input; bzero(&input, sizeof(ASTInput)); 
        input.role = N_FLOAT; 
        input.value = createValue(T_FLOAT, 0, false, $1, 0, NULL);
        $$ = buildAST(input, NULL, NULL);
        addVarID(yytext, "float" , "Constanta", floattoStr($1), scopeTxt(scope));
    }
;

definitions: definitions func_def
|   definitions type_def
| /* EMPTY */ ;

return_type: type 
{ 
    switch($1)
    {
    	case 0: symbol_table1[noSym1].data_type = strdup("int"); break;
    	case 1: symbol_table1[noSym1].data_type = strdup("bool"); break;
    	case 2: symbol_table1[noSym1].data_type = strdup("string"); break;
    	case 3: symbol_table1[noSym1].data_type = strdup("char"); break;
    	case 4: symbol_table1[noSym1].data_type = strdup("float"); break;
    	case 5: symbol_table1[noSym1].data_type = strdup("user"); break;
    	default: printf(" ");
    }
}
|   VOID { symbol_table1[noSym1].data_type = strdup("VOID"); } ;

func_def: FUNC ID TAKES '(' params ')' RETURNS return_type { scope = 3; symbol_table1[noSym1].id_name=strdup($2); symbol_table1[noSym1].line_no = yylineno; symbol_table1[noSym1].defined = strdup("Global"); noSym1++; } statement ;

params: params ',' type ID 
{   parTable[nopar].name = strdup($4);
    parTable[nopar].parType.type = $3;
    parTable[nopar].ident = funccount;
    nopar++;
}
|   params ',' ID ID
{   parTable[nopar].name = strdup($4);
    parTable[nopar].parType.type = 5;
    parTable[nopar].ident = funccount;
    nopar++;
}											
|   type ID
{   parTable[nopar].name = strdup($2);
    parTable[nopar].parType.type = $1;
    parTable[nopar].ident = funccount;
    nopar++;
    funccount++;
}
|   ID ID
{   parTable[nopar].name = strdup($2);
    parTable[nopar].parType.type = 5;
    parTable[nopar].ident = funccount;
    nopar++;
    funccount++;
}
|   /* EMPTY */ { funccount++; }
;

type_def: NEWTYPE ID { scope = 2; addVarID($2, "user", "Constanta", "newtype", "Global"); } '{' var_defs '}' ';' ;

entry_point : { scope = 4; } START ':' statement ;

statement:  com_stmt '}'
|   IF '(' expr ')' THEN statement 
|   IF '(' expr ')' THEN statement ELSE statement
|   WHILE '(' expr ')' statement
|   FOR '(' expr ';' expr ';' expr ')' statement
|   RETURN expr ';'
|   var_def ';'
|   expr ';'
|   PRINT '(' STRINGCONST ',' expr ')' ';' {print($3, $5); free($3); free_tree($5);}
|     ';'
; 



com_stmt: '{'
|       com_stmt statement; 


expr: constant {$$ = $1;}
|   id '[' expr ']' ASSGN expr{
    if(strchr($1, '.') != NULL){
        ASTInput input;  
        input.role = N_OTHER; 
        input.string = "array_access";
        $$ = buildAST(input, NULL, NULL);
    }
    else{
        identif** id = lookupIdTableByName($1);
        if(id == NULL) {yyerror("Can't use an undefined identifier in an expression");}
        else if((*id)->isConst){yyerror("Can't assign value to const identifier");}
        else if((*id)->idType.isArray == false){yyerror("Can't use array acces brackets on non-array identifier");}
        else{
            value_struct* value = evalAST($6);
            free_tree($6);
            value_struct* index = evalAST($3);
            free_tree($3);

            if((*id)->idType.type != value->type){
                yyerror("Both sides of the assignment must be of the same type");
            }
            else if(index->type != T_INT){
                yyerror("Array access index must be of integer type");
            }
            else if(index->intvalue >=(*id)->arrSize || index->intvalue < 0){
                yyerror("Array access index out of bounds");
            }
            else {
                (*id)->isAssigned = true;
                switch((*id)->idType.type)
                {
                    case T_INT:
                        (*id)->intArray[index->intvalue] = value->intvalue;
                        updateVarValue($1, inttoStr(value->intvalue));
                        break;
                    case T_FLOAT:
                        (*id)->floatArray[index->intvalue] = value->floatvalue;
                        updateVarValue($1, floattoStr(value->floatvalue));
                        break;
                    case T_BOOL:
                        (*id)->boolArray[index->intvalue] = value->boolvalue;
                        updateVarValue($1, value->boolvalue ? "true" : "false");
                        break;
                    case T_CHAR:
                        (*id)->charArray[index->intvalue] = value->charvalue;
                        updateVarValue($1, chartoStr(value->charvalue));
                        break;
                    case T_STRING:
                        (*id)->strArray[index->intvalue] = strdup(value->strvalue);
                        updateVarValue($1, value->strvalue);
                        break;
                    default:
                        yyerror("Can't use assignment on user defined types");
                } 
            }
            
            ASTInput input; bzero(&input, sizeof(ASTInput)); 
            input.role = N_ARRAY_ELEM; input.string = strdup((*id)->name); input.index = index->intvalue; $$ = buildAST(input, NULL, NULL);
            free(input.string);
            freeValue(index);
        }
    }
    }
|   id  {
    if(strchr($1, '.') != NULL){
        ASTInput input;  
        input.role = N_OTHER; 
        input.string = "member_access";
        $$ = buildAST(input, NULL, NULL);
    }
    else{
        identif** id = lookupIdTableByName($1);
        if(id == NULL) yyerror("Can't use an undefined identifier in an expression");
        else if(!(*id)->isAssigned) yyerror("Can't use an unassigned identifier in an expression");
        else
        {
            ASTInput input; bzero(&input, sizeof(ASTInput)); 
            input.role = N_IDENTIFIER; input.string = strdup((*id)->name); $$ = buildAST(input, NULL, NULL);
            free(input.string);
        }
        }
    }

|   id ASSGN expr {
    if(strchr($1, '.') != NULL){
        ASTInput input;  
        input.role = N_OTHER; 
        input.string = "member_access";
        $$ = buildAST(input, NULL, NULL);
    }
    else{
        identif** id = lookupIdTableByName($1);
        if(id == NULL) {yyerror("Can't use an undefined identifier in an expression");}
        else if((*id)->isConst){yyerror("Can't assign value to const identifier");}
        else{
            value_struct* value = evalAST($3);
            free_tree($3);

            if((*id)->idType.type != value->type){
                yyerror("Both sides of the assignment must be of the same type");
            }
            else {
                (*id)->value = value;
                (*id)->isAssigned = true;
                switch((*id)->idType.type)
                {
                    case T_INT:
                        updateVarValue($1, inttoStr(value->intvalue));
                        break;
                    case T_FLOAT:
                        updateVarValue($1, floattoStr(value->floatvalue));
                        break;
                    case T_BOOL:
                        updateVarValue($1, value->boolvalue ? "true" : "false");
                        break;
                    case T_CHAR:
                        updateVarValue($1, chartoStr(value->charvalue));
                        break;
                    case T_STRING:
                        updateVarValue($1, value->strvalue);
                        break;
                    default:
                        yyerror("Can't use assignment on user defined types");
                } 
            }

            
            ASTInput input; bzero(&input, sizeof(ASTInput)); 
            input.role = N_IDENTIFIER; input.string = strdup((*id)->name); $$ = buildAST(input, NULL, NULL);
            free(input.string);
        }
    }
    }
    
|   '(' expr ')'    {
        value_struct* value = evalAST($2); 
        ASTInput input; bzero(&input, sizeof(ASTInput));
        switch (value->type)
        {
            case T_INT:
            input.role = N_INTEGER;
            break;
            case T_FLOAT:
            input.role = N_FLOAT;
            break;
            case T_BOOL:
            input.role = N_BOOLEAN;
            break;
            case T_STRING:
            input.role = N_STRING;
            break;
            case T_CHAR:
            input.role = N_CHARACHTER;
            break;
        default:
            input.role = N_OTHER;
            input.string = strdup("Undefined");
            break;
        }

        input.value = value; $$ = buildAST(input, NULL, NULL);
        free_tree($2);
    }
|   id '[' expr ']'{
    if(strchr($1, '.') != NULL){
        ASTInput input;  
        input.role = N_OTHER; 
        input.string = "array_access";
        $$ = buildAST(input, NULL, NULL);
    }
    else{
        identif** id = lookupIdTableByName($1);
        if(id == NULL) yyerror("Can't use an undefined identifier in an expression");
        else if(!(*id)->isAssigned) yyerror("Can't use an unassigned identifier in an expression");
        else{
            value_struct* index = evalAST($3);
            free_tree($3);
            if(index->type != T_INT){
                yyerror("Array access index is not integer");
            }
            else{
                ASTInput input; bzero(&input, sizeof(ASTInput)); 
                input.role = N_ARRAY_ELEM; input.string = strdup((*id)->name); input.index = index->intvalue; 
                $$ = buildAST(input, NULL, NULL);
                free(input.string);
            }  
        }
    }
    }
|   expr '(' exprs ')'  {
        ASTInput input;  
        input.role = N_OTHER; 
        input.string = "function_call";
        $$ = buildAST(input, NULL, NULL);
    }
|   expr '+' expr   {
        ASTInput input; 
        input.role = N_OP; input.op = O_PLUS;
        $$ = buildAST(input, $1, $3);
}
|   expr '-' expr %prec '+' {
        ASTInput input; bzero(&input, sizeof(ASTInput)); 
        input.role = N_OP; input.op = O_MINUS;
        $$ = buildAST(input, $1, $3);
}
|   expr '*' expr {
        ASTInput input; bzero(&input, sizeof(ASTInput)); 
        input.role = N_OP; input.op = O_MULTIPLY;
        $$ = buildAST(input, $1, $3);
}
|   expr '/' expr {
        ASTInput input; bzero(&input, sizeof(ASTInput)); 
        input.role = N_OP; input.op = O_DEVIDE;
        $$ = buildAST(input, $1, $3);
}
|   '-' expr    {
        ASTInput input; bzero(&input, sizeof(ASTInput)); 
        input.role = N_OP; input.op = O_UMINUS;
        $$ = buildAST(input, $2, NULL);
}
|   PP expr {
        if($2->role != N_IDENTIFIER){
            yyerror("Can't use ++ operator on rvalue");
        }
        else{
             identif** id = lookupIdTableByName($2->idName);
            if(id == NULL) yyerror("Can't use an undefined identifier in an expression");
            else if(!(*id)->isAssigned) yyerror("Can't use an unassigned identifier in an expression");
            else{
                ASTInput input; bzero(&input, sizeof(ASTInput)); 
                input.role = N_OP; input.op = O_PPL;
                $$ = buildAST(input, $2, NULL);
            }  
        }       
}

|   MM expr %prec PP  {
        if($2->role != N_IDENTIFIER){
            yyerror("Can't use -- operator on rvalue");
        }
        else{
             identif** id = lookupIdTableByName($2->idName);
            if(id == NULL) yyerror("Can't use an undefined identifier in an expression");
            else if(!(*id)->isAssigned) yyerror("Can't use an unassigned identifier in an expression");
            else{
                ASTInput input; bzero(&input, sizeof(ASTInput)); 
                input.role = N_OP; input.op = O_MML;
                $$ = buildAST(input, $2, NULL);
            }  
        }       
}
|   expr PP {
        if($1->role != N_IDENTIFIER){
            yyerror("Can't use -- operator on rvalue");
        }
        else{
             identif** id = lookupIdTableByName($1->idName);
            if(id == NULL) yyerror("Can't use an undefined identifier in an expression");
            else if(!(*id)->isAssigned) yyerror("Can't use an unassigned identifier in an expression");
            else{
                ASTInput input; bzero(&input, sizeof(ASTInput)); 
                input.role = N_OP; input.op = O_PPR;
                $$ = buildAST(input, $1, NULL);
            }  
        }       
}
|   expr MM %prec PP {
        if($1->role != N_IDENTIFIER){
            yyerror("Can't use -- operator on rvalue");
        }
        else{
             identif** id = lookupIdTableByName($1->idName);
            if(id == NULL) yyerror("Can't use an undefined identifier in an expression");
            else if(!(*id)->isAssigned) yyerror("Can't use an unassigned identifier in an expression");
            else{
                ASTInput input; bzero(&input, sizeof(ASTInput)); 
                input.role = N_OP; input.op = O_MMR;
                $$ = buildAST(input, $1, NULL);
            }  
        }       
}
|   expr AND expr   {
        ASTInput input; bzero(&input, sizeof(ASTInput)); 
        input.role = N_OP; input.op = O_AND;
        $$ = buildAST(input, $1, $3);
}
|   expr OR expr     {
        ASTInput input; bzero(&input, sizeof(ASTInput)); 
        input.role = N_OP; input.op = O_OR;
        $$ = buildAST(input, $1, $3);
}
|   '!' expr   {
        ASTInput input; bzero(&input, sizeof(ASTInput)); 
        input.role = N_OP; input.op = O_NEG;
        $$ = buildAST(input, $2, NULL);
}
|   expr EQ expr {
        ASTInput input; bzero(&input, sizeof(ASTInput)); 
        input.role = N_OP; input.op = O_EQ;
        $$ = buildAST(input, $1, $3);
}
|   expr NEQ expr {
        ASTInput input; bzero(&input, sizeof(ASTInput)); 
        input.role = N_OP; input.op = O_NEQ;
        $$ = buildAST(input, $1, $3);
}
|   expr LEQ expr {
        ASTInput input; bzero(&input, sizeof(ASTInput)); 
        input.role = N_OP; input.op = O_LEQ;
        $$ = buildAST(input, $1, $3);
}
|   expr GEQ expr {
        ASTInput input; bzero(&input, sizeof(ASTInput)); 
        input.role = N_OP; input.op = O_GEQ;
        $$ = buildAST(input, $1, $3);
}
|   expr '<' expr {
        ASTInput input; bzero(&input, sizeof(ASTInput)); 
        input.role = N_OP; input.op = O_LESSER;
        $$ = buildAST(input, $1, $3);
}
|   expr '>' expr {
        ASTInput input; bzero(&input, sizeof(ASTInput)); 
        input.role = N_OP; input.op = O_GREATER;
        $$ = buildAST(input, $1, $3);
}
;

exprs: exprs ',' expr
|   expr
|   /*EMPTY*/;

%%

value_struct* executeArithmeticOp(AST* left, AST* right, Operation op){
    value_struct* value;
    value_struct* leftVal;
    value_struct* rightVal;
    identif** id;

    leftVal = evalAST(left);
    rightVal = evalAST(right);
    if((left != NULL && !(leftVal->type == T_INT || leftVal->type == T_FLOAT))||
    (right != NULL && !(rightVal->type == T_INT || rightVal->type == T_FLOAT))){
        yyerror("Invalid types for arithmetic operator, operands must be of type int or float");
        return NULL;
    }
    

    bool leftIsFloat = (leftVal->type == T_FLOAT);
    bool rightIsFloat = (rightVal->type == T_FLOAT);
    

    switch(op){
        case O_PLUS:
            if(leftIsFloat && rightIsFloat){
                value = createValue(T_FLOAT, 0, false, leftVal->floatvalue + rightVal->floatvalue, 0, NULL);
            }
            else if(leftIsFloat){
                 value = createValue(T_FLOAT, 0, false, leftVal->floatvalue + (float)rightVal->intvalue, 0, NULL);
            }
            else if(rightIsFloat){
                 value = createValue(T_FLOAT, 0, false, (float)leftVal->intvalue + rightVal->floatvalue, 0, NULL);
            }
            else{
               
                int left = leftVal->intvalue;
                int right = rightVal->intvalue;
                value = createValue(T_INT, leftVal->intvalue + rightVal->intvalue, false, 0.0 , 0, NULL);
            }
            break;
        case O_MINUS:
                    if(leftIsFloat && rightIsFloat){
                        value = createValue(T_FLOAT, 0, false, leftVal->floatvalue - rightVal->floatvalue, 0, NULL);
                    }
                    else if(leftIsFloat){
                        value = createValue(T_FLOAT, 0, false, leftVal->floatvalue - (float)rightVal->intvalue, 0, NULL);
                    }
                    else if(rightIsFloat){
                        value = createValue(T_FLOAT, 0, false, (float)leftVal->intvalue - rightVal->floatvalue, 0, NULL);
                    }
                    else{
                        value = createValue(T_INT, leftVal->intvalue - rightVal->intvalue, false, 0.0 , 0, NULL);
                    }
                    break;
        case O_MULTIPLY:
                    if(leftIsFloat && rightIsFloat){
                        value = createValue(T_FLOAT, 0, false, leftVal->floatvalue * rightVal->floatvalue, 0, NULL);
                    }
                    else if(leftIsFloat){
                        value = createValue(T_FLOAT, 0, false, leftVal->floatvalue * (float)rightVal->intvalue, 0, NULL);
                    }
                    else if(rightIsFloat){
                        value = createValue(T_FLOAT, 0, false, (float)leftVal->intvalue * rightVal->floatvalue, 0, NULL);
                    }
                    else{
                        value = createValue(T_INT, leftVal->intvalue * rightVal->intvalue, false, 0.0 , 0, NULL);
                    }
                    break;
        case O_DEVIDE:
                    if((rightIsFloat && rightVal->floatvalue == 0)||(!rightIsFloat && rightVal->intvalue == 0)){
                        yyerror("Division by zero, change right value");
                        return NULL;
                    }
                    
                    if(leftIsFloat && rightIsFloat){
                        value = createValue(T_FLOAT, 0, false, leftVal->floatvalue / rightVal->floatvalue, 0, NULL);
                    }
                    else if(leftIsFloat){
                        value = createValue(T_FLOAT, 0, false, leftVal->floatvalue / (float)rightVal->intvalue, 0, NULL);
                    }
                    else if(rightIsFloat){
                        value = createValue(T_FLOAT, 0, false, (float)leftVal->intvalue / rightVal->floatvalue, 0, NULL);
                    }
                    else{
                        value = createValue(T_INT, leftVal->intvalue / rightVal->intvalue, false, 0.0 , 0, NULL);
                    }
                    break;
        case O_PPL:
                    id = lookupIdTableByName(left->idName);
                    if((*id)->idType.type == T_INT){
                        (*id)->value->intvalue = (*id)->value->intvalue + 1;
                        value = createValue(T_INT, (*id)->value->intvalue, false, 0.0, 0, NULL);
                    }
                    else if((*id)->idType.type == T_FLOAT){
                        (*id)->value->floatvalue = (*id)->value->floatvalue + 1;
                        value = createValue(T_FLOAT, 0, false, (*id)->value->floatvalue, 0, NULL);
                    }
                    else{
                        yyerror("Identifier must be of type int or float for arithmetic operation");
                        return NULL;
                    }
                    break;                  
        case O_PPR:
                    id = lookupIdTableByName(left->idName);
                    if((*id)->idType.type == T_INT){
                        value = createValue(T_INT, (*id)->value->intvalue, false, 0.0, 0, NULL);
                        (*id)->value->intvalue = (*id)->value->intvalue + 1;      
                    }
                    else if((*id)->idType.type == T_FLOAT){
                        value = createValue(T_FLOAT, 0, false, (*id)->value->floatvalue, 0, NULL);
                        (*id)->value->floatvalue = (*id)->value->floatvalue + 1;   
                    }
                    else{
                        yyerror("Identifier must be of type int or float for arithmetic operation");
                        return NULL;
                    }
                    break;                  
        case O_MML:
                    id = lookupIdTableByName(left->idName);
                    if((*id)->idType.type == T_INT){
                        (*id)->value->intvalue = (*id)->value->intvalue - 1;
                        value = createValue(T_INT, (*id)->value->intvalue, false, 0.0, 0, NULL);
                    }
                    else if((*id)->idType.type == T_FLOAT){
                        (*id)->value->floatvalue = (*id)->value->floatvalue - 1;
                        value = createValue(T_FLOAT, 0, false, (*id)->value->floatvalue, 0, NULL);
                    }
                    else{
                        yyerror("Identifier must be of type int or float for arithmetic operation");
                        return NULL;
                    }
                    break;                  
        case O_MMR:
                    id = lookupIdTableByName(left->idName);
                    if((*id)->idType.type == T_INT){
                        value = createValue(T_INT, (*id)->value->intvalue, false, 0.0, 0, NULL);
                        (*id)->value->intvalue = (*id)->value->intvalue - 1;      
                    }
                    else if((*id)->idType.type == T_FLOAT){
                        value = createValue(T_FLOAT, 0, false, (*id)->value->floatvalue, 0, NULL);
                        (*id)->value->floatvalue = (*id)->value->floatvalue - 1;   
                    }
                    else{
                        yyerror("Identifier must be of type int or float for arithmetic operation");
                        return NULL;
                    }
                    break;                  
        case O_UMINUS:
                if(leftIsFloat){
                    value = createValue(T_FLOAT, 0, false, -leftVal->floatvalue, 0, NULL);
                }
                else{
                     value = createValue(T_INT, -leftVal->intvalue, false, 0.0, 0, NULL);
                }
                break;
    }
    return value;
}

value_struct* executeLogicalOp(AST* left, AST* right, Operation op){
    value_struct* value;
    value_struct* leftVal;
    value_struct* rightVal;

    if(left == NULL){
        yyerror("NULL values for logical operation");
        return NULL;
    }

    leftVal = evalAST(left);
    rightVal = evalAST(right);

    if((rightVal->type != T_BOOL && leftVal->type != T_BOOL && op != O_NEG) || ( leftVal->type != T_BOOL)){
        yyerror("Invalid types for logical operation, operands must be of type bool");
        return NULL;
    }

    switch(op){
        case O_NEG:
            value = createValue(T_BOOL, 0, !leftVal->boolvalue, 0.0, 0, NULL);
        case O_AND:
            value = createValue(T_BOOL, 0, leftVal->boolvalue && rightVal->boolvalue, 0.0, 0, NULL);
        case O_OR:
            value = createValue(T_BOOL, 0, leftVal->boolvalue || rightVal->boolvalue, 0.0, 0, NULL);
    }

    return value;
}

value_struct* executeComparisonOp(AST* left, AST* right, Operation op){
    value_struct* value;
    value_struct* leftVal;
    value_struct* rightVal;

    if(left == NULL || right == NULL){
        yyerror("No values for comparison operation");
        return NULL;
    }

    leftVal = evalAST(left);
    rightVal = evalAST(right);
    bool leftIsNumeric = (leftVal->type == T_FLOAT || leftVal->type == T_INT);
    bool rightIsNumeric = (rightVal->type == T_FLOAT || rightVal->type == T_INT);

    if(!(leftIsNumeric && rightIsNumeric) &&  leftVal->type != rightVal->type){
        yyerror("Operators must both either be of numeric type, or of the same type for comparison operation");
        return NULL;
    }

    switch(op){
        case O_EQ:
            switch(leftVal->type){
                case T_INT:
                    if(rightVal->type == T_FLOAT) value = createValue(T_BOOL, 0, leftVal->intvalue == rightVal->floatvalue, 0.0, 0, NULL);
                    else value = createValue(T_BOOL, 0, leftVal->intvalue == rightVal->intvalue, 0.0, 0, NULL);
                    break;
                case T_FLOAT:
                    if(rightVal->type == T_INT) value = createValue(T_BOOL, 0, leftVal->floatvalue == rightVal->intvalue, 0.0, 0, NULL);
                    else value = createValue(T_BOOL, 0, leftVal->floatvalue == rightVal->floatvalue, 0.0, 0, NULL);
                    break;
                case T_STRING:
                    value = createValue(T_BOOL, 0, strcmp(leftVal->strvalue, rightVal->strvalue) == 0, 0.0, 0, NULL);
                    break;
                case T_BOOL:
                    value = createValue(T_BOOL, 0, leftVal->boolvalue == rightVal->boolvalue, 0.0, 0, NULL);
                    break;
                case T_CHAR:
                    value = createValue(T_BOOL, 0, leftVal->charvalue == rightVal->charvalue, 0.0, 0, NULL);
                    break;
                default:
                    yyerror("Unsupported type");
                    return NULL;
            }
            break;
        case O_NEQ:
            switch(leftVal->type){
                case T_INT:
                    if(rightVal->type == T_FLOAT) value = createValue(T_BOOL, 0, leftVal->intvalue != rightVal->floatvalue, 0.0, 0, NULL);
                    else value = createValue(T_BOOL, 0, leftVal->intvalue != rightVal->intvalue, 0.0, 0, NULL);
                    break;
                case T_FLOAT:
                    if(rightVal->type == T_INT) value = createValue(T_BOOL, 0, leftVal->floatvalue != rightVal->intvalue, 0.0, 0, NULL);
                    else value = createValue(T_BOOL, 0, leftVal->floatvalue != rightVal->floatvalue, 0.0, 0, NULL);
                    break;
                case T_STRING:
                    value = createValue(T_BOOL, 0, strcmp(leftVal->strvalue, rightVal->strvalue) != 0, 0.0, 0, NULL);
                    break;
                case T_BOOL:
                    value = createValue(T_BOOL, 0, leftVal->boolvalue != rightVal->boolvalue, 0.0, 0, NULL);
                    break;
                case T_CHAR:
                    value = createValue(T_BOOL, 0, leftVal->charvalue != rightVal->charvalue, 0.0, 0, NULL);
                    break;
                default:
                    yyerror("Unsupported type");
                    return NULL;
            }
            break;
        case O_LESSER:
            switch(leftVal->type){
                case T_INT:
                    if(rightVal->type == T_FLOAT) value = createValue(T_BOOL, 0, leftVal->intvalue < rightVal->floatvalue, 0.0, 0, NULL);
                    else value = createValue(T_BOOL, 0, leftVal->intvalue < rightVal->intvalue, 0.0, 0, NULL);
                    break;
                case T_FLOAT:
                    if(rightVal->type == T_INT) value = createValue(T_BOOL, 0, leftVal->floatvalue < rightVal->intvalue, 0.0, 0, NULL);
                    else value = createValue(T_BOOL, 0, leftVal->floatvalue < rightVal->floatvalue, 0.0, 0, NULL);
                    break;
                case T_STRING:
                    value = createValue(T_BOOL, 0, strcmp(leftVal->strvalue, rightVal->strvalue) < 0, 0.0, 0, NULL);
                    break;
                case T_BOOL:
                    value = createValue(T_BOOL, 0, leftVal->boolvalue < rightVal->boolvalue, 0.0, 0, NULL);
                    break;
                case T_CHAR:
                    value = createValue(T_BOOL, 0, leftVal->charvalue < rightVal->charvalue, 0.0, 0, NULL);
                    break;
                default:
                    yyerror("Unsupported type");
                    return NULL;
            }
            break;
        case O_GREATER:
            switch(leftVal->type){
                case T_INT:
                    if(rightVal->type == T_FLOAT) value = createValue(T_BOOL, 0, leftVal->intvalue > rightVal->floatvalue, 0.0, 0, NULL);
                    else value = createValue(T_BOOL, 0, leftVal->intvalue > rightVal->intvalue, 0.0, 0, NULL);
                    break;
                case T_FLOAT:
                    if(rightVal->type == T_INT) value = createValue(T_BOOL, 0, leftVal->floatvalue > rightVal->intvalue, 0.0, 0, NULL);
                    else value = createValue(T_BOOL, 0, leftVal->floatvalue > rightVal->floatvalue, 0.0, 0, NULL);
                    break;
                case T_STRING:
                    value = createValue(T_BOOL, 0, strcmp(leftVal->strvalue, rightVal->strvalue) > 0, 0.0, 0, NULL);
                    break;
                case T_BOOL:
                    value = createValue(T_BOOL, 0, leftVal->boolvalue > rightVal->boolvalue, 0.0, 0, NULL);
                    break;
                case T_CHAR:
                    value = createValue(T_BOOL, 0, leftVal->charvalue > rightVal->charvalue, 0.0, 0, NULL);
                    break;
                default:
                    yyerror("Unsupported type");
                    return NULL;
            }
            break;
        case O_LEQ:
            switch(leftVal->type){
                case T_INT:
                    if(rightVal->type == T_FLOAT) value = createValue(T_BOOL, 0, leftVal->intvalue <= rightVal->floatvalue, 0.0, 0, NULL);
                    else value = createValue(T_BOOL, 0, leftVal->intvalue <= rightVal->intvalue, 0.0, 0, NULL);
                    break;
                case T_FLOAT:
                    if(rightVal->type == T_INT) value = createValue(T_BOOL, 0, leftVal->floatvalue <= rightVal->intvalue, 0.0, 0, NULL);
                    else value = createValue(T_BOOL, 0, leftVal->floatvalue <= rightVal->floatvalue, 0.0, 0, NULL);
                    break;
                case T_STRING:
                    value = createValue(T_BOOL, 0, strcmp(leftVal->strvalue, rightVal->strvalue) <= 0, 0.0, 0, NULL);
                    break;
                case T_BOOL:
                    value = createValue(T_BOOL, 0, leftVal->boolvalue <= rightVal->boolvalue, 0.0, 0, NULL);
                    break;
                case T_CHAR:
                    value = createValue(T_BOOL, 0, leftVal->charvalue <= rightVal->charvalue, 0.0, 0, NULL);
                    break;
                default:
                    yyerror("Unsupported type");
                    return NULL;
            }
            break;
        case O_GEQ:
            switch(leftVal->type){
                case T_INT:
                    if(rightVal->type == T_FLOAT) value = createValue(T_BOOL, 0, leftVal->intvalue >= rightVal->floatvalue, 0.0, 0, NULL);
                    else value = createValue(T_BOOL, 0, leftVal->intvalue >= rightVal->intvalue, 0.0, 0, NULL);
                    break;
                case T_FLOAT:
                    if(rightVal->type == T_INT) value = createValue(T_BOOL, 0, leftVal->floatvalue >= rightVal->intvalue, 0.0, 0, NULL);
                    else value = createValue(T_BOOL, 0, leftVal->floatvalue >= rightVal->floatvalue, 0.0, 0, NULL);
                    break;
                case T_STRING:
                    value = createValue(T_BOOL, 0, strcmp(leftVal->strvalue, rightVal->strvalue) >= 0, 0.0, 0, NULL);
                    break;
                case T_BOOL:
                    value = createValue(T_BOOL, 0, leftVal->boolvalue >= rightVal->boolvalue, 0.0, 0, NULL);
                    break;
                case T_CHAR:
                    value = createValue(T_BOOL, 0, leftVal->charvalue >= rightVal->charvalue, 0.0, 0, NULL);
                    break;
                default:
                    yyerror("Unsupported type");
                    return NULL;
            }
            break;
    }

    return value;
}

void copyValue(value_struct** dest, value_struct** source){
    memcpy((*dest), (*source), sizeof(value_struct));
    if((*source)->strvalue != NULL){
        (*dest)->strvalue = strdup((*source)->strvalue);
    }
}

value_struct* createValue(Type t, int ival, bool bval, float fval, char cval, char* sval){
    value_struct* value = (value_struct*)calloc(1, sizeof(value_struct));
    value->type = t;
    switch(t){
        case T_INT:
            value->intvalue = ival;
            break;
        case T_BOOL:
            value->boolvalue = bval;
            break;
        case T_STRING:
            if(sval != NULL) value->strvalue = strdup(sval);
            else value->strvalue = sval;
            break;
        case T_CHAR:
            value->charvalue = cval;
            break;
        case T_FLOAT:
            value->floatvalue = fval;
            break;
        case T_USER:
            value->strvalue = strdup("USER");
            break;
        default:
            yyerror("Invalid type to createValue");
    }

    return value;
}

void freeValue(value_struct* value){
    if(value != NULL){
        if(value->strvalue != NULL) free(value->strvalue);
        free(value);
    }
    
}

void id_assignment(char* name, Type type, int size, value_struct* value, bool isArray, bool isConst, bool isType, bool isAssigned ){
    type_struct typeInfo;
    typeInfo.type = type; typeInfo.isArray = isArray;
    identif* id = create_id(isConst, isType, isAssigned, name, typeInfo);
    free(name);
    if(isArray){
        id->arrSize = size;
        switch(type){
        case T_INT:
            id->intArray = (int*)malloc(sizeof(int)*size);
            if(isAssigned){
                for(int i = 0; i < size; ++i){
                    id->intArray[i] = value->intvalue;
                }
            }
            else bzero(id->intArray, sizeof(int)*size);
            break;

        case T_BOOL:
            id->boolArray = (bool*)malloc(sizeof(bool)*size);
            if(isAssigned){
                for(int i = 0; i < size; ++i){
                    id->boolArray[i] = value->boolvalue;
                }
            }
            else bzero(id->boolArray, sizeof(bool)*size);
            break;

        case T_STRING:
            id->strArray = (char**)malloc(sizeof(char*)*size);
            if(isAssigned){
                for(int i = 0; i < size; ++i){
                    id->strArray[i] = strdup(value->strvalue);
                }
            }
            else bzero(id->strArray, sizeof(char*)*size);
            break;

        case T_CHAR:
            id->charArray = (char*)malloc(sizeof(char)*size);
            if(isAssigned){
                for(int i = 0; i < size; ++i){
                    id->charArray[i] = value->charvalue;
                }
            }
            else bzero(id->charArray, sizeof(char)*size);
            break;

        case T_FLOAT:
            id->floatArray = (float*)malloc(sizeof(float)*size);
            if(isAssigned){
                for(int i = 0; i < size; ++i){
                    id->floatArray[i] = value->floatvalue;
                }
            }
            else bzero(id->floatArray, sizeof(float)*size);
            break;         
        }
    }
     else{
        id->value = value;
     }

    id_table[fillAmount++] = id;  
}

AST* buildAST(ASTInput in, AST* child1, AST* child2){
    AST* root = (AST*)malloc(sizeof(AST));
    bzero(root, sizeof(AST));
    root->left = child1;
    root->right = child2;
    root->role = in.role;
    switch(in.role){
        case N_OP:
            root->op = in.op;
            break;
        case N_ARRAY_ELEM:
            root->idName = strdup(in.string);
            root->index = in.index; 
            break;
        case N_OTHER:
        case N_IDENTIFIER:
            root->idName = strdup(in.string);
            break;
        case N_INTEGER:
        case N_FLOAT:
        case N_BOOLEAN:
        case N_CHARACHTER:
        case N_STRING:
            root->value = in.value;
            break;
    }

    return root;
}

value_struct* evalAST(AST* root){
    if(root == NULL) return createValue(T_INT, 0, false, 0, 0, NULL);
    identif** id;
    value_struct* value;

    switch(root->role){
        case N_OP:
            switch(root->op){
                case O_PLUS:
                case O_MINUS:
                case O_MULTIPLY:
                case O_DEVIDE:
                case O_PPL:
                case O_PPR:
                case O_MML:
                case O_MMR:
                case O_UMINUS:
                    value = executeArithmeticOp(root->left, root->right, root->op);
                    break;
                case O_NEG:
                case O_AND:
                case O_OR:
                    value = executeLogicalOp(root->left, root->right, root->op);
                    break;
                case O_EQ:
                case O_NEQ:
                case O_LESSER:
                case O_GREATER:
                case O_LEQ:
                case O_GEQ:
                    value = executeComparisonOp(root->left, root->right, root->op);
                    break;
            }
            if(value == NULL) return createValue(T_INT, 0, false, 0.0, 0, NULL);
            else return value;
        case N_ARRAY_ELEM:
            id = lookupIdTableByName(root->idName);
            if(root->index < 0 ||  root->index >= (*id)->arrSize){
                char error[512];
                sprintf(error, "Array index out of range, array %s at index %d",root->idName, root->index);
                yyerror(error);
                return createValue(T_INT, 0, false, 0.0, 0, NULL);
            }

            switch((*id)->idType.type){
                case T_INT:
                    value = createValue(T_INT, (*id)->intArray[root->index], false, 0.0, 0, NULL);
                    break;
                case T_FLOAT:
                    value = createValue(T_FLOAT, 0, false, (*id)->floatArray[root->index], 0, NULL);
                    break;
                case T_BOOL:
                    value = createValue(T_BOOL, 0, (*id)->boolArray[root->index], 0.0 , 0, NULL);
                    break;
                case T_CHAR:
                    value = createValue(T_CHAR, 0, false, 0.0 , (*id)->charArray[root->index], NULL);
                    break;
                 case T_STRING:
                    value = createValue(T_STRING, 0, false, 0.0 , 0, (*id)->strArray[root->index]);
                    break;
                default:
                    yyerror("Expresion contains invalid type");
                    value = createValue(T_INT, 0, false, 0.0, 0, NULL);
                    break;
            }
            return value;

        case N_OTHER:
            return createValue(T_INT, 0, false, 0.0, 0, NULL);
        case N_IDENTIFIER:
            id = lookupIdTableByName(root->idName);
            if((*id)->idType.isArray){
                switch((*id)->idType.type){
                case T_INT:
                    value = createValue(T_INT, (*id)->value->intvalue, false, 0.0, 0, NULL);
                    break;
                case T_FLOAT:
                    value = createValue(T_FLOAT, 0, false, (*id)->value->floatvalue, 0, NULL);
                    break;
                case T_BOOL:
                    value = createValue(T_BOOL, 0, (*id)->value->boolvalue, 0.0 , 0, NULL);
                    break;
                case T_CHAR:
                    value = createValue(T_CHAR, 0, false, 0.0 , (*id)->value->charvalue, NULL);
                    break;
                 case T_STRING:
                    value = createValue(T_STRING, 0, false, 0.0 , 0, (*id)->value->strvalue);
                    break;
                default:
                    yyerror("Expresion contains invalid type");
                    value = createValue(T_INT, 0, false, 0.0, 0, NULL);
                    break;
                }
                return value;
            }
            value = (value_struct*)calloc(1, sizeof(value_struct));
            copyValue(&value, &(*id)->value);
            return value;
        case N_INTEGER:
        case N_FLOAT:
        case N_BOOLEAN:
        case N_CHARACHTER:
        case N_STRING:
            value = (value_struct*)calloc(1, sizeof(value_struct));
            copyValue(&value, &root->value);
            return value;
    }
}

void free_tree(AST* root){
    if(root != NULL){
        free_tree(root->left);
        free_tree(root->right);
        if(root->idName != NULL) free(root->idName);
        if(root->value != NULL) freeValue(root->value);
        free(root);
    }
}


identif** lookupIdTableByName(char* s){
    for(int i = 0; i < fillAmount; ++i){
        if(id_table[i] != NULL && strcmp(s, id_table[i]->name) == 0 ){
            return &id_table[i];
        }
    }
    return NULL;
} 


void print(char* string, AST* root){
    printf("%s", string);

    if(root == NULL){
        printf ("NULL VAL\n");
        return;
    }

    value_struct* value = evalAST(root);
    switch(value->type){
        case T_INT:
            printf("%d", value->intvalue);
            break;
        case T_FLOAT:
            printf("%f", value->floatvalue);
            break;
        case T_CHAR:
            printf("%c", value->charvalue);
            break;
        case T_BOOL:
            ;
            char* response = value->boolvalue ? "true":"false";
            printf("%s", response);
            break;
        case T_STRING:
            printf("%s", value->strvalue);
            break;
    }

    freeValue(value);  
} 


identif* create_id(bool isConst, bool isType, bool isAssigned, char* Name, type_struct type){
    identif* id = (identif*)malloc(sizeof(identif));
    bzero(id, sizeof(identif));
    id->isConst = isConst;
    id->isType = isType;
    id->isAssigned = isAssigned;
    id->idType = type;

    if(Name == NULL){
        yyerror("Nameless identifiers aren't allowed");
        return NULL;
    }
    else{
        id->name = strdup(Name);
    }

    return id;
}
 
void free_id(identif* id){
    if(id != NULL){
    if(id->name != NULL) free(id->name);
    if(id->intArray != NULL) free(id->intArray);
    if(id->floatArray != NULL) free(id->floatArray);
    if(id->boolArray != NULL) free(id->boolArray);
    if(id->value != NULL) freeValue(id->value);
    if(id->strArray != NULL){
        for(int i = 0; i < id->arrSize; i++){
            if(id->strArray[i] != NULL){
                free(id->strArray[i]);
            }
        }
    }
    free(id);
    }
}

void freeIdTable(){
    for(int i = 0; i < fillAmount; ++i){
        free(id_table[i]);
    }
}

void yyerror(char* s){
    fprintf(stderr, "\neroare: %s la linia:%d\n", s, yylineno);
}

int main(int argc, char** argv){
    yyin = fopen(argv[1], "r");
    yyparse();
    for(int i = 0; i < fillAmount; ++i){
        free(id_table[i]);
    }
    printSymbolTable();
    return 0;
}

char* inttoStr(int x)
{
    snprintf(buff, sizeof buff + 1, "%d", x);
    return buff;
}

char* floattoStr(float x)
{
    snprintf(buff, sizeof buff + 1, "%f", x);
    return buff;
}

char* chartoStr(char x)
{
    snprintf(buff, sizeof buff + 1, "%c", x);
    return buff;
}

void printSymbolTable(){
    FILE * file1;
    FILE * file2;
    file1 = fopen("symbol_table.txt","w");
    file2 = fopen("symbol_table_functions.txt", "w");
    
    int parct=0;
    int funcct=0;
    
	fprintf(file1,"\nSYMBOL   DATATYPE   TYPE   LINE NUMBER   VALUE   DEFINED IN\n");
	fprintf(file1,"_____________________________________________________________\n");
	fprintf(file2,"\nSYMBOL   RETURN   LINE NUMBER   SCOPE   Parameters + Types\n");
	fprintf(file2,"_____________________________________________________________\n");
	int i=0;
	for(i=0; i<noSym1; i++)
	    if(symbol_table1[i].type)
		fprintf(file1,"%s\t%s\t%s\t%d\t%s\t%s\t\n", symbol_table1[i].id_name, symbol_table1[i].data_type, symbol_table1[i].type, symbol_table1[i].line_no, symbol_table1[i].value, symbol_table1[i].defined);
	    else
	    {
	    	fprintf(file2,"%s\t%s\t%d\t%s\t", symbol_table1[i].id_name, symbol_table1[i].data_type, symbol_table1[i].line_no, symbol_table1[i].defined);
	    	fprintf(file2,"| %d,%d |", nopar, funccount);
	    	while(parTable[parct].ident == funcct)
	    	{
	    	    switch(parTable[parct].parType.type)
	    	    {
	    	        case 0: fprintf(file2, " %s,int ", parTable[parct].name); break;
	    	        case 1: fprintf(file2, " %s,bool ", parTable[parct].name); break;
	    	        case 2: fprintf(file2, " %s,string ", parTable[parct].name); break;
	    	        case 3: fprintf(file2, " %s,char ", parTable[parct].name); break;
	    	        case 4: fprintf(file2, " %s,float ", parTable[parct].name); break;
	    	        case 5: fprintf(file2, " %s,user ", parTable[parct].name); break;
	    	        default: fprintf(file2, "error");
	    	    }
	    	    parct++;
	    	}
	    	funcct++;
	    }
	for(i=0; i<nopar; i++)
	    free(parTable[i].name);
	for(i=0; i<noSym1; i++)
	{
		free(symbol_table1[i].id_name);
		free(symbol_table1[i].data_type);
		free(symbol_table1[i].type);
		free(symbol_table1[i].value);
		free(symbol_table1[i].defined);
	}
	fclose(file1);
	fclose(file2);
}

int inSymbolTable1(char * temp){
	int i;
	for(i=noSym1-1; i>=0; i--) {
		if(strcmp(symbol_table1[i].id_name, temp)==0) {
			return -1;
			break;
		}
	}
	return 0;
}

char* scopeTxt(int scope)
{
    switch(scope)
			{
			    case 1: return "Global"; break;
			    case 2: return "User Defined"; break;
			    case 3: return "In function"; break;
			    case 4: return "In Start"; break;
			    default: printf("Error: scope is 0");
			}
}

void updateVarValue(char* name, char* val)
{
    int i;
    for(i=noSym1-1; i>=0; i--) 
    {
    	if(strcmp(symbol_table1[i].id_name, name)==0) 
    	{
    	    symbol_table1[i].value = strdup(val);
    	    break;
	}
    }
}

void addVarID(char* name, char* datatype, char* type1, char* value1, char* defined)
{
    inTable=inSymbolTable1(name);
    if(!inTable) 
    {
        symbol_table1[noSym1].id_name=strdup(name);
        symbol_table1[noSym1].data_type=strdup(datatype);
        symbol_table1[noSym1].type=strdup(type1);
        symbol_table1[noSym1].line_no=yylineno;
        symbol_table1[noSym1].value=strdup(value1);
        symbol_table1[noSym1].defined=strdup(defined);
        noSym1++;
    }
}

void addVar(int x) {
  inTable=inSymbolTable1(yytext);
  if(!inTable) {
	switch(x)
	{
	case 1:
			symbol_table1[noSym1].id_name=strdup(id_table[fillAmount-1]->name);
			switch(id_table[fillAmount-1]->idType.type)
			{
			    case 0: symbol_table1[noSym1].data_type = strdup("int"); break;
			    case 1: symbol_table1[noSym1].data_type = strdup("bool"); break;
			    case 2: symbol_table1[noSym1].data_type = strdup("string"); break;
			    case 3: symbol_table1[noSym1].data_type = strdup("char"); break;
			    case 4: symbol_table1[noSym1].data_type = strdup("float"); break;
			    case 5: symbol_table1[noSym1].data_type = strdup("user"); break;
			    default: printf("Error at assigning type to var case 1");
			}
			symbol_table1[noSym1].type = strdup("Variabila");
			symbol_table1[noSym1].line_no = yylineno;
			symbol_table1[noSym1].defined = strdup(scopeTxt(scope));
			noSym1++;
			break;
			
	case 2:
			symbol_table1[noSym1].id_name=strdup(id_table[fillAmount-1]->name);
			switch(id_table[fillAmount-1]->idType.type)
			{
			    case 0: symbol_table1[noSym1].data_type = strdup("int"); break;
			    case 1: symbol_table1[noSym1].data_type = strdup("bool"); break;
			    case 2: symbol_table1[noSym1].data_type = strdup("string"); break;
			    case 3: symbol_table1[noSym1].data_type = strdup("char"); break;
			    case 4: symbol_table1[noSym1].data_type = strdup("float"); break;
			    case 5: symbol_table1[noSym1].data_type = strdup("user"); break;
			    default: printf("Error at assigning type to var case 1");
			}
			symbol_table1[noSym1].type = strdup("Variabila");
			symbol_table1[noSym1].line_no = yylineno;
			switch(id_table[fillAmount-1]->idType.type)
			{
			    case 0: symbol_table1[noSym1].value = strdup(inttoStr(id_table[fillAmount-1]->value->intvalue)); break;
			    case 1: symbol_table1[noSym1].value = strdup(id_table[fillAmount-1]->value->boolvalue ? "true" : "false"); break;
			    case 2: symbol_table1[noSym1].value = strdup(id_table[fillAmount-1]->value->strvalue); break;
			    case 3: symbol_table1[noSym1].value = strdup(chartoStr(id_table[fillAmount-1]->value->charvalue)); break;
			    case 4: symbol_table1[noSym1].value = strdup(floattoStr(id_table[fillAmount-1]->value->floatvalue)); break;
			    default: printf("Error at assigning value to var case 1");
			}
			symbol_table1[noSym1].defined = strdup(scopeTxt(scope));
			noSym1++;
			break;
			
	case 3:
			symbol_table1[noSym1].id_name=strdup(id_table[fillAmount-1]->name);
			switch(id_table[fillAmount-1]->idType.type)
			{
			    case 0: symbol_table1[noSym1].data_type = strdup("int"); break;
			    case 1: symbol_table1[noSym1].data_type = strdup("bool"); break;
			    case 2: symbol_table1[noSym1].data_type = strdup("string"); break;
			    case 3: symbol_table1[noSym1].data_type = strdup("char"); break;
			    case 4: symbol_table1[noSym1].data_type = strdup("float"); break;
			    case 5: symbol_table1[noSym1].data_type = strdup("user"); break;
			    default: printf("Error at assigning type to var case 1");
			}
			symbol_table1[noSym1].type = strdup("Constanta");
			symbol_table1[noSym1].line_no = yylineno;
			switch(id_table[fillAmount-1]->idType.type)
			{
			    case 0: symbol_table1[noSym1].value = strdup(inttoStr(id_table[fillAmount-1]->value->intvalue)); break;
			    case 1: symbol_table1[noSym1].value = strdup(id_table[fillAmount-1]->value->boolvalue ? "true" : "false"); break;
			    case 2: symbol_table1[noSym1].value = strdup(id_table[fillAmount-1]->value->strvalue); break;
			    case 3: symbol_table1[noSym1].value = strdup(chartoStr(id_table[fillAmount-1]->value->charvalue)); break;
			    case 4: symbol_table1[noSym1].value = strdup(floattoStr(id_table[fillAmount-1]->value->floatvalue)); break;
			    default: printf("Error at assigning value to var case 1");
			}
			symbol_table1[noSym1].defined = strdup(scopeTxt(scope));
			noSym1++;
			break;
	
	case 4:
			symbol_table1[noSym1].id_name=strdup(id_table[fillAmount-1]->name);
			switch(id_table[fillAmount-1]->idType.type)
			{
			    case 0: symbol_table1[noSym1].data_type = strdup("int"); break;
			    case 1: symbol_table1[noSym1].data_type = strdup("bool"); break;
			    case 2: symbol_table1[noSym1].data_type = strdup("string"); break;
			    case 3: symbol_table1[noSym1].data_type = strdup("char"); break;
			    case 4: symbol_table1[noSym1].data_type = strdup("float"); break;
			    case 5: symbol_table1[noSym1].data_type = strdup("user"); break;
			    default: printf("Error at assigning type to var case 1");
			}
			symbol_table1[noSym1].type = strdup("Constanta");
			symbol_table1[noSym1].line_no = yylineno;
			symbol_table1[noSym1].defined = strdup(scopeTxt(scope));
			noSym1++;
			break;
	
	default:	printf("error at switch in addVar");
	}	//switch
	}	//if
}


