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
    void id_assignment(char* name, Type type, int size, int value, bool isArray, bool isConst, bool isType, bool isAssigned );
    // void fill_id(identif* id, bool isConst, bool isType, bool isAssigned, int index, type_struct type, expression* expression);
    void free_id(identif* id);
    void free_tree(AST* root);
    AST* buildAST(ASTInput in, AST* child1, AST* child2);
    int evalAST(AST* root);
    
    

    void print(char* string, AST* root);

    identif* id_table[100] = {0};
    unsigned int fillAmount = 0;
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

%start s
%%
s:  global_scope definitions entry_point;

global_scope: GLOBAL '{' var_defs '}'
|   /* EMPTY */ 
;

var_defs: var_defs var_def ';'
|   /* EMPTY */ 
;

var_def: VAR type ID {id_assignment($3, $2, -1, -1, false, false, false, false);}
    
|   VAR type ID ASSGN expr  {
    int value = evalAST($5);
    free_tree($5);
    id_assignment($3, $2, -1, value, false, false, false, true);
    }

|   CONST type ID ASSGN expr {
    int value = evalAST($5);
    free_tree($5);
    id_assignment($3, $2, -1, value, false, true, false, true);
    }
|   VAR type '['INTCONST']' ID {id_assignment($6, $2, $4, -1, true, false, false, false);}
|   VAR type'['INTCONST']' ID ASSGN '{'expr'}'{
    int value = evalAST($9);
    free_tree($9); 
    id_assignment($6, $2, $4, value, true, false, false, true);
    }
|   CONST type'['INTCONST']' ID ASSGN '{'expr'}'{
    int value = evalAST($9);
    free_tree($9); 
    id_assignment($6, $2, $4, value, true, true, false, true);
    }
|   VAR ID ID {id_assignment($3, T_USER, -1, -1, false, false, false, false );}
|   VAR ID'['INTCONST']' ID {id_assignment($6, T_USER, -1, $4, true, false, false, false );}
|   CONST ID ID {id_assignment($3, T_USER, -1, -1, false, true, false, false );}
|   CONST ID'['INTCONST']' ID {id_assignment($6, T_USER, -1, $4, true, true, false, false );}
;


type: STRING {$$ = T_STRING; }
|   FLOAT   {$$ = T_FLOAT; }
|   BOOL    {$$ = T_BOOL; }
|   CHAR    {$$ = T_CHAR; }
|   INT     {$$ = T_INT; }
;



constant: STRINGCONST /*{$$ = create_str_expr($1);free($1);}*/
|   BOOLCONST   /*{$$ = create_bool_expr($1);}*/
|   INTCONST    {
    ASTInput input; bzero(&input, sizeof(ASTInput)); 
    input.role = N_INTEGER; input.value = $1; $$ = buildAST(input, NULL, NULL);
    }
|   CHARCONST   /*{$$ = create_char_expr($1);}*/
|   FLOATCONST  /*{$$ = create_float_expr($1);}*/
;

definitions: definitions func_def
|   definitions type_def
| /* EMPTY */ ;

return_type: type
|   VOID ;

func_def: FUNC ID TAKES '(' params ')' RETURNS return_type statement ;

params: params ',' type ID
|   params ',' ID ID
|   type ID
|   ID ID
|   /* EMPTY */ 
;

type_def: NEWTYPE ID '{' var_defs '}' ';' ;

entry_point : START ':' statement ;

statement:  com_stmt '}'
|   IF '(' expr ')' THEN statement 
|   IF '(' expr ')' THEN statement ELSE statement
|   WHILE '(' expr ')' statement
|   FOR '(' expr ';' expr ';' expr ')' statement
|   RETURN expr ';'
|   var_def ';'
|   expr ';'
|   PRINT '(' STRINGCONST ',' expr ')' ';' {printf("\nStarted print\n");print($3, $5);printf("\nFinished print\n"); free($3); free_tree($5);}
|     ';'
; 



com_stmt: '{'
|       com_stmt statement; 

expr: constant {$$ = $1;}
|   ID  {
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

|   ID ASSGN expr {
        identif** id = lookupIdTableByName($1);
        if(id == NULL) yyerror("Can't use an undefined identifier in an expression");
        else if(!(*id)->isAssigned) yyerror("Can't use an unassigned identifier in an expression");
        else{
            (*id)->intvalue = evalAST($3);
            free_tree($3);
            ASTInput input; bzero(&input, sizeof(ASTInput)); 
            input.role = N_IDENTIFIER; input.string = strdup((*id)->name); $$ = buildAST(input, NULL, NULL);
            free(input.string);
        }
    }
    
|   '(' expr ')'    {
        int value = evalAST($2); 
        ASTInput input; bzero(&input, sizeof(ASTInput)); 
        input.role = N_INTEGER; input.value = value; $$ = buildAST(input, NULL, NULL);
        free_tree($2);
        printf("\nFinished (expr)\n");
    }
|   ID '[' expr ']'{
        identif** id = lookupIdTableByName($1);
        if(id == NULL) yyerror("Can't use an undefined identifier in an expression");
        else if(!(*id)->isAssigned) yyerror("Can't use an unassigned identifier in an expression");
        else{
            int index = evalAST($3);
            free_tree($3);
            ASTInput input; bzero(&input, sizeof(ASTInput)); 
            input.role = N_ARRAY_ELEM; input.string = strdup((*id)->name); input.index = index; 
            $$ = buildAST(input, NULL, NULL);
            free(input.string);
        }
    }
|   expr '(' exprs ')'
|   expr '.' ID
|   expr '+' expr   {
        ASTInput input; bzero(&input, sizeof(ASTInput)); 
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
|   expr AND expr   /*{
    if($1->exprType.type == T_BOOL && $3->exprType.type == T_BOOL && $1->exprType.isArray == false && $3->exprType.isArray == false) 
        $$ = create_bool_expr($1->boolvalue && $3->boolvalue); 
    else 
        yyerror("for binary '&&' operator both operands must be of type bool\n"); 
    free_expr($1);
    free_expr($3); 
    }*/

|   expr OR expr     /*{
    if($1->exprType.type == T_BOOL && $3->exprType.type == T_BOOL && $1->exprType.isArray == false && $3->exprType.isArray == false) 
        $$ = create_bool_expr($1->boolvalue || $3->boolvalue); 
    else 
        yyerror("for binary '||' operator both operands must be of type bool\n"); 
    free_expr($1);
    free_expr($3); 
    }*/
|   '!' expr   /*{
    if($2->exprType.type == T_BOOL &&  $2->exprType.isArray == false) 
        $$ = create_bool_expr(!$2->boolvalue); 
    else 
        yyerror("for unary '!' operator the operand must be of type bool"); 
    free_expr($2);
    }*/
|   expr EQ expr /*{createCompareOperation(&$1, &$3, &$$, OP_EQ);}*/
|   expr NEQ expr /*{createCompareOperation(&$1, &$3, &$$, OP_NEQ);}*/
|   expr LEQ expr /*{createCompareOperation(&$1, &$3, &$$, OP_LEQ);}*/
|   expr GEQ expr /*{createCompareOperation(&$1, &$3, &$$, OP_GEQ);}*/
|   expr '<' expr /*{createCompareOperation(&$1, &$3, &$$, OP_L);}*/
|   expr '>' expr /*{createCompareOperation(&$1, &$3, &$$, OP_G);}*/
;

exprs: exprs ',' expr
|   expr
|   /*EMPTY*/;

%%
 void id_assignment(char* name, Type type, int size, int value, bool isArray, bool isConst, bool isType, bool isAssigned ){
    type_struct typeInfo;
    typeInfo.type = type; typeInfo.isArray = isArray;
    identif* id = create_id(isConst, isType, isAssigned, name, typeInfo);
    free(name);
    if(isArray){
        id->arrSize = size;
        id->intArray = (int*)malloc(sizeof(int)*size);
        if(isAssigned){
            for(int i = 0; i < size; ++i){
                id->intArray[i] = value;
            }
        }
        else bzero(id->intArray, sizeof(int)*size);
     }
     else{
         if(isAssigned) id->intvalue = value;
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
            root->intvalue = in.value;
            break;
    }

    return root;
}

int evalAST(AST* root){
    if(root == NULL) return 0;
    identif** id;
    int left, right;

    switch(root->role){
        case N_OP:
            left = evalAST(root->left);
            printf("\nEvaluated left\n");
            right = evalAST(root->right);
            printf("\nEvaluated right\n");
            switch(root->op){
                case O_PLUS:
                    return left + right;
                case O_MINUS:
                    return left - right;
                case O_MULTIPLY:
                    return left * right;
                case O_DEVIDE:
                    if(right == 0){
                        yyerror("Division by zero, change left value");
                        return 0;
                    }
                    else return left/right;
                case O_PPL:
                    id = lookupIdTableByName(root->left->idName);
                    (*id)->intvalue = (*id)->intvalue + 1;
                    return left + 1;
                case O_PPR:
                    id = lookupIdTableByName(root->left->idName);
                    (*id)->intvalue = (*id)->intvalue + 1;
                    return left;
                case O_MML:
                    id = lookupIdTableByName(root->left->idName);
                    (*id)->intvalue = (*id)->intvalue - 1;
                    return left - 1;
                case O_MMR:
                    id = lookupIdTableByName(root->left->idName);
                    (*id)->intvalue = (*id)->intvalue - 1;
                    return left;
                case O_UMINUS:
                    return -left;
            }
        case N_ARRAY_ELEM:
            id = lookupIdTableByName(root->idName);
            if(root->index < 0 ||  root->index >= (*id)->arrSize){
                char error[512];
                sprintf(error, "Array index out of range, array %s at index %d",root->idName, root->index);
                yyerror(error);
                return 0;
            }
            return (*id)->intArray[root->index];
        case N_OTHER:
            return 0;
        case N_IDENTIFIER:
            ;
            id = lookupIdTableByName(root->idName);
            return (*id)->intvalue;
        case N_INTEGER:
            return root->intvalue;
    }
}

void free_tree(AST* root){
    if(root != NULL){
        free_tree(root->left);
        free_tree(root->right);
        if(root->idName != NULL) free(root->idName);
        if(root->strvalue != NULL) free(root->strvalue);
        free(root);
    }
}



/* 
void createCompareOperation(expression** expr1, expression** expr2, expression** expr3, Comparison op){
    char err[264] = {0};
    if((*expr1)->exprType.type == (*expr2)->exprType.type && (*expr1)->exprType.isArray == false && (*expr2)->exprType.isArray == false){
        switch ((*expr2)->exprType.type)
        {
        case T_INT:
            switch (op)
            {
                case OP_EQ:
                    (*expr3) = create_bool_expr((*expr1)->intvalue == (*expr2)->intvalue);
                    break;
                case OP_NEQ:
                    (*expr3) = create_bool_expr((*expr1)->intvalue != (*expr2)->intvalue);
                    break;
                case OP_LEQ:
                    (*expr3) = create_bool_expr((*expr1)->intvalue <= (*expr2)->intvalue);
                    break;
                case OP_GEQ:
                    (*expr3) = create_bool_expr((*expr1)->intvalue >= (*expr2)->intvalue);
                    break;
                case OP_L:
                    (*expr3) = create_bool_expr((*expr1)->intvalue < (*expr2)->intvalue);
                    break;
                case OP_G:
                    (*expr3) = create_bool_expr((*expr1)->intvalue > (*expr2)->intvalue);
                    break;
                default:
                    sprintf(err, "Invalid comaprison operation");
                    yyerror(err);
            }       
            break;
        case T_CHAR:
            switch (op)
            {
                case OP_EQ:
                    (*expr3) = create_bool_expr((*expr1)->charvalue == (*expr2)->charvalue);
                    break;
                case OP_NEQ:
                    (*expr3) = create_bool_expr((*expr1)->charvalue != (*expr2)->charvalue);
                    break;
                case OP_LEQ:
                    (*expr3) = create_bool_expr((*expr1)->charvalue <= (*expr2)->charvalue);
                    break;
                case OP_GEQ:
                    (*expr3) = create_bool_expr((*expr1)->charvalue >= (*expr2)->charvalue);
                    break;
                case OP_L:
                    (*expr3) = create_bool_expr((*expr1)->charvalue < (*expr2)->charvalue);
                    break;
                case OP_G:
                    (*expr3) = create_bool_expr((*expr1)->charvalue > (*expr2)->charvalue);
                    break;
                default:
                    sprintf(err, "Invalid comaprison operation");
                    yyerror(err);
            }       
            break;
        case T_FLOAT:
            switch(op){
                case OP_EQ:
                    (*expr3) = create_bool_expr((*expr1)->floatvalue == (*expr2)->floatvalue);
                    break;
                case OP_NEQ:
                    (*expr3) = create_bool_expr((*expr1)->floatvalue != (*expr2)->floatvalue);
                    break;
                case OP_LEQ:
                    (*expr3) = create_bool_expr((*expr1)->floatvalue <= (*expr2)->floatvalue);
                    break;
                case OP_GEQ:
                    (*expr3) = create_bool_expr((*expr1)->floatvalue >= (*expr2)->floatvalue);
                    break;
                case OP_L:
                    (*expr3) = create_bool_expr((*expr1)->floatvalue < (*expr2)->floatvalue);
                    break;
                case OP_G:
                    (*expr3) = create_bool_expr((*expr1)->floatvalue > (*expr2)->floatvalue);
                    break;
                default:
                    sprintf(err, "Invalid comaprison operation");
                    yyerror(err);
            }       
            break;
        case T_STRING:
            switch(op){
                    case OP_EQ:
                        (*expr3) = create_bool_expr(strcmp((*expr1)->strvalue, (*expr2)->strvalue) == 0);
                        break;
                    case OP_NEQ:
                        (*expr3) = create_bool_expr(strcmp((*expr1)->strvalue, (*expr2)->strvalue) != 0);
                        break;
                    case OP_LEQ:
                        (*expr3) = create_bool_expr(strcmp((*expr1)->strvalue, (*expr2)->strvalue) <= 0);
                        break;
                    case OP_GEQ:
                        (*expr3) = create_bool_expr(strcmp((*expr1)->strvalue, (*expr2)->strvalue) >= 0);
                        break;
                    case OP_L:
                        (*expr3) = create_bool_expr(strcmp((*expr1)->strvalue, (*expr2)->strvalue) < 0);
                        break;
                    case OP_G:
                        (*expr3) = create_bool_expr(strcmp((*expr1)->strvalue, (*expr2)->strvalue) > 0);
                        break;
                    default:
                        sprintf(err, "Invalid comaprison operation");
                        yyerror(err);
                }       
                break;
        case T_BOOL:
            switch(op){
                    case OP_EQ:
                        (*expr3) = create_bool_expr((*expr1)->boolvalue == (*expr2)->boolvalue);
                        break;
                    case OP_NEQ:
                        (*expr3) = create_bool_expr((*expr1)->boolvalue != (*expr2)->boolvalue);
                        break;
                    case OP_LEQ:
                        (*expr3) = create_bool_expr((*expr1)->boolvalue <= (*expr2)->boolvalue);
                        break;
                    case OP_GEQ:
                        (*expr3) = create_bool_expr((*expr1)->boolvalue >= (*expr2)->boolvalue);
                        break;
                    case OP_L:
                        (*expr3) = create_bool_expr((*expr1)->boolvalue < (*expr2)->boolvalue);
                        break;
                    case OP_G:
                        (*expr3) = create_bool_expr((*expr1)->boolvalue > (*expr2)->boolvalue);
                        break;
                    default:
                        sprintf(err, "Invalid comaprison operation");
                        yyerror(err);
                }       
                break;
       
        default:
            sprintf(err, "invalid type for comparison operator");
            yyerror(err);
            break;
        }
    }
    else {
            sprintf(err, "for comparison operator both operands must be of same type");
            yyerror(err); 
        }
    if(!(*expr1)->isLvalue) free_expr((*expr1));
    if(!(*expr2)->isLvalue) free_expr((*expr2));
}

void createBinaryOperation(expression** expr1, expression** expr2, expression** expr3, char op){   
    char err[264] = {0};
    if((*expr1)->exprType.type == (*expr2)->exprType.type && (*expr1)->exprType.isArray == false && (*expr2)->exprType.isArray == false){
        switch ((*expr2)->exprType.type)
        {
        case T_INT:
            switch (op)
            {
                case '+':
                    (*expr3) = create_int_expr((*expr1)->intvalue + (*expr2)->intvalue);
                    break;
                case '*':
                    (*expr3) = create_int_expr((*expr1)->intvalue * (*expr2)->intvalue);
                    break;
                case '-':
                    (*expr3) = create_int_expr((*expr1)->intvalue - (*expr2)->intvalue);
                    break;
                case '/':
                    (*expr3) = create_int_expr((*expr1)->intvalue / (*expr2)->intvalue);
                    break;
            }       
            break;
        case T_CHAR:
            switch (op)
            {
                case '+':
                    (*expr3) = create_char_expr((*expr1)->charvalue + (*expr2)->charvalue);
                    break;
                case '*':
                    (*expr3) = create_char_expr((*expr1)->charvalue * (*expr2)->charvalue);
                    break;
                case '-':
                    (*expr3) = create_char_expr((*expr1)->charvalue - (*expr2)->charvalue);
                    break;
                case '/':
                    (*expr3) = create_char_expr((*expr1)->charvalue / (*expr2)->charvalue);
                    break;
            }       
            break;
        case T_FLOAT:
            switch (op)
            {
                case '+':
                    (*expr3) = create_float_expr((*expr1)->floatvalue + (*expr2)->floatvalue);
                    break;
                case '*':
                    (*expr3) = create_float_expr((*expr1)->floatvalue * (*expr2)->floatvalue);
                    break;
                case '-':
                    (*expr3) = create_float_expr((*expr1)->floatvalue - (*expr2)->floatvalue);
                    break;
                case '/':
                    (*expr3) = create_float_expr((*expr1)->floatvalue / (*expr2)->floatvalue);
                    break;
            }       
            break;
        default:
            sprintf(err, "one or both of the operands for '%c' is non-numeric", op);
            yyerror(err);
            break;
        }
    }
    else {
            sprintf(err, "for binary '%c' operator both operands must be of same type", op);
            yyerror(err); 
        }
    if(!(*expr1)->isLvalue) free_expr((*expr1));
    if(!(*expr2)->isLvalue) free_expr((*expr2));
} */

/* void lookupIdTable(identif** id){
    for(int i = 0; i < fillAmount; ++i){
        if(id_table[i] != NULL && strcmp((*id)->name, id_table[i]->name) == 0 ){
            free_id(*id);
            *id = id_table[i];
            return;
        }
    }
    printf("match not found\n");
}
*/
identif** lookupIdTableByName(char* s){
    for(int i = 0; i < fillAmount; ++i){
        if(id_table[i] != NULL && strcmp(s, id_table[i]->name) == 0 ){
            printf(" match found \n");
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
    int value = evalAST(root);
    printf("\nFinsished print eval\n");
    printf("%d", value);
    
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
/*
void fill_id(identif* id, bool isConst, bool isType, bool isAssigned, int index, type_struct type, expression* expr){
    id->isConst = isConst;
    id->isType = isType;
    id->idType = type;
    id->value = expr;
    id->isAssigned = isAssigned;
    if(expr != NULL){
        expr->idName = strdup(id->name);
        expr->index = index;
        expr->isLvalue = true;
    }
} */
 
void free_id(identif* id){
    if(id != NULL){
    if(id->name != NULL) free(id->name);
    if(id->intArray != NULL) free(id->intArray);
    if(id->floatArray != NULL) free(id->floatArray);
    if(id->boolArray != NULL) free(id->boolArray);
    if(id->strvalue != NULL) free(id->strvalue);
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
/*
expression* create_int_expr(int value){
    expression* expr = (expression*)malloc(sizeof(expression));
    expr->intvalue = value;
    expr->exprType.type = T_INT;
    expr->exprType.isArray == false;
    expr->isLvalue = false;
    expr->idName = NULL;
    return expr;
}

expression* create_char_expr(char value){
    expression* expr = (expression*)malloc(sizeof(expression));
    expr->charvalue = value;
    expr->exprType.type = T_CHAR;
    expr->exprType.isArray == false;
    expr->isLvalue = false;
    expr->idName = NULL;
    return expr;
}

expression* create_bool_expr(bool value){
    expression* expr = (expression*)malloc(sizeof(expression));
    expr->boolvalue = value;
    expr->exprType.type = T_BOOL;
    expr->exprType.isArray == false;
    expr->isLvalue = false;
    expr->idName = NULL;
    return expr;
}

expression* create_float_expr(float value){
    expression* expr = (expression*)malloc(sizeof(expression));
    expr->floatvalue = value;
    expr->exprType.type = T_FLOAT;
    expr->exprType.isArray == false;
    expr->isLvalue = false;
    expr->idName = NULL;
    return expr;
}

expression* create_str_expr(char* value){
    expression* expr = (expression*)malloc(sizeof(expression));
    if(value == NULL) expr->strvalue = NULL;
    else expr->strvalue = strdup(value);
    expr->exprType.type = T_STRING;
    expr->exprType.isArray == false;
    expr->isLvalue = false;
    expr->idName = NULL;
    return expr;
}

expression* create_expr_expr(expression* value){
    expression* expr = (expression*)malloc(sizeof(expression));
    expr->exprType.isArray = value->exprType.isArray;
    expr->isLvalue = value->isLvalue;
    if(expr->isLvalue){
        expr->idName = strdup(value->idName);
    }
    switch(value->exprType.type)
    {
        case T_INT:
            expr->exprType.type = T_INT;
            expr->intvalue = value->intvalue;
            break;

        case T_FLOAT:
            expr->exprType.type = T_FLOAT;
            expr->floatvalue = value->floatvalue;
            break;

        case T_STRING:
            expr->exprType.type = T_STRING;
            expr->strvalue = strdup(value->strvalue);
            break;

        case T_CHAR:
            expr->exprType.type = T_CHAR;
            expr->charvalue = value->charvalue;
            break;

        case T_BOOL:
            expr->exprType.type = T_BOOL;
            expr->boolvalue = value->boolvalue;
            break;

        default:
            yyerror("Feature not implemented yet");
    }

    return expr;
}

void free_expr(expression* expr){
    if(expr != NULL){
        if(expr->exprType.type == T_STRING && expr->strvalue != NULL)
        {
            free(expr->strvalue);
        }
        if(expr->idName != NULL){
            free(expr->idName);
        }
        free(expr);
    }
} */

void yyerror(char* s){
    printf("\neroare: %s la linia:%d\n", s, yylineno);
}

int main(int argc, char** argv){
    yyin = fopen(argv[1], "r");
    yyparse();
    for(int i = 0; i < fillAmount; ++i){
        free(id_table[i]);
    }
    return 0;
}

