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
    expression* create_int_expr(int value);
    expression* create_bool_expr(bool value);
    expression* create_char_expr(char value);
    expression* create_float_expr(float value);
    expression* create_str_expr(char* value);
    expression* create_expr_expr(expression* value);
    void createBinaryOperation(expression** expr1, expression** expr2, expression** expr3, char op);
    void createCompareOperation(expression** expr1, expression** expr2, expression** expr3, Comparison op);
    void free_expr(expression* expr);
    identif* create_id(bool isConst, bool isType, bool isAssigned, char* Name, type_struct type, expression* expression);
    void fill_id(identif* id, bool isConst, bool isType, bool isAssigned, int index, type_struct type, expression* expression);
    void free_id(identif* id);
    
    

    void print(char* string, expression* expr);

    identif* id_table[100] = {0};
    unsigned int fillAmount = 0;
%}

%union{
    int intval;
    char* strval;
    char charval;
    bool boolval;
    float floatval;
    expression* expr_ptr;
    identif* id_ptr;
    type_struct type_str;
}

%token GLOBAL /*global*/ VAR /*var*/ CONST /*const*/ STRING/*string*/ BOOL/*bool*/ FLOAT/*float*/  NEWTYPE/*newtype*/ 
%token CHAR/*char*/ INT/*int*/ FUNC/*func*/ TAKES/*takes*/ RETURNS/*returns*/ START/*start*/ 
%token VOID/*null*/ IF/*if*/ ELSE/*else*/ WHILE/*while*/ RETURN/*return*/ FOR/*for*/
%token ASSGN/*<-*/ EQ/*==*/ NEQ/*!=*/ GEQ/*>=*/ LEQ/*<=*/ AND/*&&*/ OR/*||*/ PP/*++*/ MM/*--*/ PRINT /*Print()*/

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
%token <id_ptr> ID /*identifier*/ 
%type <expr_ptr> expr
%type <expr_ptr> constant
%type <type_str> type;

%start s
%%
s:  global_scope definitions entry_point;

global_scope: GLOBAL '{' var_defs '}'
|   /* EMPTY */ ;

var_defs: var_defs var_def ';'
|   /* EMPTY */ ;

var_def: VAR type ID {printf("\nVAR type ID\n"); fill_id($3, false, false, false, fillAmount ,$2,NULL ); id_table[fillAmount++] = $3; }
|   VAR type ID ASSGN expr  {printf("\nVAR type ID ASSGN expr\n");fill_id($3, false, false, true, fillAmount ,$2 ,$5 ); id_table[fillAmount++] = $3;}
|   CONST type ID ASSGN expr {printf("\nCONST type ID ASSGN expr\n");fill_id($3, true, false, true, fillAmount ,$2 ,$5 ); id_table[fillAmount++] = $3;};


type: STRING {printf("\nSTRING\n");type_struct str; str.type = T_STRING; str.isArray = false; $$ = str; }
|   FLOAT   {printf("\nFLOAT\n");type_struct flt; flt.type = T_FLOAT; flt.isArray = false; $$ = flt; }
|   BOOL    {printf("\nBOOL\n");type_struct b; b.type = T_BOOL; b.isArray = false; $$ = b; }
|   CHAR    {printf("\nCHAR\n");type_struct chr; chr.type = T_CHAR; chr.isArray = false; $$ = chr; }
|   INT     {printf("\nINT\n");type_struct i; i.type = T_INT; i.isArray = false; $$ = i; }
|   ID      {printf("\nIDpr\n");type_struct id; id.type = T_USER; id.isArray = false; $$ = id; }
|   type '[' ']' {printf("\ntype []\n"); $$.isArray = true; };

constant: STRINGCONST {printf("\nSTRINGCONST\n");$$ = create_str_expr($1);free($1);}
|   BOOLCONST   {printf("\nBOOLCONST\n");$$ = create_bool_expr($1);}
|   INTCONST    {printf("\nINTCONST\n");$$ = create_int_expr($1);}
|   CHARCONST   {printf("\nCHARCONST\n");$$ = create_char_expr($1);}
|   FLOATCONST  {printf("\nFLOATCONST\n");$$ = create_float_expr($1);};

definitions: definitions func_def
|   definitions type_def
| /* EMPTY */ ;

return_type: type
|   VOID ;

func_def: FUNC ID TAKES '(' params ')' RETURNS return_type statement ;

params: params ',' type ID
|   type ID
|   /* EMPTY */ ;

type_def: NEWTYPE ID '{' var_defs '}' ';' ;

entry_point : START ':' statement ;

statement:  com_stmt '}'
|   IF '(' expr ')' statement ELSE statement
|   WHILE '(' expr ')' statement
|   FOR '(' expr ';' expr ';' expr ')' statement
|   RETURN expr ';'
|   var_def ';'
|   expr ';'
|   PRINT '(' STRINGCONST ',' expr ')' ';' {printf("\nPRINT ( STRINGCONST , expr ) ;\n");print($3, $5); free($3); if($5->isLvalue == false) free_expr($5);}
|     ';'; 


com_stmt: '{'
|       com_stmt statement; 

expr: constant {printf("\nconstant\n");$$ = $1;}
|   ID  {printf("\nID\n");lookupIdTable(&$1); if(!$1->isAssigned) yyerror("Can't use an unassigned id in an expression"); else $$ = $1->value;}
|   expr ASSGN  expr { printf("\nexpr ASSGN expr\n");
    if($1->exprType.type == $3->exprType.type && $1->exprType.isArray == false && $3->exprType.isArray == false && 
    $1->isLvalue){
        identif** idptr = lookupIdTableByName($1->idName);
        expression* assignedValue = create_expr_expr($3);
        (*idptr)->value = assignedValue;
        if((*idptr)->value->idName != NULL) free((*idptr)->value->idName);
        (*idptr)->value->idName = strdup((*idptr)->name);
        (*idptr)->value->isLvalue = true;
        (*idptr)->value->index = $1->index;
        free_expr($1);
        $$ = create_expr_expr($3);
        if($$->idName !=NULL) free($$->idName);
        $$->isLvalue = false;
    }else {
        if(!$1->isLvalue){
            yyerror("can't assign to right value");
            free_expr($1);
        }
        yyerror("left and right value aren't of the same type");
        if(!$3->isLvalue) free_expr($3);
    }
    
    }
|   '(' expr ')'    {printf("\n( expr )\n"); $$ = $2;}
|   expr '[' expr ']'
|   expr '(' exprs ')'
|   expr '.' ID
|   expr '+' expr   {printf("\nexpr * expr\n");createBinaryOperation(&$1, &$3, &$$, '+');}
|   expr '-' expr %prec '+' {printf("\nexpr - expr\n");createBinaryOperation(&$1, &$3, &$$, '-');}
|   expr '*' expr {printf("\nexpr * expr\n");createBinaryOperation(&$1, &$3, &$$, '*');}
|   expr '/' expr {printf("\nexpr / expr\n");createBinaryOperation(&$1, &$3, &$$, '/');}
|   '-' expr    { printf("\n- expr\n");
    if(!$2->exprType.isArray){      
        switch ($2->exprType.type)
        {
        case T_INT:
            $$ = create_int_expr(-($2->intvalue));
            break;
        case T_FLOAT:
            $$ = create_float_expr(-($2->floatvalue));
            break;
        case T_CHAR:
            $$ = create_char_expr(-($2->charvalue));
            break;
        default:
            yyerror("cannot apply unary '-' operator on non-numeric type");
            break;
        }
    }
    else  yyerror("can't negate array type");
    free_expr($2);
    }
|   PP expr {printf("\nPP expr\n");
    if(!$2->isLvalue){
        yyerror("can't apply '++' operator on rigthvalue");
        exit(1);
    }
    if(!$2->exprType.isArray){
        switch ($2->exprType.type)
        {
        case T_INT:
            $2->intvalue = $2->intvalue + 1;
            $$ = create_int_expr( $2->intvalue);
            break;
        case T_FLOAT:
            $2->floatvalue = $2->floatvalue + 1;
            $$ = create_float_expr($2->floatvalue);
            break;
        case T_CHAR:
            $2->charvalue = $2->charvalue + 1;
            $$ = create_float_expr($2->charvalue);
            break;
        default:
            yyerror("cannot apply '++' operator on non-numeric type");
            break;
        }
    }
    else yyerror("Left to be implemented");
    }

|   MM expr %prec PP  {printf("\nMM expr\n");
    if(!$2->isLvalue){
        yyerror("can't apply '--'' operator on rigthvalue");
        exit(1);
    }
    if(!$2->exprType.isArray){
        switch ($2->exprType.type)
        {
        case T_INT:
            $2->intvalue = $2->intvalue - 1;
            $$ = create_int_expr( $2->intvalue);
            break;
        case T_FLOAT:
            $2->floatvalue = $2->floatvalue - 1;
            $$ = create_float_expr($2->floatvalue);
            break;
        case T_CHAR:
            $2->charvalue = $2->charvalue - 1;
            $$ = create_float_expr($2->charvalue);
            break;
        default:
            yyerror("cannot apply '--' operator on non-numeric type");
            break;
        }
    }
    else yyerror("Left to be implemented");
    }
|   expr PP {printf("\expr PP\n");
    if(!$1->isLvalue){
        yyerror("can't apply '++' operator on rigthvalue");
        exit(1);
    }
    if(!$1->exprType.isArray){
        switch ($1->exprType.type)
        {
        case T_INT:
            $1->intvalue = $1->intvalue + 1;
            $$ = create_int_expr( $1->intvalue - 1);
            break;
        case T_FLOAT:
            $1->floatvalue = $1->floatvalue + 1;
            $$ = create_float_expr($1->floatvalue - 1);
            break;
        case T_CHAR:
            $1->charvalue = $1->charvalue + 1;
            $$ = create_float_expr($1->charvalue - 1);
            break;
        default:
            yyerror("cannot apply '++' operator on non-numeric type");
            break;
        }
    }
    else yyerror("Left to be implemented");
    }
|   expr MM %prec MM {printf("\expr MM\n");
    if(!$1->isLvalue){
        yyerror("can't apply '--' operator on rigthvalue");
        exit(1);
    }
    if(!$1->exprType.isArray){
        switch ($1->exprType.type)
        {
        case T_INT:
            $1->intvalue = $1->intvalue - 1;
            $$ = create_int_expr( $1->intvalue + 1);
            break;
        case T_FLOAT:
            $1->floatvalue = $1->floatvalue - 1;
            $$ = create_float_expr($1->floatvalue + 1);
            break;
        case T_CHAR:
            $1->charvalue = $1->charvalue - 1;
            $$ = create_float_expr($1->charvalue + 1);
            break;
        default:
            yyerror("cannot apply '--' operator on non-numeric type");
            break;
        }
    }
    else yyerror("Left to be implemented");
    }
|   expr AND expr   {   printf("\nexpr AND expr\n");
    if($1->exprType.type == T_BOOL && $3->exprType.type == T_BOOL && $1->exprType.isArray == false && $3->exprType.isArray == false) 
        $$ = create_bool_expr($1->boolvalue && $3->boolvalue); 
    else 
        yyerror("for binary '&&' operator both operands must be of type bool\n"); 
    free_expr($1);
    free_expr($3); 
    }

|   expr OR expr     {  printf("\nexpr OR expr\n");
    if($1->exprType.type == T_BOOL && $3->exprType.type == T_BOOL && $1->exprType.isArray == false && $3->exprType.isArray == false) 
        $$ = create_bool_expr($1->boolvalue || $3->boolvalue); 
    else 
        yyerror("for binary '||' operator both operands must be of type bool\n"); 
    free_expr($1);
    free_expr($3); 
    }
|   '!' expr   {    printf("\n!expr\n");
    if($2->exprType.type == T_BOOL &&  $2->exprType.isArray == false) 
        $$ = create_bool_expr(!$2->boolvalue); 
    else 
        yyerror("for unary '!' operator the operand must be of type bool"); 
    free_expr($2);
    }
|   expr EQ expr {printf("\nexpr EQ expr\n");createCompareOperation(&$1, &$3, &$$, OP_EQ);}
|   expr NEQ expr {printf("\nexpr EQ expr\n");createCompareOperation(&$1, &$3, &$$, OP_NEQ);}
|   expr LEQ expr {printf("\nexpr EQ expr\n");createCompareOperation(&$1, &$3, &$$, OP_LEQ);}
|   expr GEQ expr {printf("\nexpr EQ expr\n");createCompareOperation(&$1, &$3, &$$, OP_GEQ);}
|   expr '<' expr {printf("\nexpr EQ expr\n");createCompareOperation(&$1, &$3, &$$, OP_L);}
|   expr '>' expr {printf("\nexpr EQ expr\n");createCompareOperation(&$1, &$3, &$$, OP_G);};

exprs: exprs ',' expr
|   expr
|   /*EMPTY*/;

%%
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
}

void lookupIdTable(identif** id){
    for(int i = 0; i < fillAmount; ++i){
        if(id_table[i] != NULL && strcmp((*id)->name, id_table[i]->name) == 0 ){
            free_id(*id);
            *id = id_table[i];
            return;
        }
    }
    printf("match not found\n");
}

identif** lookupIdTableByName(char* s){
    for(int i = 0; i < fillAmount; ++i){
        if(id_table[i] != NULL && strcmp(s, id_table[i]->name) == 0 ){
            printf("match found\n");
            return &id_table[i];
        }
    }
    return NULL;
}

void print(char* string, expression* expr){
    printf("%s", string);
    if(expr == NULL){
        printf ("NULL VAL\n");
        return;
    }
    switch (expr->exprType.type)
    {
        case T_INT:
            printf("%d\n", expr->intvalue);
            break;
        case T_CHAR:
            printf("%c\n", expr->charvalue);
            break;
        case T_FLOAT:
            printf("%f\n", expr->floatvalue);
            break;
        case T_STRING:
            printf("%s\n", expr->strvalue);
            break;
        case T_BOOL:
            printf(expr->boolvalue == true ? "true\n" : "false\n");
            break;
    default:
        printf("No string representation for this type\n");
    }
} 

identif* create_id(bool isConst, bool isType, bool isAssigned, char* Name, type_struct type, expression* expr){
    identif* id = (identif*)malloc(sizeof(identif));
    id->isConst = isConst;
    id->isType = isType;
    id->isAssigned = isAssigned;
    id->idType = type;
    id->value = expr;
    if(Name == NULL){
        yyerror("Nameless identifiers aren't allowed");
        return NULL;
    }
    else{
        id->name = strdup(Name);
    }

    return id;
}

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
}

void free_id(identif* id){
    if(id != NULL){
    if(id->name != NULL) free(id->name);
    free_expr(id->value);
    free(id);
    }
    
}

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
}

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

