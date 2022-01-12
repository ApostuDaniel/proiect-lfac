#ifndef RESTART_H
#define RESTART_H

#include <stdbool.h>

typedef enum 
{
    T_INT,
    T_BOOL,
    T_STRING,
    T_CHAR,
    T_FLOAT,
    T_USER
}Type;

typedef struct{
    Type type;
    bool isArray;
} type_struct;

typedef struct{
    Type type;
    int intvalue;
    char* strvalue;
    float floatvalue;
    char charvalue;
    bool boolvalue;   
} value_struct;

typedef enum{
    N_OP,
    N_IDENTIFIER,
    N_INTEGER,
    N_FLOAT,
    N_BOOLEAN,
    N_CHARACHTER,
    N_STRING,
    N_ARRAY_ELEM,
    N_OTHER
} NodeType;

typedef enum{
    O_PLUS,
    O_MINUS,
    O_MULTIPLY,
    O_DEVIDE,
    O_PPL,
    O_PPR,
    O_MML,
    O_MMR,
    O_UMINUS,
    O_NEG,
    O_AND,
    O_OR,
    O_EQ,
    O_NEQ,
    O_LESSER,
    O_GREATER,
    O_LEQ,
    O_GEQ,
} Operation;

typedef struct Node
{
    struct Node* left;
    struct Node* right;
    NodeType role;
    Operation op;
    char* idName;
    int index;
    value_struct* value;
} AST;

typedef struct 
{
    Operation op;
    char* string;
    NodeType role;
    int index;
    value_struct* value;
} ASTInput;


typedef struct{
    char* name;
    bool isAssigned;
    bool isConst;
    bool isType;
    type_struct idType;
    int arrSize;
    value_struct* value;
    int* intArray;
    char* charArray;
    char** strArray;
    float* floatArray;
    bool* boolArray;  
} identif;

struct varTable {
    char * id_name;
    char * data_type;
    char * type;
    int line_no;
    char* value;
    char * defined;
} symbol_table1[100];

struct parTable {
    char * name;
    type_struct parType;
    int ident;
} parTable[100];

#endif // !RESTART_H
