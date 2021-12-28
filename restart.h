#ifndef RESTART_H
#define RESTART_H

#include <stdbool.h>

typedef enum{
    OP_EQ,
    OP_NEQ,
    OP_LEQ,
    OP_GEQ,
    OP_L,
    OP_G
} Comparison;

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

typedef struct 
{
    int intvalue;
    char* strvalue;
    float floatvalue;
    char charvalue;
    bool boolvalue;
    bool isLvalue;
    char* idName;
    int index;
    type_struct exprType;
} expression;

typedef struct{
    char* name;
    bool isAssigned;
    bool isConst;
    bool isType;
    type_struct idType;
    expression* value;
} identif;

#endif // !RESTART_H