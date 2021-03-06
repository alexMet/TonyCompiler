/******************************************************************************
 *  CVS version:
 *     $Id: symbol.c,v 1.3 2004/05/05 22:00:08 nickie Exp $
 ******************************************************************************
 *
 *  C code file : symbol.c
 *  Project     : PCL Compiler
 *  Version     : 1.0 alpha
 *  Written by  : Nikolaos S. Papaspyrou (nickie@softlab.ntua.gr)
 *  Date        : May 14, 2003
 *  Description : Generic symbol table in C
 *
 *  Comments: (in Greek iso-8859-7)
 *  ---------
 *  ������ �������� �����������.
 *  ����� ������������ ��������� ��� ��������� �����������.
 *  ������ ����������� ������������ ��� �����������.
 *  ���������� ����������� ����������
 */

#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include "error.h"
#include "general.h"
#include "symbol.h"

/* --- Symbol table's global variables. --- */
 
Scope        *currentScope;           /* Current Scope          */
unsigned int  quadNext;               /* Next quad number       */
unsigned int  quadOffset;             /* Quad generated offset  */
unsigned int  tempNumber;             /* Next temporary number  */

static unsigned int  hashTableSize;   /* Hash table size        */
static SymbolEntry **hashTable;       /* Hash table             */

static struct Type_tag typeConst [] = {
    { TYPE_VOID,    NULL, 0 },
    { TYPE_INTEGER, NULL, 0 },
    { TYPE_BOOLEAN, NULL, 0 },
    { TYPE_CHAR,    NULL, 0 },
    { TYPE_ANY,     NULL, 0 }
};

const Type typeVoid      = &(typeConst[0]);
const Type typeInteger   = &(typeConst[1]);
const Type typeBoolean   = &(typeConst[2]);
const Type typeChar      = &(typeConst[3]);
const Type typeAny       = &(typeConst[4]);

/* --- Implemantation of symbol table helper functions. --- */

typedef unsigned long int HashType;

static HashType PJW_hash(const char *key) {
    /*
     *  P.J. Weinberger's hashing function. See also:
     *  Aho A.V., Sethi R. & Ullman J.D, "Compilers: Principles,
     *  Techniques and Tools", Addison Wesley, 1986, pp. 433-437.
     */

    const HashType PJW_OVERFLOW = (((HashType) 0xf) << (8 * sizeof(HashType) - 4));
    const int PJW_SHIFT = (8 * (sizeof(HashType) - 1));
    
    HashType h, g;
    
    for (h = 0; *key != '\0'; key++) {
        h = (h << 4) + (*key);
        
        if ((g = h & PJW_OVERFLOW) != 0) {
            h ^= g >> PJW_SHIFT;
            h ^= g;
        }
    }
    
    return h;
}

/* --- Implemantation of symbol table utilities functions. --- */

/* Initialize the symbol table. Size should be a prime number. */
void initSymbolTable(unsigned int size) {
    unsigned int i;
    
    currentScope = NULL;
    quadNext     = 0;
    quadOffset   = 0;
    tempNumber   = 1;
    
    hashTableSize = size;
    hashTable = (SymbolEntry **) new(size * sizeof(SymbolEntry *));
    
    for (i = 0; i < size; i++)
        hashTable[i] = NULL;
}

/* Free each element of the symbol table and then the symbol table. */
void destroySymbolTable() {
    unsigned int i;
    
    for (i = 0; i < hashTableSize; i++)
        if (hashTable[i] != NULL)
            destroyEntry(hashTable[i]);

    delete(hashTable);
}

/*
 *  Open a new scope. Set the current scope as new's parent, increment the nesting level
 *  and set the new scope as current. Each scope represents a function and we set the 
 *  returnType of the scope as the return type of the current function.
 */
void openScope(Type t) {
    Scope *newScope = (Scope *) new(sizeof(Scope));

    newScope->negOffset  = START_NEGATIVE_OFFSET;
    newScope->parent     = currentScope;
    newScope->entries    = NULL;
    newScope->returnType = t; // TODO prepei na to aukshsw kata ena?

    if (currentScope == NULL)
        newScope->nestingLevel = 1;
    else
        newScope->nestingLevel = currentScope->nestingLevel + 1;
    
    currentScope = newScope;
}

/*
 *  Destroy the current scope, free all the entries in this scope
 *  and also rearange the entries in the symbol table's list.
 */
void closeScope() {
    SymbolEntry *e = currentScope->entries;
    Scope       *t = currentScope;
    
    while (e != NULL) {
        SymbolEntry *next = e->nextInScope;
        
        hashTable[e->hashValue] = e->nextHash;
        destroyEntry(e);
        e = next;
    }
    
    currentScope = currentScope->parent;
    delete(t);
}

/*
 *  Insert a new entry in the symbol table at the beginning of the
 *  list and also at the beginning of the scope entries.
 */
static void insertEntry(SymbolEntry *e) {
    e->nextHash             = hashTable[e->hashValue];
    hashTable[e->hashValue] = e;
    e->nextInScope          = currentScope->entries;
    currentScope->entries   = e;
}

/* 
 *  Create and initialize a new symbol entry except of type and u. First we check
 *  if identifier already exists in this scope and then we initialize.
 */
static SymbolEntry *newEntry(const char *name) {
    SymbolEntry *e;
        
    for (e = currentScope->entries; e != NULL; e = e->nextInScope)
        if (strcmp(name, e->id) == 0) {
            error("Duplicate identifier: %s", name);
            return NULL;
        }
        
    e = (SymbolEntry *) new(sizeof(SymbolEntry));
    e->id = (const char *) new(strlen(name) + 1);

    strcpy((char *) (e->id), name);
    e->hashValue    = PJW_hash(name) % hashTableSize;
    e->nestingLevel = currentScope->nestingLevel;
    insertEntry(e);
    
    return e;
}

/*
 *  Create a new variable and set the type, increment the reference
 *  count, decrement the current negative offset by the variable's
 *  size, set the variable's offset as the scope's current neg offset.
 */
SymbolEntry *newVariable(const char *name, Type type) {
    SymbolEntry *e = newEntry(name);
    
    if (e != NULL) {
        e->entryType = ENTRY_VARIABLE;
        e->u.eVariable.type = type;
        type->refCount++;
        currentScope->negOffset -= sizeOfType(type);
        e->u.eVariable.offset = currentScope->negOffset;
    }
    
    return e;
}

/*
 *  We lookup the name of the new function that we want to create. If it
 *  doesn't exist we create a new entry and set the pardef as DEFINE in order
 *  to indicate that we are defining the function. If it exists and is a forward
 *  declared function, we set the isForward flag to false and we set the pardef
 *  as PARDEF_CHECK to indicate that we compare the declaration with the prototype.
 *  At last, if it already exists and it isn't a function we print am error message.
 */
SymbolEntry *newFunction(const char *name) {
    SymbolEntry *e = lookupEntry(name, LOOKUP_CURRENT_SCOPE, false);

    if (e == NULL) {
        e = newEntry(name);
        
        if (e != NULL) {
            e->entryType = ENTRY_FUNCTION;
            e->u.eFunction.isForward = false;
            e->u.eFunction.pardef = PARDEF_DEFINE;
            e->u.eFunction.firstArgument = e->u.eFunction.lastArgument = NULL;
            e->u.eFunction.resultType = NULL;
        }
        
        return e;
    }
    else if (e->entryType == ENTRY_FUNCTION && e->u.eFunction.isForward) {
        e->u.eFunction.isForward = false;
        e->u.eFunction.pardef = PARDEF_CHECK;
        e->u.eFunction.lastArgument = NULL;
        
        return e;
    }
    else {
       error("Duplicate identifier: %s", name);
       return NULL;
    }
}

/*
 *  We add a new parameter to the function. If f isn't a function there's an internal error.
 *  
 */
SymbolEntry *newParameter(const char *name, Type type, PassMode mode, SymbolEntry *f) {
    SymbolEntry *e;
    
    if (f->entryType != ENTRY_FUNCTION)
        internal("Cannot add a parameter to a non-function");
        
    switch (f->u.eFunction.pardef) {
        case PARDEF_DEFINE:
            e = newEntry(name);
            
            if (e != NULL) {
                e->entryType = ENTRY_PARAMETER;
                e->u.eParameter.type = type;
                type->refCount++;
                e->u.eParameter.mode = mode;
                e->u.eParameter.next = NULL;
            }
            
            if (f->u.eFunction.lastArgument == NULL)
                f->u.eFunction.firstArgument = f->u.eFunction.lastArgument = e;
            else {
                f->u.eFunction.lastArgument->u.eParameter.next = e;
                f->u.eFunction.lastArgument = e;
            }
            
            return e;            
        case PARDEF_CHECK:
            e = f->u.eFunction.lastArgument;
            
            if (e == NULL)
                e = f->u.eFunction.firstArgument;
            else
                e = e->u.eParameter.next;
            
            if (e == NULL)
                error("More parameters than expected in redeclaration of function %s", f->id);
            else if (!equalType(e->u.eParameter.type, type))
                error("Parameter type mismatch in redeclaration of function %s", f->id);
            else if (e->u.eParameter.mode != mode)
                error("Parameter passing mode mismatch in redeclaration of function %s", f->id);
            else if (strcmp(e->id, name) != 0)
                error("Parameter name mismatch in redeclaration of function %s", f->id);
            else
                insertEntry(e);
                
            f->u.eFunction.lastArgument = e;
            return e;
        case PARDEF_COMPLETE:
            fatal("Cannot add a parameter to an already defined function");
    }
    
    return NULL;
}

static unsigned int fixOffset(SymbolEntry *args) {
    if (args == NULL)
        return 0;
    else {
        unsigned int rest = fixOffset(args->u.eParameter.next);
        
        args->u.eParameter.offset = START_POSITIVE_OFFSET + rest;
        if (args->u.eParameter.mode == PASS_BY_REFERENCE)
            return rest + 2;
        else
            return rest + sizeOfType(args->u.eParameter.type);
    }
}

/* If the argument is a function, we declare it as forward. */
void forwardFunction(SymbolEntry *f) {
    if (f->entryType != ENTRY_FUNCTION)
        internal("Cannot make a non-function forward");
        
    f->u.eFunction.isForward = true;
}

void endFunctionHeader(SymbolEntry *f, Type type) {
   
    if (f->entryType != ENTRY_FUNCTION)
        internal("Cannot end parameters in a non-function");
    
    switch (f->u.eFunction.pardef) {
        case PARDEF_COMPLETE:
            internal("Cannot end parameters in an already defined function");
            break;
        case PARDEF_DEFINE:
            fixOffset(f->u.eFunction.firstArgument);
            f->u.eFunction.resultType = type;
            type->refCount++;
            break;
        case PARDEF_CHECK:
            if ((f->u.eFunction.lastArgument != NULL &&
                 f->u.eFunction.lastArgument->u.eParameter.next != NULL) ||
                (f->u.eFunction.lastArgument == NULL &&
                 f->u.eFunction.firstArgument != NULL))
                error("Fewer parameters than expected in redeclaration of function %s", f->id);
            
            if (!equalType(f->u.eFunction.resultType, type))
                error("Result type mismatch in redeclaration of function %s", f->id);
            
            break;
    }
    
    f->u.eFunction.pardef = PARDEF_COMPLETE;
}

SymbolEntry *newTemporary(Type type) {
    char buffer[10];
    SymbolEntry *e;

    sprintf(buffer, "$%d", tempNumber);
    e = newEntry(buffer);
    
    if (e != NULL) {
        e->entryType = ENTRY_TEMPORARY;
        e->u.eVariable.type = type;
        type->refCount++;
        currentScope->negOffset -= sizeOfType(type);
        e->u.eTemporary.offset = currentScope->negOffset;
        e->u.eTemporary.number = tempNumber++;
    }
    
    return e;
}

void destroyEntry(SymbolEntry *e) {
    SymbolEntry *args;
    
    switch (e->entryType) {
        case ENTRY_VARIABLE:
            destroyType(e->u.eVariable.type);
            break;
        case ENTRY_FUNCTION:
            args = e->u.eFunction.firstArgument;
            
            while (args != NULL) {
                SymbolEntry *p = args;
                
                destroyType(args->u.eParameter.type);
                delete((char *) (args->id));
                args = args->u.eParameter.next;
                delete(p);
            }
            
            destroyType(e->u.eFunction.resultType);
            break;
        case ENTRY_CONSTANT:
            /* We do not keep constants in the symbol table. */
        case ENTRY_PARAMETER:
            /* Parameters are destroyed with the function. */
            return;
        case ENTRY_TEMPORARY:
            destroyType(e->u.eTemporary.type);
            break;
    }
    
    delete((char *) (e->id));
    delete(e);        
}

/*
 *  We look up an entry in the symbol table. There are two types of searching one is
 *  to LOOKUP_ALL_SCOPES and the other is to LOOKUP_CURRENT_SCOPE. If err is true then
 *  we also print an error message if we can't find the particular name.
 */
SymbolEntry *lookupEntry(const char *name, LookupType type, bool err) {
    unsigned int hashValue = PJW_hash(name) % hashTableSize;
    SymbolEntry *e         = hashTable[hashValue];
    
    switch (type) {
        case LOOKUP_CURRENT_SCOPE:
            while (e != NULL && e->nestingLevel == currentScope->nestingLevel)
                if (strcmp(e->id, name) == 0)
                    return e;
                else
                    e = e->nextHash;
                    
            break;
        case LOOKUP_ALL_SCOPES:
            while (e != NULL)
                if (strcmp(e->id, name) == 0)
                    return e;
                else
                    e = e->nextHash;
            
            break;
    }
    
    if (err)
        error("Unknown identifier: %s", name);
        
    return NULL;
}

Type typeIArray(Type refType) {
    Type n = (Type) new(sizeof(struct Type_tag));

    n->kind     = TYPE_IARRAY;
    n->refType  = refType;
    n->refCount = 1;
    refType->refCount++;

    return n;
}

Type typeList(Type refType) {
    Type n = (Type) new(sizeof(struct Type_tag));

    n->kind     = TYPE_LIST;
    n->refType  = refType;
    n->refCount = 1;
    refType->refCount++;

    return n;
}

Type typePointer(Type refType) {
    Type n = (Type) new(sizeof(struct Type_tag));

    n->kind     = TYPE_POINTER;
    n->refType  = refType;
    n->refCount = 1;
    refType->refCount++;

    return n;
}

/*  */
void destroyType(Type type) {

    switch (type->kind) {
        case TYPE_IARRAY:
        case TYPE_LIST:
        case TYPE_POINTER:
            if (--(type->refCount) == 0) {
                destroyType(type->refType);
                delete(type);
            }
            break;
        case TYPE_INTEGER:
        case TYPE_BOOLEAN:
        case TYPE_CHAR:
        case TYPE_VOID:
            break;
    }
}

/* 
 *  Returns the size of the type. If it is an array we multiply the array's
 *  size with the size of the reference type of the array.
 */
unsigned int sizeOfType(Type type) {

    switch (type->kind) {
        case TYPE_VOID:
            internal("Type void has no size");
            break;
        case TYPE_INTEGER:
        case TYPE_IARRAY:
        case TYPE_LIST:
        case TYPE_POINTER:
            return 2;
        case TYPE_BOOLEAN:
        case TYPE_CHAR:
            return 1;
        case TYPE_ANY:
            return 0;
    }
    
    return 0;
}

/* 
 *  Check if two types are of the same kind. If they are arrays then check
 *  if their sizes are the same. Also, check if their refType is the same.
 */
bool equalType(Type type1, Type type2) {

    if((type1->kind == TYPE_ANY && type2->kind != TYPE_VOID) || (type2->kind == TYPE_ANY && type1->kind != TYPE_VOID))
        return true;

    if (type1->kind != type2->kind)
        return false;
        
    switch (type1->kind) {
        case TYPE_IARRAY:
        case TYPE_LIST:
        case TYPE_POINTER:
            return equalType(type1->refType, type2->refType);
        case TYPE_INTEGER:
        case TYPE_BOOLEAN:
        case TYPE_CHAR:
        case TYPE_VOID:
            break;
    }
    
    return true;        
}

/* --- Helper functions to print symbol table. --- */
 
/* Gets a Type and prints its type in human readable form. */ 
void printType(Type type) {

    if (type == NULL) {
        printf("<undefined>");
        return;
    }
    
    switch (type->kind) {
        case TYPE_VOID:
            printf("void");
            break;
        case TYPE_INTEGER:
            printf("integer");
            break;
        case TYPE_BOOLEAN:
            printf("boolean");
            break;
        case TYPE_CHAR:
            printf("char");
            break;
        case TYPE_IARRAY:
            printf("array of ");
            printType(type->refType);
            break;
        case TYPE_LIST:
            printf("list of ");
            printType(type->refType);
            break;    
        case TYPE_POINTER:
            printf("^");
            printType(type->refType);
            break;
    }
}

/* Prints "var" if variable is passed to function by reference. */
void printMode(PassMode mode) {

    if (mode == PASS_BY_REFERENCE)
        printf("var ");
}

/*
 *  Prints the symbol table, scope by scope. For each scope it prints
 *  the defined temporaries, variables and functions with their parameters.
 */
#define SHOW_OFFSETS

void printSymbolTable() {
    Scope       * scp;
    SymbolEntry * e;
    SymbolEntry * args;
    
    scp = currentScope;
    if (scp == NULL)
        printf("no scope\n");
    else
        while (scp != NULL) {
            printf("scope: ");
            e = scp->entries;
            
            while (e != NULL) {
                if (e->entryType == ENTRY_TEMPORARY)
                    printf("$%d", e->u.eTemporary.number);
                else
                    printf("%s", e->id);
                    
                switch (e->entryType) {
                    case ENTRY_FUNCTION:
                        printf("(");
                        args = e->u.eFunction.firstArgument;
                        
                        while (args != NULL) {
                            printMode(args->u.eParameter.mode);
                            printf("%s : ", args->id);
                            printType(args->u.eParameter.type);
                            args = args->u.eParameter.next;
                            
                            if (args != NULL)
                                printf("; ");
                        }
                        
                        printf(") : ");
                        printType(e->u.eFunction.resultType);
                        break;
                    case ENTRY_CONSTANT:
                        /* We do not keep constants in the symbol table. */
                        break;
#ifdef SHOW_OFFSETS
                    case ENTRY_VARIABLE:
                        printf("[%d]", e->u.eVariable.offset);
                        break;
                    case ENTRY_PARAMETER:
                        printf("[%d]", e->u.eParameter.offset);
                        break;
                    case ENTRY_TEMPORARY:
                        printf("[%d]", e->u.eTemporary.offset);
                        break;
#endif
                }
                
                e = e->nextInScope;
                if (e != NULL)
                    printf(", ");
            }
            
            scp = scp->parent;
            printf("\n");
        }
        
    printf("----------------------------------------\n");
}
