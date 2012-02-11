#include "cudd_wrappers.h"

DdNode *
Cudd_Not_Wrapper (DdNode *node)
{
    return Cudd_Not (node);
}
