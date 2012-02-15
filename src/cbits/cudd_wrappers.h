#ifndef CUDD_WRAPPERS_H
#define CUDD_WRAPPERS_H

#include <stdio.h>
#include <cudd.h>

DdNode *
Cudd_Not_Wrapper (DdNode *node);

void
Cudd_Quit_Wrapper (DdManager *mgr);

void
Cudd_RecursiveDeref_Wrapper (DdManager *mgr, DdNode *node);

#endif /* CUDD_WRAPPERS_H */
