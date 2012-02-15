#include "cudd_wrappers.h"

DdNode *
Cudd_Not_Wrapper (DdNode *node)
{
    return Cudd_Not (node);
}

void
Cudd_Quit_Wrapper (DdManager *mgr)
{
//    fprintf (stderr, "!!! Cudd_Quit_Wrapper (%p)\n", (void *) mgr);
    Cudd_Quit (mgr);
}

void
Cudd_RecursiveDeref_Wrapper (DdManager *mgr, DdNode *node)
{
//    fprintf (stderr, "!!! Cudd_RecursiveDeref_Wrapper (%p, %p)\n",
//             (void *) mgr, (void *) node);
    Cudd_RecursiveDeref (mgr, node);
}
