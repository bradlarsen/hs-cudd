#ifndef CUDD_WRAPPERS_H
#define CUDD_WRAPPERS_H

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include "cudd.h"

/* We use a reference-counting layer on top of DdNode and DdManager values,
   to avoid problems with Haskell finalizer order.
 */ 
typedef struct Mgr Mgr;
Mgr * cw_init ();
void cw_quit (Mgr *mgr);

typedef struct Bdd Bdd;
void cw_bdd_destroy (Bdd *bdd);

Mgr * cw_bdd_get_manager (Bdd *b);

Cudd_ErrorType cw_read_error_code (Mgr *mgr);
void cw_clear_error_code (Mgr *mgr);

Bdd * cw_read_one (Mgr *mgr);
Bdd * cw_read_logic_zero (Mgr *mgr);
Bdd * cw_bdd_ith_var (Mgr *mgr, unsigned i);

int cw_bdd_is_one (Bdd *b);
int cw_bdd_is_logic_zero (Bdd *b);

int cw_bdd_equal (Bdd *b1, Bdd *b2);

Bdd * cw_bdd_not (Bdd *bdd);
Bdd * cw_bdd_ite (Bdd *b1, Bdd *b2, Bdd *b3);

Bdd * cw_bdd_and (Bdd *b1, Bdd *b2);
Bdd * cw_bdd_or (Bdd *b1, Bdd *b2);
Bdd * cw_bdd_xor (Bdd *b1, Bdd *b2);
Bdd * cw_bdd_nand (Bdd *b1, Bdd *b2);
Bdd * cw_bdd_nor (Bdd *b1, Bdd *b2);
Bdd * cw_bdd_xnor (Bdd *b1, Bdd *b2);
Bdd * cw_bdd_compose (Bdd *b1, Bdd *b2, unsigned v);
Bdd * cw_bdd_restrict (Bdd *b1, Bdd *b2);

unsigned cw_num_bdd_vars (Mgr *mgr);
unsigned cw_num_nodes (Mgr *mgr);
unsigned cw_bdd_size (Bdd *b);

#endif /* CUDD_WRAPPERS_H */
