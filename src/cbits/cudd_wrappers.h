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

DdManager * cw_mgr_ddmanager (Mgr *mgr);

typedef struct Bdd Bdd;
void cw_bdd_destroy (Bdd *bdd);

DdNode * cw_bdd_ddnode (Bdd *bdd);
Mgr * cw_bdd_mgr (Bdd *b);
DdManager * cw_bdd_ddmanager (Bdd *b);

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
Bdd * cw_bdd_exist_abstract (Bdd *b1, Bdd *b2);
Bdd * cw_bdd_univ_abstract (Bdd *b1, Bdd *b2);

double cw_bdd_count_minterm (Bdd *b);

unsigned cw_bdd_size (Bdd *b);

int cw_bdd_pick_one_cube (Bdd *b, char *varsOut);

#endif /* CUDD_WRAPPERS_H */
