#include "cudd_wrappers.h"

static void mgr_deref (Mgr *mgr);
static Bdd * bdd_create (Mgr *manager, DdNode *node);


struct Mgr
{
    DdManager *manager;
    unsigned ref_count;
};

#define mgr_good(m) ((m) && (m)->manager && (m)->ref_count > 0)
#define mgr_ddmanager(m) ((m)->manager)

Mgr * cw_init ()
{
    DdManager *ddmgr = Cudd_Init (0, 0, CUDD_UNIQUE_SLOTS, CUDD_CACHE_SLOTS, 0);
    assert (ddmgr);
    Mgr *mgr = (Mgr *) malloc (sizeof (Mgr));
    assert (mgr);
    mgr->manager = ddmgr;
    mgr->ref_count = 1;
    assert (mgr_good (mgr));
    return mgr;
}

static void mgr_deref (Mgr *mgr)
{
    assert (mgr);
    assert (mgr->ref_count > 0);
    mgr->ref_count -= 1;
    if (mgr->ref_count == 0) {
        Cudd_Quit (mgr->manager);
        free (mgr);
    }
}

void cw_quit (Mgr *mgr)
{
    mgr_deref (mgr);
}

DdManager * cw_mgr_ddmanager (Mgr *mgr)
{
    assert (mgr_good (mgr));
    return mgr->manager;
}


struct Bdd
{
    Mgr *manager;
    DdNode *node;
};

#define bdd_good(b) ((b) && mgr_good ((b)->manager) && (b)->node)
#define bdd_ddmanager(b) ((b)->manager->manager)
#define bdd_ddnode(b) ((b)->node)
#define bdd_mgr(b) ((b)->manager)
#define bdd_same_mgr(b1, b2) ((b1)->manager == (b2)->manager)

static Bdd * bdd_create (Mgr *mgr, DdNode *node)
{
    assert (mgr_good (mgr));
    if (!node) return NULL;
    Bdd *bdd = (Bdd *) malloc (sizeof (Bdd));
    assert (bdd);
    bdd->manager = mgr;
    mgr->ref_count += 1;
    bdd->node = node;
    Cudd_Ref (bdd_ddnode (bdd));
    assert (bdd_good (bdd));
    return bdd;
}

void cw_bdd_destroy (Bdd *bdd)
{
    assert (bdd_good (bdd));
    Cudd_RecursiveDeref (bdd_ddmanager (bdd), bdd_ddnode (bdd));
    mgr_deref (bdd_mgr (bdd));
    free (bdd);
}

DdNode * cw_bdd_ddnode (Bdd *bdd)
{
    assert (bdd_good (bdd));
    return bdd_ddnode (bdd);
}

Mgr * cw_bdd_mgr (Bdd *b)
{
    assert (bdd_good (b));
    return bdd_mgr (b);
}

DdManager * cw_bdd_ddmanager (Bdd *b)
{
    assert (bdd_good (b));
    return mgr_ddmanager (bdd_mgr (b));
}

Bdd * cw_read_one (Mgr *mgr)
{
    assert (mgr_good (mgr));
    return bdd_create (mgr, Cudd_ReadOne (mgr_ddmanager (mgr)));
}

Bdd * cw_read_logic_zero (Mgr *mgr)
{
    assert (mgr_good (mgr));
    return bdd_create (mgr, Cudd_ReadLogicZero (mgr_ddmanager (mgr)));
}

Bdd * cw_bdd_ith_var (Mgr *mgr, unsigned i)
{
    assert (mgr_good (mgr));
    return bdd_create (mgr, Cudd_bddIthVar (mgr_ddmanager (mgr), i));
}

Bdd * cw_bdd_not (Bdd *bdd)
{
    assert (bdd_good (bdd));
    return bdd_create (bdd_mgr (bdd), Cudd_Not (bdd_ddnode (bdd)));
}

Bdd * cw_bdd_ite (Bdd *b1, Bdd *b2, Bdd *b3)
{
    assert (bdd_good (b1));
    assert (bdd_good (b2));
    assert (bdd_good (b3));
    assert (bdd_same_mgr (b1, b2));
    assert (bdd_same_mgr (b2, b3));
    return bdd_create (bdd_mgr (b1),
                       Cudd_bddIte (bdd_ddmanager (b1), bdd_ddnode (b1),
                                    bdd_ddnode (b2), bdd_ddnode (b3)));
}

#define bdd_binop(name, cuddOp) \
  Bdd * name (Bdd *b1, Bdd *b2) \
  { \
    assert (bdd_good (b1)); \
    assert (bdd_good (b2)); \
    assert (bdd_same_mgr (b1, b2)); \
    return bdd_create (bdd_mgr (b1), cuddOp (bdd_ddmanager (b1), \
                                             bdd_ddnode (b1), \
                                             bdd_ddnode (b2))); \
  }

bdd_binop (cw_bdd_and, Cudd_bddAnd)
bdd_binop (cw_bdd_or, Cudd_bddOr)
bdd_binop (cw_bdd_xor, Cudd_bddXor)
bdd_binop (cw_bdd_nand, Cudd_bddNand)
bdd_binop (cw_bdd_nor, Cudd_bddNor)
bdd_binop (cw_bdd_xnor, Cudd_bddXnor)
bdd_binop (cw_bdd_restrict, Cudd_bddRestrict)
bdd_binop (cw_bdd_exist_abstract, Cudd_bddExistAbstract)
bdd_binop (cw_bdd_univ_abstract, Cudd_bddUnivAbstract)

Bdd * cw_bdd_compose (Bdd *b1, Bdd *b2, unsigned v)
{
    assert (bdd_good (b1));
    assert (bdd_good (b2));
    return bdd_create (bdd_mgr (b1),
                       Cudd_bddCompose (bdd_ddmanager (b1), bdd_ddnode (b1),
                                        bdd_ddnode (b2), v));
}


unsigned cw_mgr_nodes_at_level (Mgr *mgr, unsigned level)
{
    assert (mgr_good (mgr));
    assert (level < mgr_ddmanager (mgr)->size);

    DdManager *ddmgr = mgr_ddmanager (mgr);
    DdSubtable subtable = ddmgr->subtables[level];
    assert (subtable.keys >= subtable.dead);
    unsigned count = subtable.keys - subtable.dead;
    if (ddmgr->vars[level]->ref == 1) {
        assert (count > 0);
        count -= 1;
    }
    return count;
}