#include <Rversion.h>
#include <Rinternals.h>
#include <R_ext/Parse.h>

extern Rboolean R_Visible;

/* this is really convoluted - we want to be guaranteed to not leave the call
   on one hand, but on the other hand R_ToplevelExec() removes the context
   which also removes the traceback. So the trick is to use R_ExecWithCleanup()
   to add another layer where we stash the traceback before R_ToplevelExec()
   blows it away. It would be really just one extra line in R sources, but
   what can you do ...

   R_ToplevelExec(Rserve_eval_) -> R_ExecWithCleanup(Rserve_eval_do -> eval)

 */

typedef struct rs_eval {
    SEXP what, rho, ctx_obj, last, traceback, handlers;
    int exp;
} rs_eval_t;

static SEXP last_condition;

SEXP repl_set_last_condition(SEXP sCond) {
    if (last_condition && last_condition != R_NilValue)
	R_ReleaseObject(last_condition);
    if (!sCond || sCond == R_NilValue)
	last_condition = 0;
    else {
	last_condition = sCond;
	R_PreserveObject(last_condition);
    }
    return R_NilValue;
}

static SEXP repl_eval_do(void *arg) {
    rs_eval_t *e = (rs_eval_t*) arg;
    SEXP what = e->what, rho = e->rho, x;
    int i, n;

    /* add calling handlers to catch error conditions so we can report them - see #154 */
    if (e->handlers) {
	SEXP sInternal = Rf_install(".Internal");
	SEXP addCondHands = Rf_install(".addCondHands");
	SEXP ach = PROTECT(lang2(sInternal,
			    lang6(addCondHands,
				  Rf_getAttrib(e->handlers, R_NamesSymbol),
				  e->handlers,
				  rho, R_NilValue, PROTECT(Rf_ScalarLogical(1)))));
	eval(ach, rho);
	UNPROTECT(2);
    }

    if (TYPEOF(what) == EXPRSXP) {
        n = LENGTH(what);
        for (i = 0; i < n; i++) {
            e->exp = i;
            x = eval(VECTOR_ELT(what, i), rho);
            if (i == n - 1) {
                R_PreserveObject(x);
                e->last = x;
            }
            if (R_Visible)
                PrintValue(x);
        }
    } else {
        e->exp = -1;
        x = eval(what, rho);
        R_PreserveObject(x);
        /* intentionally we don't print if it is not an expression vector */
        e->last = x;
    }
    return R_NilValue;
}

/* it's really stupid becasue R has R_GetTraceback() but we have to
   jump through eval() just because it's hidden so we can't access it ... */
static SEXP R_GetTraceback(int skip) {
    SEXP d_int = Rf_install(".Internal"),
	 tb = Rf_install("traceback"),
	 sSkip = PROTECT(ScalarInteger(skip));
    SEXP what = PROTECT(lang2(d_int, lang2(tb, sSkip)));
    SEXP res = eval(what, R_GlobalEnv);
    UNPROTECT(2);
    return res;
}

static void repl_eval_cleanup(void *arg) {
    rs_eval_t *e = (rs_eval_t*) arg;
    SEXP tb = R_GetTraceback(0);
    if (tb && tb != R_NilValue)
        R_PreserveObject((e->traceback = tb));
}

static void repl_eval_(void *arg) {
    R_ExecWithCleanup(repl_eval_do, arg, repl_eval_cleanup, arg);
}

static SEXP RS_current_context;
static int  RS_current_context_is_protected;

SEXP repl_get_context() {
    return RS_current_context ? RS_current_context : R_NilValue;
}

SEXP repl_set_context(SEXP sObj) {
    if (!sObj)
        sObj = R_NilValue;
    if (RS_current_context == sObj) return sObj;
    if (RS_current_context != R_NilValue && RS_current_context_is_protected)
        R_ReleaseObject(RS_current_context);
    RS_current_context = sObj;
    RS_current_context_is_protected = 0;
    if (RS_current_context != R_NilValue) {
        R_PreserveObject(RS_current_context);
        RS_current_context_is_protected = 1;
    }
    return RS_current_context;
}

SEXP repl_eval(SEXP what, SEXP rho, SEXP retLast, SEXP retExp, SEXP ctxObj, SEXP sHandlers) {
    int need_last = asInteger(retLast), exp_value = asInteger(retExp);
    rs_eval_t e = { what, rho, 0, 0, 0, 0, 0 };
    SEXP saved_context = RS_current_context;
    int  saved_context_is_protected = RS_current_context_is_protected;
    if (ctxObj != R_NilValue) {
        RS_current_context = ctxObj; /* this is transient so no protection */
        RS_current_context_is_protected = 0;
    }
    e.ctx_obj = RS_current_context;
    if (sHandlers != R_NilValue)
	e.handlers = sHandlers;
    repl_set_last_condition(0);
    if (!R_ToplevelExec(repl_eval_, &e)) {
        RS_current_context = saved_context;
        RS_current_context_is_protected = saved_context_is_protected;
        SEXP res = PROTECT(mkNamed(VECSXP, (const char*[]) { "error", "traceback", "expression", "context", "condition", "" }));
        SET_VECTOR_ELT(res, 1, e.traceback ? e.traceback : R_NilValue);
        const char *errmsg = R_curErrorBuf();
        SET_VECTOR_ELT(res, 0, errmsg ? mkString(errmsg) : R_NilValue);
        if (exp_value)
            SET_VECTOR_ELT(res, 2, (e.exp == -1) ? what : VECTOR_ELT(what, e.exp));
        else
            SET_VECTOR_ELT(res, 2, ScalarInteger(e.exp < 0 ? NA_INTEGER : (e.exp + 1)));
        SET_VECTOR_ELT(res, 3, e.ctx_obj ? e.ctx_obj : R_NilValue);
	SET_VECTOR_ELT(res, 4, last_condition ? last_condition : R_NilValue);
        setAttrib(res, R_ClassSymbol, mkString("repl-eval-error"));
        UNPROTECT(1);
        return res;
    }
    RS_current_context = saved_context;
    RS_current_context_is_protected = saved_context_is_protected;

    if (need_last) {
        if (e.last) {
            R_ReleaseObject(e.last);
            return e.last;
        }
        return R_NilValue;
    }
    return ScalarLogical(TRUE);
}

/* registration */

static const R_CallMethodDef CallAPI[] = {
    {"repl_eval",               (DL_FUNC) &repl_eval, 5},
    {"repl_get_context",        (DL_FUNC) &repl_get_context, 0},
    {"repl_set_context",        (DL_FUNC) &repl_set_context, 1},
    {"repl_set_last_condition", (DL_FUNC) &repl_set_last_condition, 1},
    {NULL, NULL, 0}
};

void R_init_repl(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallAPI, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
