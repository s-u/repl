.save.condition <- function(cond)
  .Call(repl_set_last_condition, cond)

repl.eval <- function(what, where=.GlobalEnv, last.value=FALSE, exp.value=FALSE,
	    context=NULL, handlers=list(error=.save.condition))
    .Call(repl_eval, what, where, last.value, exp.value, context, handlers)

repl.context <- function(what)
    if (missing(what)) .Call(repl_get_context) else .Call(repl_set_context, what)
