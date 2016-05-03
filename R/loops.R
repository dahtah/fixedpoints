run.fp <- function(f,x0,...,.convergence=function(a,b) isTRUE(all.equal(a,b)),.maxIter=1e3,.tol=.Machine$double.eps,.viewer,.store,.viewer_every=1,.store_every=1,.throw=TRUE,.return_stored=FALSE)
{
    tst <- .convergence(x0,x0)
    if (is.numeric(tst))
    {
        if (length(tst)==1)
        {
            bool.conv <- FALSE
        }
        else
        {
            error('Please provide a positive, one-dimensional convergence criterion')
        }
    }
    else if (is.logical(tst))
    {
        bool.conv <- TRUE
    }
    else
    {
        error('Please provide a one-dimensional (logical or numerical) convergence criterion')
    }

    viewer <- !missing(.viewer)
    if (missing(.store))
    {
        store <- FALSE
    }
    else
    {
        store <- TRUE
        stored <- c()
    }
    
    converged <- FALSE
    indIter <- 1
    x <- x0
    if (store) iter <- list()
    f <- safely(f)
    
    while (!converged & indIter <= .maxIter)
    {
        res <- f(x,...)
        if (!is.null(res$error))
        {
            msg <- "Caught error at iteration %i: %s"
            msg <- sprintf(msg,indIter,as.character(res$error))
            if (.throw)
            {
                stop(msg)
            }
            else
            {
                warning(msg)
                break
            }
        }
        else
        {
            nx <- res$result
            d <- .convergence(x,nx)
            if (bool.conv)
            {
                converged <- d
            }
            else
            {
                converged <- d < .tol
            }
            x <- nx
        }
        if (viewer ) runViewers(.viewer,.viewer_every,x,indIter)
        if (store)
        {
            out <- runStorage(.store,.store_every,x,indIter)
            if (!is.null(out)) stored[[length(stored)+1]] <- out
        }
        indIter <- indIter+1
    }
    class(x) <- c("iterOutput",class(x))
    attr(x,"nIter") <- indIter-1
    attr(x,"converged") <- converged
    if (store)
    {
        if (.return_stored)
        {
            stored
        }
        else
        {
            attr(x,"stored") <- stored
            x
        }
    }
    else
    {
        x
    }

}

run.iter <- function(f,x0,...,nIter=0,.viewer,.store,.viewer_every=1,.store_every=1,.return_stored=FALSE,.throw=FALSE)
{
    iter <- 0
    x <- x0
    viewer <- !missing(.viewer)
    if (missing(.store))
    {
        store <- FALSE
    }
    else
    {
        store <- TRUE
        stored <- c()
    }
    f <- safely(f)

    if (nIter > 0)
    {
        for (indIter in 1:nIter)
        {
            res <- f(x,...)
            if (!is.null(res$error))
            {
                msg <- "Caught error at iteration %i: %s"
                msg <- sprintf(msg,indIter,as.character(res$error))
                if (.throw)
                {
                    stop(msg)
                }
                else
                {
                    warning(msg)
                    break
                }
            }
            else {
                x <- res$result
                if (viewer ) runViewers(.viewer,.viewer_every,x,indIter)
                if (store)
                {
                    out <- runStorage(.store,.store_every,x,indIter)
                    if (!is.null(out)) stored[[length(stored)+1]] <- out
                }
            }
        }
    }
    else {
        indIter <- 0
    }
    class(x) <- c("iterOutput",class(x))
    attr(x,"nIter") <- indIter
    if (store)
    {
        if (.return_stored)
        {
            stored
        }
        else
        {
            attr(x,"stored") <- stored
            x
        }
    }
    else
    {
        x
    }
}
