
##' @export
print.iterOutput <- function(x)
{
    
    if (!is.null(attr(x,"converged")))
    {
        msg <- "Result after %i iterations. Converged:  %s.\n"
        sprintf(msg,attr(x,"nIter"),as.character(attr(x,"converged"))) %>% cat
    }
    else
    {
        msg <- "Result after %i iterations \n"
        sprintf(msg,attr(x,"nIter")) %>% cat
    }
    attr(x,"nIter") <- NULL
    attr(x,"stored") <- NULL
    attr(x,"converged") <- NULL
    class(x) <- setdiff(class(x),"iterOutput")
    NextMethod()
}




runViewers <- function(.viewer,.viewer_every, x,ind)
{
    if (length(.viewer)==1)
        {
            if ((ind %% .viewer_every)==0) .viewer(x,ind)
        }
    else
    {
        go <- ((ind %% .viewer_every)==0)
        if (any(go))
        {
            map(.viewer[go],function(f) f(x,ind))
        }
    }
    NULL
}

runStorage <- function(.store,.store_every, x,ind)
{
    res <- NULL
    if (length(.store)==1)
        {
            res <- if ((ind %% .store_every)==0) .store(x,ind)
        }
    else
    {
        go <- ((ind %% .store_every)==0)
        if (any(go))
        {
            res <- map(.store[go],function(f) f(x,ind))
        }
    }
    res
}

##' Turn a function into an iterated mapping
##'
##' Given a function f, return g such that g(x,k) = { f^k } (x) = f(f(f(f(...f(x)))))
##'
##' @param f a function
##' @return g another function
##' @author Simon Barthelme
##' @seealso fp (for fixed points), viewer (for viewing intermediate results), store (for storing them)
##' @examples
##' f <- function(x) x*2
##' g <- iter(f)
##' g
##' g(3,nIter=2)  ## Corresponds to:
##' f(3) %>% f
##' g(3,nIter=3) ## Corresponds to: 
##' f(3) %>% f %>% f
##' @export
iter <- function(f)
{
    if (!is.function(f))     stop("f must be a function")
    force(f)
    g <- function(x,nIter,...) run.iter(f,x,nIter=nIter,...)
    attr(g,"type") <- "iter"
    attr(g,"base.fun") <- f
    attr(g,"fun") <- g
    attr(g,"args") <- c()
    class(g) <- c("iterfunc","iterarg","function")
    g
}


##' @export
print.iterfunc <- function(x,...)
{
    if (attr(x,"type")=="iter")
    {
        msg <- "Iterated function system. Base function: \n"
    }
    else if (attr(x,"type")=="fp")
    {
        msg <- "Fixed point iteration. Base function: \n"
    }
    else
    {
        sprinftf("Unknown type: %s",attr(x,"type")) %>% stop
    }
    cat(msg)
    print(attr(x,"base.fun"))
}
##' Turn a function into a fixed point iteration
##'
##' Given a function f, return g such that g(x) is a fixed point of f (or at least an approximate one). 
##' Generally speaking the idea will be iterate f(f(f(f(....f(x))))) until a fixed point is reached, which of course may fail entirely if there are no stable fixed points.   
##' @param f a function
##' @return g another function
##' @author Simon Barthelme
##' @seealso iter (for fixed length iterations), viewer (for viewing intermediate results), store (for storing them)
##' @examples
##' #This function has a fixed point at x = 0, i.e. 0/2 = 0
##' f <- function(x) x/2
##' g <- fp(f)
##' g
##' g(3) #The fixed point is only approximate.
##' g(-2) 
##' #The default criterion is all.equal(f(x),x). Check:
##' all.equal(g(3),f(g(3)))
##' @export
fp <- function(f)
{
    if (is.function(f))
    {
        force(f)
        g  <- function(x,...) run.fp(f,x,...)
        attr(g,"fun") <- g
        attr(g,"base.fun") <- f
        attr(g,"type") <- "fp"
        attr(g,"args") <- c()
        class(g) <- c("iterfunc","iterarg","function")
        g
    }
    else
    {
        stop("f must be a function")
    }
}

printIter <- function(ind)
{
    msg <- "Iter %i: \n"
    cat(sprintf(msg,ind))
}

##' Turn on error catching
##'
##' Sometimes iterations diverge, leading to errors. If error catching is turned on, the iteration will return a warning and the last non-error value. 
##' @return used for side effects
##' @author Simon Barthelme
##' @examples
##' f <- function(x) if (x > 0) { x-2*x} else { stop('aargh') }
##' \dontrun{
##' g <- fp(f)
##' g(3)
##' }
##' g <- fp(f)+catch()
##' g(3)
##' @export
catch <- function()
{
    call <- list(.throw=FALSE)
    class(call) <- "iterarg"
    call
}

##' Output intermediate results in iterations
##'
##' Add a viewer to an iteration, for monitoring progress. Argument "fun" must be a function with at most two arguments. It will be passed the current value and the iteration number.
##' Some default viewing functions are already defined like "print". 
##' @param fun a viewer function
##' @param every frequency: run every k iteration (default 1, meaning at every iteration)
##' @param print.iter print iteration number
##' @return used for side effects
##' @author Simon Barthelme
##' @examples
##' f <- function(x) x/2
##' g <- fp(f)
##' (g+viewer(print))(.1)
##' # Now print only after 10 iterations
##' (g+viewer(print,every=10))(3)
##' # Print iteration number
##' (g+viewer(print,every=5,TRUE))(3)
##' 
##' # Define custom viewer:
##' v = function(x) print(x,digits=2)
##' (g+viewer(v,every=5,TRUE))(3)
##' v2 = function(x,ind) sprintf("At iteration %i. Current value %.3f",ind,x) %>% print
##' (g+viewer(v2,every=5))(3)
##' #You can have multiple viewers, running at different frequencies
##' v1 = function(x,ind) sprintf("At iteration %i",ind)%>%print
##' (g+viewer(v1)+viewer(v2,every=5))(3)
##' #You can also use plotting
##' x0 <- rnorm(40)
##' plot(x0)
##' v <- function(x) points(x,col="grey")
##' (g+viewer(v))(x0)
##' @export
viewer <- function(fun,every=1,print.iter=FALSE)
{
    fname <- deparse(substitute(fun))
    if (fname == "print")
    {
        fun <- function(x,ind) print(x)
    }
    else if (fname == "summary")
    {
        fun <- function(x,ind) print(summary(x))
    }
    else if (fname == "plot")
    {
        fun <- function(x,ind) plot(x,main=paste("Iteration",ind))
    }
    else
    {
        nArgs <- length(formals(fun))
        if (nArgs==1)
        {
            g <- fun
            fun <- function(x,ind) g(x)
        }
        else if (nArgs > 2)
        {
            stop('a viewer function cannot have more than two arguments')
        }
    }
    if ((round(every)!=every) | every < 1)
    {
        stop('argument every must be a positive integer')
    }
    if (print.iter)
    {
        call <- list(.viewer=function(x,ind) { printIter(ind);fun(x,ind) },.viewer_every=every)
    }
    else
    {
            call <- list(.viewer=fun,.viewer_every=every)
    }
    class(call) <- "iterarg"

    call
}

##' Store intermediate iterations
##'
##' Store intermediate values in a function iteration. Argument "fun" must be a function with at most two arguments. It will be passed the current value and the iteration number, and it must return an object that will be stored. See examples
##' @param fun a function 
##' @param every frequency: run every k iteration (default 1, meaning at every iteration)
##' @return used for side effects
##' @seealso viewer,stored
##' @author Simon Barthelme
##' @examples
##' f <- function(x) x/2
##' g <- iter(f)+store(function(x) x)
##' res <- g(3,5)
##' stored(res) #The result of 5 iterations, as a vector
##' #Store more info:
##' st <- function(x,ind) list(value=x,iter=ind)
##' res <- (iter(f)+store(st))(3,5)
##' stored(res) ## We now have a list of lists
##' #To extract elements, use map_ functions from package purrr
##' stored(res) %>% purrr::map_int("iter")
##' stored(res) %>% purrr::map_dbl("value")
##' stored(res) %>% purrr::map_df(identity)
##' #Store a summary
##' st <- function(x,ind) list(mvalue=mean(x),iter=ind)
##' g <- iter(f)+store(st)
##' g(rnorm(10),5) %>% stored %>% purrr::map_df(identity)
##' @export
store <- function(fun,every=1)
{
    fname <- deparse(substitute(fun))
    if (fname == "identity")
    {
        fun <- function(x,ind) list(x=x,iter=ind)
    }
    else if (fname == "summary")
    {
        fun <- function(x,ind) list(summary.x=summary(x),iter=ind)
    }
    else
    {
        nArgs <- length(formals(fun))
        if (nArgs==1)
        {
            g <- fun
            fun <- function(x,ind) g(x)
        }
        else if (nArgs > 2)
        {
            stop('a store function cannot have more than two arguments')
        }
    }
    if ((round(every)!=every) | every < 1)
    {
        stop('argument every must be a positive integer')
    }
    call <- list(.store=fun,.store_every=every)
    class(call) <- "iterarg"

    call
}

##' Change convergence criterion in fixed point iteration
##'
##' A fixed point is defined by f(x)=x. Often this condition can't be fulfilled exactly, and so we must check convergence using a looser criterion, like abs(f(x)-x) < tol.
##' The convergence criterion can be specified using a function of two arguments: d(x,x'), returning either a logical value or a positive scalar, to be checked against the tolerance 'tol'. 
##' The predefined convergence functions are:
##' mse=function(a,b) mean((a-b)^2)
##' rmse=function(a,b) sqrt(mean((a-b)^2))
##' mad=function(a,b) mean(abs(a-b))
##' mre=function(a,b) mean(abs(a-b)/abs(a+b)
##' If convergence can't be reached at all, we interrupt the search after a maximum number of iterations (set by maxIter).
##' @param fun convergence check: either a function with two arguments or a character string like "rmse"
##' @param tol tolerance
##' @param maxIter 
##' @return used for side effects
##' @author Simon Barthelme
##' @examples
##' f <- function(x) x/2
##' g <- fp(f)
##' g(3) #by default convergence is assessed using all.equal
##' (g+convergence("rmse",tol=.1))(3) #root-mean-square less than .1
##' (g+convergence("rmse",tol=.01))(3) #root-mean-square less than .01
##' #Custom function
##' d = function(a,b) sum(abs(log(a)-log(b)))
##' (g+convergence(d,tol=.01))(3) #can't converge because fixed point is at 0
##' @export
convergence <- function(fun,tol,maxIter)
{
    call <- list()
    if (!missing(fun))
    {
        if (is.character(fun))
        {
            if (fun %in% names(conv.fun))
            {
                call$.convergence <- conv.fun[[fun]]
            }
            else
            {
                stop('Unknown convergence function')
            }
        }
        else
        {
            nArgs <- length(formals(fun))
            if (nArgs > 2)
            {
                stop('A convergence function must have two arguments')
            }
            call$.convergence <- fun
        }
    }
    if (!missing(tol))
    {
        if (is.numeric(tol) && (length(tol)==1) && (tol >= 0))
        {
            call$.tol <- tol
        }
        else
        {
            stop('tol must be a positive scalar')
        }
    }
    if (!missing(maxIter))
    {
        if (is.numeric(maxIter) && (round(maxIter)==maxIter) && (maxIter>0))
        {
            call$.maxIter <- maxIter
        }
        else
        {
            stop('maxIter but be a positive integer')
        }
    }
    class(call) <- "iterarg"
    call
}

conv.fun <- list(mse=function(a,b) mean((a-b)^2),
                rmse=function(a,b) sqrt(mean((a-b)^2)),
                mad=function(a,b) mean(abs(a-b)),
                mre=function(a,b) mean(abs(a-b)/abs(a+b)))
                

mergeLists <- function(l1,l2,nomerge=NULL)
{
    if (!is.null(nomerge))
    {
        nm <- (names(l1) %in% nomerge)  & (names(l1) %in% names(l2))
        if (any(nm))
        {
            msg <- paste("Overriding parameters", paste(names(l1)[nm],collapse=" "))
            warning(msg)
            l1 <- l1[!nm]
        }
    }
    if (length(l1)==0)
    {
        l2
    }
    else if (length(l2)==0)
    {
        l1
    }
    else
    {
        n1 <- names(l1)
        n2 <- names(l2)
        common <- intersect(n1,n2)
        l <- map(common,function(n) c(l1[[n]],l2[[n]]))
        names(l) <- common
        c(l,l1[setdiff(n1,n2)],l2[setdiff(n2,n1)])
    }
}

##' @export
"+.iterarg" <- function(a,b)
{
    if ("iterfunc" %in% class(a) )
    {
        args <- mergeLists(attr(a,"args"),b,dont.merge)
        
        nf <- function(v,...) do.call(attr(a,"fun"),c(list(v),...,args))
        attributes(nf) <- attributes(a)
        attr(nf,"args") <- args
        attr(nf,"fun") <- attr(a,"fun")
        class(nf) <- c("iterfunc","iterarg","function")
        nf
    }
    else
    {
        l <- mergeLists(a,b,dont.merge)
        class(l) <- c("iterarg","list")
        l
    }
}

##' @describeIn store extracted stored values
##' @export
stored <- function(x)
{
    attr(x,"stored")
}


dont.merge <- c(".convergence",".tol",".throw",".maxIter")
