timedep <- function(f,dt=1)
{
    force(f)
    function(state) list(x=f(state$x,state$t),t=state$t+dt)
}
