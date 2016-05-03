# fixedpoints: an R package for doing the same thing 

This package provides tools for doing things repeatedly (a fixed number of times, or until convergence). It does absolutely nothing you couldn't do with a while loop, but it makes for clean and concise code in a functional-programming style.

	install_github("dahtah/fixedpoints")
	div2 = function(x) x/2
	g = iter(div2)
	g(2,nIter=3) #equals div2(div2(div2(2)))
	h = fp(div2)
	h(2) #run div2(div2(div2(...))) until convergence

fixedpoints provides "viewers" which print or plot intermediate results:

	g = iter(div2)+viewer(print)
	g(2,nIter=3)

It can also store intermediate values and has various options for convergence control. See vignette and doc for more examples.

Warning: early release, there will be bugs. 

Author: Simon Barthelmé, CNRS, Gipsa-lab. 
