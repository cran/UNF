#. unf.R
#
# Computes a universal numeric digital fingerprint of a vector
#
# Part of the UNF package. Available from www.r-project.org and
# www.hmdc.harvard.edu/numerical_issues/
#
#    Copyright (C) 2004  Micah Altman
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

"unf" <-
function(data, digits=6, version=3) {

	if (version!=3) {
		warning("older versions of fingerprints are not recommended, current is version 3")
	} 
	if (is.vector(data)) {
		len = 1
	} else if (is.data.frame(data)) {
		len = length(data)
	} else {
		warning("forcing to data frame")
		data=as.data.frame(data)
		len = length(data)
	}

	if (len==0) {
		warning ("NULL data")
		return(NULL)
	}
	if (is.factor(data)) {
		data = as.character(data)
	}

	r = vector(mode="list", length=len)
	if (is.vector(data)) {
		r[[1]] =  unfV(data,digits,version)
	} else {
		for ( i in 1:len) {
			r[[i]] =  unfV(data[[i]],digits,version)
        	}
	}

	class(r)="unf"
	return(r)
}

summary.unf<-function(object,...) {
	if (length(object)==1) {
		#return(attr(object[[1]],"base64"))
		return(object)
	} 
		
	sigs = as.character(sapply(object,attributes)["base64",])
	return(unf(sigs))
	#return(attr(unf(sigs,32,3)[[1]],"base64"));	
}


"unfV" <-
function(v, digits=6, version=3) {
	INITSTRING = sprintf("%0.20i",as.integer(0))

	if (version == 1) {
		if (is.character(v)) {
		    r = .C("R_unf1_char", NAOK=TRUE, 
			PACKAGE="UNF", 
			as.character(v), as.integer(is.na(v)), 
			as.integer(length(v)), as.integer(digits), 
			fingerprint =double(length=1),
			base64= INITSTRING 
			)
		} else {
		    r = .C("R_unf1_double", NAOK=TRUE, as.double(v), as.integer(length(v)),
			PACKAGE="UNF", 
			as.integer(digits), 
			fingerprint=double(length=1),
			base64= INITSTRING 
			)
		}
	} else if (version == 2) {
		if (is.character(v)) {
		    r = .C("R_unf2_char", NAOK=TRUE, 
			PACKAGE="UNF", 
			as.character(v), as.integer(is.na(v)), 
			as.integer(length(v)), as.integer(digits), 
			fingerprint =double(length=1),
			base64= INITSTRING 
			)
		} else {
		    r = .C("R_unf2_double", as.double(v), as.integer(length(v)),
			PACKAGE="UNF", 
			as.integer(digits), NAOK=TRUE,
			fingerprint=double(length=1),
			base64= INITSTRING 
			)
		}
	} else {
		if (version!=3)  {
			warning("unsupported fingerprint version, using version 3")
		}
		if (is.character(v)) {
		   r = .C("R_unf3_char",  NAOK=TRUE,
			PACKAGE="UNF", 
			as.character(v), as.integer(is.na(v)),
			 as.integer(length(v)), as.integer(digits), 
			fingerprint =integer(length=16),
			base64= INITSTRING 
			)
		} else {
		   r = .C("R_unf3_double", PACKAGE="UNF", 
			NAOK=TRUE, as.double(v), as.integer(length(v)),
			as.integer(digits), 
			fingerprint =integer(length=16),
			base64= INITSTRING
			)
		}
	} 
	
	sig = r$base64;	
	class(sig)="unfV"
	attr(sig,"digits")=digits
	attr(sig,"version")=version
	attr(sig,"isnested")=FALSE
	attr(sig,"base64")=r$base64
	attr(sig,"fingerprint")=r$fingerprint
	return(sig)
}

print.unf<-function(x,...) {
	for (i in 1:length(x)) {
	   print (paste("UNF",attr(x[[i]],"version"), attr(x[[i]],"digits"),
		attr(x[[i]],"base64")
		 ,sep=":") ) 
	}
}

"unfTest" <-
function(silent=TRUE) {
	ret = TRUE

   x1 = 1:20
   x2 = x1 +.00001

   if (as.character(unf(x1))==as.character(unf(x2))) {
	ret=FALSE
	if (!silent) {
		warning("Failed discrimination test.")
        }
   }
   if (as.character(unf(x1,digits=5))!=as.character(unf(x2,digits=5))) {
	ret=FALSE
	if (!silent) {
		warning("Failed significance test 1.")
        }
   }
   if (as.character(unf(x1))!=as.character(unf(x2,digits=5))) {
	ret=FALSE
	if (!silent) {
		warning("Failed significance test 2.")
        }
   }
	
   cv = c(29,20,166,62,47,80,103,57,72,3,226,176,152,51,79,243)
  if (sum(attr(unf(x1)[[1]],"fingerprint")!=cv)>0) {
	ret=FALSE
	if (!silent) {
		warning("Failed replication.")
       }
  }


   return(ret)
}
