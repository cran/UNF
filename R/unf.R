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

v4DefaultNdig = 7;
v4DefaultCdig = 128;

"unf" <-
function(data, 
	digits=NULL,
	ndigits= {if (is.null(digits)) {7} else (digits)}, 
	cdigits= {if (is.null(digits)) {128} else (digits)}, 
	version=4, 
	rowIndexVar=NULL,
	rowOrder={if(is.null(rowIndexVar)) {NULL} else {order(rowIndexVar)}}
) {


	if (version<3) {
		warning("older versions of fingerprints are not recommended, current is version 4")
	} 

	ndigits=as.integer(ndigits)
	cdigits=as.integer(cdigits)
	if (ndigits<1) {
		warning("ndigits can't be less then 1")
		ndigits =1
	}
	if (ndigits>15) {
		warning("ndigits can't be greater than 15")
		ndigits = 15
	}
	if (cdigits<1) {
		warning("cdigits can't be less then 1")
		cdigits = 1
	}

	if (is.vector(data)) {
		len = 1
	} else if (is.factor(data)) {
		len = 1
		data=as.character(data)
		warning("forcing to character")
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

	# canonicalize row order
	if(!is.null(rowOrder)) {
		df=df[rowOrder,]
	}

	r = vector(mode="list", length=len)
	if (is.vector(data)) {
		r[[1]] =  unfV(data,ndigits=ndigits,cdigits=cdigits,version=version)
	} else {
		for ( i in 1:len) {
			r[[i]] =  unfV(data[[i]],ndigits=ndigits,cdigits=cdigits,version)
        	}
	}

	class(r)="unf"
	return(r)
}

summary.unf<-function(object,...) {
	if (length(object)==1) {
		return(object)
	} 
		
	unfattr = sapply(object,attributes)
	sigs = unlist(unfattr["base64",])
	cdigits = unlist(unfattr["cdigits",])
	ndigits = unlist(unfattr["ndigits",])
	versions= unlist(unfattr["version",])

	# Sort order under US_ENGLISH is problematic
	ol = Sys.getlocale("LC_COLLATE")
	Sys.setlocale("LC_COLLATE","C")
	ret = unf(sort(sigs),cdigits=256,version=versions[1])	
	Sys.setlocale("LC_COLLATE",ol)
	attr(ret[[1]],"cdigits")=cdigits[1];
	attr(ret[[1]],"ndigits")=ndigits[1];
	if ((sum(ndigits!=ndigits[1])>0) || (sum(cdigits!=cdigits[1])>0) || 
		(sum(versions!=versions[1])>0))
		{
		warning("UNF's being combined have different precisions or versions")
		if (sum(ndigits!=ndigits[1])>0)  {
			attr(ret[[1]],"ndigits")="mixed"	
		} 
		if (sum(cdigits!=cdigits[1])>0)  {
			attr(ret[[1]],"cdigits")="mixed"	
		} 
	}

	return(ret)
}

as.character.unf<-function(x) {
	ret = character(length=length(x));
	for (i in 1:length(x)) {
	   version = attr(x[[i]],"version")
	   ret[i]=paste("UNF:",attr(x[[i]],"version"),":",
		 if ( (version!=4) || (attr(x[[i]],"ndigits")!=v4DefaultNdig)
			 || (attr(x[[i]],"cdigits")!=v4DefaultCdig)
			) {
			paste( attr(x[[i]],"ndigits"), "," , 
		 	attr(x[[i]],"cdigits"),":",
			sep="")
		},
		 attr(x[[i]],"base64") ,sep="")  
	}
	return(ret)	
}

as.unf<-function(char) {
	 if (!is.character(char)) {
                warning("coercing to character string")
		char=as.character(char)
         }
	ret = vector(mode="list",length=length(char));
	for (i in 1:length(char)) {
		if ( regexpr("^UNF:[0-9a-zA-Z]+:([0-9]+(,[0-9]+)*:)?[a-zA-Z1-9]+=?=?",char[[i]],perl=TRUE)<0) {
			warning("does not appear to be a UNF")
			return(NULL)
		} else {
			tmp= strsplit(char[[i]],":")[[1]];	
			ret[[i]]=tmp[length(tmp)]
	        	class(ret[[i]])="unfV"
        		attr(ret[[i]],"base64")=tmp[length(tmp)]
        		attr(ret[[i]],"version")=tmp[2]
			attr(ret[[i]],"isnested")=FALSE

			if (length(tmp)==3) {
        			attr(ret[[i]],"ndigits")=v4DefaultNdig
        			attr(ret[[i]],"cdigits")=v4DefaultCdig
			} else  {
			 	tmpdig = strsplit(tmp[3],",")[[1]]
				if (length(tmpdig)==1) {
					attr(ret[[i]],"ndigits")=tmpdig
					attr(ret[[i]],"cdigits")=tmpdig
				} else {		
					attr(ret[[i]],"ndigits")=tmpdig[1]
					attr(ret[[i]],"cdigits")=tmpdig[2]
				}
			}
		}
	}
	class(ret)="unf"
	return(ret)	
}

unf2base64<-function(x) {
	ret = character(length=length(x))
	for (i in 1:length(x)) {
	   ret[i]= attr(x[[i]],"base64")
	}
	return(ret)	
}

"unfV" <-
function(v, 
	ndigits= NULL,
	cdigits= NULL,
	version=4 ) {

	INITSTRING = sprintf("%0.64i",as.integer(0))
	if (is.null(v) || is.null(ndigits) || is.null(cdigits)) {
		warning("unV called with NULL arguments")
		return(NULL)
	}

	if (version == 1) {
		if (is.character(v)) {
		    r = .C("R_unf1_char", NAOK=TRUE, 
			PACKAGE="UNF", 
			as.character(v), as.integer(is.na(v)), 
			as.integer(length(v)), as.integer(cdigits), 
			fingerprint =double(length=1),
			base64= INITSTRING 
			)
		} else {
		    r = .C("R_unf1_double", NAOK=TRUE, as.double(v), as.integer(length(v)),
			PACKAGE="UNF", 
			as.integer(ndigits), 
			fingerprint=double(length=1),
			base64= INITSTRING 
			)
		}
	} else if (version == 2) {
		if (is.character(v)) {
		    r = .C("R_unf2_char", NAOK=TRUE, 
			PACKAGE="UNF", 
			as.character(v), as.integer(is.na(v)), 
			as.integer(length(v)), as.integer(cdigits), 
			fingerprint =double(length=1),
			base64= INITSTRING 
			)
		} else {
		    r = .C("R_unf2_double", as.double(v), as.integer(length(v)),
			PACKAGE="UNF", 
			as.integer(ndigits), NAOK=TRUE,
			fingerprint=double(length=1),
			base64= INITSTRING 
			)
		}
	} else if (version==3) {
		if (is.character(v)) {
		   r = .C("R_unf3_char",  NAOK=TRUE,
			PACKAGE="UNF", 
			as.character(v), as.integer(is.na(v)),
			 as.integer(length(v)), as.integer(cdigits), 
			fingerprint =integer(length=16),
			base64= INITSTRING 
			)
		} else {
		   r = .C("R_unf3_double", PACKAGE="UNF", 
			NAOK=TRUE, as.double(v), as.integer(length(v)),
			as.integer(ndigits), 
			fingerprint =integer(length=32),
			base64= INITSTRING
			)
		}
	} else {
		if (version!=4 && version !="4a" )  {
			warning("unsupported fingerprint version, using version 4, resetting default digits")
			version = 4
			
		}
		if (is.character(v)) {
		   r = .C("R_unf4_char",  NAOK=TRUE,
			PACKAGE="UNF", 
			as.character(v), as.integer(is.na(v)),
			 as.integer(length(v)), as.integer(cdigits), 
			fingerprint =integer(length=16),
			base64= INITSTRING 
			)
		} else {
		   r = .C("R_unf4_double", PACKAGE="UNF", 
			NAOK=TRUE, as.double(v), as.integer(length(v)),
			as.integer(ndigits), 
			fingerprint =integer(length=32),
			base64= INITSTRING
			)
		}
	} 
	
	sig = r$base64;	
	class(sig)="unfV"
	attr(sig,"ndigits")=ndigits
	attr(sig,"cdigits")=cdigits
	attr(sig,"version")=version
	attr(sig,"isnested")=FALSE
	attr(sig,"base64")=r$base64
	attr(sig,"fingerprint")=r$fingerprint
	return(sig)
}

print.unf<-function(x,...) {
	invisible(print(as.character(x),...));
}

signifz<-function(x,digits=6) {
  if (class(x)=="data.frame") {
	ret=as.data.frame(sapply(x,function(y)signifz(y,digits=digits)
		,simplify=F))
	rownames(ret)=rownames(x)
	return(ret)
  }
  magnitude = floor(log10(abs(x)))
  scale = 10^(digits-magnitude-1)
  signs =  sign(x)

  ret=x
  g0 = which(signs>=0)
  ret[g0]= floor(x[g0]*scale[g0])/scale[g0]
  l0 = which(signs<0) 
  ret[l0]=  ceiling(x[l0]*scale[l0])/scale[l0]
  return(ret)
}

"unfTest" <-
function(silent=TRUE) {
	ret = TRUE

   x1 = 1:20
   x2 = x1 +.00001

   if (unf2base64(unf(x1))==unf2base64(unf(x2))) {
	ret=FALSE
	if (!silent) {
		warning("Failed discrimination test.")
        }
   }
   if (unf2base64(unf(x1,digits=5))!=unf2base64(unf(x2,digits=5))) {
	ret=FALSE
	if (!silent) {
		warning("Failed significance test 1.")
        }
   }
   if (unf2base64(unf(x1))!=unf2base64(unf(x2,digits=5))) {
	ret=FALSE
	if (!silent) {
		warning("Failed significance test 2.")
        }
   }
	
  cv="HRSmPi9QZzlIA+KwmDNP8w==";
  if (unf2base64(unf(x1,version=3))!=cv) {
	ret=FALSE
	if (!silent) {
		warning("Failed replication.")
       }
  }

  cvs = "E8+DS5SG4CSoM7j8KAkC9A==";
  if (unf2base64(summary(unf(as.data.frame(cbind(x1,x2)),ndigits=10,version=3)))!=cvs) {
	ret=FALSE
	if (!silent) {
		warning("Failed replication.")
       }
  }

  cv3="PjAV6/R6Kdg0urKrDVDzfMPWJrsBn5FfOdZVr9W8Ybg="
  if (  unf2base64(summary(unf(longley,digits=3))) != cv3 ||
	unf2base64(summary(unf(signifz(longley,digits=3)))) != cv3) {
	ret = FALSE
	if (!silent) {
		warning("Failed longley v 4")
       }
  }

   return(ret)
}
