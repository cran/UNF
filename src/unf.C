/*
*  unf.C
*
* Universal numeric fingerprints. Computes a fingerprint of a vector of
* observation at a specified level of numerical precision.
*
* Part of the Accuracy package. Available from www.r-project.org and
* www.hmdc.harvard.edu/numerical_issues/
*
*    Copyright (C) 2004  Micah Altman
*
*    This program is free software; you can redistribute it and/or modify
*    it under the terms of the GNU General Public License as published by
*    the Free Software Foundation; either version 2 of the License, or
*    (at your option) any later version.
*
*    This program is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*    GNU General Public License for more details.
*
*    You should have received a copy of the GNU General Public License
*    along with this program; if not, write to the Free Software
*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/



#include "unf.h"

#define FORCELOCALE

/*
 * TYPEDEFS 
 */

typedef unsigned char cbyte;

/*
 * PROTOTYPES FOR INTERNAL FUNCTIONS
 */

int selftest (int);
int check_little_endian(void);
uint64_t ntoh64(uint64_t);
uint64_t hton64(uint64_t);
long int htonl (long int);
long int ntohl (long int);

/*
 * GLOBALS
 */

int static PASSED_INIT = UNF_init(1);
int static IS_LITTLE_ENDIAN= check_little_endian();

//#define DEBUG

/*
 * Canonicalization routines --  string output
 */



/*
 * Generalized Rounding Routines
 *
 * Genround()
 *
 * args:
 * 	n - number
 * 	digits - significant digits to be rounded to
 *
 * returns:
 * 	Char string representing properly rounded value.
 *  	This string must be free()'d.	
 *
 * Note: requires stdio.h , locale.h, string.h 
 *
 * Assumes Init routine has been called to set locale.
 */


#ifdef NOLONGDOUBLE
char *Genround(double n, int digits ) {
#else
char *Genround(long double n, int digits ) {
#endif
	#ifdef FORCELOCALE
	char *oldlocale;
	#endif

	char *buf = (char*) malloc(digits+20) ;
	char *buf2 = (char*) malloc(digits+20) ;

	#ifdef FORCELOCALE
	oldlocale = setlocale(LC_ALL, "POSIX");
	#endif 

	#ifdef NOLONGDOUBLE
	sprintf(buf,"%+#.*e\n", digits-1,  n);
	#else
	sprintf(buf,"%+#.*Le\n", digits-1,  n);
	#endif

	// Canonical form is 
	//		- leading + or -
	//		- leading digit
	//		- decimal point
	//		- up to digit-1 digits, no trailing zeros
	//		- 'e' (lower-case e)
	//		- '+' or '-'
	//		- exponent digits, with no leading zeros
	// E.g: +1.2e+1 -2.e+  +1.362e-17 +0.e+
	//remove trailing zeros in the mantissa  (can't just use %+$.*Lg )
	//remove leading zeros in the exponent
	int i,j, mantissa_end, exponent_begin;
	mantissa_end =(2+digits-1); 
	while(buf[mantissa_end]=='0' && (mantissa_end >2)) {
		mantissa_end--;
	}
	exponent_begin = 4+digits;
	while (buf[exponent_begin]=='0') {
		exponent_begin++;
	}

	for (i=0; i<= mantissa_end; i++) {
		buf2[i]=buf[i];
	}
	buf2[i]='e';
	buf2[i+1]=buf[digits+3];
	j=i+2;		
	for (i=exponent_begin; buf[i]!=0; i++) {
		buf2[j]=buf[i];
		j++;
	}
	buf2[j]='\0';
	free(buf);

	/*int i,ep,zp;
	i = strlen(buf); ep=0; zp=0;
	while (ep==0 && i>0) {
		if (buf[i]=='e') { 
			ep=i; 
		}
		i--;
	}
	zp=i+1;
	while (buf[i]=='0' && i>0) {
		zp=i; 
		i--;
	}
	if (ep!=zp) {
		char *buf2=(char *)malloc(strlen(buf)+1);
		buf2=strncpy(buf2,buf,strlen(buf)+1);
		buf[zp]='\0';
		strcat(buf,buf2+ep);
		free(buf2);
	}*/

	#ifdef FORCELOCALE
	setlocale(LC_ALL, oldlocale );
	#endif 
	return(buf2);
}


char *Genround(char *n, int digits) {
	#ifdef FORCELOCALE
	char *oldlocale;
	#endif

	char *buf = (char*) malloc(digits+20) ;

	#ifdef FORCELOCALE
	oldlocale = setlocale(LC_ALL, "POSIX");
	#endif
	sprintf(buf,"%.*s\n", digits,  n);
	#ifdef FORCELOCALE
	setlocale(LC_ALL, oldlocale );
	#endif
	return(buf);
}

/* these are simply wrappers around long double versions */

#ifdef NOLONGDOUBLE
char *Genround(long long int n, int digits ) {
	return(Genround((double) n, digits));
}

char *Genround(short int n, int digits ) {
	return(Genround((double) n, digits));
}

char *Genround(int n, int digits ) {
	return(Genround((double) n, digits));
}

char *Genround(long int n, int digits ) {
	return(Genround((double) n, digits));
}

char *Genround(float n, int digits ) {
	return(Genround((double) n, digits));
}
#else
char *Genround(long long int n, int digits ) {
	return(Genround((long double) n, digits));
}

char *Genround(short int n, int digits ) {
	return(Genround((long double) n, digits));
}

char *Genround(int n, int digits ) {
	return(Genround((long double) n, digits));
}

char *Genround(long int n, int digits ) {
	return(Genround((long double) n, digits));
}

char *Genround(double n, int digits ) {
	return(Genround((long double) n, digits));
}

char *Genround(float n, int digits ) {
	return(Genround((long double) n, digits));
}
#endif


/*
 * Fingerprinting methods
 *
 * These take a stream of bytes and return a fingerprint for the stream,
 *
 * Algorithms to do this include (in fastest but least robust to slowest but
 * most robust order):
 *
 * 	- checksums			( better than nothing, but not completely 
 * 						robust to accidental modification ) 
 * 	- cyclic-redundancy-checks 	( almost completely robust
 * 					 to most accidental modification, but not to 
 * 					  intentional tampering )
 * 	- message-digest algorithms	( robust to intentional tampering )
 *
 */

uint64_t Checksum_bytes(uint64_t previous , cbyte* sequence, int len) {
   int i;
   uint64_t r = previous;
   if (len<0) {
	   return(previous);
   }
   for ( i=0; i<len; i++) { 
	  r += (unsigned short int) sequence[i];
   }	    
   return(r);
}


uint64_t CRC64(uint64_t previous , cbyte* sequence, int len){

	// Based on SPcrc, a C implementation by Christian Iseli

  #define AUTODIN_IIREV	0xEDB88320
  #define POLY64REV	0xd800000000000000ULL
  static uint64_t CRCTable[256];
  uint64_t crc = ntoh64(previous);
  static int init = 0;
  int i;

  if (!init) {
    init = 1;
    for (i = 0; i <= 255; i++) {
      int j;
      uint64_t part = i;
      for (j = 0; j < 8; j++) {
        if (part & 1)
          part = (part >> 1) ^ POLY64REV;
        else
          part >>= 1;
      }
      CRCTable[i] = part;
    }
  }

  for (i = 0; i < len; i++) {
    uint64_t temp1 = crc >> 8;
    uint64_t temp2 = CRCTable[(crc ^ (unsigned long long) sequence[i]) & 0xff];
    crc = temp1 ^ temp2;
  }

  return(hton64(crc));
  #undef AUTODIN_IIREV
  #undef POLY64REV
}
  

/* 
 * Convert string to canonical form
 *
 * Use of iconv based on example code from GCC manual.
 *
 */

char* Canonicalize_unicode(const char *charset, char *inbuf, int *bytes_converted)
{
  char *wrptr, *inptr, *outbuf;
  size_t nconv,insize;
  iconv_t cd;

  int bufsize = (strlen(inbuf)+1)* 8;
  size_t avail = bufsize;

  outbuf = (char *) calloc(bufsize, 1); 
  if (outbuf==NULL) {
      perror("calloc");
      *bytes_converted=-1;
      return(NULL);
  }

  /* set pointers to beginning of buffers */
  wrptr=outbuf;
  inptr=inbuf;

  cd = iconv_open ( "UTF-32BE", charset); if (cd == (iconv_t) -1) {
      if (errno == EINVAL)
        fprintf(stderr, "conversion from '%s' to UTF-32BE  not available",
               charset);
      else
        perror ("iconv_open");

      *bytes_converted=-1;
      free(outbuf);
      return(NULL);
   }

   /* Now write out the byte sequence to get into the
             initial state if this is necessary.  */
   iconv (cd, NULL, NULL, &wrptr, &avail);

   /* Do the conversion.  */
   insize = strlen(inbuf);
   #ifdef ICONV18
   nconv = iconv (cd, (const char **) &inptr, &insize, &wrptr, &avail);
   #else
   nconv = iconv (cd, &inptr, &insize, &wrptr, &avail);
   #endif 
   if (nconv == (size_t) -1) {
	    if (errno != E2BIG) {
            	perror ("iconv_open");
      		*bytes_converted=-1;
      		free(outbuf);
      		return(NULL);
	    }
   }

   /* Terminate the output string.  */
    *wrptr = '\0'; 

   if (iconv_close (cd) != 0)
    	perror ("iconv_close");

   *bytes_converted =  wrptr - outbuf;
   return(outbuf);
}

/*
 * Portable Multi-Resolution Numeric Fingerprint routines
 *
 * This is mathematically a composition of the
 * fingerpring, canonicalization and rounding method:
 *
 * FingerPrint(Canoicalization(Round(value,digits)))
 *
 * PMRNF1 is for test purposes.
 * PMRNF2 is more robust.
 *
 *
 */

#ifdef NOLONGDOUBLE
uint64_t UNF1 (double n, int digits, uint64_t previous, int miss) {
#else
uint64_t UNF1 (long double n, int digits, uint64_t previous, int miss) {
#endif
	char *tmps, *tmpu=NULL;
	int bytes_converted;
	uint64_t r;
	
	if (miss) {
		tmpu = Canonicalize_unicode("ASCII", "miss", &bytes_converted);
	} else {
		tmps = Genround(n, digits);
		if (tmps == NULL) {
			return (0);
		}
		tmpu = Canonicalize_unicode("ASCII", tmps, &bytes_converted);
		free (tmps);
	} 

	if (tmpu == NULL) {
		return (0);
	}

	r = Checksum_bytes(previous , (cbyte*) tmpu, bytes_converted);
	free(tmpu);
	return(r);
}

uint64_t UNF1 (char *n, int digits, uint64_t previous, int miss) {
	char *tmps, *tmpu;
	int bytes_converted;
	uint64_t r;
	
	if (miss) {
		tmpu = Canonicalize_unicode("ASCII", "miss", &bytes_converted);
	} else {
		tmps = Genround(n, digits);
		if (tmps == NULL) {
			return (0);
		}
		tmpu = Canonicalize_unicode("ASCII", tmps, &bytes_converted);
		free (tmps);
	} 

	if (tmpu == NULL) {
		return (0);
	}

	r = Checksum_bytes(previous , (cbyte*) tmpu, bytes_converted);
	free(tmpu);
	return(r);
}

#ifdef NOLONGDOUBLE
uint64_t UNF2 (double n, int digits, uint64_t previous, int miss) {
#else
uint64_t UNF2 (long double n, int digits, uint64_t previous, int miss) {
#endif
	char *tmps, *tmpu=NULL;
	int bytes_converted;
	const char *missv="\0\0\0"; int missl=3;
	uint64_t r;
	
	if (!miss) {
		tmps = Genround(n, digits);
		if (tmps == NULL) {
			return (0);
		}
		tmpu = Canonicalize_unicode("ASCII", tmps, &bytes_converted);
		free (tmps);
		if (tmpu == NULL) {
			return (0);
		}
	} 

	// we use bytes converted +1 to include the null terminator in the
	// checksum, which ensures each entry is clearly separated from the next
	if (miss) {
		r = CRC64(previous , (cbyte*) missv, missl);
	} else {
		r = CRC64(previous , (cbyte*) tmpu, bytes_converted+1);
		free(tmpu);
	}
	return(r);
}

uint64_t UNF2 (char *n, int digits, uint64_t previous, int miss) {
	char *tmps, *tmpu=NULL;
	int bytes_converted;
	const char *missv="\0\0\0"; int missl=3;
	uint64_t r;
	
	if (!miss) {
		tmps = Genround(n, digits);
		if (tmps == NULL) {
			return (0);
		}
		tmpu = Canonicalize_unicode("ASCII", tmps, &bytes_converted);
		free (tmps);
		if (tmpu == NULL) {
			return (0);
		}
	} 

	// we use bytes converted +1 to include the null terminator in the
	// checksum, which ensures each entry is clearly separated from the next
	if (miss) {
		r = CRC64(previous , (cbyte*) missv, missl);
	} else {
		r = CRC64(previous , (cbyte*) tmpu, bytes_converted+1);
		free(tmpu);
	}
	return(r);
}

/*	md5_state_t state;
	md5_byte_t digest[16];

	md5_init(&state);
	md5_finish(&state, digest); */

int UNF3 (char *n, int digits, md5_state_t *previous, int miss) {
	char *tmps, *tmpu=NULL;
	const char *missv="\0\0\0"; int missl=3;
	int bytes_converted;
	
	if (!miss) {
		tmps = Genround(n, digits);
		#ifdef DEBUG
		fprintf (stderr,"%s\n",tmps);
		#endif 

		if (tmps == NULL) {
			return (0);
		}
		tmpu = Canonicalize_unicode("ASCII", tmps, &bytes_converted);
		free (tmps);
		if (tmpu == NULL) {
			return (0);
		}
	} 

	if (miss) {
		#ifdef DEBUG
		fprintf (stderr,"miss\n");
		#endif
		md5_append(previous, (const md5_byte_t *) missv, missl);
	} else {
		// we use bytes converted +1 to include the null terminator in the
		// checksum, which ensures each entry is clearly separated from the next
		md5_append(previous, (const md5_byte_t *) tmpu, bytes_converted+1);
		free(tmpu);
	}
	return(1);
}

#ifdef NOLONGDOUBLE
int UNF3 (double n, int digits, md5_state_t *previous, int miss) {
#else
int UNF3 (long double n, int digits, md5_state_t *previous, int miss) {
#endif
	char *tmps, *tmpu=NULL;
	const char *missv="\0\0\0"; int missl=3;
	int bytes_converted;
	
	if (!miss) {
		tmps = Genround(n, digits);
		#ifdef DEBUG
		fprintf (stderr,"After Genround:%s:\n",tmps);
		#endif
		if (tmps == NULL) {
			return (0);
		}
		tmpu = Canonicalize_unicode("ASCII", tmps, &bytes_converted);
		#ifdef DEBUG
		{ int i;
		  fprintf (stderr, "UNICODE BYTES (%d): ", bytes_converted);
		  for (i =0; i<bytes_converted; i++) {
			fprintf(stderr, "%d,", (int) tmpu[i]);
		  }
		  fprintf (stderr, "\n");
		}
		#endif
		free (tmps);
		if (tmpu == NULL) {
			return (0);
		}
	} 

	if (miss) {
		#ifdef DEBUG
		fprintf (stderr,"miss\n");
		#endif
		md5_append(previous, (const md5_byte_t *) missv, missl);
	} else {
		// we use bytes converted +1 to include the null terminator in the
		// checksum, which ensures each entry is clearly separated from the next
		md5_append(previous, (const md5_byte_t *) tmpu, bytes_converted+1);
		free(tmpu);
	}
	return(1);
}

/*
 * Utility Routines
 */

/*
 * Init_check
 *
 * - Checks that assumptions about bit and byte length have been met.
 * - Initializes globale values and environment 
 */

int UNF_init (int quiet) {

   	int mantissa_bits= 1, bitsperbyte = 0, retval=0;
        long double x1 = 1.0, delta = 0.5, x2 = x1 + delta;

	unsigned char t=1;
	while (t!=0 && bitsperbyte< 1024) {
        	bitsperbyte++;
        	t=t<<1;
	}
	// Canonicalization of floats and ints may be different with > 8 bits
	// -- test byte size
	if (bitsperbyte != 8) {
	   if (quiet !=0) {
	   	fprintf(stderr, "Warning: math check failed, not 8 bits per byte \n");;
	   }
	   retval = -1;
	}

	// test mantissa size of long double
        while ( x1!=x2 ) {
		   delta *= 0.5;
		   x2 = x1 + delta;
		   mantissa_bits++;
        }

	if (mantissa_bits <  bitsperbyte*(signed int) sizeof(long long unsigned int)) {
		retval = -1;
		if (quiet!=0) {
			fprintf(stderr, "Warning: long long ints may lose precision when "
						"converted to long doubles \n");
			fprintf(stderr, "long double mantissa %d \n", mantissa_bits);
			fprintf(stderr, "sizeof long long unsigned int %d \n", 
					(signed int) sizeof (long long unsigned int));
		}
	}


	#ifndef FORCELOCALE
	// POSIX locale required for string conversions
	setlocale(LC_ALL, "POSIX"); 	    
	#endif

	IS_LITTLE_ENDIAN = check_little_endian();
	return(retval);
}

int check_little_endian (void) {
	union {
		long l;
	        char c[sizeof (long)];
	} u;
	u.l = 1;
	return (u.c[sizeof (long) - 1] == 1);
}


uint64_t ntoh64(uint64_t n) {
	/* Derived from GNUnet, by Christian Grothoff, et. al */
	if (IS_LITTLE_ENDIAN) {
		return (((uint64_t) ntohl(n)) << 32) + ntohl(n >> 32);
	} else {
		return(n);
	}
}

uint64_t hton64 ( uint64_t n) {
	/* Derived from GNUnet, by Christian Grothoff, et. al */

	if (IS_LITTLE_ENDIAN) {
		return (((uint64_t)htonl(n)) << 32) + htonl(n >> 32);
	} else {
		return n; 
	}
}



void tobase64(unsigned char *out, md5_byte_t *in, int inlen)
/* raw bytes in quasi-big-endian order to base 64 string (NUL-terminated) */
{
   static const char base64digits[] =
   "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

   /* This code adapted from fetchmail sources by
   * Eric S. Raymond <esr@snark.thyrsus.com>.  */

	for (; inlen >= 3; inlen -= 3) {
		*out++ = base64digits[in[0] >> 2];
		*out++ = base64digits[((in[0] << 4) & 0x30) | (in[1] >> 4)];
		*out++ = base64digits[((in[1] << 2) & 0x3c) | (in[2] >> 6)];
		*out++ = base64digits[in[2] & 0x3f];
		in += 3;
	}
	if (inlen > 0) {
	unsigned char fragment;

	*out++ = base64digits[in[0] >> 2];
	fragment = (in[0] << 4) & 0x30;
	if (inlen > 1)
		fragment |= in[1] >> 4;
		*out++ = base64digits[fragment];
		*out++ = (inlen < 2) ? '=' : base64digits[(in[1] << 2) & 0x3c];
		*out++ = '=';
	}
	*out = '\0';
}

/*
	ntohl routines derived fromlinux srcs
	
	Note: we're not using netinet/in.h because mingw doesn't include it :-(
		This version is slower, but only used in version 2 unfs
		anyway. 
*/

long int ntohl(long int x) {
     if (IS_LITTLE_ENDIAN) {
      x = ((((x) & 0xff000000) >> 24) | (((x) & 0x00ff0000) >>  8) |               \
      (((x) & 0x0000ff00) <<  8) | (((x) & 0x000000ff) << 24));
     }
     return(x);
}

long int htonl(long int x) {
     if (IS_LITTLE_ENDIAN) {
     x= ((((x) & 0xff000000) >> 24) | (((x) & 0x00ff0000) >>  8) |               \
      (((x) & 0x0000ff00) <<  8) | (((x) & 0x000000ff) << 24));
     }
     return(x);
}
