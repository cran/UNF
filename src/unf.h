/*
 * PMNF -- Portable multi-resolution numeric check
 *
 * Provides checks that a stat package accurately read a data
 * matrix, even across packages. 
 * 
 *
 *  Citations:
 *
 *  Altman, Gill, McDonald 2004.
 *  GCC Manual
 *  CRC Cites
 *
 */


#include <stdint.h>
#include <stdlib.h>
#include <locale.h>
#include <iconv.h> 
#include <stdio.h> 
#include <errno.h> 
#include <string.h>
#include <strings.h>

#include "md5.h"

/*
 * TYPEDEFS 
 */


typedef unsigned char cbyte;

/*
 * IFDEFS
 */ 

/* #define FORCELOCALE*/

#ifndef NOLONGDOUBLE
char *Genround(long double , int );
#endif
char *Genround(char*, int );
char *Genround(long long int , int );
char *Genround(short int , int );
char *Genround(int , int ) ;
char *Genround(long int , int ); 
char *Genround(double , int );
char *Genround(float , int ) ;

uint64_t Checksum_bytes(uint64_t , cbyte* , int );
uint64_t CRC64(uint64_t , cbyte* , int );
char* Canonicalize_unicode(const char*, char*, int*);

#ifdef NOLONGDOUBLE
uint64_t UNF1 (double , int , uint64_t , int ) ;
uint64_t UNF2 (double , int , uint64_t , int ) ;
int UNF3 (double , int , md5_state_t* , int ) ;
#else
uint64_t UNF1 (long double , int , uint64_t , int ) ;
uint64_t UNF2 (long double , int , uint64_t , int ) ;
int UNF3 (long double , int , md5_state_t* , int ) ;
#endif
uint64_t UNF1 (char* , int , uint64_t , int ) ;
int UNF3 (char*, int , md5_state_t* , int );
uint64_t UNF2 (char* , int , uint64_t , int ) ;
int UNF_init(int) ;

void tobase64(unsigned char *out, md5_byte_t *in, int inlen);
