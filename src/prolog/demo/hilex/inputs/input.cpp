//-----------------------------------------------------------------------------
//
//                                CRC16/CRC32
//
//                               Martin Dvorak
//                                    1996
//-----------------------------------------------------------------------------
#include <mem.h>
#include <stdio.h>
#include <stdlib.h>

#include "crc.h"

// #define PRINTEACH

#define  OK        0     // functions returns 0 if success
#define  TRUE      1
#define  FALSE     !TRUE
#define  SZ_CRCTAB 257
#define  COUNTER   256   // index in CRCtab[] where is stored number of
			 // pointers which points to it
#ifdef CRC16
 #define MK_TAB    0x8000         // 1<<15
#else
 #define MK_TAB    0x80000000ul   // 1<<31
#endif

static word  CRCconv[]=    // array which uses BinSearch to convert 16-bit
			   // value to array index - GetCRCVal()
		       { 1, 1<<1, 1<<2, 1<<3, 1<<4, 1<<5, 1<<6, 1<<7,
			 1<<8, 1<<9, 1<<10, 1<<11, 1<<12, 1<<14, 1<<15
		       };
static DATA CRCval[16],       // array of CRC values
	    CRCpoly[16],      // array of CRC polynomials
	    *CRCtab[]=        // array of pointers to tabs
		       { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 };

void InitCRC( void  )
{
 byte i;
 setmem( CRCpoly, 16*sizeof(DATA), 0 );
 setmem( CRCval , 16*sizeof(DATA), 0 );
 for( i=0; i<=15; i++ )
  if( CRCtab[i] ) // exists >=1
   { if( !(--CRCtab[i][COUNTER]) ) free(CRCtab[i]); CRCtab[i]=0; }
}

int SetCRCPoly( DATA npol, word mask )
{
 word  i, j;
 DATA  val, dat;
 DATA  *actualtab=NULL; // pointer to the CRCtab of npol
 byte  found, di, dj;

 if( !mask ) return BADMASK;

 for( i=1, di=0; di<=15; i<<=1, di++ )
  if( i&mask )
   {
    if(!(CRCpoly[di]==npol && CRCtab[i])) // if doesn't exist the same pol with tab
     {
      // free old tab if exists
      if( CRCtab[di] )      // exist >= 1
	if( !(--CRCtab[di][COUNTER]) ) free(CRCtab[di]);

      CRCtab[di]=0; // there is no tab now

      if( actualtab ) // tab was created or exists
       { CRCtab[di]=actualtab; CRCtab[di][COUNTER]++; CRCpoly[di]=npol; }
      else
       {
	// searching the same polynomial
	found=FALSE;
	for( j=0; j<=15; j++ )
	 if( CRCpoly[j]==npol )
	  { actualtab=CRCtab[di]=CRCtab[j]; CRCtab[j][COUNTER]++;
		  CRCpoly[di]=npol; found=TRUE; break;  }

	if( !found ) // making new one
	 {
	  if( (CRCtab[di]=(DATA*)malloc(SZ_CRCTAB*sizeof(DATA)))==NULL ) return NOMEM;
	  for ( dat=0; dat<=255; dat++ )
	   {
	    for ( j=0, val=dat<<(DATASIZE-8); j<=7; j++ )
	     val=val&MK_TAB?(val<<1)^npol:val<<1;
	    CRCtab[di][dat]=val; // save new value
	   }
	  CRCtab[di][COUNTER]=1; // there is 1 pointer now
	  CRCpoly[di]=npol;
	  actualtab=CRCtab[di];
	 } // make
       }   // else
     }     // !=
   }
 return OK;
}


int SetCRCVal( DATA nval, word mask )
{
 word j=1; byte i;
 if( mask )
  {  for( i=0; i<=15; i++,j<<=1 )
      if(j&mask) CRCval[i]=nval; return mask; }
 else return BADMASK;
}


DATA GetCRCVal( word mask, int *err )
{
 byte beg=0, end=0xF, i;
 if( !((mask-1)&mask) && mask )      // ONE bit is set
  {
   // searching index by BinSearch
   do
    { i=(beg+end)>>1; if( mask>CRCconv[i] ) beg=i+1; else end=i-1; }
   while( CRCconv[i]!=mask );
   return CRCval[i];
  }
 else { *err=BADMASK; return BADMASK; }
}


void UpdateCRCValByByte( byte val, word mask )
{
 word i; byte j;
 for( i=1, j=0; j<=15 ; i<<=1, j++ )
  if( i&mask )
   CRCval[j]=(CRCval[j]<<8) ^ CRCtab[j][(CRCval[j]>>(DATASIZE-8))^val];
 #ifdef PRINTEACH
   printf("%x\n",CRCval[0]);
 #endif
}

void CloseCRC( void )
{
 byte i;
 setmem( CRCpoly, 16*sizeof(DATA), 0 );
 setmem( CRCval , 16*sizeof(DATA), 0 );
 for( i=0; i<=15; i++ )
  if( CRCtab[i] )
   { if( !(--CRCtab[i][COUNTER]) ) free( CRCtab[i] ); CRCtab[i]=0; }
}


void CRCerror( int val )
{
  switch( val ) {
   case NOMEM  : puts(" CRCerror: Not enought memory!"); break;
   case BADMASK: puts(" CRCerror: Given mask has no sense!"); break;
   case OK     : /* success */ break;
  }
}
