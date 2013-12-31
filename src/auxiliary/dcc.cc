//---------------------------------------------------------------------------
//
//                              DConvert
//
//                            Martin Dvorak
//                              1997-99
//
//---------------------------------------------------------------------------
// Hints:
//  - must be compiled with some c++ compiler such us g++
//  - under Borland C/C++ use compact model (all the pointers are far)
//
// Known constraints:
// - conversion of HTML tables and forms is not supported
//
// ToDo:
// - conversion of &...; , tables,
// - volitelny parametr sirky
// - UNICODE
//---------------------------------------------------------------------------

// select OS: ifndef LINUXIK => compilation for DOS
 #define LINUXIK

//---------------------------------------------------------------------------

#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef LINUXIK
 #include <sys/stat.h>
 #include <sys/types.h>
 #include <unistd.h>
#else
 #include <io.h>
 #include <mem.h>
 #include <process.h>
 #include <alloc.h>
 #include <dos.h>
 #include <sys\stat.h>
#endif

//- Defines -------------------------------------------------------------------

#define DCC_VER "2.6"    // version of DCC

#define FALSE 0
#define TRUE  !FALSE

#define MAXBUFFERSIZE 64000ul
#define CZARRAYSIZE   29
#define CUTTO         70
#define IO_BLOCK_SIZE 3000 // how many bytes write/read

// lenght of filename
#define OUT_FILE_LENGHT 100
// estimate max number of lines in converted file for buffer reserve
#define LINES_ESTIM 30000



#ifdef LINUXIK
 #define farmalloc malloc
 #define farfree   free
 #define huge
#endif

#define VERBOSE

//- Types ---------------------------------------------------------------------

typedef unsigned char byte;
#ifndef LINUXIK
 typedef unsigned char bool;
#endif
typedef unsigned int  word;
typedef unsigned long dword;

//- Globals -------------------------------------------------------------------

static char headT602[]= "@CT 1\n"
			"@LM 1\n"
			"@RM 78\n"
			"@PL 60\n"
			"@TB -----T-----T-----T-----T-----T-----T-----T-----T-----T-----T-----T-----T-----T-----T-----T-----T-----T-----T-----T-----T-----T-----T\n"
			"@MT 3\n"
			"@MB 3\n"
			"@PO 0\n"
			"@PN 1\n"
			"@OP\n"
			"@LH 6\n";

char czT602[]= " ŸÔ‚Ø¡å¢ýçœ£…ì§µ¬Ò·ÖÕàüæ›éÞí¦";     // Latin2
char czWRI[] = "áèïéìíòóøšúùýžÁÈÏÉÌÍÒÓØŠÚÙÝŽ";     // Win-1250
char czLINX[]= "áèïéìíòóø¹»úùý¾ÁÈÏÉÌÍÒÓØ©«ÚÙÝ®";     // ISO Latin2
char czTXT[] = "acdeeinorstuuyzACDEEINORSTUUYZ";
char czWF[]  = " ‡ƒ‚ˆ¡¤¢©¨Ÿ£–˜‘€…‰‹¥•ž›†——’";     // kamenici

//---------------------------------------------------------------------------

int  FindMe( char itsiam, char *where );
int  ChangeCZ( char *fromCZ, char *toCZ, char huge *buf, int outhandle,
	       long sourcelength );
int  HtmlCZ( char *fromCZ, char *toCZ, char huge *buf, int outhandle,
	     long sourcelength );
void SaveResult( long w, int  outhandle, char huge *buf );

//---------------------------------------------------------------------------

int main( int argc, char **argv )
{
 char huge *buf,
      huge *rbuf,
      huge *hbuf,		// helping buffer

      outfile[200];		// infile ... all the time cmd param is used
      outfile[0]  =0;

 int  inhandle,
      outhandle,

      cutTo;

 long sourcelength,

      r=0, 			// read index
      w=0, 			// write index

      ii,

      bufsize;



 int  samefile;
 int  inhtml =FALSE,
      verbose=FALSE;            // if true -> verbose output

 char c;
 int  column=0;

 #ifdef LINUXIK
  struct stat filedescription;
 #endif



 // set write mode to binary
 #ifndef LINUXIK
  _fmode = O_BINARY;
 #endif

 if( argc != 4 && argc != 5 )
  {
   printf(
	  "\n DCC - Dvorka's convert utility v"DCC_VER
	  #ifndef LINUXIK
	   ", mem: %luB"
	  #endif
	  "\n         Version release: "__DATE__" " __TIME__
	  "\n Usage: dcc <verbose><option> <source> <dest> [<cols>][<frag>]"
	  "\n  Verbose:"
	  "\n      + ... verbose output"
	  "\n      - ... only short process info is shown" 
	  "\n  Short options:"
	  "\n      d ... 0xA & 0xD 0xA (UNIX) -> 0xD 0xA (DOS)"
	  "\n      u ... 0xD 0xA       (DOS)  -> 0xA     (UNIX)"
	  "\n      e ... throws UniX's ESCape sequences FUC"
	  "\n      h ... throws HTML kecy fuc and cuts lines to 70 columns"
	  "\n      c ... converts T602 file into WinWrite text file"
	  "\n      t ... trims lines to <cols> columns (only 0xA used)"
	  "\n      s ... split source file into dsts of <frag> bytes"
	  "\n  Long options:"
	  "\n      <srcCoding><dstCoding> where *Coding can be:"
	  "\n        w ... Win-1250   (M$ Windows)"
	  "\n        k ... kamenici   (WordFugue)"
	  "\n        i ... ISO 8859-2 (Linux)"
	  "\n        l ... latin2     (T602)"
	  "\n        a ... ascii      (no hacky&carky)"
	  "\n      example: dconvert +wi source.txt dest.txt"
	  "\n  Long HTML options:"
	  "\n      <srcCoding><dstCoding>H where *Coding is the same as above."
	  "\n        This option throws HTML kecy fuc, cuts lines to 70 columns"
	  "\n        and changes czech coding."
	  "\n      example: dconvert -wiH source.txt dest.txt"
	  #ifndef LINUXIK
	   ,(unsigned long)farcoreleft()
	  #endif
	 );

   return 0;
  }
 else
  {

   // check verbose mode
   switch( argv[1][0] )
       {
       case '+':
	   verbose=TRUE;
	   break;
       case '-':
	   verbose=FALSE;
	   break;
       default:
	   printf(" Error: unknown verbose option (must be +/-)...\nBye\n");
	   return 0;
       }

   if( verbose )
    printf("DCC v"DCC_VER
	   #ifndef LINUXIK
	    ", mem: %luB"
	    ,(unsigned long)farcoreleft()
	   #endif
	  );
   else // verbose
    printf("DCC");
    


   samefile = strcmp(argv[2], argv[3]);

   if( !samefile ) // file names are the same
   {
    strcpy(outfile,"dconvert.swp");

    if( verbose )
     printf("\n Source file name == dest file name -> renaming %s to %s",
            argv[3],
	    outfile);
    else 
     printf(" src==dst,");    
   }
   else
    strcpy(outfile,argv[3]);



   #ifdef LINUXIK
    if((inhandle = open(argv[2], O_RDWR))            < 0) { perror("\n Error: cann't open source file ..."); return 1; }
    if((outhandle= creat(outfile, S_IREAD |S_IWRITE))< 0) { perror("\n Error: unable to create output file"); return 1; }
   #else
    if((inhandle =open(argv[2], O_RDWR | O_BINARY))  < 0) { perror("\n Error: cann't open source file ..."); return 1; }
    if((outhandle=creat(outfile, S_IREAD | S_IWRITE))< 0) { perror("\n Error: unable to create output file"); return 1; }
   #endif



   #ifdef LINUXIK
    fstat(inhandle, &filedescription );
    sourcelength=filedescription.st_size;
   #else
    sourcelength=filelength(inhandle);
   #endif



   if( verbose )
    printf("\n %s size: %ldB -> allocating buffers...", 
            argv[2], 
            sourcelength 
          );
   else
    printf(" %s %ldB,", argv[2], sourcelength );

   if( argv[1][1]=='d' ) // output will be stretched when converting Unix->DOS
   {
    if( verbose )
     printf("\n  -> stretch *rbuf*...");

    bufsize = sourcelength+LINES_ESTIM;
    if( (rbuf=(char *)farmalloc(bufsize)) == NULL )
    {
     printf(" not enough memory to allocate such a big buffer (%luB)!"
	    "\nBye!\n"
	    ,bufsize
	   );
     return 0;
    }
    else
    {
     if( verbose )
      printf("OK (%i)",bufsize);
    } 
   }

   if( verbose )
    printf("\n  -> main *buf*...");

   if( (buf=(char *)farmalloc(sourcelength)) == NULL )
    {
     printf(" not enough memory to allocate such a big buffer (%luB)!"
	    "\nBye!\n"
	    ,sourcelength
	   );
     return 0;
    }
   else // buffer successfuly allocated
    {
     if( verbose )
      printf("OK (%i)",sourcelength); // buffer successfuly allocated

     // load file
     hbuf=buf;      // buf=hbuf=filelength, rbuf=filelength+Const

     r=0;

     if( verbose )
      printf("\n RD");
     else
      printf(" RD");

     while( r<sourcelength )
     {
      printf(".");

      w=sourcelength-r;

      if( IO_BLOCK_SIZE>w )
      {
       read(inhandle, hbuf, w);
       r=sourcelength;
      }
      else
      {
       read(inhandle, hbuf, IO_BLOCK_SIZE);
       r+=IO_BLOCK_SIZE;
       hbuf=buf+r;
      }
     }
     printf("OK");

     //- Core ----------------------------------------------------------------

     printf(" working");

     // SHORT option eg. -a
     if(argv[1][2] == 0 )
     {
      switch( argv[1][1] )
      {
       case 'd':// 0xA and 0xD 0xA (UNIX) to 0xD 0xA (DOS)
		// error: writing buffer can owerflow - example input == 64k

		// output is longer => 2 buffers needed
		// copy content of the buffer
	   //		for(r=0; r<sourcelength; r++)
	   // 		 rbuf[r]=buf[r];

		r=w=0;

		// rbuf ... buffer for reading input (size+Const)
		// buf  ... output buffer for result (size)
		while( r<sourcelength )
		{
		 if( buf[r]==0xA && buf[r-1]!=0xD )
		  {
		   rbuf[w++]=0xD;
		   rbuf[w++]=buf[r++];  // writes 0xA
		  }
		 else
		  {
		   rbuf[w++]=buf[r++];
		  }
		}

		SaveResult(w,outhandle,rbuf);

		break;

       case 'u': // 0xD 0xA (DOS) to 0xA (UNIX)

		r=w=0;


		while( r<sourcelength )
		{
		 if( buf[r]==0xD && buf[r+1l]==0xA )
		 {
		  r++;
		 }
		 else
		 {
		  buf[w++]=buf[r++];
		 }
		}

		printf("(%lu->%lu)",r,w);

		SaveResult(w,outhandle,buf);

		break;

       case 'e': // UNIX ESC fuc
		 while( r<sourcelength )
		  {
		   if( buf[r]=='' ) // ESC
		    {
		     while( buf[r]!='m' && r<sourcelength ) r++;
		     if( r<sourcelength )
		      {
		       r++;
		       if( buf[r]=='' )
			{
			 if( r<sourcelength ) r++;
			 else
			  {
			   puts("Success!");close(inhandle);
			   close(outhandle); return 0;
			  }
			}
		      }
		     else {puts("Success!");close(inhandle); close(outhandle); return 0;}
		    }
		   else // ESC
		    {
		     buf[w]=buf[r]; w++;
		     if( buf[r]!='' ) r++;
		    }
		  } // while

		 SaveResult(w,outhandle,buf);

		break;

       case 'h': // HTML fuc, in inhtml is depth in tags <<><< => 3

		 if(argc != 5)
		 {
		  printf("\n Error: parameter missing (probably <cols>)..."
			"\nBye!\n"
			);
		  return 0;
		 }

		 w=r=0;

		 cutTo = atoi(argv[4]);

		 inhtml=FALSE;
		 while( r<sourcelength )
		  {
		   switch( buf[r] )
		   {
		    case 0xD: column=0; break;
		    case '<': inhtml++;  // opening html tag
			      break;
		    case '>': inhtml--;  // closing html tag
			      break;
		    default: column++;
		   }

		   if( !inhtml )
		    {
		      if( buf[r]!='>') // have to jump across >
		       buf[w++]=buf[r++];
		      else
		       r++;
		    }
		   else r++;

		   if( (buf[r-1]==' ') && (column >=cutTo) )
		    { buf[w-1]=0xA; column=0; } // Unix line breaker
		  } // while sourcelength

		 SaveResult(w,outhandle,buf);

		break;

       case 's': // split file

		 if(argc != 5)
		 {
		  printf("\n Error: parameter missing (probably <frag>)..."
			"\nBye!\n"
			);
		  return 0;
		 }

                 // frag to int
#ifdef LINUXIK
                 w=atoi(argv[4]);
                 if(verbose)
                     printf("(frag:%i)",w);

#else
                 printf("\n Error: string2int not implemented..."
                        "\nBye!\n"
                       );
                 return 0;
#endif
                 // w           ... frag size
                 // r           ... how much was read
                 // column      ... bytes to write
                 // ii          ... bytes written
                 // c           ... file number
                 r=sourcelength;
                 ii=0;
                 c=0;

                 // close old
                 close(outhandle); unlink(outfile);
                 // create new file
                 sprintf(outfile,"%s.%i",argv[3],c++);
                 if((outhandle= creat(outfile, S_IREAD |S_IWRITE))< 0) { perror("\n Error: unable to create output file"); return 1; }

                 do
                 {
                     if(w>r)
                     {
                         column=r;
                         r=0;
                     }
                     else
                     {
                         column=w;
                         r-=w;
                     }

                     SaveResult(column,outhandle,(buf+ii));
                     ii+=column;
                     if(r)
                     {
                      // close old
                      close(outhandle);
                      // create new file
                      sprintf(outfile,"%s.%i",argv[3],c++);
                       printf("\n %i %i %s",column,ii,outfile);
                      if((outhandle= creat(outfile, S_IREAD |S_IWRITE))< 0) { perror("\n Error: unable to create output file"); return 1; }
                     }
                 }
                 while(r);

                break;


       case 't': // trim lines

		if(argc != 5)
		{
		 printf("\n Error: parameter missing (probably <cols>)..."
			"\nBye!\n"
		       );
		 return 0;
		}

		w=r=0;

		cutTo = atoi(argv[4]);

		while( r<sourcelength )
		{
		 switch( buf[r] )
		 {
		  case 0xD:
			   column=0;
			   break;
		  default:
			   column++;
		 }

		 buf[w++]=buf[r++];

		 if( (buf[r-1]==' ') && (column >=cutTo) )
		 {
		  buf[w-1]=0xA;
		  column=0;
		 } // 0xA
		} // while sourcelength

		SaveResult(w,outhandle,buf);

		break;

       case 'c':// t602 to wri

		//Znaky T602:
		//  tucne
		//  kurziva
		//  horni indx
		//  dolni indx
		//  siroke
		//  vysoke
		//  velke
		//  podtrzene
		// dalsi ®, ¯
		while( r<sourcelength )
		 {
		  if( buf[r]==''|| buf[r]==''|| buf[r]==''|| buf[r]==''||
		      buf[r]==''|| buf[r]==''|| buf[r]==''|| buf[r]==''||
		      buf[r]==''|| buf[r]=='®'|| buf[r]=='¯' )
		   {
		    r++;
		   }
		  else
		   {
		    if( (c=FindMe(buf[r],czT602)) <= CZARRAYSIZE )
		     { r++; buf[w++]=czWRI[c]; }
		    else { buf[w++]=buf[r++]; }
		   }
		 }

		 SaveResult(w,outhandle,buf);

	       break;

       default:
		printf("\nError: unknown option \"%s\""
		       "\nBye!"
		       ,argv[1]
		      );
		return 0;
      } // case
     }
     else
     {
      // LONG option eg. -wk

      char *srcCoding,
	   *dstCoding;

      // Czech options:
      // w ... Win-1250
      // k ... kamenici   (WordFugue)
      // i ... ISO Latin2 (Linux)
      // l ... latin2     (T602)
      // a ... ascii - no hacky & carky

      // source coding
      switch(argv[1][1])
      {
       case 'w':
       case 'W':
		srcCoding=czWRI;
		break;
       case 'k':
       case 'K':
		srcCoding=czWF;
		break;
       case 'i':
       case 'I':
		srcCoding=czLINX;
		break;
       case 'l':
       case 'L':
		srcCoding=czT602;
		break;
       case 'a':
       case 'A':
		srcCoding=czTXT;
		break;
       default:
		printf("\nError: unknown option \"%s\""
		       "\nBye!"
		       ,argv[1]
		      );
		return 0;
      }
      // destination coding
      switch(argv[1][2])
      {
       case 'w':
       case 'W':
		dstCoding=czWRI;
		break;
       case 'k':
       case 'K':
		dstCoding=czWF;
		break;
       case 'i':
       case 'I':
		dstCoding=czLINX;
		break;
       case 'l':
       case 'L':
		dstCoding=czT602;
		break;
       case 'a':
       case 'A':
		dstCoding=czTXT;
		break;
       default:
		printf("\nError: unknown option \"%s\""
		       "\nBye!"
		       ,argv[1]
		      );
		return 0;
      }



      // decide if HTML conversion is wanted or is not
      switch(argv[1][3])
      {
       case 0:
	      ChangeCZ( srcCoding, dstCoding, buf, outhandle, sourcelength);
	      break;
       case 'h':
       case 'H':
	      HtmlCZ( srcCoding, dstCoding, buf, outhandle, sourcelength);
	      break;
       default:
		printf("\nError: unknown option \"%s\""
		       "\nBye!"
		       ,argv[1]
		      );
		return 0;
      }

     } // else long option
    } // else buffer successfuly alocated
  } // else work ( no help )



  //- Core ----------------------------------------------------------------



  if( argv[1][1]=='d' ) farfree(rbuf);
  farfree(buf);



  close(inhandle);
  close(outhandle);



  // if file names are the same -> delete old and rename new to the name
  if( !samefile )
   {
    unlink( argv[2] );

    if
     (rename("dconvert.swp", argv[2]) == 0);
    else
     perror("\n rename");
   }


 if( verbose )
 {
  printf(
	   #ifndef LINUXIK
	    "\nMem: %luB"
	   #endif 
	    "\nBye!\n"
	   #ifndef LINUXIK
	    ,(unsigned long)farcoreleft()
	   #endif 
	);    
 }
 else
  printf(" Bye!\n");

 return 0;

}

//---------------------------------------------------------------------------

int FindMe( char itsiam, char *where )
// returns CZARRAYSIZE if nothing was found
{
 int i;
 for(i=0;i<=CZARRAYSIZE;i++)
 {
  if( itsiam == where[i] )
  return i;
 }

 return (CZARRAYSIZE+1);
}

//---------------------------------------------------------------------------

void SaveResult( long w, 		// bytes about to write
		 int  outhandle,
		 char huge *buf         // buffer
	       )
{
 // values in params can be destroyed (OK I have local copy)

 printf(" WR");

 while( w )
 {
  if( w<IO_BLOCK_SIZE )
  {
   if( write( outhandle, buf, w )==-1 ) { perror("Error: "); exit(0); }
   w=0;
  }
  else
  {
   if( write( outhandle, buf, IO_BLOCK_SIZE )==-1 ) { perror("Error: "); exit(0); }
   w-=IO_BLOCK_SIZE;
   buf+=IO_BLOCK_SIZE;
  }

  printf(".");
 }

 printf("OK,");
}


//---------------------------------------------------------------------------

int ChangeCZ( char *fromCZ, char *toCZ, char huge *buf, int outhandle,
	      long sourcelength )
// returns TRUE if error occured
{
 long r=0,
      w=0;
 int  c;



 while( r<sourcelength )
 {
  if( (c=FindMe(buf[r],fromCZ)) <= CZARRAYSIZE )
  {
   r++;
   buf[w++]=toCZ[c];
  }
  else
  {
   r++;
   w++;
  }
 }

 SaveResult(w,outhandle,buf);

 return 0;
}

//---------------------------------------------------------------------------

int HtmlCZ( char *fromCZ, char *toCZ, char huge *buf, int outhandle,
               long sourcelength )
// returns TRUE if error occured
{
 long column=0,
      r=0,
      w=0;
 int  c,
      inhtml=FALSE;

 while( r<sourcelength )
 {
  switch( buf[r] )
  {
   case 0xD: column=0; break;
   case '<': inhtml++;  // opening html tag
             break;
   case '>': inhtml--;  // closing html tag
             break;
   default: column++;
  }

  if( !inhtml )
  {
   if( buf[r]!='>')
   {
    if( ((c=FindMe(buf[r],fromCZ)) <= CZARRAYSIZE))
    { r++; buf[w++]=toCZ[c]; }
    else
     buf[w++]=buf[r++];
   }
   else
    r++;
  }
  else
   r++;
  if( (buf[r-1]==' ') && (column >=CUTTO) )
  #ifdef LINUXIK
   { buf[w-1]=' '; column=0; }// 0xD, 0xA
  #else
   { buf[w-1]='\n'; column=0; }// 0xD, 0xA
  #endif
 }// while

 SaveResult(w,outhandle,buf);

 return 0;
}

//- EOF --------------------------------------------------------------------------
