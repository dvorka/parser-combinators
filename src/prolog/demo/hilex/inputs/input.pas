{-----------------------------------------------------------------------------
                           Program NOTEDIT

                          Martin Dvorak  I8
                                1995
-----------------------------------------------------------------------------}
{$A+,B-,D+,E+,F-,G-,I+,L+,N+,O-,P-,Q+,R+,S+,T-,V+,X+,Y+}
{$M 16384,0,655360}
unit PTisk;

INTERFACE

uses Crt,Dos;

var registr        : Registers;
    i              : integer;

 { AH sluzba
   DX cislo tiskarny }

procedure Inicializuj_tiskarnu;
procedure Posli_znak( znak:byte );
procedure Roluj_min;
procedure Hlava_home;
procedure Nastav_graf_rezim_960;
procedure Odroluj_sektor;
procedure Odroluj_papir;


IMPLEMENTATION
{----------------------------------------------------------------------------------}
procedure Inicializuj_tiskarnu;
begin

     registr.ah := $01;
     registr.dx := $00;{ cislo tiskarny }

     intr($17,registr);
end;
{----------------------------------------------------------------------------------}
procedure Posli_znak( znak:byte );
begin

     registr.ah := $00;
     registr.dx := $00;
     registr.al := znak;

     intr($17,registr);

end;
{----------------------------------------------------------------------------------}
procedure Roluj_min;
begin
 posli_znak( 10 );
end;
{----------------------------------------------------------------------------------}
procedure Hlava_home;
begin
 Posli_znak( $1b );Posli_znak( $3c );
end;
{----------------------------------------------------------------------------------}
procedure Nastav_graf_rezim_960;
begin
 Posli_znak( $1b );Posli_znak( $4c );{ 960 $4c ,480 $4b}
 Posli_znak( 127 );Posli_znak( $02 );
end;
{----------------------------------------------------------------------------------}
procedure Odroluj_sektor;
var i : byte;
begin
for i := 1 to 8 do
 begin
    { nastav roztec vertikalni 1/216'' }
    posli_znak( $1b );
    posli_znak( { $4a}$33 );
    posli_znak(  22 );
    { roluj - vertikalni tabulator }
    posli_znak( 11 );
 end;

    posli_znak( 13 );
    posli_znak( 13 );
    posli_znak( 13 );
end;
{----------------------------------------------------------------------------------}
procedure Odroluj_papir;
begin
 Posli_znak ( 12 );
end;
{----------------------------------------------------------------------------------}
end.
