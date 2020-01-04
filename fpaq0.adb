-- fpaq0 - Stationary order 0 file compressor.
-- ADA port by Kaido Orav, 2019
-- source: http://mattmahoney.net/dc/fpaq0.cpp
with Ada.Text_IO, Ada.Integer_Text_IO;
use Ada.Text_IO, Ada.Integer_Text_IO;
with Ada.Command_line; use Ada.Command_Line;
with Ada.Sequential_IO;
with Ada.Strings.Bounded;

procedure Fpaq0 is
   type bit is range 0..1;
   type cxt is range 1..512; -- context range
   type bitcount is mod 2**16; -- bitcount range
   type U32 is mod 2**32; -- unsigned 32bit integer
   type prediction is range 0..4096; -- prediction range
   type mode is (COMPRESS, DECOMPRESS);
   function Shift_Right (Val : U32; Count : Natural) return U32;
   pragma Import (Intrinsic, Shift_Right);
   function Shift_Left (Val : U32; Count : Natural) return U32;
   pragma Import (Intrinsic, Shift_Left);
   package Char_IO is new Ada.Sequential_IO (Character);
   use Char_IO;
   package SB is new Ada.Strings.Bounded.Generic_Bounded_Length(Max=>256);
   use SB;
   arg:SB.Bounded_String ;
   infile:SB.Bounded_String;
   outfile:SB.Bounded_String;
   input,output:Char_IO.File_Type;
   compressionMode:mode;
   x,x1:U32:=0;
   x2:U32:=16#ffffffff#;
   countbytes:U32:=0;
    -- Array of bitcounts inizialized to 0
   ct : array (cxt,bit) of bitcount:=(others => (others => 0));
    -- Context inizialized to 1
   context : cxt:=1;
   c: Character; -- For encoding and decoding
   c1:U32; -- For decoding char
   -- Predict
   function p return prediction is
   begin
      return prediction(4096*(U32(ct(context,1))+1)/(U32(ct(context,0))+U32(ct(context,1))+2));
   end p;
   -- Update bit
   procedure update(y:bit) is
      c:Integer;
   begin
      ct(context,y):=ct(context,y)+1;
      if (ct(context,y) = 65535) then
         ct(context,0) :=bitcount(Shift_Right(U32(ct(context,0)) ,1));
         ct(context,1) :=bitcount(Shift_Right(U32(ct(context,1)) ,1));
      end if;
      c:=(cxt'Pos(context)*2+bit'Pos(y));
      if (c >= 512) then
         context:=1;
      else
         context:=cxt(c);
      end if;
   end update;
   -- Encode bit
   procedure encode(y:bit) is
      xmid:U32:=0;
      outc:Character;
   begin
      -- Update the range
      xmid:=x1+Shift_Right(x2-x1,12)*U32(p);
      if y=1 then
         x2:=xmid;
      else
         x1:=xmid+1;
      end if;
      update(y);
      -- Shift equal MSB's out
      while (((x1 xor x2) and 16#ff000000#)=0) loop
         outc:=Character'Val(Shift_Right(x2,24));
         Write(File=>output, Item=>outc);
         x1:=Shift_Left(x1,8);
         x2:=Shift_Left(x2,8)+255;
      end loop;
   end encode;
   -- Decode bit
   function decode return bit is
      y:bit:=0;
      xmid:U32:=0;
   begin
      --// Update the range
      xmid:=x1+Shift_Right(x2-x1,12)*U32(p);
      if (x<=xmid) then
         y:=1;
         x2:=xmid;
      else
         x1:=xmid+1;
      end if;
      update(y);
      --// Shift equal MSB's out
      while (((x1 xor x2) and 16#ff000000#)=0) loop
         x1:=Shift_Left(x1,8);
         x2:=Shift_Left(x2,8)+255;
         -- int c=getc(archive);
         if (not End_Of_File(input)) then
            Read(File=>input, Item=>c);
         else
            c:=Character'Val(0);
         end if;
         x:=Shift_Left(x,8)+Character'Pos(c);
      end loop;
      return y;
   end decode;
begin
   --  Parse command line
   if (Argument_Count/=3) then
      Put_Line("Usage: c/d in out");
      return;
   end if;
   arg:=SB.To_Bounded_String(Argument(1));
   infile:=SB.To_Bounded_String(Argument(2));
   outfile:=SB.To_Bounded_String(Argument(3));
   -- Set mode
   if arg="c" then
      compressionMode:=COMPRESS;
   elsif arg="d" then
      compressionMode:=DECOMPRESS;
    else
      Put_Line("Usage: c/d in out");
      return;
   end if;
   -- Try to open files
   begin
   Open(File=>input, Mode=>In_File, Name=>SB.To_String(infile));
   exception
      when others =>
         Put_Line(Standard_Error,
                  "Can not open the file " & SB.To_String(infile));
         return;
   end;
   begin
      Create (File=>output, Mode=>Out_File, Name=>SB.To_String(outfile));
   exception
      when others =>
         Put_Line(Standard_Error,
                  "Can not create a file " & SB.To_String(outfile));
         return;
   end;
   --Compress
   if compressionMode=COMPRESS then
    Put_Line("Compress");
      while (not End_Of_File(input)) loop
         Read(File=>input, Item=>c);
         encode(0);
         for i in reverse 0 .. 7 loop
            encode(bit(Shift_Right(U32(Character'Pos(c)),i) and 1));
         end loop;
          countbytes:=countbytes+1;
      end loop;
      encode(1);
      while (((x1 xor x2) and 16#ff000000#)=0) loop
         c:=Character'Val(Shift_Right(x2,24));
         Write(File=>output, Item=>c);
         x1:=Shift_Left(x1,8);
         x2:=Shift_Left(x2,8)+255;
      end loop;
      c:=Character'Val(Shift_Right(x2,24));
      Write(File=>output, Item=>c);
   end if;
   -- Decompress file
   if compressionMode=DECOMPRESS then
      Put_Line("Decompress");
      for i in 0 .. 3 loop
         if (not End_Of_File(input)) then
            Read(File=>input, Item=>c);
         else
            c:=Character'Val(0);
         end if;
         x:=Shift_Left(x,8)+Character'Pos(c);
      end loop;
      while (decode=0) loop
         c1:=1;
         while (c1<256) loop
            c1:=c1+c1+U32(decode);
         end loop;
         c:=Character'Val(c1-256);
         Write(File=>output, Item =>c);
         countbytes:=countbytes+1;
      end loop;
   end if;
   Put_Line("Processed bytes:");
   Put(Integer(countbytes),5);
end Fpaq0;
