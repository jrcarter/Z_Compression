-- Standalone version of the Zlib Deflate compression and Inflate decompression algorithm, derived from de Montmollin's Zip-Ada
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com

separate (Z_Compression.Decompress)
package body UnZ_IO is
   -- Input buffer
   Inbuf   : Byte_Buffer (0 .. Inbuf_Size - 1);
   Inpos   : Integer := 0; --  Index of next byte to obtain from Inbuf
   Readpos : Integer := -1; -- Last filled index in Inbuf

   function EOF return Boolean is
      (Out_Of_Data and Inpos > Readpos);

   procedure Read_Buffer is
      -- Empty
   begin
      Fill : for I in Inbuf'Range loop
         exit Fill when Out_Of_Data;

         Inbuf (I) := Next;
         Readpos := I;
      end loop Fill;

      Inpos := 0;
   end Read_Buffer;

   function Next_Byte return Byte_Value is
      Bt : Byte_Value;
   begin -- Next_Byte
      if Inpos > Readpos then
         Read_Buffer;
      end if;

      Bt := Inbuf (Inpos);
      Inpos := Inpos + 1;

      return Bt;
   end Next_Byte;

   package body Bit_Buffer is
      B : Interfaces.Unsigned_32 := 0;
      K : Integer     := 0;

      use type Interfaces.Unsigned_32;

      procedure Need (N : Natural) is
         pragma Inline (Need);
      begin
         while K < N loop
            B := B or Interfaces.Shift_Left (Interfaces.Unsigned_32 (Next_Byte), K);
            K := K + 8;
         end loop;
      end Need;

      procedure Dump (N : Natural) is
      begin
         B := Interfaces.Shift_Right (B, N);
         K := K - N;
      end Dump;

      procedure Dump_To_Byte_Boundary is
      begin
         Dump (K mod 8);
      end Dump_To_Byte_Boundary;

      function Read_U32 (N : Natural) return Interfaces.Unsigned_32 is
      begin
         Need (N);
         return B and (Interfaces.Shift_Left (Interfaces.Unsigned_32'(1), N) - 1);
      end Read_U32;

      function Read (N : Natural) return Integer is
      begin
         return Integer (Read_U32 (N));
      end Read;

      function Read_And_Dump (N : Natural) return Integer is
         Res : Integer;
      begin
         Res := Read (N);
         Dump (N);
         return Res;
      end Read_And_Dump;

      function Read_And_Dump_U32 (N : Natural) return Interfaces.Unsigned_32 is
         Res : Interfaces.Unsigned_32;
      begin
         Res := Read_U32 (N);
         Dump (N);
         return Res;
      end Read_And_Dump_U32;
   end Bit_Buffer;

   procedure Flush (X : Natural) is
      -- Empty;
   begin -- Flush
      Put_All : for I in 0 .. X - 1 loop
         Put (Byte => Slide (I) );
      end loop Put_All;
   end Flush;

   procedure Flush_If_Full (W : in out Integer) is
   begin
      if W = Wsize then
         Flush (Wsize);
         W := 0;
      end if;
   end Flush_If_Full;

   ----------------------------------------------------
   -- Reproduction of sequences in the output slide. --
   ----------------------------------------------------

   --  Internal:

   procedure Adjust_To_Slide (
                              Source         : in out Integer;
                              Remain         : in out Natural;
                              Part           :    out Integer;
                              Index          :        Integer)
   is
      pragma Inline (Adjust_To_Slide);
   begin
      Source := Source mod Wsize;
      --  source and index are now in 0 .. WSize-1
      if Source > Index then
         Part := Wsize - Source;
      else
         Part := Wsize - Index;
      end if;
      --  NB: part is in 1..WSize (part cannot be 0)
      if Part > Remain then
         Part := Remain;
      end if;
      --  Now part <= remain
      Remain := Remain - Part;
      --  NB: remain cannot be < 0
   end Adjust_To_Slide;

   procedure Copy_Range (Source, Index : in out Natural; Amount : Positive) is
      pragma Inline (Copy_Range);
   begin
      if abs (Index - Source) < Amount then
         --  if source >= index, the effect of copy is just like the non-overlapping case
         for Count in reverse 1 .. Amount loop
            Slide (Index) := Slide (Source);
            Index  := Index  + 1;
            Source := Source + 1;
         end loop;
      else  --  non-overlapping -> copy slice
         Slide (Index .. Index + Amount - 1) :=
            Slide (Source .. Source + Amount - 1);
         Index  := Index  + Amount;
         Source := Source + Amount;
      end if;
   end Copy_Range;

   --  The copying routines:

   procedure Copy (Distance, Copy_Length : Natural; Index : in out Natural) is
      Source, Part, Remain : Integer;
   begin
      Source := Index - Distance;
      Remain := Copy_Length;
      loop
         Adjust_To_Slide (Source, Remain, Part, Index);
         Copy_Range (Source, Index, Part);
         Flush_If_Full (Index);
         exit when Remain = 0;
      end loop;
   end Copy;
end UnZ_IO;
