-- Z_Decomp: decompress a Deflate-compressed file (raw or zlib format)
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com

with Ada.Command_Line;
with Ada.Directories;
with Ada.Sequential_IO;
with Ada.Text_IO;

with Adler_32_Checksums;

with Z_Compression;

procedure Z_Decomp is
   function Out_Of_Data return Boolean;

   subtype Byte_Value is Z_Compression.Byte_Value;

   function Next return Byte_Value with
      Pre => not Out_Of_Data or else raise Z_Compression.Data_Exhausted;

   procedure Put (Byte : in Byte_Value);

   procedure Decompress is new Z_Compression.Decompress (Out_Of_Data => Out_Of_Data, Next => Next, Put => Put);

   package Byte_IO is new Ada.Sequential_IO (Element_Type => Byte_Value);

   Input  : Byte_IO.File_Type;
   Output : Byte_IO.File_Type;
   First  : Boolean := True; -- First call to next
   Check  : Adler_32_Checksums.Checksum_Info;
   Read   : Natural := 0;

   Name : constant String  := Ada.Command_Line.Argument (1);
   Zlib : constant Boolean := Name'Length > 3 and then Name (Name'Last - 2 .. Name'Last) = ".zz";
   Size : constant Natural := Integer (Ada.Directories.Size (Name) ) - (if Zlib then 6 else 0); -- 2 for zlib header; 4 for Adler
                                                                                                -- checksum

   function Out_Of_Data return Boolean is
      (Byte_IO.End_Of_File (Input) or Read >= Size);

   function Next return Byte_Value is
      Byte : Byte_Value;

      Invalid_Zlib_Format : exception;

      use type Byte_Value;
   begin -- Next
      if First and Zlib then
         Byte_IO.Read (File => Input, Item => Byte);

         if Byte /= 16#78# then
            raise Invalid_Zlib_Format;
         end if;

         Byte_IO.Read (File => Input, Item => Byte);
         First := False;
      end if;

      Byte_IO.Read (File => Input, Item => Byte);
      Read := Read + 1;

      return Byte;
   exception -- Next
   when Byte_IO.End_Error =>
      raise Z_Compression.Data_Exhausted;
   end Next;

   procedure Put (Byte : in Byte_Value) is
      -- Empty
   begin -- Put
      Byte_IO.Write (File => Output, Item => Byte);

      if Zlib then
         Adler_32_Checksums.Update (Info => Check, Byte => Byte);
      end if;
   end Put;
begin -- Z_Decomp
   Byte_IO.Open (File => Input, Mode => Byte_IO.In_File, Name => Name);
   Byte_IO.Create (File => Output, Mode => Byte_IO.Out_File, Name => Name & ".zdc");
   Decompress;

   if Zlib then
      Checksum : declare
         Sum : Adler_32_Checksums.Checksum_List;

         use type Adler_32_Checksums.Checksum_List;
      begin -- Checksum
         Fill : for Byte of Sum loop
            Byte_IO.Read (File => Input, Item => Byte);
         end loop Fill;

         if Sum /= Adler_32_Checksums.Checksum (Check) then
            Ada.Text_IO.Put_Line (Item => "Incorrect checksum");
         end if;
      end Checksum;
   end if;

   Byte_IO.Close (File => Input);
   Byte_IO.Close (File => Output);
end Z_Decomp;
