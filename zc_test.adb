-- Test/demo program for Z_Compression: creates a Zlib-format compressed version of a file
-- Copyright (C) by Pragmada Software Engineering
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses

with Ada.Command_Line;
with Ada.Sequential_IO;

with Z_Compression;

procedure ZC_Test is
   function Out_Of_Data return Boolean;

   subtype Byte_Value is Z_Compression.Byte_Value;

   function Next return Byte_Value with
      Pre => not Out_Of_Data or else raise Z_Compression.Data_Exhausted;

   procedure Put (Byte : in Byte_Value);

   procedure Compress is new Z_Compression.Compress (Out_Of_Data => Out_Of_Data, Next => Next, Put => Put);

   package Byte_IO is new Ada.Sequential_IO (Element_Type => Byte_Value);

   Input  : Byte_IO.File_Type;
   Output : Byte_IO.File_Type;

   function Out_Of_Data return Boolean is
      (Byte_IO.End_Of_File (Input) );

   function Next return Byte_Value is
      Byte : Byte_Value;
   begin -- Next
      Byte_IO.Read (File => Input, Item => Byte);

      return Byte;
   end Next;

   procedure Put (Byte : in Byte_Value) is
      -- Empty
   begin -- Put
      Byte_IO.Write (File => Output, Item => Byte);
   end Put;
begin -- ZC_Test
   Byte_IO.Open (File => Input, Mode => Byte_IO.In_File, Name => Ada.Command_Line.Argument (1) );
   Byte_IO.Create (File => Output, Mode => Byte_IO.Out_File, Name => Ada.Command_Line.Argument (1) & ".zz");
   Compress (Method => Z_Compression.Deflate_3);
   Byte_IO.Close (File => Input);
   Byte_IO.Close (File => Output);
end ZC_Test;
