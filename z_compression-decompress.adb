-- Standalone version of the Zlib Deflate compression and Inflate decompression algorithm, derived from de Montmollin's Zip-Ada
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com

separate (Z_Compression)
procedure Decompress is
   subtype Long_Integer is Interfaces.Integer_64; -- Standard.Long_Integer is not portable
   use type Long_Integer;

   package Huffman is
      --  Huffman tree generation and deletion.
      --  Originally from info-zip's unzip, data structure rewritten by G. de Montmollin

      type HufT_Table;
      type P_HufT_Table is access HufT_Table;

      type HufT is record
         Extra_Bits : Natural;
         Bits       : Natural;
         N          : Natural;
         Next_Table : P_HufT_Table;
      end record;

      Invalid : constant := 99; -- invalid value for extra bits

      type HufT_Table is array (Natural range <>) of HufT;

      --  Linked list just for destroying Huffman tables

      type Table_List;
      type P_Table_List is access Table_List;

      type Table_List is record
         Table : P_HufT_Table;
         Next  : P_Table_List;
      end record;

      type Length_Array is array (Integer range <>) of Natural;

      Empty : constant Length_Array (1 .. 0) := (others => 0);

      --  Free huffman tables starting with table where t points to
      procedure HufT_Free (Tl : in out P_Table_List);

      --  Build huffman table from code lengths given by array b.all
      procedure HufT_Build (B               : Length_Array;
                            S               : Integer;
                            D, E            : Length_Array;
                            Tl              :    out P_Table_List;
                            M               : in out Integer;
                            Huft_Incomplete :    out Boolean);

      --  Possible exceptions occuring in huft_build
      Huft_Error,                    -- bad tree constructed
      Huft_Out_Of_Memory : exception; -- not enough memory
   end Huffman;
   use Huffman;

   package body Huffman is separate;

   type Byte_Buffer is array (Integer range <>) of aliased Byte_Value;
   subtype Zip_64_Data_Size_Type is Interfaces.Unsigned_64;
   use type Zip_64_Data_Size_Type;
   --  I/O Buffers: Size of input buffer
   Inbuf_Size : constant := 16#8000#;  --  (orig: 16#1000# B =  4 KiB)
   --  I/O Buffers: Size of sliding dictionary and output buffer
   Wsize : constant := 16#10000#;  --  (orig: 16#8000# B = 32 KiB)

   --  Sliding dictionary for unzipping, and output buffer as well
   Slide       : Byte_Buffer (0 .. Wsize);
   Slide_Index : Integer := 0;  --  Current Position in slide

   package UnZ_IO is
      function EOF return Boolean;

      package Bit_Buffer is
         --  Read at least n bits into the bit buffer, returns the n first bits
         function Read (N : Natural) return Integer;
         pragma Inline (Read);

         --  Dump n bits no longer needed from the bit buffer
         procedure Dump (N : Natural);
         pragma Inline (Dump);

         procedure Dump_To_Byte_Boundary;

         function Read_And_Dump (N : Natural) return Integer;
         pragma Inline (Read_And_Dump);

         function Read_And_Dump_U32 (N : Natural) return Interfaces.Unsigned_32;
         pragma Inline (Read_And_Dump_U32);
      end Bit_Buffer;

      procedure Flush (X : Natural);  --  directly from slide to output stream

      procedure Flush_If_Full (W : in out Integer);
      pragma Inline (Flush_If_Full);

      procedure Copy (Distance, Copy_Length : Natural; Index : in out Natural);
      pragma Inline (Copy);
   end UnZ_IO;

   package body UnZ_IO is separate;

   Lt_Count,     Dl_Count,
   Lt_Count_0,   Dl_Count_0,
   Lt_Count_Dyn, Dl_Count_Dyn,
   Lt_Count_Fix, Dl_Count_Fix : Long_Integer := 0;  --  Statistics of LZ codes per block

   procedure Inflate_Codes (Tl, Td : P_Table_List; Bl, Bd : Integer) is
      CT      : P_HufT_Table;       -- current table
      CT_Idx  : Natural;            -- current table's index
      Length  : Natural;
      E       : Integer;      -- table entry flag/number of extra bits
      W       : Integer := Slide_Index;  -- more local variable for slide index
      Literal : Byte_Value;
   begin
      --  inflate the coded data
      Main_Loop :
      while not UnZ_IO.EOF loop
         if Tl = null then
            raise Invalid_Data with
               "Null table list (on data decoding, Huffman tree for literals or LZ lengths)";
         end if;
         CT := Tl.Table;
         CT_Idx := UnZ_IO.Bit_Buffer.Read (Bl);
         loop
            E := CT (CT_Idx).Extra_Bits;
            exit when E <= 16;
            if E = Invalid then
               raise Invalid_Data;
            end if;

            --  then it's a literal
            UnZ_IO.Bit_Buffer.Dump (CT (CT_Idx).Bits);
            E := E - 16;
            CT := CT (CT_Idx).Next_Table;
            CT_Idx := UnZ_IO.Bit_Buffer.Read (E);
         end loop;

         UnZ_IO.Bit_Buffer.Dump (CT (CT_Idx).Bits);

         case E is
         when 16 =>      --  CT(CT_idx).N is a Literal (code 0 .. 255)
            Literal := Byte_Value (CT (CT_Idx).N);
            Slide (W) :=  Literal;
            W := W + 1;
            UnZ_IO.Flush_If_Full (W);

         when 15 =>      --  End of block (EOB, code 256)
            exit Main_Loop;

         when others =>  --  We have a length/distance code
            --  Get length of block to copy:
            Length := CT (CT_Idx).N + UnZ_IO.Bit_Buffer.Read_And_Dump (E);

            --  Decode distance of block to copy:
            if Td = null then
               raise Invalid_Data with
                  "Null table list (on data decoding, Huffman tree for LZ distances)";
            end if;
            CT := Td.Table;
            CT_Idx := UnZ_IO.Bit_Buffer.Read (Bd);
            loop
               E := CT (CT_Idx).Extra_Bits;
               exit when E <= 16;
               if E = Invalid then
                  raise Invalid_Data;
               end if;
               UnZ_IO.Bit_Buffer.Dump (CT (CT_Idx).Bits);
               E := E - 16;
               CT := CT (CT_Idx).Next_Table;
               CT_Idx := UnZ_IO.Bit_Buffer.Read (E);
            end loop;
            UnZ_IO.Bit_Buffer.Dump (CT (CT_Idx).Bits);
            UnZ_IO.Copy (
                         Distance    => CT (CT_Idx).N + UnZ_IO.Bit_Buffer.Read_And_Dump (E),
                         Copy_Length => Length,
                         Index       => W
                        );
         end case;
      end loop Main_Loop;

      Slide_Index := W;
   end Inflate_Codes;

   procedure Inflate_Stored_Block is -- Actually, nothing to inflate
      N : Integer;

      use type Interfaces.Unsigned_32;
   begin
      UnZ_IO.Bit_Buffer.Dump_To_Byte_Boundary;
      --  Get the block length and its complement
      N := UnZ_IO.Bit_Buffer.Read_And_Dump (16);
      if N /= Integer ( (not UnZ_IO.Bit_Buffer.Read_And_Dump_U32 (16) ) and 16#ffff#) then
         raise Invalid_Data;
      end if;
      while N > 0 and then not UnZ_IO.EOF loop
         --  Read and output the non-compressed data
         N := N - 1;
         Slide (Slide_Index) :=
            Byte_Value (UnZ_IO.Bit_Buffer.Read_And_Dump (8));
         Slide_Index := Slide_Index + 1;
         UnZ_IO.Flush_If_Full (Slide_Index);
      end loop;
   end Inflate_Stored_Block;

   Copy_Lengths_Literal : Length_Array (0 .. 30) :=
                            (3,  4,  5,  6,  7,  8,  9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
                             35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258, 0, 0);

   --  Extra bits for literal codes 257..285

   Extra_Bits_Literal : Length_Array (0 .. 30) :=
                          (0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
                           3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0, Invalid, Invalid);

   --  Copy offsets for distance codes 0..29 (30..31: deflate_e)

   Copy_Offset_Distance : constant Length_Array (0 .. 31) :=
                            (1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
                             257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145,
                             8193, 12289, 16385, 24577, 32769, 49153);

   --  Extra bits for distance codes

   Extra_Bits_Distance : constant Length_Array (0 .. 31) :=
                           (0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
                            7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13, 14, 14);

   Max_Dist : constant := 29;

   Length_List_For_Fixed_Block_Literals : constant Length_Array (0 .. 287) :=
                                            (0 .. 143 => 8, 144 .. 255 => 9, 256 .. 279 => 7, 280 .. 287 => 8);

   procedure Inflate_Fixed_Block is
      Tl,                        --   literal/length code table
      Td              : P_Table_List;            --  distance code table
      Bl, Bd          : Integer;          --  lookup bits for tl/bd
      Huft_Incomplete : Boolean;
   begin
      --  Make a complete, but wrong [why ?] code set (see Appnote: 5.5.2, RFC 1951: 3.2.6)
      Bl := 7;
      HufT_Build (Length_List_For_Fixed_Block_Literals, 257, Copy_Lengths_Literal,
                  Extra_Bits_Literal, Tl, Bl, Huft_Incomplete);
      --  Make an incomplete code set (see Appnote: 5.5.2, RFC 1951: 3.2.6)
      Bd := 5;
      begin
         HufT_Build ( (0 .. Max_Dist => 5), 0,
                     Copy_Offset_Distance, Extra_Bits_Distance,
                     Td, Bd, Huft_Incomplete);
      exception
      when Huft_Out_Of_Memory | Huft_Error =>
         Huft_Free (Tl);
         raise Invalid_Data;
      end;
      --  Decompress the block's data, until an end-of-block code.
      Inflate_Codes (Tl, Td, Bl, Bd);
      --  Done with this block, free resources.
      HufT_Free (Tl);
      HufT_Free (Td);
   end Inflate_Fixed_Block;

   Bit_Order_For_Dynamic_Block : constant array (0 .. 18) of Natural :=
                                   (16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15);

   procedure Inflate_Dynamic_Block is
      Lbits : constant := 9;
      Dbits : constant := 6;

      Current_Length             : Natural;
      Defined, Number_Of_Lengths : Natural;

      Tl,                             -- literal/length code tables
      Td : P_Table_List;            -- distance code tables

      CT     : P_HufT_Table;       -- current table
      CT_Idx : Natural;            -- current table's index

      Bl, Bd : Integer;                  -- lookup bits for tl/bd
      Nb : Natural;  -- number of bit length codes
      Nl : Natural;  -- number of literal length codes
      Nd : Natural;  -- number of distance codes

      --  literal/length and distance code lengths
      Ll : Length_Array (0 .. 288 + 32 - 1) := (others => 0);

      Huft_Incomplete : Boolean;

      procedure Repeat_Length_Code (Amount : Natural) is
      begin
         if Defined + Amount > Number_Of_Lengths then
            raise Invalid_Data;
         end if;
         for C in reverse 1 .. Amount loop
            Ll (Defined) := Current_Length;
            Defined := Defined + 1;
         end loop;
      end Repeat_Length_Code;
   begin
      --  Read in table lengths
      Nl := 257 + UnZ_IO.Bit_Buffer.Read_And_Dump (5);
      Nd :=   1 + UnZ_IO.Bit_Buffer.Read_And_Dump (5);
      Nb :=   4 + UnZ_IO.Bit_Buffer.Read_And_Dump (4);

      if Nl > 288 or else Nd > 32 then
         raise Invalid_Data;
      end if;

      --  Read in bit-length-code lengths for decoding the compression structure.
      --  The rest, Ll( Bit_Order( Nb .. 18 ) ), is already = 0
      for J in  0 .. Nb - 1  loop
         Ll (Bit_Order_For_Dynamic_Block (J)) := UnZ_IO.Bit_Buffer.Read_And_Dump (3);
      end loop;

      --  Build decoding table for trees--single level, 7 bit lookup
      Bl := 7;
      begin
         HufT_Build (
                     Ll (0 .. 18), 19, Empty, Empty, Tl, Bl, Huft_Incomplete
                    );
         if Huft_Incomplete then
            HufT_Free (Tl);
            raise Invalid_Data with "Incomplete code set for compression structure";
         end if;
      exception
      when others =>
         raise Invalid_Data with "Error when building tables for compression structure";
      end;

      --  Read in the compression structure: literal and distance code lengths
      Number_Of_Lengths := Nl + Nd;
      Defined := 0;
      Current_Length := 0;

      while  Defined < Number_Of_Lengths  loop
         if Tl = null then
            raise Invalid_Data with
               "Null table list (on compression structure)";
         end if;
         CT := Tl.Table;
         CT_Idx := UnZ_IO.Bit_Buffer.Read (Bl);
         UnZ_IO.Bit_Buffer.Dump (CT (CT_Idx).Bits);

         case CT (CT_Idx).N is
         when 0 .. 15 =>     --  Length of code for symbol of index 'defined', in bits (0..15)
            Current_Length := CT (CT_Idx).N;
            Ll (Defined) := Current_Length;
            Defined := Defined + 1;
         when 16 =>          --  16 means: repeat last bit length 3 to 6 times
            if Defined = 0 then
               --  Nothing in the Ll array has been defined so far. Then, current_length is
               --  (theoretically) undefined and cannot be repeated.
               --  This unspecified case is treated as an error by zlib's inflate.c.
               raise Invalid_Data with
                  "Illegal data for compression structure (repeat an undefined code length)";
            end if;
            Repeat_Length_Code (3 + UnZ_IO.Bit_Buffer.Read_And_Dump (2));
         when 17 =>          --  17 means: the next 3 to 10 symbols' codes have zero bit lengths
            Current_Length := 0;
            Repeat_Length_Code (3 + UnZ_IO.Bit_Buffer.Read_And_Dump (3));
         when 18 =>          --  18 means: the next 11 to 138 symbols' codes have zero bit lengths
            Current_Length := 0;
            Repeat_Length_Code (11 + UnZ_IO.Bit_Buffer.Read_And_Dump (7));
         when others =>      --  Shouldn't occur if this tree is correct
            raise Invalid_Data with
               "Illegal data for compression structure (values should be in the range 0 .. 18): "
               & Integer'Image (CT (CT_Idx).N);
         end case;
      end loop;
      --  Free the Huffman tree that was used for decoding the compression
      --  structure, which is contained now in Ll.
      HufT_Free (Tl);
      if Ll (256) = 0 then
         --  No code length for the End-Of-Block symbol, implies infinite stream!
         --  This case is unspecified but obviously we must stop here.
         raise Invalid_Data with "No code length for End-Of-Block symbol #256";
      end if;
      --  Build the decoding tables for literal/length codes
      Bl := Lbits;
      begin
         HufT_Build (Ll (0 .. Nl - 1), 257,
                     Copy_Lengths_Literal, Extra_Bits_Literal,
                     Tl, Bl, Huft_Incomplete);
         if Huft_Incomplete then
            HufT_Free (Tl);
            raise Invalid_Data with "Incomplete code set for literals/lengths";
         end if;
      exception
      when others =>
         raise Invalid_Data with "Error when building tables for literals/lengths";
      end;
      --  Build the decoding tables for distance codes
      Bd := Dbits;
      begin
         HufT_Build (
                     Ll (Nl .. Nl + Nd - 1), 0,
                     Copy_Offset_Distance, Extra_Bits_Distance,
                     Td, Bd, Huft_Incomplete
                    );
         if Huft_Incomplete then
            raise Invalid_Data with "Incomplete code set for distances";
         end if;
      exception
      when Huft_Out_Of_Memory | Huft_Error =>
         HufT_Free (Tl);
         raise Invalid_Data with "Error when building tables for distances";
      end;
      --  Decompress the block's data, until an end-of-block code.
      Inflate_Codes (Tl, Td, Bl, Bd);
      --  Done with this block, free resources.
      HufT_Free (Tl);
      HufT_Free (Td);
   end Inflate_Dynamic_Block;

   procedure Inflate_Block (Last_Block : out Boolean; Fix, Dyn : in out Long_Integer) is
      subtype Block_Type is Integer range 0 .. 3;
   begin
      Last_Block := Boolean'Val (UnZ_IO.Bit_Buffer.Read_And_Dump (1));
      case Block_Type'(UnZ_IO.Bit_Buffer.Read_And_Dump (2) ) is
      when 0 =>
         Inflate_Stored_Block;
      when 1 =>
         Inflate_Fixed_Block;
         Fix := Fix + 1;
      when 2 =>
         Inflate_Dynamic_Block;
         Dyn := Dyn + 1;
      when 3 =>
         raise Invalid_Data with "Inflate: Bad block type 3";
      end case;
   end Inflate_Block;

   Is_Last_Block : Boolean;
   Blocks, Blocks_Fix, Blocks_Dyn : Long_Integer := 0;
begin --Decompress
   loop
      Blocks := Blocks + 1;
      Inflate_Block (Is_Last_Block, Blocks_Fix, Blocks_Dyn);
      exit when Is_Last_Block;
   end loop;
   UnZ_IO.Flush (Slide_Index);
   Slide_Index := 0;
end Decompress;
