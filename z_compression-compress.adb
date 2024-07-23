-- Standalone version of the Zlib Deflate compression algorithm, derived from de Montmollin's Zip-Ada
-- Copyright (C) by Pragmada Software Engineering
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses

with Ada.Finalization;
with Ada.Unchecked_Deallocation;

with Z_Compression.LZ77;
with Z_Compression.Length_Limited_Huffman_Code_Lengths;

with System;

separate (Z_Compression)
procedure Compress (Method : In Method_ID; Zlib_Format : in Boolean := True) is
   use Interfaces;

   -- Buffered I/O - Byte granularity

   type Buffer_Info is record
      Data : Byte_Buffer (1 .. 10_000);
      Next : Natural := 1; -- Position of next unused byte (output) or next byte to process (input)
      Last : Natural := 0; -- Index of last position containing valid data for input
   end record;

   In_Buf  : Buffer_Info;
   Out_Buf : Buffer_Info;

   -- Output

   procedure Put_Byte (Byte : in Byte_Value); -- Global in out Out_Buf
   -- Put Byte into Out_Buf; flushing it if it becomes full

   procedure Flush_Byte_Buffer; -- Global in out Out_Buf
   -- Empty Out_Buf

   procedure Put_Byte (Byte : in Byte_Value) is
      -- Empty
   begin -- Put_Byte
      Out_Buf.Data (Out_Buf.Next) := Byte;
      Out_Buf.Next := Out_Buf.Next + 1;

      if Out_Buf.Next > Out_Buf.Data'Last then -- Buffer is full, so empty it
         Flush_Byte_Buffer;
      end if;
   end Put_Byte;

   procedure Flush_Byte_Buffer is
      -- Empty
   begin -- Flush_Byte_Buffer
      Dump : for Byte of Out_Buf.Data (1 .. Out_Buf.Next - 1) loop
         Put (Byte => Byte);
      end loop Dump;

      Out_Buf.Next := 1;
   end Flush_Byte_Buffer;

   -- Input

   function Buffer_Empty return Boolean is -- Global in In_Buf
      (In_Buf.Next > In_Buf.Last);

   function Next_Byte return Byte_Value with -- Global in out In_Buf
      Pre => not (Buffer_Empty and Out_Of_Data);
   -- Return the next byte in In_Buf, filling it if needed

   subtype U32 is Unsigned_32;

   use type U32;

   Adler_1 : U32 := 1; -- Adler-32 checksum
   Adler_2 : U32 := 0;

   function Next_Byte return Byte_Value is
      Modulus : constant := 65521;
   begin -- Next_Byte
      if In_Buf.Next > In_Buf.Last then
         In_Buf.Last := 0;

         Fill : for I in In_Buf.Data'Range loop
            exit Fill when Out_Of_Data;

            In_Buf.Data (I) := Next;
            In_Buf.Last := I;

            if Zlib_Format then
               Adler_1 := (Adler_1 + U32 (In_Buf.Data (I) ) ) rem Modulus;
               Adler_2 := (Adler_2 + Adler_1) rem Modulus;
            end if;
         end loop Fill;

         In_Buf.Next := 1;
      end if;

      In_Buf.Next := In_Buf.Next + 1;

      return In_Buf.Data (In_Buf.Next - 1);
   end Next_Byte;

   -- Bit code output buffer, for sending data at bit level
   -- Bits are inserted starting at the right (least
   -- significant bits). The width of bit_buffer must be at least 16 bits.

   Bit_Buffer : U32 := 0;
   Valid_Bits : Integer := 0; -- Number of valid bits in bit_buffer.  All bits above the last valid bit are always zero.

   procedure Flush_Bit_Buffer is
   begin
      while Valid_Bits > 0 loop
         Put_Byte (Byte_Value (Bit_Buffer and 16#FF#));
         Bit_Buffer := Shift_Right (Bit_Buffer, 8);
         Valid_Bits := Integer'Max (0, Valid_Bits - 8);
      end loop;
      Bit_Buffer := 0;
   end Flush_Bit_Buffer;

   -- Bit codes are at most 15 bits for Huffman codes,
   -- or 13 for explicit codes (distance extra bits).
   subtype Code_Size_Value is Integer range 1 .. 15;

   -- Send a value on a given number of bits.
   procedure Put_Code (Code : U32; Code_Size : Code_Size_Value) is
      -- Empty
   begin
      -- Put bits from code at the left of existing ones. They might be shifted away
      -- partially on the left side (or even entirely if valid_bits is already = 32).
      Bit_Buffer := Bit_Buffer or Shift_Left (Code, Valid_Bits);
      Valid_Bits := Valid_Bits + Code_Size;
      if Valid_Bits > 32 then
         -- Flush 32 bits to output as 4 bytes
         Put_Byte (Byte_Value (Bit_Buffer and 16#FF#));
         Put_Byte (Byte_Value (Shift_Right (Bit_Buffer,  8) and 16#FF#));
         Put_Byte (Byte_Value (Shift_Right (Bit_Buffer, 16) and 16#FF#));
         Put_Byte (Byte_Value (Shift_Right (Bit_Buffer, 24) and 16#FF#));
         Valid_Bits := Valid_Bits - 32;
         -- Empty buffer and put on it the rest of the code
         Bit_Buffer := Shift_Right (Code, Code_Size - Valid_Bits);
      end if;
   end Put_Code;

   -- Deflate, post LZ encoding, with Huffman encoding

   Invalid : constant := -1;

   subtype Huffman_Code_Range is Integer range Invalid .. Integer'Last;

   type Length_Code_Pair is record
      Bit_Length : Natural;                        -- Huffman code length, in bits
      Code       : Huffman_Code_Range := Invalid;  -- The code itself
   end record;

   procedure Invert (Lc : in out Length_Code_Pair) is
      pragma Inline (Invert);
      A : Natural := Lc.Code;
      B : Natural := 0;
   begin
      for I in 1 .. Lc.Bit_Length loop
         B := B * 2 + A mod 2;
         A := A / 2;
      end loop;
      Lc.Code := B;
   end Invert;

   -- The Huffman code set (and therefore the Huffman tree) is completely determined by
   -- the bit length to be used for reaching leaf nodes, thanks to two special
   -- rules (explanation in RFC 1951, section 3.2.2).
   -- So basically the process is the following:
   -- (A) Gather statistics (just counts) for the alphabet.
   -- (B) Turn these counts into code lengths, by calling Length_limited_Huffman_code_lengths.
   -- (C) Build Huffman codes (the bits to be sent) with a call to Prepare_Huffman_codes.
   -- In short:
   -- data -> (A) -> stats -> (B) -> Huffman codes' bit lengths -> (C) -> Huffman codes

   type Huff_Descriptor is array (Natural range <>) of Length_Code_Pair;

   type Bit_Length_Array is array (Natural range <>) of Natural;
   subtype Alphabet_Lit_Len is Natural range 0 .. 287;
   subtype Bit_Length_Array_Lit_Len is Bit_Length_Array (Alphabet_Lit_Len);
   subtype Alphabet_Dis is Natural range 0 .. 31;
   subtype Bit_Length_Array_Dis is Bit_Length_Array (Alphabet_Dis);

   type Deflate_Huff_Descriptors is record
      Lit_Len : Huff_Descriptor (0 .. 287); -- Tree descriptor for Literal, EOB or Length encoding
      Dis     : Huff_Descriptor (0 .. 31); -- Tree descriptor for Distance encoding
   end record;
   -- NB: Appnote: "Literal codes 286-287 and distance codes 30-31 are never used
   -- but participate in the Huffman construction."
   -- Setting upper bound to 285 for literals leads to invalid codes, sometimes.

   -- Copy bit length vectors into Deflate Huffman descriptors

   function Build_Descriptors (Bl_For_Lit_Len : Bit_Length_Array_Lit_Len; Bl_For_Dis : Bit_Length_Array_Dis)
   return Deflate_Huff_Descriptors is
      New_D : Deflate_Huff_Descriptors;
   begin
      for I in New_D.Lit_Len'Range loop
         New_D.Lit_Len (I) := (Bit_Length => Bl_For_Lit_Len (I), Code => Invalid);
      end loop;
      for I in New_D.Dis'Range loop
         New_D.Dis (I) := (Bit_Length => Bl_For_Dis (I), Code => Invalid);
      end loop;

      return New_D;
   end Build_Descriptors;

   type Count_Type is range 0 .. Unsigned_64'Last / 2 - 1;
   type Stats_Type is array (Natural range <>) of Count_Type;

   -- The following is a translation of Zopfli's OptimizeHuffmanForRle (v. 11-May-2016).
   -- Possible gain: shorten the compression header containing the Huffman trees' bit lengths.
   -- Possible loss: since the stats do not correspond anymore exactly to the data
   -- to be compressed, the Huffman trees might be suboptimal.
   -- Zopfli comment:
   -- Changes the population counts in a way that the consequent Huffman tree
   -- compression, especially its rle-part, will be more likely to compress this data
   -- more efficiently.
   procedure Tweak_For_Better_RLE (Counts : in out Stats_Type) is
      Length                        : Integer := Counts'Length;
      Stride                        : Integer;
      Symbol, Sum, Limit, New_Count : Count_Type;
      Good_For_Rle                  : array (Counts'Range) of Boolean := (others => False);
   begin
      -- 1) We don't want to touch the trailing zeros. We may break the
      -- rules of the format by adding more data in the distance codes.
      loop
         if Length = 0 then
            return;
         end if;
         exit when Counts (Length - 1) /= 0;
         Length := Length - 1;
      end loop;
      -- Now counts(0..length - 1) does not have trailing zeros.
      -- 2) Let's mark all population counts that already can be encoded with an rle code.
      -- Let's not spoil any of the existing good rle codes.
      -- Mark any seq of 0's that is longer than 5 as a good_for_rle.
      -- Mark any seq of non-0's that is longer than 7 as a good_for_rle.
      Symbol := Counts (0);
      Stride := 0;
      for I in 0 .. Length loop
         if I = Length or else Counts (I) /= Symbol then
            if (Symbol = 0 and then Stride >= 5) or else (Symbol /= 0 and then Stride >= 7) then
               for K in 0 .. Stride - 1 loop
                  Good_For_Rle (I - K - 1) := True;
               end loop;
            end if;
            Stride := 1;
            if I /= Length then
               Symbol := Counts (I);
            end if;
         else
            Stride := Stride + 1;
         end if;
      end loop;
      -- 3) Let's replace those population counts that lead to more rle codes.
      Stride := 0;
      Limit := Counts (0);
      Sum := 0;
      for I in 0 .. Length loop
         if I = Length or else Good_For_Rle (I)
            or else (I > 0 and then Good_For_Rle (I - 1))  -- Added from Brotli, item #1
         -- Heuristic for selecting the stride ranges to collapse.
            or else abs (Counts (I) - Limit) >= 4
         then
            if Stride >= 4 or else (Stride >= 3 and then Sum = 0) then
               -- The stride must end, collapse what we have, if we have enough (4).
               -- GdM: new_count is the average of counts on the stride's interval, upper-rounded.
               New_Count := Count_Type'Max (1, (Sum + Count_Type (Stride) / 2) / Count_Type (Stride));
               if Sum = 0 then
                  -- Don't make an all zeros stride to be upgraded to ones.
                  New_Count := 0;
               end if;
               for K in 0 .. Stride - 1 loop
                  -- We don't want to change value at counts(i),
                  -- that is already belonging to the next stride. Thus - 1.
                  Counts (I - K - 1) := New_Count;  -- GdM: Replace histogram value by averaged value.
               end loop;
            end if;
            Stride := 0;
            Sum := 0;
            if I < Length - 3 then
               -- All interesting strides have a count of at least 4, at least when non-zeros.
               -- GdM: limit is the average of next 4 counts, upper-rounded.
               Limit := (Counts (I) + Counts (I + 1) + Counts (I + 2) + Counts (I + 3) + 2) / 4;
            elsif I < Length then
               Limit := Counts (I);
            else
               Limit := 0;
            end if;
         end if;
         Stride := Stride + 1;
         if I /= Length then
            Sum := Sum + Counts (I);
         end if;
      end loop;
   end Tweak_For_Better_RLE;

   subtype Stats_Lit_Len_Type is Stats_Type (Alphabet_Lit_Len);
   subtype Stats_Dis_Type is Stats_Type (Alphabet_Dis);

   -- Phase (B) : we turn statistics into Huffman bit lengths.
   function Build_Descriptors (Stats_Lit_Len : Stats_Lit_Len_Type; Stats_Dis : Stats_Dis_Type) return Deflate_Huff_Descriptors is
      Bl_For_Lit_Len : Bit_Length_Array_Lit_Len;
      Bl_For_Dis     : Bit_Length_Array_Dis;

      procedure LLHCL_Lit_Len is new
         Length_Limited_Huffman_Code_Lengths (Alphabet_Lit_Len, Count_Type, Stats_Lit_Len_Type, Bit_Length_Array_Lit_Len, 15);
      procedure LLHCL_Dis is new
         Length_Limited_Huffman_Code_Lengths (Alphabet_Dis, Count_Type, Stats_Dis_Type, Bit_Length_Array_Dis, 15);

      Stats_Dis_Copy : Stats_Dis_Type := Stats_Dis;

      procedure Patch_Statistics_For_Buggy_Decoders is
         -- Workaround for buggy Info-Zip decoder versions.
         -- See "PatchDistanceCodesForBuggyDecoders" in Zopfli's deflate.c
         -- NB: here, we patch the statistics and not the resulting bit lengths,
         -- to be sure we avoid invalid Huffman code sets in the end.
         -- The decoding bug concerns Zlib v.<= 1.2.1, UnZip v.<= 6.0, WinZip v.<=10.0.
         Used : Natural := 0;
      begin
         for I in Stats_Dis_Copy'Range loop
            if Stats_Dis_Copy (I) /= 0 then
               Used := Used + 1;
            end if;
         end loop;
         case Used is
         when 0 =>  -- No distance code used at all (data must be almost random).
            Stats_Dis_Copy (0 .. 1) := (1, 1);
         when 1 =>
            if Stats_Dis_Copy (0) = 0 then
               Stats_Dis_Copy (0) := 1;  -- Now, code 0 and some other code have non-zero counts.
            else
               Stats_Dis_Copy (1) := 1;  -- Now, codes 0 and 1 have non-zero counts.
            end if;
         when others =>
            null;  -- No workaround needed when 2 or more distance codes are defined.
         end case;
      end Patch_Statistics_For_Buggy_Decoders;
   begin
      Patch_Statistics_For_Buggy_Decoders;
      LLHCL_Lit_Len (Stats_Lit_Len, Bl_For_Lit_Len);  -- Call the magic algorithm for setting
      LLHCL_Dis (Stats_Dis_Copy, Bl_For_Dis);         -- up Huffman lengths of both trees

      return Build_Descriptors (Bl_For_Lit_Len, Bl_For_Dis);
   end Build_Descriptors;

   -- Here is one original part in the Taillaule algorithm: use of basic
   -- topology (L1, L2 distances) to check similarities between Huffman code sets.

   --  System.Word_Size: 13.3(8): A word is the largest amount of storage
   --  that can be conveniently and efficiently manipulated by the hardware,
   --  given the implementation's run-time model.

   Min_Bits_32 : constant := Integer'Max (32, System.Word_Size);

   --  We define an Integer type which is at least 32 bits, but n bits
   --  on a native n (> 32) bits architecture.

   type Integer_M32 is range -(2 ** (Min_Bits_32 - 1) ) .. 2 ** (Min_Bits_32 - 1) - 1;
   subtype Natural_M32  is Integer_M32 range 0 .. Integer_M32'Last;
   subtype Positive_M32 is Integer_M32 range 1 .. Integer_M32'Last;

   -- Bit length vector. Convention: 16 is unused bit length (close to the bit length for the
   -- rarest symbols, 15, and far from the bit length for the most frequent symbols, 1).
   -- Deflate uses 0 for unused.
   subtype BL_Code is Integer_M32 range 1 .. 16;
   type BL_Vector is array (1 .. 288 + 32) of BL_Code; -- This is < 3000 bytes worse case

   function Convert (H : Deflate_Huff_Descriptors) return BL_Vector is
      Bv : BL_Vector;
      J  : Positive := 1;
   begin
      for I in H.Lit_Len'Range loop
         if H.Lit_Len (I).Bit_Length = 0 then
            Bv (J) := 16;
         else
            Bv (J) := Integer_M32 (H.Lit_Len (I).Bit_Length);
         end if;
         J := J + 1;
      end loop;
      for I in H.Dis'Range loop
         if H.Dis (I).Bit_Length = 0 then
            Bv (J) := 16;
         else
            Bv (J) := Integer_M32 (H.Dis (I).Bit_Length);
         end if;
         J := J + 1;
      end loop;
      return Bv;
   end Convert;

   -- L1 or Manhattan distance
   function L1_Distance (B1, B2 : BL_Vector) return Natural_M32 is
      S : Natural_M32 := 0;
   begin
      for I in B1'Range loop
         S := S + abs (B1 (I) - B2 (I));
      end loop;
      return S;
   end L1_Distance;

   -- L1, tweaked
   Tweak : constant array (BL_Code) of Positive_M32 :=
   -- For the origin of the tweak function, see "za_work.xls", sheet "Deflate".
   -- function f3 = 0.20 f1 [logarithmic] + 0.80 * identity
   -- NB: all values are multiplied by 100 for accuracy.
             (100, 255, 379, 490, 594, 694, 791, 885, 978, 1069, 1159, 1249, 1338, 1426, 1513, 1600);
   -- Neutral is:
   -- (100, 200, 300, 400, 500, 600, 700, 800, 900, 1000, 1100, 1200, 1300, 1400, 1500, 1600)

   function L1_Tweaked (B1, B2 : BL_Vector) return Natural_M32 is
      S : Natural_M32 := 0;
   begin
      for I in B1'Range loop
         S := S + abs (Tweak (B1 (I)) - Tweak (B2 (I)));
      end loop;
      return S;
   end L1_Tweaked;

   -- L2 or Euclidean distance
   function L2_Distance_Square (B1, B2 : BL_Vector) return Natural_M32 is
      S : Natural_M32 := 0;
   begin
      for I in B1'Range loop
         S := S + (B1 (I) - B2 (I)) ** 2;
      end loop;
      return S;
   end L2_Distance_Square;

   -- L2, tweaked
   function L2_Tweaked_Square (B1, B2 : BL_Vector) return Natural_M32 is
      S : Natural_M32 := 0;
   begin
      for I in B1'Range loop
         S := S + (Tweak (B1 (I)) - Tweak (B2 (I))) ** 2;
      end loop;
      return S;
   end L2_Tweaked_Square;

   type Distance_Type is (L1, L1_Tweaked, L2, L2_Tweaked);

   function Similar (H1, H2    : Deflate_Huff_Descriptors;
                     Dist_Kind : Distance_Type;
                     Threshold : Natural;
                     Comment   : String)
   return Boolean is
      Dist  : Natural_M32;
      Thres : Natural_M32 := Natural_M32 (Threshold);
   begin
      case Dist_Kind is
      when L1 =>
         Dist := L1_Distance (Convert (H1), Convert (H2));
      when L1_Tweaked =>
         Thres := Thres * Tweak (1);
         Dist  := L1_Tweaked (Convert (H1), Convert (H2));
      when L2 =>
         Thres := Thres * Thres;
         Dist  := L2_Distance_Square (Convert (H1), Convert (H2));
      when L2_Tweaked =>
         Thres := (Thres * Thres) * (Tweak (1) * Tweak (1));
         Dist  := L2_Tweaked_Square (Convert (H1), Convert (H2));
      end case;
      return Dist < Thres;
   end Similar;

   -- Another original part in the Taillaule algorithm: the possibility of recycling
   -- Huffman codes. It is possible only if previous block was not stored and if
   -- the new block's used alphabets are included in the old block's used alphabets.
   function Recyclable (H_Old, H_New : Deflate_Huff_Descriptors) return Boolean is
   begin
      for I in H_Old.Lit_Len'Range loop
         if H_Old.Lit_Len (I).Bit_Length = 0 and H_New.Lit_Len (I).Bit_Length > 0 then
            return False;  -- Code used in new, but not in old
         end if;
      end loop;
      for I in H_Old.Dis'Range loop
         if H_Old.Dis (I).Bit_Length = 0 and H_New.Dis (I).Bit_Length > 0 then
            return False;  -- Code used in new, but not in old
         end if;
      end loop;
      return True;
   end Recyclable;

   -- Phase (C): the Prepare_Huffman_codes procedure finds the Huffman code for each
   -- value, given the bit length imposed as input.
   procedure Prepare_Huffman_Codes (Hd : in out Huff_Descriptor) is
      Max_Huffman_Bits    : constant := 15;
      Bl_Count, Next_Code : array (0 .. Max_Huffman_Bits) of Natural := (others => 0);
      Code                : Natural := 0;
      Bl                  : Natural;
   begin
      -- Algorithm from RFC 1951, section 3.2.2.
      -- Step 1)
      for I in Hd'Range loop
         Bl := Hd (I).Bit_Length;
         Bl_Count (Bl) := Bl_Count (Bl) + 1;  -- One more code to be defined with bit length bl
      end loop;
      -- Step 2)
      for Bits in 1 .. Max_Huffman_Bits loop
         Code := (Code + Bl_Count (Bits - 1)) * 2;
         Next_Code (Bits) := Code;  -- This will be the first code for bit length "bits"
      end loop;
      -- Step 3)
      for N in Hd'Range loop
         Bl := Hd (N).Bit_Length;
         if Bl > 0 then
            Hd (N).Code := Next_Code (Bl);
            Next_Code (Bl) := Next_Code (Bl) + 1;
         else
            Hd (N).Code := 0;
         end if;
      end loop;
      -- Invert bit order for output:
      for I in Hd'Range loop
         Invert (Hd (I));
      end loop;
   end Prepare_Huffman_Codes;

   -- This is the phase (C) for the pair of alphabets used in the Deflate format.
   function Prepare_Huffman_Codes (Dhd : Deflate_Huff_Descriptors) return Deflate_Huff_Descriptors
   is
      Dhd_Var : Deflate_Huff_Descriptors := Dhd;
   begin
      Prepare_Huffman_Codes (Dhd_Var.Lit_Len);
      Prepare_Huffman_Codes (Dhd_Var.Dis);
      return Dhd_Var;
   end Prepare_Huffman_Codes;

   -- Emit a variable length Huffman code
   procedure Put_Huffman_Code (Lc : Length_Code_Pair) is
      pragma Inline (Put_Huffman_Code);
   begin
      -- Huffman code of length 0 should never occur: when constructing
      -- the code lengths (LLHCL) any single occurrence in the statistics
      -- will trigger the build of a code length of 1 or more.
      Put_Code (Code => U32 (Lc.Code), Code_Size => Code_Size_Value (Lc.Bit_Length) ); -- Range check for length 0 (if enabled).
   end Put_Huffman_Code;

   -- This is where the "dynamic" Huffman trees are sent before the block's data are sent.
   -- The decoder needs to know in advance the pair of trees (1st tree for literals-eob-LZ
   -- lengths, 2nd tree for LZ distances) for decoding the compressed data.
   -- But this information takes some room. Fortunately Deflate allows for compressing it
   -- with a combination of Huffman and Run-Length Encoding (RLE) to make this header smaller.
   -- Concretely, the trees are described by the bit length of each symbol, so the header's
   -- content is a vector of length max 320, whose contents are in the 0 .. 18 range and typically
   -- look like:  ... 8, 8, 9, 7, 8, 10, 6, 8, 8, 8, 8, 8, 11, 8, 9, 8, ...
   -- Clearly this vector has redundancies and can be sent in a compressed form.
   -- In this example, the RLE will compress the string of 8's with a single code 8, then a code 17
   -- (repeat x times). Anyway, the very frequent 8's will be encoded with a small number of
   -- bits (less than the 5 plain bits, or maximum 7 Huffman-encoded bits
   -- needed for encoding integers in the 0 .. 18 range).
   procedure Put_Compression_Structure
      (Dhd           :        Deflate_Huff_Descriptors;
       Cost_Analysis :        Boolean;    -- If True: just simulate the whole, and count needed bits
       Bits          : in out Count_Type) -- This is incremented when cost_analysis = True
   is
      subtype Alphabet is Integer range 0 .. 18;
      type Alpha_Array is new Bit_Length_Array (Alphabet);
      Truc_Freq, Truc_Bl : Alpha_Array;
      Truc : Huff_Descriptor (Alphabet);
      -- Compression structure: cs_bl is the "big" array with all bit lengths
      -- for compressing data. cs_bl will be sent compressed, too.
      Cs_Bl : array (1 .. Dhd.Lit_Len'Length + Dhd.Dis'Length) of Natural;
      Last_Cs_Bl : Natural;
      Max_Used_Lln_Code : Alphabet_Lit_Len := 0;
      Max_Used_Dis_Code : Alphabet_Dis     := 0;
      procedure Concatenate_All_Bit_Lengths is
         Idx : Natural := 0;
      begin
         for A in reverse Alphabet_Lit_Len loop
            if Dhd.Lit_Len (A).Bit_Length > 0 then
               Max_Used_Lln_Code := A;
               exit;
            end if;
         end loop;
         for A in reverse Alphabet_Dis loop
            if Dhd.Dis (A).Bit_Length > 0 then
               Max_Used_Dis_Code := A;
               exit;
            end if;
         end loop;
         -- Copy bit lengths for both trees into one array, cs_bl.
         for A in 0 .. Max_Used_Lln_Code loop
            Idx := Idx + 1;
            Cs_Bl (Idx) := Dhd.Lit_Len (A).Bit_Length;
         end loop;
         for A in 0 .. Max_Used_Dis_Code loop
            Idx := Idx + 1;
            Cs_Bl (Idx) := Dhd.Dis (A).Bit_Length;
         end loop;
         Last_Cs_Bl := Idx;
      end Concatenate_All_Bit_Lengths;

      Extra_Bits_Needed : constant array (Alphabet) of Natural := (16 => 2, 17 => 3, 18 => 7, others => 0);

      type Emission_Mode is (Simulate, Effective);

      procedure Emit_Data_Compression_Structures (Emit_Mode : Emission_Mode) is
         procedure Emit_Data_Compression_Atom (X : Alphabet; Extra_Code : U32 := 0) is
            -- x is a bit length (value in 0..15), or a RLE instruction
         begin
            case Emit_Mode is
            when Simulate =>
               Truc_Freq (X) := Truc_Freq (X) + 1;  -- +1 for x's histogram bar
            when Effective =>
               Put_Huffman_Code (Truc (X));
               declare
                  Extra_Bits : constant Natural := Extra_Bits_Needed (X);
               begin
                  if Extra_Bits > 0 then
                     Put_Code (Extra_Code, Extra_Bits);
                  end if;
               end;
            end case;
         end Emit_Data_Compression_Atom;

         Idx : Natural := 0;
         Rep : Positive;  -- Number of times current atom is repeated, >= 1
      begin
         -- Emit the bit lengths, with some RLE encoding (Appnote: 5.5.3; RFC 1951: 3.2.7)
         Idx := 1;
         loop
            Rep := 1;  -- Current atom, cs_bl(idx), is repeated 1x so far - obvious, isn't it ?
            for J in Idx + 1 .. Last_Cs_Bl loop
               exit when Cs_Bl (J) /= Cs_Bl (Idx);
               Rep := Rep + 1;
            end loop;
            -- Now rep is the number of repetitions of current atom, including itself.
            if Idx > 1 and then Cs_Bl (Idx) = Cs_Bl (Idx - 1) and then Rep >= 3
            -- Better repeat a long sequence of zeros by using codes 17 or 18
            -- just after a 138-long previous sequence:
               and then not (Cs_Bl (Idx) = 0 and then Rep > 6)
            then
               Rep := Integer'Min (Rep, 6);
               Emit_Data_Compression_Atom (16, U32 (Rep - 3));    -- 16: "Repeat previous 3 to 6 times"
               Idx := Idx + Rep;
            elsif Cs_Bl (Idx) = 0 and then Rep >= 3 then
               -- The 0 bit length may occur on long ranges of an alphabet (unused symbols)
               if Rep <= 10 then
                  Emit_Data_Compression_Atom (17, U32 (Rep - 3));  -- 17: "Repeat zero 3 to 10 times"
               else
                  Rep := Integer'Min (Rep, 138);
                  Emit_Data_Compression_Atom (18, U32 (Rep - 11)); -- 18: "Repeat zero 11 to 138 times"
               end if;
               Idx := Idx + Rep;
            else
               Emit_Data_Compression_Atom (Cs_Bl (Idx));
               Idx := Idx + 1;
            end if;
            exit when Idx > Last_Cs_Bl;
         end loop;
      end Emit_Data_Compression_Structures;

      -- Alphabet permutation for shortening in-use alphabet.
      -- After the RLE codes 16, 17, 18 and the bit length 0, which are assumed to be always used,
      -- the most usual bit lengths (around 8, which is the "neutral" bit length) appear first.
      -- For example, if the rare bit lengths 1 and 15 don't occur in any of the two Huffman trees
      -- for LZ data, then codes 1 and 15 have a length 0 in the local Alphabet and we can omit
      -- sending the last two bit lengths.
      Alphabet_Permutation : constant array (Alphabet) of Natural :=
                               (16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15);
      procedure LLHCL is new
         Length_Limited_Huffman_Code_Lengths (Alphabet, Natural, Alpha_Array, Alpha_Array, 7);

      A_Non_Zero : Alphabet;
   begin
      Concatenate_All_Bit_Lengths;
      Truc_Freq := (others => 0);
      Emit_Data_Compression_Structures (Simulate);
      -- We have now statistics of all bit lengths occurrences of both Huffman
      -- trees used for compressing the data.
      -- We turn these counts into bit lengths for the local tree
      -- that helps us to store the compression structure in a more compact form.
      LLHCL (Truc_Freq, Truc_Bl);  -- Call the magic algorithm for setting up Huffman lengths
      -- At least lengths for codes 16, 17, 18, 0 will always be sent,
      -- even if all other bit lengths are 0 because codes 1 to 15 are unused.
      A_Non_Zero := 3;
      for A in Alphabet loop
         if A > A_Non_Zero and then Truc_Bl (Alphabet_Permutation (A)) > 0 then
            A_Non_Zero := A;
         end if;
      end loop;
      if Cost_Analysis then
         -- In this mode, no data output: we sum up the exact
         -- number of bits needed by the compression header.
         Bits := Bits + 14 + Count_Type (1 + A_Non_Zero) * 3;
         for A in Alphabet loop
            Bits := Bits + Count_Type (Truc_Freq (A) * (Truc_Bl (A) + Extra_Bits_Needed (A)));
         end loop;
      else
         -- We output the compression header to the output stream.
         for A in Alphabet loop
            Truc (A).Bit_Length := Truc_Bl (A);
         end loop;
         Prepare_Huffman_Codes (Truc);
         -- Output of the compression structure
         Put_Code (U32 (Max_Used_Lln_Code - 256), 5);  -- max_used_lln_code is always >= 256 = EOB code
         Put_Code (U32 (Max_Used_Dis_Code), 5);
         Put_Code (U32 (A_Non_Zero - 3), 4);
         -- Save the local alphabet's Huffman lengths. It's the compression structure
         -- for compressing the data compression structure. Easy, isn't it ?
         for A in 0 .. A_Non_Zero loop
            Put_Code (U32 (Truc (Alphabet_Permutation (A)).Bit_Length), 3);
         end loop;
         -- Emit the Huffman lengths for encoding the data, in the local Huffman-encoded fashion.
         Emit_Data_Compression_Structures (Effective);
      end if;
   end Put_Compression_Structure;

   End_Of_Block : constant := 256;

   -- Default Huffman trees, for "fixed" blocks, as defined in appnote.txt or RFC 1951
   Default_Lit_Len_Bl : constant Bit_Length_Array_Lit_Len := (  0 .. 143   => 8,  -- For literals ("plain text" bytes)
                                                              144 .. 255   => 9,  -- For more literals ("plain text" bytes)
                                                              End_Of_Block => 7,  -- For EOB (256)
                                                              257 .. 279   => 7,  -- For length codes
                                                              280 .. 287   => 8); -- For more length codes
   Default_Dis_Bl : constant Bit_Length_Array_Dis := (others => 5);

   Deflate_Fixed_Descriptors : constant Deflate_Huff_Descriptors :=
      Prepare_Huffman_Codes (Build_Descriptors (Default_Lit_Len_Bl, Default_Dis_Bl));

   -- Current tree descriptors
   Curr_Descr : Deflate_Huff_Descriptors := Deflate_Fixed_Descriptors;

   -- Write a normal, "clear-text" (post LZ, pre Huffman), 8-bit character (literal)
   procedure Put_Literal_Byte (B : Byte_Value) is
   begin
      Put_Huffman_Code (Curr_Descr.Lit_Len (Integer (B)));
   end Put_Literal_Byte;

   -- Possible ranges for distance and length encoding in the Zip-Deflate format:
   subtype Length_Range is Integer range 3 .. 258;
   subtype Distance_Range is Integer range 1 .. 32768;

   -- This is where LZ distance-length tokens are written to the output stream.
   -- The Deflate format defines a sort of logarithmic compression, with codes
   -- for various distance and length ranges, plus extra bits for specifying the
   -- exact values. The codes are sent as Huffman codes with variable bit lengths
   -- (nothing to do with the lengths of LZ distance-length tokens).

   -- Length Codes
   -- ------------
   -- Extra             Extra              Extra              Extra
   -- Code Bits Length  Code Bits Lengths  Code Bits Lengths  Code Bits Length(s)
   -- ---- ---- ------ ---- ---- ------- ---- ---- ------- ---- ---- ---------
   -- 257   0     3     265   1   11,12    273   3   35-42    281   5  131-162
   -- 258   0     4     266   1   13,14    274   3   43-50    282   5  163-194
   -- 259   0     5     267   1   15,16    275   3   51-58    283   5  195-226
   -- 260   0     6     268   1   17,18    276   3   59-66    284   5  227-257
   -- 261   0     7     269   2   19-22    277   4   67-82    285   0    258
   -- 262   0     8     270   2   23-26    278   4   83-98
   -- 263   0     9     271   2   27-30    279   4   99-114
   -- 264   0    10     272   2   31-34    280   4  115-130
   -- Example: the code # 266 means the LZ length (# of message bytes to be copied)
   -- shall be 13 or 14, depending on the extra bit value.

   Deflate_Code_For_Lz_Length : constant array (Length_Range) of Natural :=
                                  (3          => 257,  -- Codes 257..264, with no extra bit
                                   4          => 258,
                                   5          => 259,
                                   6          => 260,
                                   7          => 261,
                                   8          => 262,
                                   9          => 263,
                                   10         => 264,
                                   11  .. 12  => 265,  -- Codes 265..268, with 1 extra bit
                                   13  .. 14  => 266,
                                   15  .. 16  => 267,
                                   17  .. 18  => 268,
                                   19  .. 22  => 269,  -- Codes 269..272, with 2 extra bits
                                   23  .. 26  => 270,
                                   27  .. 30  => 271,
                                   31  .. 34  => 272,
                                   35  .. 42  => 273,  -- Codes 273..276, with 3 extra bits
                                   43  .. 50  => 274,
                                   51  .. 58  => 275,
                                   59  .. 66  => 276,
                                   67  .. 82  => 277,  -- Codes 277..280, with 4 extra bits
                                   83  .. 98  => 278,
                                   99  .. 114 => 279,
                                   115 .. 130 => 280,
                                   131 .. 162 => 281,  -- Codes 281..284, with 5 extra bits
                                   163 .. 194 => 282,
                                   195 .. 226 => 283,
                                   227 .. 257 => 284,
                                   258        => 285 );-- Code 285, with no extra bit

   Extra_Bits_For_Lz_Length_Offset : constant array (Length_Range) of Integer :=
                                       (3 ..  10 | 258   =>  Invalid,  -- just a placeholder, there is no extra bit there!
                                        11 ..  18        =>  11,
                                        19 ..  34        =>  19,
                                        35 ..  66        =>  35,
                                        67 .. 130        =>  67,
                                        131 .. 257       => 131);

   Extra_Bits_For_Lz_Length : constant array (Length_Range) of Natural :=
                                (3 ..  10 | 258   => 0,
                                 11 ..  18        => 1,
                                 19 ..  34        => 2,
                                 35 ..  66        => 3,
                                 67 .. 130        => 4,
                                 131 .. 257       => 5);

   procedure Put_DL_Code (Distance : Distance_Range; Length : Length_Range) is
      Extra_Bits : Natural;
   begin
      Put_Huffman_Code (Curr_Descr.Lit_Len (Deflate_Code_For_Lz_Length (Length)));
      -- Extra bits are needed to differentiate lengths sharing the same code.
      Extra_Bits := Extra_Bits_For_Lz_Length (Length);
      if Extra_Bits > 0 then
         -- We keep only the last extra_bits bits of the length (minus given offset).
         -- Example: if extra_bits = 1, only the parity is sent (0 or 1);
         -- the rest has been already sent with Put_Huffman_code above.
         -- Equivalent: x:= x mod (2 ** extra_bits);
         Put_Code (
                   U32 (Length - Extra_Bits_For_Lz_Length_Offset (Length))
                   and
                      (Shift_Left (U32'(1), Extra_Bits) - 1),
                   Extra_Bits);
      end if;
      -- Distance Codes
      -- --------------
      -- Extra           Extra             Extra               Extra
      -- Code Bits Dist  Code Bits  Dist   Code Bits Distance  Code Bits Distance
      -- ---- ---- ---- ---- ---- ------ ---- ---- -------- ---- ---- --------
      -- 0   0    1      8   3   17-24    16    7  257-384    24   11  4097-6144
      -- 1   0    2      9   3   25-32    17    7  385-512    25   11  6145-8192
      -- 2   0    3     10   4   33-48    18    8  513-768    26   12  8193-12288
      -- 3   0    4     11   4   49-64    19    8  769-1024   27   12 12289-16384
      -- 4   1   5,6    12   5   65-96    20    9 1025-1536   28   13 16385-24576
      -- 5   1   7,8    13   5   97-128   21    9 1537-2048   29   13 24577-32768
      -- 6   2   9-12   14   6  129-192   22   10 2049-3072
      -- 7   2  13-16   15   6  193-256   23   10 3073-4096
      -- Example: the code # 10 means the LZ distance (# positions back in the circular
      -- message buffer for starting the copy) shall be 33, plus the value given
      -- by the 4 extra bits (between 0 and 15).
      case Distance is
      when 1 .. 4 =>          -- Codes 0..3, with no extra bit
         Put_Huffman_Code (Curr_Descr.Dis (Distance - 1));
      when 5 .. 8 =>          -- Codes 4..5, with 1 extra bit
         Put_Huffman_Code (Curr_Descr.Dis (4 + (Distance - 5) / 2));
         Put_Code (U32 ((Distance - 5) mod 2), 1);
      when 9 .. 16 =>         -- Codes 6..7, with 2 extra bits
         Put_Huffman_Code (Curr_Descr.Dis (6 + (Distance - 9) / 4));
         Put_Code (U32 ((Distance - 9) mod 4), 2);
      when 17 .. 32 =>        -- Codes 8..9, with 3 extra bits
         Put_Huffman_Code (Curr_Descr.Dis (8 + (Distance - 17) / 8));
         Put_Code (U32 ((Distance - 17) mod 8), 3);
      when 33 .. 64 =>        -- Codes 10..11, with 4 extra bits
         Put_Huffman_Code (Curr_Descr.Dis (10 + (Distance - 33) / 16));
         Put_Code (U32 ((Distance - 33) mod 16), 4);
      when 65 .. 128 =>       -- Codes 12..13, with 5 extra bits
         Put_Huffman_Code (Curr_Descr.Dis (12 + (Distance - 65) / 32));
         Put_Code (U32 ((Distance - 65) mod 32), 5);
      when 129 .. 256 =>      -- Codes 14..15, with 6 extra bits
         Put_Huffman_Code (Curr_Descr.Dis (14 + (Distance - 129) / 64));
         Put_Code (U32 ((Distance - 129) mod 64), 6);
      when 257 .. 512 =>      -- Codes 16..17, with 7 extra bits
         Put_Huffman_Code (Curr_Descr.Dis (16 + (Distance - 257) / 128));
         Put_Code (U32 ((Distance - 257) mod 128), 7);
      when 513 .. 1024 =>     -- Codes 18..19, with 8 extra bits
         Put_Huffman_Code (Curr_Descr.Dis (18 + (Distance - 513) / 256));
         Put_Code (U32 ((Distance - 513) mod 256), 8);
      when 1025 .. 2048 =>    -- Codes 20..21, with 9 extra bits
         Put_Huffman_Code (Curr_Descr.Dis (20 + (Distance - 1025) / 512));
         Put_Code (U32 ((Distance - 1025) mod 512), 9);
      when 2049 .. 4096 =>    -- Codes 22..23, with 10 extra bits
         Put_Huffman_Code (Curr_Descr.Dis (22 + (Distance - 2049) / 1024));
         Put_Code (U32 ((Distance - 2049) mod 1024), 10);
      when 4097 .. 8192 =>    -- Codes 24..25, with 11 extra bits
         Put_Huffman_Code (Curr_Descr.Dis (24 + (Distance - 4097) / 2048));
         Put_Code (U32 ((Distance - 4097) mod 2048), 11);
      when 8193 .. 16384 =>   -- Codes 26..27, with 12 extra bits
         Put_Huffman_Code (Curr_Descr.Dis (26 + (Distance - 8193) / 4096));
         Put_Code (U32 ((Distance - 8193) mod 4096), 12);
      when 16385 .. 32768 =>  -- Codes 28..29, with 13 extra bits
         Put_Huffman_Code (Curr_Descr.Dis (28 + (Distance - 16385) / 8192));
         Put_Code (U32 ((Distance - 16385) mod 8192), 13);
      end case;
   end Put_DL_Code;

   function Deflate_Code_For_LZ_Distance (Distance : Distance_Range) return Natural is
   begin
      case Distance is
      when 1 .. 4 => -- Codes 0..3, with no extra bit
         return Distance - 1;
      when 5 .. 8 => -- Codes 4..5, with 1 extra bit
         return  4 + (Distance - 5) / 2;
      when 9 .. 16 => -- Codes 6..7, with 2 extra bits
         return  6 + (Distance - 9) / 4;
      when 17 .. 32 => -- Codes 8..9, with 3 extra bits
         return  8 + (Distance - 17) / 8;
      when 33 .. 64 => -- Codes 10..11, with 4 extra bits
         return  10 + (Distance - 33) / 16;
      when 65 .. 128 => -- Codes 12..13, with 5 extra bits
         return  12 + (Distance - 65) / 32;
      when 129 .. 256 => -- Codes 14..15, with 6 extra bits
         return  14 + (Distance - 129) / 64;
      when 257 .. 512 => -- Codes 16..17, with 7 extra bits
         return  16 + (Distance - 257) / 128;
      when 513 .. 1024 => -- Codes 18..19, with 8 extra bits
         return  18 + (Distance - 513) / 256;
      when 1025 .. 2048 => -- Codes 20..21, with 9 extra bits
         return  20 + (Distance - 1025) / 512;
      when 2049 .. 4096 => -- Codes 22..23, with 10 extra bits
         return  22 + (Distance - 2049) / 1024;
      when 4097 .. 8192 => -- Codes 24..25, with 11 extra bits
         return  24 + (Distance - 4097) / 2048;
      when 8193 .. 16384 => -- Codes 26..27, with 12 extra bits
         return  26 + (Distance - 8193) / 4096;
      when 16385 .. 32768 => -- Codes 28..29, with 13 extra bits
         return  28 + (Distance - 16385) / 8192;
      end case;
   end Deflate_Code_For_LZ_Distance;

   -- LZ Buffer

   -- We buffer the LZ codes (plain, or distance/length) in order to
   -- analyse them and try to do smart things.

   Max_Expand          : constant :=  14;  -- *Tuned* Sometimes it is better to store data and expand short strings
   Code_For_Max_Expand : constant := 266;

   subtype Expanded_Data is Byte_Buffer (1 .. Max_Expand);

   type LZ_Atom_Kind is (Plain_Byte, Distance_Length);
   type LZ_Atom is record
      Kind          : LZ_Atom_Kind;
      Plain         : Byte_Value;
      Lz_Distance   : Natural;
      Lz_Length     : Natural;
      Lz_Expanded   : Expanded_Data;
   end record;

   -- *Tuned*. Min: 2**14, = 16384 (min half buffer 8192)
   -- Optimal so far: 2**17
   LZ_Buffer_Size : constant := 2 ** 17;

   type LZ_Buffer_Index_Type is mod LZ_Buffer_Size;
   type LZ_Buffer_Type is array (LZ_Buffer_Index_Type range <>) of LZ_Atom;

   Empty_Lit_Len_Stat : constant Stats_Lit_Len_Type := (End_Of_Block => 1, others => 0);
   -- End_Of_Block will have to happen once, but never appears in the LZ statistics...
   Empty_Dis_Stat     : constant Stats_Dis_Type := (others => 0);

   -- Compute statistics for both Literal-length, and Distance alphabets, from a LZ buffer
   procedure Get_Statistics (Lzb           :  in LZ_Buffer_Type;
                             Stats_Lit_Len : out Stats_Lit_Len_Type;
                             Stats_Dis     : out Stats_Dis_Type)
   is
      Lit_Len : Alphabet_Lit_Len;
      Dis     : Alphabet_Dis;
   begin
      Stats_Lit_Len := Empty_Lit_Len_Stat;
      Stats_Dis     := Empty_Dis_Stat;
      for I in Lzb'Range loop
         case Lzb (I).Kind is
         when Plain_Byte =>
            Lit_Len := Alphabet_Lit_Len (Lzb (I).Plain);
            Stats_Lit_Len (Lit_Len) := Stats_Lit_Len (Lit_Len) + 1;     -- +1 for this literal
         when Distance_Length =>
            Lit_Len := Deflate_Code_For_Lz_Length (Lzb (I).Lz_Length);
            Stats_Lit_Len (Lit_Len) := Stats_Lit_Len (Lit_Len) + 1;     -- +1 for this length code
            Dis := Deflate_Code_For_LZ_Distance (Lzb (I).Lz_Distance);
            Stats_Dis (Dis) := Stats_Dis (Dis) + 1;                     -- +1 for this distance code
         end case;
      end loop;
   end Get_Statistics;

   -- Send a LZ buffer using currently defined Huffman codes
   procedure Put_LZ_Buffer (Lzb : LZ_Buffer_Type) is
   begin
      for I in Lzb'Range loop
         case Lzb (I).Kind is
         when Plain_Byte =>
            Put_Literal_Byte (Lzb (I).Plain);
         when Distance_Length =>
            Put_DL_Code (Lzb (I).Lz_Distance, Lzb (I).Lz_Length);
         end case;
      end loop;
   end Put_LZ_Buffer;

   Block_To_Finish   : Boolean := False;
   Last_Block_Marked : Boolean := False;

   type Block_Type is (Stored, Fixed, Dynamic, Reserved);  -- Appnote, 5.5.2
   -- If last_block_type = dynamic, we may recycle previous block's Huffman codes

   Last_Block_Type : Block_Type := Reserved;

   procedure Mark_New_Block (Last_Block_For_Stream : Boolean) is
   begin
      if Block_To_Finish and Last_Block_Type in Fixed .. Dynamic then
         Put_Huffman_Code (Curr_Descr.Lit_Len (End_Of_Block));  -- Finish previous block
      end if;
      Block_To_Finish := True;
      Put_Code (Code => Boolean'Pos (Last_Block_For_Stream), Code_Size => 1);
      Last_Block_Marked := Last_Block_For_Stream;
   end Mark_New_Block;

   -- Send a LZ buffer completely decoded as literals (LZ compression is discarded)
   procedure Expand_LZ_Buffer (Lzb : LZ_Buffer_Type; Last_Block : Boolean) is
      B1, B2     : Byte_Value;
      To_Be_Sent : Natural_M32 := 0;
      -- to_be_sent is not always equal to lzb'Length: sometimes you have a DL code
      Mid        : LZ_Buffer_Index_Type;
   begin
      for I in Lzb'Range loop
         case Lzb (I).Kind is
         when Plain_Byte =>
            To_Be_Sent := To_Be_Sent + 1;
         when Distance_Length =>
            To_Be_Sent := To_Be_Sent + Natural_M32 (Lzb (I).Lz_Length);
         end case;
      end loop;
      if To_Be_Sent > 16#FFFF# then  -- Ow, cannot send all that in one chunk.
         -- Instead of a tedious block splitting, just divide and conquer:
         Mid := LZ_Buffer_Index_Type ((Natural_M32 (Lzb'First) + Natural_M32 (Lzb'Last)) / 2);
         Expand_LZ_Buffer (Lzb (Lzb'First .. Mid), Last_Block => False);
         Expand_LZ_Buffer (Lzb (Mid + 1   .. Lzb'Last), Last_Block => Last_Block);
         return;
      end if;
      B1 := Byte_Value (To_Be_Sent mod 256);
      B2 := Byte_Value (To_Be_Sent  /  256);
      Mark_New_Block (Last_Block_For_Stream => Last_Block);
      Last_Block_Type := Stored;
      Put_Code (Code => 0, Code_Size => 2);  -- Signals a "stored" block
      Flush_Bit_Buffer;  -- Go to Byte_Value boundary
      Put_Byte (B1);
      Put_Byte (B2);
      Put_Byte (not B1);
      Put_Byte (not B2);
      for I in Lzb'Range loop
         case Lzb (I).Kind is
         when Plain_Byte =>
            Put_Byte (Lzb (I).Plain);
         when Distance_Length =>
            for J in 1 .. Lzb (I).Lz_Length loop
               Put_Byte (Lzb (I).Lz_Expanded (J));
            end loop;
         end case;
      end loop;
   end Expand_LZ_Buffer;

   -- Extra bits that need to be sent after various Deflate codes

   Extra_Bits_For_Lz_Length_Code : constant array (257 .. 285) of Natural :=
                                     (257 .. 264 => 0,
                                      265 .. 268 => 1,
                                      269 .. 272 => 2,
                                      273 .. 276 => 3,
                                      277 .. 280 => 4,
                                      281 .. 284 => 5,
                                      285        => 0);

   Extra_Bits_For_Lz_Distance_Code : constant array (0 .. 29) of Natural :=
                                       (0 ..  3  =>  0,
                                        4 ..  5  =>  1,
                                        6 ..  7  =>  2,
                                        8 ..  9  =>  3,
                                        10 .. 11 =>  4,
                                        12 .. 13 =>  5,
                                        14 .. 15 =>  6,
                                        16 .. 17 =>  7,
                                        18 .. 19 =>  8,
                                        20 .. 21 =>  9,
                                        22 .. 23 => 10,
                                        24 .. 25 => 11,
                                        26 .. 27 => 12,
                                        28 .. 29 => 13);

   subtype Long_Length_Codes is
      Alphabet_Lit_Len range Code_For_Max_Expand + 1 .. Alphabet_Lit_Len'Last;

   Zero_Bl_Long_Lengths : constant Stats_Type (Long_Length_Codes) := (others => 0);

   -- Send_as_block.
   -- lzb (can be a slice of the principal buffer) will be sent as:
   -- * a new "dynamic" block, preceded by a compression structure header
   -- or  * the continuation of previous "dynamic" block
   -- or  * a new "fixed" block, if lz data's Huffman descriptor is close enough to "fixed"
   -- or  * a new "stored" block, if lz data are too random

   procedure Send_As_Block (Lzb : LZ_Buffer_Type; Last_Block : Boolean) is
      New_Descr, New_Descr_2 : Deflate_Huff_Descriptors;

      procedure Send_Fixed_Block is
      begin
         if Last_Block_Type = Fixed then
            -- Cool, we don't need to mark a block boundary: the Huffman codes are already
            -- the expected ones. We can just continue sending the LZ atoms.
            null;
         else
            Mark_New_Block (Last_Block_For_Stream => Last_Block);
            Curr_Descr := Deflate_Fixed_Descriptors;
            Put_Code (Code => 1, Code_Size => 2);  -- Signals a "fixed" block
            Last_Block_Type := Fixed;
         end if;
         Put_LZ_Buffer (Lzb);
      end Send_Fixed_Block;

      Stats_Lit_Len, Stats_Lit_Len_2 : Stats_Lit_Len_Type;
      Stats_Dis, Stats_Dis_2 : Stats_Dis_Type;

      procedure Send_Dynamic_Block (Dyn : Deflate_Huff_Descriptors) is
         Dummy : Count_Type := 0;
      begin
         Mark_New_Block (Last_Block_For_Stream => Last_Block);
         Curr_Descr := Prepare_Huffman_Codes (Dyn);
         Put_Code (Code => 2, Code_Size => 2);  -- Signals a "dynamic" block
         Put_Compression_Structure (Curr_Descr, Cost_Analysis => False, Bits => Dummy);
         Put_LZ_Buffer (Lzb);
         Last_Block_Type := Dynamic;
      end Send_Dynamic_Block;

      -- The following variables will contain the *exact* number of bits taken
      -- by the block to be sent, using different Huffman encodings, or stored.
      Stored_Format_Bits,                      -- Block is stored (no compression)
      Fixed_Format_Bits,                       -- Fixed (preset) Huffman codes
      Dynamic_Format_Bits,                     -- Dynamic Huffman codes using block's statistics
      Dynamic_Format_Bits_2,                   -- Dynamic Huffman codes after Tweak_for_better_RLE
      Recycled_Format_Bits   : Count_Type := 0;  -- Continue previous block, use current Huffman codes
      Stored_Format_Possible : Boolean;  -- Can we store (needs expansion of DL codes) ?
      Recycling_Possible     : Boolean;  -- Can we recycle current Huffman codes ?

      procedure Compute_Sizes_Of_Variants is
         C     : Count_Type;
         Extra : Natural;
      begin
         -- We count bits taken by literals, for each block format variant.
         for I in 0 .. 255 loop
            C := Stats_Lit_Len (I);  -- This literal appears c times in the LZ buffer
            Stored_Format_Bits    := Stored_Format_Bits    + 8                                               * C;
            Fixed_Format_Bits     := Fixed_Format_Bits     + Count_Type (Default_Lit_Len_Bl (I))             * C;
            Dynamic_Format_Bits   := Dynamic_Format_Bits   + Count_Type (New_Descr.Lit_Len (I).Bit_Length)   * C;
            Dynamic_Format_Bits_2 := Dynamic_Format_Bits_2 + Count_Type (New_Descr_2.Lit_Len (I).Bit_Length) * C;
            Recycled_Format_Bits  := Recycled_Format_Bits  + Count_Type (Curr_Descr.Lit_Len (I).Bit_Length)  * C;
         end loop;
         -- We count bits taken by DL codes.
         if Stored_Format_Possible then
            for I in Lzb'Range loop
               case Lzb (I).Kind is
               when Plain_Byte =>
                  null;  -- Already counted
               when Distance_Length =>
                  -- In the stored format, DL codes are expanded
                  Stored_Format_Bits := Stored_Format_Bits + 8 * Count_Type (Lzb (I).Lz_Length);
               end case;
            end loop;
         end if;
         -- For compressed formats, count Huffman bits and extra bits.
         -- Lengths codes:
         for I in 257 .. 285 loop
            C := Stats_Lit_Len (I);  -- This length code appears c times in the LZ buffer
            Extra := Extra_Bits_For_Lz_Length_Code (I);
            Fixed_Format_Bits     := Fixed_Format_Bits     + Count_Type (Default_Lit_Len_Bl (I) + Extra)             * C;
            Dynamic_Format_Bits   := Dynamic_Format_Bits   + Count_Type (New_Descr.Lit_Len (I).Bit_Length + Extra)   * C;
            Dynamic_Format_Bits_2 := Dynamic_Format_Bits_2 + Count_Type (New_Descr_2.Lit_Len (I).Bit_Length + Extra) * C;
            Recycled_Format_Bits  := Recycled_Format_Bits  + Count_Type (Curr_Descr.Lit_Len (I).Bit_Length + Extra)  * C;
         end loop;
         -- Distance codes:
         for I in 0 .. 29 loop
            C := Stats_Dis (I);  -- This distance code appears c times in the LZ buffer
            Extra := Extra_Bits_For_Lz_Distance_Code (I);
            Fixed_Format_Bits     := Fixed_Format_Bits     + Count_Type (Default_Dis_Bl (I) + Extra)             * C;
            Dynamic_Format_Bits   := Dynamic_Format_Bits   + Count_Type (New_Descr.Dis (I).Bit_Length + Extra)   * C;
            Dynamic_Format_Bits_2 := Dynamic_Format_Bits_2 + Count_Type (New_Descr_2.Dis (I).Bit_Length + Extra) * C;
            Recycled_Format_Bits  := Recycled_Format_Bits  + Count_Type (Curr_Descr.Dis (I).Bit_Length + Extra)  * C;
         end loop;
         -- Supplemental bits to be counted
         Stored_Format_Bits := Stored_Format_Bits +
            (1 + (Stored_Format_Bits / 8) / 65_535)  -- Number of stored blocks needed
               * 5  -- 5 bytes per header
            * 8; -- ... converted into bits
         C := 1;  -- Is-last-block flag
         if Block_To_Finish and Last_Block_Type in Fixed .. Dynamic then
            C := C + Count_Type (Curr_Descr.Lit_Len (End_Of_Block).Bit_Length);
         end if;
         Stored_Format_Bits    := Stored_Format_Bits + C;
         Fixed_Format_Bits     := Fixed_Format_Bits + C + 2;
         Dynamic_Format_Bits   := Dynamic_Format_Bits + C + 2;
         Dynamic_Format_Bits_2 := Dynamic_Format_Bits_2 + C + 2;
         -- For both dynamic formats, we also counts the bits taken by the compression header!
         Put_Compression_Structure (New_Descr,   Cost_Analysis => True, Bits => Dynamic_Format_Bits);
         Put_Compression_Structure (New_Descr_2, Cost_Analysis => True, Bits => Dynamic_Format_Bits_2);
      end Compute_Sizes_Of_Variants;
      Optimal_Format_Bits    : Count_Type;
   begin
      Get_Statistics (Lzb, Stats_Lit_Len, Stats_Dis);
      New_Descr := Build_Descriptors (Stats_Lit_Len, Stats_Dis);
      Stats_Lit_Len_2 := Stats_Lit_Len;
      Stats_Dis_2 := Stats_Dis;
      Tweak_For_Better_RLE (Stats_Lit_Len_2);
      Tweak_For_Better_RLE (Stats_Dis_2);
      New_Descr_2 := Build_Descriptors (Stats_Lit_Len_2, Stats_Dis_2);
      -- For "stored" block format, prevent expansion of DL codes with length > max_expand.
      -- We check stats are all 0 for long length codes:
      Stored_Format_Possible := Stats_Lit_Len (Long_Length_Codes) = Zero_Bl_Long_Lengths;
      Recycling_Possible :=
         Last_Block_Type = Fixed  -- The "fixed" alphabets use all symbols, then always recyclable.
         or else
            (Last_Block_Type = Dynamic and then Recyclable (Curr_Descr, New_Descr));
      Compute_Sizes_Of_Variants;
      if not Stored_Format_Possible then
         Stored_Format_Bits := Count_Type'Last;
      end if;
      if not Recycling_Possible then
         Recycled_Format_Bits := Count_Type'Last;
      end if;
      Optimal_Format_Bits := Count_Type'Min (Count_Type'Min (Stored_Format_Bits, Fixed_Format_Bits),
                                             Count_Type'Min (Count_Type'Min (Dynamic_Format_Bits, Dynamic_Format_Bits_2),
                                                                             Recycled_Format_Bits) );
      -- Selection of the block format with smallest size.
      if Fixed_Format_Bits = Optimal_Format_Bits then
         Send_Fixed_Block;
      elsif Dynamic_Format_Bits = Optimal_Format_Bits then
         Send_Dynamic_Block (New_Descr);
      elsif Dynamic_Format_Bits_2 = Optimal_Format_Bits then
         Send_Dynamic_Block (New_Descr_2);
      elsif Recycled_Format_Bits = Optimal_Format_Bits then
         Put_LZ_Buffer (Lzb);
      else  -- We have stored_format_bits = optimal_format_bits
         Expand_LZ_Buffer (Lzb, Last_Block);
      end if;
   end Send_As_Block;

   subtype Full_Range_LZ_Buffer_Type is LZ_Buffer_Type (LZ_Buffer_Index_Type);

   package LZ_Buffer_Holders is
      type Holder is tagged limited private;

      procedure Update_Element (Buffer : in out Holder; Process : access procedure (Buffer : in out Full_Range_LZ_Buffer_Type) );
   private -- LZ_Buffer_Holders
      type Buffer_Ptr is access Full_Range_LZ_Buffer_Type;

      type Holder is new Ada.Finalization.Limited_Controlled with record
         Ptr : Buffer_Ptr := new Full_Range_LZ_Buffer_Type;
      end record;

      overriding procedure Finalize (Object : in out Holder);
   end LZ_Buffer_Holders;

   package body LZ_Buffer_Holders is
      procedure Update_Element (Buffer : in out Holder; Process : access procedure (Buffer : in out Full_Range_LZ_Buffer_Type) )
      is
         -- Empty
      begin -- Update_Element
         Process (Buffer => Buffer.Ptr.all);
      end Update_Element;

      procedure Finalize (Object : in out Holder) is
         procedure Free is new Ada.Unchecked_Deallocation (Object => Full_Range_LZ_Buffer_Type, Name => Buffer_Ptr);
      begin -- Finalize
         Free (Object.Ptr);
      end Finalize;
   end LZ_Buffer_Holders;

   -- This is the main, big, fat, circular buffer containing LZ codes,
   -- each LZ code being a literal or a DL code.
   -- Heap allocation used to avoided stack overflow on targets with small default stacks
   Lz_Buffer       : LZ_Buffer_Holders.Holder;
   Lz_Buffer_Index : LZ_Buffer_Index_Type := 0;
   Past_Lz_Data    : Boolean := False;
   -- When True: some LZ_buffer_size data before lz_buffer_index (modulo!) are real, past data

   -- Scanning and sampling: the really sexy part of the Taillaule algorithm...

   -- We examine similarities in the LZ data flow at different step sizes.
   -- If the optimal Huffman encoding for this portion is very different, we choose to
   -- cut current block and start a new one. The shorter the step, the higher the threshold
   -- for starting a dynamic block, since the compression header is taking some room each time.

   -- *Tuned* (a bit...)
   Min_Step : constant := 750;

   type Step_Threshold_Metric is record
      Slider_Step       : LZ_Buffer_Index_Type;  -- Should be a multiple of min_step.
      Cutting_Threshold : Positive;
      Metric            : Distance_Type;
   end record;

   -- *Tuned* thresholds
   -- NB: the enwik8, then silesia, then others tests are tough for lowering any!
   Step_Choice : constant array (Positive range <>) of Step_Threshold_Metric :=
                   ((8 * Min_Step,  420, L1_Tweaked),  -- Deflate_1, Deflate_2, Deflate_3 (enwik8)
                    (4 * Min_Step,  430, L1_Tweaked),  -- Deflate_2, Deflate_3 (silesia)
                    (Min_Step, 2050, L1_Tweaked) );   -- Deflate_3 (DB test)

   Max_Choice : constant array (Taillaule_Deflation_Method) of Positive :=
                  (Deflate_1 => 1, Deflate_2 => 2, others => Step_Choice'Last);

   Slider_Size      : constant := 4096;
   Half_Slider_Size : constant := Slider_Size / 2;
   Slider_Max       : constant := Slider_Size - 1;

   -- Phases (A) and (B) are done in a single function: we get Huffman
   -- descriptors that should be good for encoding a given sequence of LZ atoms.
   function Build_Descriptors (Lzb : LZ_Buffer_Type) return Deflate_Huff_Descriptors is
      Stats_Lit_Len : Stats_Lit_Len_Type;
      Stats_Dis     : Stats_Dis_Type;
   begin
      Get_Statistics (Lzb, Stats_Lit_Len, Stats_Dis);
      return Build_Descriptors (Stats_Lit_Len, Stats_Dis);
   end Build_Descriptors;

   procedure Scan_And_Send_From_Main_Buffer (From, To : LZ_Buffer_Index_Type; Last_Flush : Boolean) is
      procedure Process (Buffer : in out Full_Range_LZ_Buffer_Type);
      -- Do the actual Scan_And_Send_From_Main_Buffer processing

      procedure Process (Buffer : in out Full_Range_LZ_Buffer_Type) is
         -- The following descriptors are *not* used for compressing, but for detecting similarities.
         Initial_Hd, Sliding_Hd      : Deflate_Huff_Descriptors;
         Start, Slide_Mid, Send_From : LZ_Buffer_Index_Type;
         Sliding_Hd_Computed         : Boolean;
      begin -- Process
         if To - From < Slider_Max then
            Send_As_Block (Buffer (From .. To), Last_Flush);
            return;
         end if;
         -- For further comments: n := LZ_buffer_size
         if Past_Lz_Data then  -- We have n / 2 previous data before 'from'.
            Start := From - LZ_Buffer_Index_Type (Half_Slider_Size);
         else
            Start := From;  -- Cannot have past data
         end if;
         if Start <= From then
            Initial_Hd := Build_Descriptors (Buffer (Start .. Start + Slider_Max));
         else  -- Looped over, (mod n). Slider data are in two chunks in main buffer
            declare
               Copy_From : LZ_Buffer_Index_Type := Start;
               Copy      : LZ_Buffer_Type (0 .. Slider_Max);
            begin
               for I in Copy'Range loop
                  Copy (I) := Buffer (Copy_From);
                  Copy_From := Copy_From + 1;  -- Loops over (mod n)
               end loop;
               Initial_Hd := Build_Descriptors (Copy);
            end;
            -- Concatenation instead of above loop bombs with a Range Check error:
            -- Buffer(start .. Buffer'Last) &
            -- Buffer(0 .. start + LZ_buffer_index_type(slider_max))
         end if;
         Send_From := From;
         Slide_Mid := From + Min_Step;
         Scan_LZ_Data :
         while Integer_M32 (Slide_Mid) + Half_Slider_Size < Integer_M32 (To) loop
            Sliding_Hd_Computed := False;

            Browse_Step_Level : for Level in Step_Choice'Range loop
               exit Browse_Step_Level when Level > Max_Choice (Method);
               if (Slide_Mid - From) mod Step_Choice (Level).Slider_Step = 0 then
                  if not Sliding_Hd_Computed then
                     Sliding_Hd := Build_Descriptors (Buffer (Slide_Mid - Half_Slider_Size .. Slide_Mid + Half_Slider_Size));
                     Sliding_Hd_Computed := True;
                  end if;
                  if not Similar (Initial_Hd,
                                  Sliding_Hd,
                                  Step_Choice (Level).Metric,
                                  Step_Choice (Level).Cutting_Threshold,
                                  "Compare sliding to initial (step size=" &
                                     LZ_Buffer_Index_Type'Image (Step_Choice (Level).Slider_Step) & ')')
                  then
                     Send_As_Block (Buffer (Send_From .. Slide_Mid - 1), Last_Block => False);
                     Send_From := Slide_Mid;
                     Initial_Hd := Sliding_Hd;  -- Reset reference descriptor for further comparisons
                     exit Browse_Step_Level;  -- Cutting once at a given place is enough :-)
                  end if;
               end if;
            end loop Browse_Step_Level;
            -- Exit before an eventual increment of slide_mid that would loop over (mod n).
            exit Scan_LZ_Data when Integer_M32 (Slide_Mid) + Min_Step + Half_Slider_Size >= Integer_M32 (To);
            Slide_Mid := Slide_Mid + Min_Step;
         end loop Scan_LZ_Data;
         -- Send last block for slice from .. to.
         if Send_From <= To then
            Send_As_Block (Buffer (Send_From .. To), Last_Block => Last_Flush);
         end if;
      end Process;
   begin -- Scan_And_Send_From_Main_Buffer
      Lz_Buffer.Update_Element (Process => Process'Access);
   end Scan_And_Send_From_Main_Buffer;

   procedure Flush_Half_Buffer (Last_Flush : Boolean) is
      Last_Idx : constant LZ_Buffer_Index_Type := Lz_Buffer_Index - 1;
      N_Div_2  : constant := LZ_Buffer_Size / 2;
   begin
      if Last_Idx < N_Div_2 then
         Scan_And_Send_From_Main_Buffer (0, Last_Idx, Last_Flush);        -- 1st half
      else
         Scan_And_Send_From_Main_Buffer (N_Div_2, Last_Idx, Last_Flush);  -- 2nd half
      end if;
      -- From this point, all further calls to Flush_half_buffer will
      -- have n_div_2 elements of past data.
      Past_Lz_Data := True;
   end Flush_Half_Buffer;

   procedure Push (A : LZ_Atom) is
      pragma Inline (Push);

      procedure Push (Buffer : in out Full_Range_LZ_Buffer_Type);
      -- Do the actual pushing

      procedure Push (Buffer : in out Full_Range_LZ_Buffer_Type) is
         -- Empty
      begin -- Push
         Buffer (Lz_Buffer_Index) := A;
         Lz_Buffer_Index := Lz_Buffer_Index + 1;  -- becomes 0 when reaching LZ_buffer_size (modular)
         if Lz_Buffer_Index * 2 = 0 then
            Flush_Half_Buffer (Last_Flush => False);
         end if;
      end Push;
   begin -- Push
      Lz_Buffer.Update_Element (Process => Push'Access);
   end Push;

   procedure Put_Or_Delay_Literal_Byte (B : Byte_Value) is
      pragma Inline (Put_Or_Delay_Literal_Byte);
   begin
      case Method is
      when Deflate_Fixed =>
         Put_Literal_Byte (B);  -- Buffering is not needed in this mode
      when Taillaule_Deflation_Method =>
         Push ((Plain_Byte, B, 0, 0, (B, others => 0)));
      end case;
   end Put_Or_Delay_Literal_Byte;

   procedure Put_Or_Delay_DL_Code (Distance, Length : Integer; Expand : Expanded_Data) is
      pragma Inline (Put_Or_Delay_DL_Code);
   begin
      case Method is
      when Deflate_Fixed =>
         Put_DL_Code (Distance, Length);  -- Buffering is not needed in this mode
      when Taillaule_Deflation_Method =>
         Push ((Distance_Length, 0, Distance, Length, Expand));
      end case;
   end Put_Or_Delay_DL_Code;

   -- LZ77 front-end compression

   procedure Encode is
      function Read_Byte return Byte_Value renames Next_Byte;

      function More_Bytes return Boolean is
         (not (Buffer_Empty and Out_Of_Data) );

      -- LZ77 parameters
      Look_Ahead_LZ77    : constant Integer := 258;
      String_Buffer_Size : constant := 2 ** 15;  -- Required: 2**15 for Deflate, 2**16 for Deflate_e

      type Text_Buffer_Index is mod String_Buffer_Size;
      type Text_Buffer is array (Text_Buffer_Index) of Byte_Value;

      Text_Buf : Text_Buffer;
      R        : Text_Buffer_Index;

      -- If the DLE coding doesn't fit the format constraints, we need
      -- to decode it as a simple sequence of literals. The buffer used is
      -- called "Text" buffer by reference to "clear-text", but actually it
      -- is any binary data.

      procedure LZ77_Emits_DL_Code (Distance, Length : Integer) is
         -- NB: no worry, all arithmetics in Text_buffer_index are modulo String_buffer_size.
         B          : Byte_Value;
         Copy_Start : Text_Buffer_Index;
         Expand     : Expanded_Data;
         Ie         : Positive := 1;
      begin
         if Distance = String_Buffer_Size then  -- Happens with 7-Zip, cannot happen with Info-Zip.
            Copy_Start := R;
         else
            Copy_Start := R - Text_Buffer_Index (Distance);
         end if;
         -- Expand into the circular text buffer to have it up to date
         for K in 0 .. Text_Buffer_Index (Length - 1) loop
            B := Text_Buf (Copy_Start + K);
            Text_Buf (R) := B;
            R := R + 1;
            if Ie <= Max_Expand then  -- Also memorize short sequences for LZ buffer
               Expand (Ie) := B;       -- for the case a block needs to be stored in clear.
               Ie := Ie + 1;
            end if;
         end loop;
         if Distance in Distance_Range and Length in Length_Range then
            Put_Or_Delay_DL_Code (Distance, Length, Expand);
         else
            for K in 0 .. Text_Buffer_Index (Length - 1) loop
               Put_Or_Delay_Literal_Byte (Text_Buf (Copy_Start + K));
            end loop;
         end if;
      end LZ77_Emits_DL_Code;

      procedure LZ77_Emits_Literal_Byte (B : Byte_Value) is
      begin
         Text_Buf (R) := B;
         R := R + 1;
         Put_Or_Delay_Literal_Byte (B);
      end LZ77_Emits_Literal_Byte;

      procedure Dummy_Estimate_DL_Codes (Matches          : in out LZ77.Matches_Array;
                                         Old_Match_Index  : in     Natural;
                                         Prefixes         : in     LZ77.Byte_Array;
                                         Best_Score_Index :    out Positive;
                                         Best_Score_Set   :    out LZ77.Prefetch_Index_Type;
                                         Match_Trace      :    out LZ77.DLP_Array)
      is null;

      LZ77_Choice : constant array (Method_ID) of LZ77.Method_Type :=
                      (Deflate_Fixed  => LZ77.IZ_4,
                       Deflate_0      => LZ77.No_LZ77,
                       Deflate_1      => LZ77.IZ_6,     -- level 6 is the default in Info-Zip's zip.exe
                       Deflate_2      => LZ77.IZ_8,
                       Deflate_3      => LZ77.IZ_10,
                       Deflate_R      => LZ77.Rich);

      procedure My_LZ77 is new LZ77.Encode
         (String_Buffer_Size => String_Buffer_Size,
          Look_Ahead         => Look_Ahead_LZ77,
          Threshold          => 2,  -- From a string match length > 2, a DL code is sent
          Method             => LZ77_Choice (Method),
          Read_Byte          => Read_Byte,
          More_Bytes         => More_Bytes,
          Write_Literal      => LZ77_Emits_Literal_Byte,
          Write_DL_Code      => LZ77_Emits_DL_Code,
          Estimate_DL_Codes  => Dummy_Estimate_DL_Codes);
   begin  -- Encode
      R := Text_Buffer_Index (String_Buffer_Size - Look_Ahead_LZ77);
      case Method is
      when Deflate_Fixed =>  -- "Fixed" (predefined) compression structure
         -- We have only one compressed data block, then it is already the last one.
         Put_Code (Code => 1, Code_Size => 1);  -- Signals last block
         Put_Code (Code => 1, Code_Size => 2);  -- Signals a "fixed" block
      when Taillaule_Deflation_Method =>
         null;  -- No start data sent, all is delayed
      end case;
      -- The whole compression is happening in the following line:
      My_LZ77;
      -- Done. Send the code signaling the end of compressed data block:
      case Method is
      when Deflate_Fixed =>
         Put_Huffman_Code (Curr_Descr.Lit_Len (End_Of_Block));
      when Taillaule_Deflation_Method =>
         if Lz_Buffer_Index * 2 = 0 then  -- Already flushed at latest Push, or empty data
            if Block_To_Finish and then Last_Block_Type in Fixed .. Dynamic then
               Put_Huffman_Code (Curr_Descr.Lit_Len (End_Of_Block));
            end if;
         else
            Flush_Half_Buffer (Last_Flush => True);
            if Last_Block_Type in Fixed .. Dynamic then
               Put_Huffman_Code (Curr_Descr.Lit_Len (End_Of_Block));
            end if;
         end if;
         if not Last_Block_Marked then
            -- Add a fake fixed block, just to have a final block...
            Put_Code (Code => 1, Code_Size => 1);  -- Signals last block
            Put_Code (Code => 1, Code_Size => 2);  -- Signals a "fixed" block
            Curr_Descr := Deflate_Fixed_Descriptors;
            Put_Huffman_Code (Curr_Descr.Lit_Len (End_Of_Block));
         end if;
      end case;
   end Encode;
begin -- Compress
   if Zlib_Format then
      Put_Byte (Byte => 16#78#); -- Zlib header
      Put_Byte (Byte => (case Method is -- Best guess; not important, apparently
                         when Deflate_Fixed .. Deflate_0 =>     1,
                         when Deflate_1                  => 16#5E#,
                         when Deflate_2                  => 16#9C#,
                         when Deflate_3 .. Deflate_R     => 16#DA#) );
   end if;

   Encode;
   Flush_Bit_Buffer;

   if Zlib_Format then
      Put_Byte (Byte => Byte_Value (Adler_2 / 256) ); -- Adler-32 checksum
      Put_Byte (Byte => Byte_Value (Adler_2 rem 256) );
      Put_Byte (Byte => Byte_Value (Adler_1 / 256) );
      Put_Byte (Byte => Byte_Value (Adler_1 rem 256) );
   end if;

   Flush_Byte_Buffer;
end Compress;
