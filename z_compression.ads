-- Standalone version of the Zlib Deflate compression algorithm, derived from de Montmollin's Zip-Ada
-- Copyright (C) by Pragmada Software Engineering
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses

with Interfaces;

package Z_Compression is
   subtype Byte_Value is Interfaces.Unsigned_8;

   type Byte_Buffer is array (Natural range <>) of Byte_Value;

   type Method_ID is (Deflate_Fixed, Deflate_0, Deflate_1, Deflate_2, Deflate_3, Deflate_R);
   --  0: No LZ77, only Huffman.
   --  Deflate_Fixed compresses the data into a single block and with predefined
   --  ("fixed") compression structures. The data are basically LZ-compressed
   --  only, since the Huffman code sets are flat and not tailored for the data.
   --  The multi-block Deflate methods (0-R) use refined techniques to decide when to
   --  start a new block and what sort of block to put next.
   subtype Taillaule_Deflation_Method is Method_ID range Deflate_0 .. Method_ID'Last;

   Data_Exhausted : exception; -- Should be raised by Next function for procedure Compress if Out_Of_Data

   generic -- Compress
      with function Out_Of_Data return Boolean;
      -- Returns True when all data have been obtained through calls to Next, and another call to Next will fail; False otherwise

      with function Next return Byte_Value;
      -- Pre => not Out_Of_Data or else raise Data_Exhausted
      -- Returns the next byte of data

      with procedure Put (Byte : in Byte_Value);
      -- Deals with result bytes as they are created
   procedure Compress (Method : in Method_ID; Zlib_Format : in Boolean := True);
   -- Puts bytes representing a compressed version of the bytes obtained with Next in the format indicated by Zlib_Format
   -- If Zlib_Format: Zlib header (2 bytes), Deflate-compressed data, Adler-32 checksum
   -- Otherwise, just the Deflate-compressed data
end Z_Compression;
