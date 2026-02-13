-- Adler-32 checksum calculation, derived from Z_Compression
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com

with Interfaces;

package Adler_32_Checksums is
   subtype Byte_Value is Interfaces.Unsigned_8;

   type Checksum_Info is limited private;
   -- Initial value: ready to start calculating a checksum

   procedure Reset (Info : in out Checksum_Info) with Inline;
   -- Makes Info ready to start calculating a checksum

   procedure Update (Info : in out Checksum_Info; Byte : in Byte_Value);
   -- Updates Info with Byte

   type Checksum_List is array (1 .. 4) of Byte_Value;

   function Checksum (Info : in Checksum_Info) return Checksum_List;
   -- Returns the checksum that has been calculated in Info
private -- Adler_32_Checksums
   type Adler_Value is mod 65521;

   type Checksum_Info is record
      Sum_1 : Adler_Value := 1;
      Sum_2 : Adler_Value := 0;
   end record;

   function Checksum (Info : in Checksum_Info) return Checksum_List is
      (Byte_Value (Info.Sum_2 / 256),
       Byte_Value (Info.Sum_2 rem 256),
       Byte_Value (Info.Sum_1 / 256),
       Byte_Value (Info.Sum_1 rem 256) );
end Adler_32_Checksums;
