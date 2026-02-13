-- Adler-32 checksum calculation, derived from Z_Compression
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com

package body Adler_32_Checksums is
   procedure Reset (Info : in out Checksum_Info) is
      -- Empty
   begin -- Reset
      Info := (others => <>);
   end Reset;

   procedure Update (Info : in out Checksum_Info; Byte : in Byte_Value) is
      -- Empty
   begin -- Update
      Info.Sum_1 := Info.Sum_1 + Adler_Value (Byte);
      Info.Sum_2 := Info.Sum_2 + Info.Sum_1;
   end Update;
end Adler_32_Checksums;
