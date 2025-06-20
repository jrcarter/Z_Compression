-- Standalone version of the Zlib Deflate compression algorithm, derived from de Montmollin's Zip-Ada
-- Copyright (C) by PragmAda Software Engineering
-- SPDX-License-Identifier: BSD-3-Clause
-- See https://spdx.org/licenses/
-- If you find this software useful, please let me know, either through
-- github.com/jrcarter or directly to pragmada@pragmada.x10hosting.com

package body Z_Compression is
   procedure Compress (Method : In Method_ID; Zlib_Format : in Boolean := True) is separate;
end Z_Compression;
