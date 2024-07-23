-- Standalone version of the Zlib Deflate compression algorithm, derived from de Montmollin's Zip-Ada
-- Copyright (C) by Pragmada Software Engineering
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses

package body Z_Compression is
   procedure Compress (Method : In Method_ID; Zlib_Format : in Boolean := True) is separate;
end Z_Compression;
