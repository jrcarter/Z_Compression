-- Standalone version of the Zlib Deflate compression algorithm, derived from de Montmollin's Zip-Ada
-- Copyright (C) by Pragmada Software Engineering
-- Released under the terms of the BSD 3-Clause license; see https://opensource.org/licenses

-- Reused from Zip-Ada, unchanged except for the name
--  There are four LZ77 encoders at choice in this package:
--
--    1/  LZ77_using_LZHuf, based on LZHuf by Haruhiko Okumura and Haruyasu Yoshizaki.
--
--    2/  LZ77_using_IZ, based on Info-Zip's Zip's deflate.c by Jean-Loup Gailly.
--          deflate.c is actually the LZ77 part of Info-Zip's compression.
--
--    3/  LZ77_using_BT4, based on LZMA SDK's BT4 algorithm by Igor Pavlov.
--
--    4/  LZ77_by_Rich, based on PROG2.C by Rich Geldreich, Jr.
--
--  Variant 1/, LZ77_using_LZHuf, is working since 2009. Two problems: it is slow
--     and not well adapted to the Deflate format (mediocre compression).
--
--  Variant 2/, LZ77_using_IZ, is much faster, and better suited for Deflate.
--     It has been added on 05-Mar-2016.
--     The code is tailored and optimized for a single set of
--     the String_buffer_size, Look_Ahead, Threshold LZ77 parameters - those for Deflate.
--
--  Variant 3/, LZ77_using_BT4, was added on 06-Sep-2016.
--     The seems to be the best match finder for LZMA on data of the >= 1 MiB scale.

--  To do:
--
--  2/
--    - LZ77 / IZ: similar to the test with TOO_FAR, try to cluster distances around
--        values needing less extra bits (may not work at all...)
--    - LZ77 / IZ: tune TOO_FAR (max: 32767), see http://optipng.sf.net/pngtech/too_far.html
--        "TOO_FAR in zlib Is Not Too Far" for discussion

--  Legal licensing note:

--  Copyright (c) 2016 .. 2020 Gautier de Montmollin (maintainer of the Ada version)
--  SWITZERLAND

--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

--  NB: this is the MIT License, as found 21-Aug-2016 on the site
--  http://www.opensource.org/licenses/mit-license.php

-- Because of this derivation, the code may not adhere to the PragmAda Coding Standard

with Ada.Text_IO, Ada.Integer_Text_IO;
with Ada.Unchecked_Deallocation;
with Interfaces; use Interfaces;
with System;

package body Z_Compression.LZ77 is

   --  System.Word_Size: 13.3(8): A word is the largest amount of storage
   --  that can be conveniently and efficiently manipulated by the hardware,
   --  given the implementation's run-time model.
   --
   Min_Bits_32 : constant := Integer'Max (32, System.Word_Size);
   Min_Bits_16 : constant := Integer'Max (16, System.Word_Size);

   --  We define an Integer type which is at least 32 bits, but n bits
   --  on a native n (> 32) bits architecture (no performance hit on 64+
   --  bits architectures).
   --  Integer_M16 not needed: Integer already guarantees 16 bits
   --
   type Integer_M32 is range -2 ** (Min_Bits_32 - 1) .. 2 ** (Min_Bits_32 - 1) - 1;
   subtype Natural_M32  is Integer_M32 range 0 .. Integer_M32'Last;
   --  subtype Positive_M32 is Integer_M32 range 1 .. Integer_M32'Last;

   type Unsigned_M16 is mod 2 ** Min_Bits_16;
   type Unsigned_M32 is mod 2 ** Min_Bits_32;

   function Are_Matches_Sorted (M : Matches_Type) return Boolean is
   begin
      for I in 2 .. M.Count loop
         if M.Dl (I).Length < M.Dl (I - 1).Length then
            return False;
         end if;
      end loop;
      return True;
   end Are_Matches_Sorted;

   procedure Encode is

      -----------------------
      --  LZHuf algorithm  --
      -----------------------

      procedure LZ77_Using_LZHuf is
         --  Based on LZHUF by OKUMURA & YOSHIZAKI.
         --  Here the adaptive Huffman coding is thrown away:
         --  algorithm is used only to find matching patterns.

         N_Char    : constant Integer := 256 - Threshold + Look_Ahead;
         --  Character code (= 0..N_CHAR-1)
         Max_Table     : constant Integer := N_Char * 2 - 1;

         type Text_Buffer is array (0 .. String_Buffer_Size + Look_Ahead - 1) of Byte;
         Empty_Buffer : constant Text_Buffer := (others => 32);  --  ' '

         --  > The Huffman frequency handling is made generic so we have
         --    one copy of the tree and of the frequency table for Encode
         --    and one for Decode

         generic
         package Huffman is
            --- Pointing parent nodes.
            --- Area [Max_Table..(Max_Table + N_CHAR - 1)] are pointers for leaves
            Parent :  array (0 .. Max_Table + N_Char - 1) of Natural;
            --- Pointing children nodes (son[], son[] + 1)
            Son    :  array (0 .. Max_Table - 1)  of Natural;

            Root_Position : constant Integer := Max_Table - 1;  --  (can be always Son'last ?)

            procedure Start;
            procedure Update_Freq_Tree (C0 : Natural);
         end Huffman;

         package body Huffman is

            Freq : array (0 .. Max_Table) of Natural;  --  Cumulative freq table

            Max_Freq : constant := 16#8000#;
            --  ^-- update when cumulative frequency reaches to this value

            procedure Start is
               I : Natural;
            begin
               for J in  0 .. N_Char - 1  loop
                  Freq (J) := 1;
                  Son (J) := J + Max_Table;
                  Parent (J + Max_Table) := J;
               end loop;

               I := 0;
               for J in N_Char .. Root_Position  loop
                  Freq (J) := Freq (I) + Freq (I + 1);
                  Son (J) := I;
                  Parent (I) := J;
                  Parent (I + 1) := J;
                  I := I + 2;
               end loop;

               Freq (Freq'Last) := 16#FFFF#;  --  ( Max_Table )
               Parent (Root_Position) := 0;
            end Start;

            procedure Update_Freq_Tree (C0 : Natural) is

               procedure Reconstruct_Freq_Tree is
                  I, J, K, F, L : Natural;
               begin
                  --  Halven cumulative freq for leaf nodes
                  J := 0;
                  for I in 0 .. Root_Position loop
                     if Son (I) >= Max_Table then
                        Freq (J) := (Freq (I) + 1) / 2;
                        Son (J) := Son (I);
                        J := J + 1;
                     end if;
                  end loop;

                  --  Make a tree : first, connect children nodes
                  I := 0;
                  for J in N_Char .. Root_Position loop  --  J : free nodes
                     K := I + 1;
                     F := Freq (I) + Freq (K); -- new frequency
                     Freq (J) := F;
                     K := J - 1;
                     while F < Freq (K) loop
                        K := K - 1;
                     end loop;

                     K := K + 1;
                     L := J - K;  --  2007: fix: was L:= (J-K)*2, memcopy parameter remain

                     Freq (K + 1 .. K + L) := Freq (K .. K + L - 1);  --  shift by one cell right
                     Freq (K) := F;
                     Son (K + 1 .. K + L) := Son (K .. K + L - 1);  --  shift by one cell right
                     Son (K) := I;
                     I := I + 2;
                  end loop;

                  --  Connect parent nodes
                  for I in 0 .. Max_Table - 1 loop
                     K := Son (I);
                     Parent (K) := I;
                     if K < Max_Table then
                        Parent (K + 1) := I;
                     end if;
                  end loop;

               end Reconstruct_Freq_Tree;

               C, I, J, K, L : Natural;

            begin  --  Update_Freq_Tree;
               if Freq (Root_Position) = Max_Freq then
                  Reconstruct_Freq_Tree;
               end if;
               C := Parent (C0 + Max_Table);
               loop
                  Freq (C) := Freq (C) + 1;
                  K := Freq (C);
                  --  Swap nodes to keep the tree freq-ordered
                  L := C + 1;
                  if  K > Freq (L) then
                     while K > Freq (L + 1) loop
                        L := L + 1;
                     end loop;

                     Freq (C) := Freq (L);
                     Freq (L) := K;

                     I := Son (C);
                     Parent (I) := L;
                     if I < Max_Table then
                        Parent (I + 1) := L;
                     end if;

                     J := Son (L);
                     Son (L) := I;

                     Parent (J) := C;
                     if J < Max_Table then
                        Parent (J + 1) := C;
                     end if;
                     Son (C) := J;

                     C := L;
                  end if;
                  C := Parent (C);
                  exit when C = 0;
               end loop;        -- do it until reaching the root
            end Update_Freq_Tree;

         end Huffman;

         Node_Nil : constant Integer := String_Buffer_Size;    --  End of tree's node

         Lson, Dad :  array (0 .. String_Buffer_Size) of Natural;
         Rson     :       array (0 .. String_Buffer_Size + 256) of Natural;

         procedure Init_Tree is
         begin
            for I in String_Buffer_Size + 1 .. Rson'Last loop
               Rson (I) := Node_Nil;
            end loop;  --  root
            for I in 0 .. String_Buffer_Size - 1 loop
               Dad (I) := Node_Nil;
            end loop;  --  node
         end Init_Tree;

         Match_Position : Natural;
         Match_Length   : Natural;

         Text_Buf : Text_Buffer := Empty_Buffer;

         procedure Insert_Node (R : Integer) is
            I, P : Integer;
            Geq  : Boolean := True;
            C    : Natural;
         begin
            P := String_Buffer_Size + 1 + Integer (Text_Buf (R));
            Rson (R) := Node_Nil;
            Lson (R) := Node_Nil;
            Match_Length := 0;
            loop
               if Geq then
                  if Rson (P) = Node_Nil then
                     Rson (P) := R;
                     Dad (R) := P;
                     return;
                  end if;
                  P := Rson (P);
               else
                  if Lson (P) = Node_Nil then
                     Lson (P) := R;
                     Dad (R) := P;
                     return;
                  end if;
                  P := Lson (P);
               end if;
               I := 1;
               while I < Look_Ahead and then Text_Buf (R + I) = Text_Buf (P + I)  loop
                  I := I + 1;
               end loop;

               Geq := Text_Buf (R + I) >= Text_Buf (P + I) or I = Look_Ahead;

               if I > Threshold then
                  if I > Match_Length then
                     Match_Position := (R - P) mod String_Buffer_Size - 1;
                     Match_Length := I;
                     exit when Match_Length >= Look_Ahead;
                  end if;
                  if I = Match_Length then
                     C := (R - P) mod String_Buffer_Size - 1;
                     if C < Match_Position then
                        Match_Position := C;
                     end if;
                  end if;
               end if;
            end loop;

            Dad (R)  := Dad (P);
            Lson (R) := Lson (P);
            Rson (R) := Rson (P);
            Dad (Lson (P)) := R;
            Dad (Rson (P)) := R;
            if Rson (Dad (P)) = P then
               Rson (Dad (P)) := R;
            else
               Lson (Dad (P)) := R;
            end if;
            Dad (P) := Node_Nil;  --  remove P
         end Insert_Node;

         procedure Delete_Node (P : Natural) is
            Q : Natural;
         begin
            if Dad (P) = Node_Nil then  --  unregistered
               return;
            end if;
            if Rson (P) = Node_Nil then
               Q := Lson (P);
            elsif Lson (P) = Node_Nil then
               Q := Rson (P);
            else
               Q := Lson (P);
               if Rson (Q) /= Node_Nil then
                  loop
                     Q := Rson (Q);
                     exit when Rson (Q) = Node_Nil;
                  end loop;
                  Rson (Dad (Q)) := Lson (Q);
                  Dad (Lson (Q)) := Dad (Q);
                  Lson (Q) := Lson (P);
                  Dad (Lson (P)) := Q;
               end if;
               Rson (Q) := Rson (P);
               Dad (Rson (P)) := Q;
            end if;
            Dad (Q) := Dad (P);
            if Rson (Dad (P)) = P then
               Rson (Dad (P)) := Q;
            else
               Lson (Dad (P)) := Q;
            end if;
            Dad (P) := Node_Nil;
         end Delete_Node;

         package Huffman_E is new Huffman;

         I, R, S, Last_Match_Length : Natural;
         Len                        : Integer;
         C                          : Byte;
      begin
         if not More_Bytes then
            return;
         end if;
         Huffman_E.Start;
         Init_Tree;
         S := 0;
         R := String_Buffer_Size - Look_Ahead;
         Len := 0;
         while Len < Look_Ahead and More_Bytes loop
            Text_Buf (R + Len) := Read_Byte;
            Len := Len + 1;
         end loop;

         --  Seems: fill dictionary with default value
         --
         --  for I in 1.. Look_Ahead loop
         --    Insert_Node(R - I);
         --  end loop;

         Insert_Node (R);

         loop
            if Match_Length > Len then
               Match_Length := Len;
            end if;
            if Match_Length <= Threshold then
               Match_Length := 1;
               Huffman_E.Update_Freq_Tree (Natural (Text_Buf (R)));
               Write_Literal (Text_Buf (R));
            else
               Write_DL_Code (Match_Position + 1, Match_Length);
            end if;
            Last_Match_Length := Match_Length;
            I := 0;
            while I < Last_Match_Length and More_Bytes loop
               I := I + 1;
               Delete_Node (S);
               C := Read_Byte;
               Text_Buf (S) := C;
               if  S < Look_Ahead - 1 then
                  Text_Buf (S + String_Buffer_Size) := C;
               end if;
               S := (S + 1) mod String_Buffer_Size;
               R := (R + 1) mod String_Buffer_Size;
               Insert_Node (R);
            end loop;

            while I < Last_Match_Length loop
               I := I + 1;
               Delete_Node (S);
               S := (S + 1) mod String_Buffer_Size;
               R := (R + 1) mod String_Buffer_Size;
               Len := Len - 1;
               if Len > 0 then
                  Insert_Node (R);
               end if;
            end loop;

            exit when Len = 0;
         end loop;
      end LZ77_Using_LZHuf;

      --------------------------
      --  Info-Zip algorithm  --
      --------------------------

      --  LZ77_using_IZ: based on deflate.c by Jean-Loup Gailly.
      --  Core description of the algorithm:
      --
      --     The most straightforward technique turns out to be the fastest for
      --     most input files: try all possible matches and select the longest.
      --     The key feature of this algorithm is that insertions into the string
      --     dictionary are very simple and thus fast, and deletions are avoided
      --     completely. Insertions are performed at each input character, whereas
      --     string matches are performed only when the previous match ends. So it
      --     is preferable to spend more time in matches to allow very fast string
      --     insertions and avoid deletions. The matching algorithm for small
      --     strings is inspired from that of Rabin & Karp [1]. A brute force approach
      --     is used to find longer strings when a small match has been found.
      --
      --     The idea of lazy evaluation of matches is due to Jan-Mark Wams.
      --
      --     [1] A description of the Rabin and Karp algorithm is given in the book
      --         "Algorithms" by R. Sedgewick, Addison-Wesley, p252.
      --
      --  About hashing: chapter 6.4 of The Art of Computer Programming, Volume 3, D.E. Knuth
      --  Rabin and Karp algorithm: http://en.wikipedia.org/wiki/Rabin%E2%80%93Karp_algorithm

      --  Compression level: 0: store, 1: best speed, 9: best compression, 10: variant of level 9
      --  Ada code: only levels 4 to 10 are supported.

      procedure LZ77_Using_IZ (Level : Natural) is
         HASH_BITS        : constant := 15;  --  13..15
         HASH_SIZE        : constant := 2 ** HASH_BITS;
         HASH_MASK        : constant := HASH_SIZE - 1;
         WSIZE            : constant Integer_M32 := Integer_M32 (String_Buffer_Size);
         WMASK            : constant Unsigned_M16 := Unsigned_M16 (WSIZE - 1);
         --  HASH_SIZE and WSIZE must be powers of two
         NIL              : constant := 0;     --  Tail of hash chains
         TOO_FAR          : constant := 4096;  --  Matches of length 3 are discarded if their distance exceeds TOO_FAR
         --
         subtype Ulg is Unsigned_M32;
         subtype Unsigned is Unsigned_M16;
         subtype Ush is Unsigned_M16;
         --  subtype long is Integer_M32;
         --  subtype int is Integer;
         subtype Pos is Unsigned_M32;  --  must be at least 32 bits
         --  subtype IPos is unsigned;
         --  A Pos is an index in the character window. IPos is used only for parameter passing.
         Window           : array (0 .. 2 * WSIZE - 1) of Byte;
         --  Sliding window. Input bytes are read into the second half of the window,
         --  and move to the first half later to keep a dictionary of at least WSIZE
         --  bytes. With this organization, matches are limited to a distance of
         --  WSIZE-MAX_MATCH bytes, but this ensures that IO is always
         --  performed with a length multiple of the block size.
         Prev             : array (0 .. Unsigned (WSIZE - 1)) of Pos;
         --  Link to older string with same hash index.
         --  This link is maintained only for the last 32K strings.
         --  An index in this array is thus a window index modulo 32K.
         Head             : array (0 .. Unsigned (HASH_SIZE - 1)) of Pos;
         --  Heads of the hash chains or NIL.
         Window_Size      : Ulg;
         --  window size, 2*WSIZE except for MMAP or BIG_MEM, where it is the
         --  input file length plus MIN_LOOKAHEAD.
         Sliding          : Boolean;  --  Set to False when the input file is already in memory  [was: int]
         Ins_H            : Unsigned;   --  hash index of string to be inserted
         MIN_MATCH        : constant Integer_M32 := Integer_M32 (Threshold) + 1;    --  Deflate: 3
         MAX_MATCH        : constant Integer_M32 := Integer_M32 (Look_Ahead);       --  Deflate: 258
         --  Minimum amount of lookahead, except at the end of the input file.
         MIN_LOOKAHEAD    : constant Integer_M32 := MAX_MATCH + MIN_MATCH + 1;  --  Deflate: 262
         --  This LZ77 compression doesn't use the full possible distance range: 32507..32768 unused!
         MAX_DIST         : constant Integer_M32 := WSIZE - MIN_LOOKAHEAD;  --  Deflate: 32506
         H_SHIFT          : constant Integer := Integer ((HASH_BITS + MIN_MATCH - 1) / MIN_MATCH);
         --  Number of bits by which ins_h and del_h must be shifted at each
         --  input step. It must be such that after MIN_MATCH steps, the oldest
         --  byte no longer takes part in the hash key, that is:
         --  H_SHIFT * MIN_MATCH >= HASH_BITS
         Prev_Length      : Natural_M32; --  [was: unsigned]
         --  Length of the best match at previous step. Matches not greater than this
         --  are discarded. This is used in the lazy match evaluation.
         Strstart         : Natural_M32;   --  start of string to insert [was: unsigned]
         Match_Start      : Natural_M32;   --  start of matching string [was: unsigned]
         Eofile           : Boolean;       --  flag set at end of input file [was: int]
         Lookahead        : Natural_M32;   --  number of valid bytes ahead in window  [was: unsigned]
         Max_Chain_Length : Unsigned;
         --  To speed up deflation, hash chains are never searched beyond this length.
         --  A higher limit improves compression ratio but degrades the speed.
         Max_Lazy_Match   : Natural_M32;  --  [was: unsigned]
         --  Attempt to find a better match only when the current match is strictly
         --  smaller than this value. This mechanism is used only for compression
         --  levels >= 4.
         Good_Match       : Natural_M32;  --  [was: unsigned]
         --  Use a faster search when the previous match is longer than this
         Nice_Match       : Integer_M32;  --  Stop searching when current match exceeds this
         --  Values for max_lazy_match, good_match, nice_match and max_chain_length,
         --  depending on the desired pack level (0..9). The values given below have
         --  been tuned to exclude worst case performance for pathological files.
         --  Better values may be found for specific files.
         type Config is record
            Good_Length  : Natural_M32;  --  reduce lazy search above this match length [was: ush]
            Max_Lazy     : Natural_M32;  --  do not perform lazy search above this match length
            Nice_Length  : Integer_M32;  --  quit search above this match length
            Max_Chain    : Ush;
         end record;

         Configuration_Table : constant array (0 .. 10) of Config := (
                                                                      --  good lazy nice chain
                                                                         (0,    0,  0,    0),    --  0: store only
                                                                      (4,    4,  8,    4),    --  1: maximum speed, no lazy matches
                                                                      (4,    5, 16,    8),
                                                                      (4,    6, 32,   32),
                                                                      (4,    4, 16,   16),    --  4: lazy matches
                                                                      (8,   16, 32,   32),
                                                                      (8,   16, 128, 128),
                                                                      (8,   32, 128, 256),
                                                                      (32, 128, 258, 1024),
                                                                      (32, 258, 258, 4096),   --  9: maximum compression
                                                                      (34, 258, 258, 4096));  --  "secret" variant of level 9

         --  Update a hash value with the given input byte
         --  IN  assertion: all calls to to UPDATE_HASH are made with consecutive
         --     input characters, so that a running hash key can be computed from the
         --     previous key instead of complete recalculation each time.

         procedure UPDATE_HASH (H : in out Unsigned; C : Byte) is
            pragma Inline (UPDATE_HASH);
         begin
            H := (Unsigned (Shift_Left (Unsigned_32 (H), H_SHIFT)) xor Unsigned (C)) and HASH_MASK;
         end UPDATE_HASH;

         --  Insert string starting at s in the dictionary and set match_head to the previous head
         --  of the hash chain (the most recent string with same hash key). Return
         --  the previous length of the hash chain.
         --  IN  assertion: all calls to to INSERT_STRING are made with consecutive
         --     input characters and the first MIN_MATCH bytes of s are valid
         --     (except for the last MIN_MATCH-1 bytes of the input file).

         procedure INSERT_STRING (S : Integer_M32; Match_Head : out Natural_M32) is
            pragma Inline (INSERT_STRING);
         begin
            UPDATE_HASH (Ins_H, Window (S + MIN_MATCH - 1));
            Match_Head := Natural_M32 (Head (Ins_H));
            Prev (Unsigned (S) and WMASK) := Pos (Match_Head);
            Head (Ins_H) := Pos (S);
         end INSERT_STRING;

         procedure Read_Buf (From : Integer_M32; Amount : Unsigned; Actual : out Integer_M32) is
            Need : Unsigned := Amount;
         begin
            --  put_line("Read buffer: from:" & from'img & ";  amount:" & amount'img);
            Actual := 0;
            while Need > 0 and then More_Bytes loop
               Window (From + Actual) := Read_Byte;
               Actual := Actual + 1;
               Need := Need - 1;
            end loop;
            --  put_line("Read buffer: actual:" & actual'img);
         end Read_Buf;

         --  Fill the window when the lookahead becomes insufficient.
         --  Updates strstart and lookahead, and sets eofile if end of input file.
         --
         --  IN assertion: lookahead < MIN_LOOKAHEAD && strstart + lookahead > 0
         --  OUT assertions: strstart <= window_size-MIN_LOOKAHEAD
         --     At least one byte has been read, or eofile is set; file reads are
         --     performed for at least two bytes (required for the translate_eol option).

         procedure Fill_Window is
            More : Unsigned;
            M    : Pos;
            N    : Natural_M32;
         begin
            loop
               More := Unsigned (Window_Size - Ulg (Lookahead) - Ulg (Strstart));
               if False then  --  C: "if (more == (unsigned)EOF) {" ?... GdM: seems a 16-bit code for EOF
                  --  Very unlikely, but possible on 16 bit machine if strstart == 0
                  --  and lookahead == 1 (input done one byte at time)
                  More := More - 1;
               elsif Strstart >= WSIZE + MAX_DIST and then Sliding then
                  --  By the IN assertion, the window is not empty so we can't confuse
                  --  more == 0 with more == 64K on a 16 bit machine.
                  Window (0 .. WSIZE - 1) := Window (WSIZE .. 2 * WSIZE - 1);
                  --  GdM: in rare cases (e.g. level 9 on test file "enwik8"), match_start happens
                  --  to be < WSIZE. We do as in the original 16-bit C code: mod 2**16, such that the
                  --  index is the window's range.
                  --  This assumes WSIZE = 2**15, which is checked at startup of LZ77_using_IZ.
                  --  Very likely, match_start is garbage anyway - see http://sf.net/p/infozip/bugs/49/
                  Match_Start := Natural_M32 (Unsigned_16 (Match_Start) - Unsigned_16 (WSIZE mod (2 ** 16)));
                  Strstart    := Strstart - WSIZE; -- we now have strstart >= MAX_DIST:
                  for Nn in 0 .. Unsigned'(HASH_SIZE - 1) loop
                     M := Head (Nn);
                     if M >= Pos (WSIZE) then
                        Head (Nn) := M - Pos (WSIZE);
                     else
                        Head (Nn) := NIL;
                     end if;
                  end loop;
                  --
                  for Nn in 0 .. Unsigned (WSIZE - 1) loop
                     M := Prev (Nn);
                     if M >= Pos (WSIZE) then
                        Prev (Nn) := M - Pos (WSIZE);
                     else
                        Prev (Nn) := NIL;
                     end if;
                     --  If n is not on any hash chain, prev[n] is garbage but its value will never be used.
                  end loop;
                  More := More + Unsigned (WSIZE);
               end if;
               exit when Eofile;
               --  If there was no sliding:
               --     strstart <= WSIZE+MAX_DIST-1 && lookahead <= MIN_LOOKAHEAD - 1 &&
               --     more == window_size - lookahead - strstart
               --  => more >= window_size - (MIN_LOOKAHEAD-1 + WSIZE + MAX_DIST-1)
               --  => more >= window_size - 2*WSIZE + 2
               --  In the MMAP or BIG_MEM case (not yet supported in gzip),
               --    window_size == input_size + MIN_LOOKAHEAD  &&
               --    strstart + lookahead <= input_size => more >= MIN_LOOKAHEAD.
               --  Otherwise, window_size == 2*WSIZE so more >= 2.
               --  If there was sliding, more >= WSIZE. So in all cases, more >= 2.
               --
               pragma Assert (More >= 2, "more < 2");
               --
               Read_Buf (Strstart + Lookahead, More, N);
               if N = 0 then
                  Eofile := True;
               else
                  Lookahead := Lookahead + N;
               end if;
               exit when Lookahead >= MIN_LOOKAHEAD or Eofile;
            end loop;
            --  put_line("Fill done - eofile = " & eofile'img);
         end Fill_Window;

         --  Initialize the "longest match" routines for a new file
         --
         --  IN assertion: window_size is > 0 if the input file is already read or
         --     mapped in the window array, 0 otherwise. In the first case,
         --     window_size is sufficient to contain the whole input file plus
         --     MIN_LOOKAHEAD bytes (to avoid referencing memory beyond the end
         --     of window when looking for matches towards the end).

         procedure LM_Init (Pack_Level : Natural) is
         begin
            --  Do not slide the window if the whole input is already in memory (window_size > 0)
            Sliding := False;
            if Window_Size = 0 then
               Sliding := True;
               Window_Size := 2 * Ulg (WSIZE);
            end if;
            --  Initialize the hash table.
            --  prev will be initialized on the fly.
            Head := (others => NIL);
            --  Set the default configuration parameters:
            Max_Lazy_Match   := Configuration_Table (Pack_Level).Max_Lazy;
            Good_Match       := Configuration_Table (Pack_Level).Good_Length;
            Nice_Match       := Configuration_Table (Pack_Level).Nice_Length;
            Max_Chain_Length := Configuration_Table (Pack_Level).Max_Chain;
            --  Info-Zip comment: ??? reduce max_chain_length for binary files
            Strstart := 0;
            Read_Buf (0, Unsigned (WSIZE), Lookahead);
            if Lookahead = 0 then
               Eofile := True;
               return;
            end if;
            Eofile := False;
            --  Make sure that we always have enough lookahead. This is important
            --  if input comes from a device such as a tty.
            if Lookahead < MIN_LOOKAHEAD then
               Fill_Window;
            end if;
            Ins_H := 0;
            for J in 0 .. Natural_M32 (MIN_MATCH) - 2 loop
               UPDATE_HASH (Ins_H, Window (J));
            end loop;
            --  If lookahead < MIN_MATCH, ins_h is garbage, but this is
            --  not important since only literal bytes will be emitted.
         end LM_Init;

         --  Set match_start to the longest match starting at the given string and
         --  return its length. Matches shorter or equal to prev_length are discarded,
         --  in which case the result is equal to prev_length and match_start is
         --  garbage.
         --  IN assertions: current_match is the head of the hash chain for the current
         --    string (strstart) and its distance is <= MAX_DIST, and prev_length >= 1

         procedure Longest_Match (Current_Match : in out Integer_M32; Longest : out Integer_M32) is
            Chain_Length : Unsigned := Max_Chain_Length;  --  max hash chain length
            Scan         : Integer_M32 := Strstart;       --  current string
            Match        : Integer_M32;                   --  matched string
            Len          : Integer_M32;                   --  length of current match
            Best_Len     : Integer_M32 := Prev_Length;    --  best match length so far
            Limit        : Natural_M32;  --  [was: IPos]
            Strend       : constant Integer_M32 := Strstart + MAX_MATCH;
            Scan_End     : Integer_M32 := Scan + Best_Len;
         begin
            --  Stop when current_match becomes <= limit. To simplify the code,
            --  we prevent matches with the string of window index 0.
            if Strstart > MAX_DIST then
               Limit := Strstart - MAX_DIST;
            else
               Limit := NIL;
            end if;
            --  Do not waste too much time if we already have a good match:
            if Prev_Length >= Good_Match then
               Chain_Length := Chain_Length / 4;
            end if;
            pragma Assert
               (Strstart <= Integer_M32 (Window_Size) - MIN_LOOKAHEAD,
                "insufficient lookahead");  --  In deflate.c
            loop
               if Current_Match >= Strstart then
                  --  Added 2020-11-07. The file test/sample.jpg bombs the assertion a few lines later.
                  Longest := MIN_MATCH - 1;
                  return;
               end if;
               pragma Assert (Current_Match < Strstart, "no future");  --  In deflate.c
               Match := Current_Match;
               --  Skip to next match if the match length cannot increase
               --  or if the match length is less than 2:
               --
               --  NB: this is the Not-UNALIGNED_OK variant in the C code.
               --      Translation of the UNALIGNED_OK variant is left as an exercise ;-).
               --      (!! worth a try: GNAT optimizes window(match..match+1[3]) to 16[32] bit)
               --
               if Window (Match + Best_Len)     /= Window (Scan_End) or else
                  Window (Match + Best_Len - 1) /= Window (Scan_End - 1) or else
                  Window (Match)                /= Window (Scan) or else
                  Window (Match + 1)            /= Window (Scan + 1)
               then
                  Match := Match + 1;  --  C: continue
               else
                  --  The check at best_len - 1 can be removed because it will be made
                  --  again later. (This heuristic is not always a win.)
                  --
                  --  It is not necessary to compare window(scan + 2) and window(match + 2) since they
                  --  are always equal when the other bytes match, given that
                  --  the hash keys are equal and that HASH_BITS >= 8.
                  Scan := Scan + 2;
                  Match := Match + 2;
                  --  C: The code is optimized for HASH_BITS >= 8 and MAX_MATCH-2 multiple of 16.
                  --     It is easy to get rid of this optimization if necessary.
                  --  Ada: see the "else" part below.
                  if MAX_MATCH = 258 then
                     --  We check for insufficient lookahead only every 8th comparison;
                     --  the 256th check will be made at strstart + 258.
                     loop
                        Scan := Scan + 1;
                        Match := Match + 1;
                        exit when Window (Scan) /= Window (Match);
                        Scan := Scan + 1;
                        Match := Match + 1;
                        exit when Window (Scan) /= Window (Match);
                        Scan := Scan + 1;
                        Match := Match + 1;
                        exit when Window (Scan) /= Window (Match);
                        Scan := Scan + 1;
                        Match := Match + 1;
                        exit when Window (Scan) /= Window (Match);
                        Scan := Scan + 1;
                        Match := Match + 1;
                        exit when Window (Scan) /= Window (Match);
                        Scan := Scan + 1;
                        Match := Match + 1;
                        exit when Window (Scan) /= Window (Match);
                        Scan := Scan + 1;
                        Match := Match + 1;
                        exit when Window (Scan) /= Window (Match);
                        Scan := Scan + 1;
                        Match := Match + 1;
                        exit when Window (Scan) /= Window (Match) or else Scan >= Strend;
                     end loop;
                  else
                     --  We check for insufficient lookahead after every comparison.
                     loop
                        Scan := Scan + 1;
                        Match := Match + 1;
                        exit when Window (Scan) /= Window (Match) or else Scan >= Strend;
                     end loop;
                  end if;
                  --  Assert(scan <= window+(unsigned)(window_size-1), "wild scan");  ??
                  Len := MAX_MATCH - (Strend - Scan);
                  Scan := Strend - MAX_MATCH;
                  if Len > Best_Len then
                     Match_Start := Current_Match;
                     Best_Len := Len;
                     exit when Len >= Nice_Match;
                     Scan_End  := Scan + Best_Len;
                  end if;
               end if;
               Current_Match := Integer_M32 (Prev (Unsigned (Current_Match) and WMASK));
               exit when Current_Match <= Limit;
               Chain_Length := Chain_Length - 1;
               exit when Chain_Length = 0;
            end loop;
            Longest := Best_Len;
         end Longest_Match;

         procedure LZ77_Part_Of_IZ_Deflate is
            Hash_Head       : Natural_M32 := NIL;              --  head of hash chain
            Prev_Match      : Natural_M32;                     --  previous match  [was: IPos]
            Match_Available : Boolean := False;           --  set if previous match exists
            Match_Length    : Natural_M32 := MIN_MATCH - 1;  --  length of best match
            Max_Insert      : Natural_M32;
         begin
            Match_Start := 0;  --  NB: no initialization in deflate.c
            --  NB: level <= 3 would call deflate_fast;
            --
            --  Process the input block.
            while Lookahead /= 0 loop
               --  Insert the string window(strstart .. strstart + 2) in the
               --  dictionary, and set hash_head to the head of the hash chain:
               if Lookahead >= MIN_MATCH then
                  INSERT_STRING (Strstart, Hash_Head);
               end if;
               --  Find the longest match, discarding those <= prev_length.
               Prev_Length  := Match_Length;
               Prev_Match   := Match_Start;
               Match_Length := MIN_MATCH - 1;
               if Hash_Head /= NIL and then
                  Prev_Length < Max_Lazy_Match and then
                  Strstart - Hash_Head <= MAX_DIST
               then
                  --  To simplify the code, we prevent matches with the string
                  --  of window index 0 (in particular we have to avoid a match
                  --  of the string with itself at the start of the input file).
                  --
                  --  Do not look for matches beyond the end of the input.
                  --  This is necessary to make deflate deterministic.
                  if Nice_Match > Lookahead then
                     Nice_Match := Lookahead;
                  end if;
                  Longest_Match (Hash_Head, Match_Length);
                  --  Longest_Match sets match_start
                  if Match_Length > Lookahead then
                     Match_Length := Lookahead;
                  end if;
                  --  Ignore a length 3 match if it is too distant:
                  if Match_Length = MIN_MATCH and then Strstart - Match_Start > TOO_FAR then
                     --  If prev_match is also MIN_MATCH, match_start is garbage
                     --  but we will ignore the current match anyway.
                     Match_Length := MIN_MATCH - 1;
                  end if;
               end if;
               --  If there was a match at the previous step and the current
               --  match is not better, output the previous match:
               if Prev_Length >= MIN_MATCH and then Match_Length <= Prev_Length then
                  Max_Insert := Strstart + Lookahead - MIN_MATCH;
                  --  C: in DEBUG mode: check_match(strstart-1, prev_match, prev_length);
                  --
                  ------------------------------------
                  --  Output a Distance-Length code --
                  ------------------------------------
                  Write_DL_Code (Positive (Strstart - 1 - Prev_Match), Positive (Prev_Length));
                  --  Insert in hash table all strings up to the end of the match.
                  --  strstart-1 and strstart are already inserted.
                  Lookahead := Lookahead - (Prev_Length - 1);
                  Prev_Length := Prev_Length - 2;
                  loop
                     Strstart := Strstart + 1;
                     if Strstart <= Max_Insert then
                        INSERT_STRING (Strstart, Hash_Head);
                        --  strstart never exceeds WSIZE - MAX_MATCH, so there
                        --  are always MIN_MATCH bytes ahead.
                     end if;
                     Prev_Length := Prev_Length - 1;
                     exit when Prev_Length = 0;
                  end loop;
                  Strstart := Strstart + 1;
                  Match_Available := False;
                  Match_Length := MIN_MATCH - 1;
               elsif Match_Available then
                  --  If there was no match at the previous position, output a
                  --  single literal. If there was a match but the current match
                  --  is longer, truncate the previous match to a single literal.
                  --
                  ------------------------
                  --  Output a literal  --
                  ------------------------
                  Write_Literal (Window (Strstart - 1));
                  Strstart := Strstart + 1;
                  Lookahead := Lookahead - 1;
               else
                  --  There is no previous match to compare with, wait for the next step to decide.
                  Match_Available := True;
                  Strstart := Strstart + 1;
                  Lookahead := Lookahead - 1;
               end if;
               --  Assert(strstart <= isize && lookahead <= isize, "a bit too far");
               --
               --  Make sure that we always have enough lookahead, except
               --  at the end of the input file. We need MAX_MATCH bytes
               --  for the next match, plus MIN_MATCH bytes to insert the
               --  string following the next match.
               if Lookahead < MIN_LOOKAHEAD then
                  Fill_Window;
               end if;
            end loop;
            -----------------------------------
            --  Output last literal, if any  --
            -----------------------------------
            if Match_Available then
               Write_Literal (Window (Strstart - 1));
            end if;
         end LZ77_Part_Of_IZ_Deflate;

         Code_Too_Clever : exception;
      begin
         if Look_Ahead /= 258 or String_Buffer_Size /= 2 ** 15 or Threshold /= 2 then
            raise Code_Too_Clever;  --  was optimized for these parameters
         end if;
         Window_Size := 0;
         LM_Init (Level);
         LZ77_Part_Of_IZ_Deflate;
      end LZ77_Using_IZ;

      ---------------------------------------------------------------------
      --  BT4  -  Binary tree of match positions selected with           --
      --          the leading 2 to 4 bytes of each possible match.       --
      ---------------------------------------------------------------------

      --  Based on BT4.java and LZMAEncoderFast.java by Lasse Collin,
      --  itself based on LzFind.c by Igor Pavlov.

      procedure LZ77_Using_BT4 is
         MATCH_LEN_MIN     : constant Integer := Threshold + 1;
         --
         ReadPos           : Integer := -1;
         Cur_Literal       : Byte;
         ReadLimit         : Integer := -1;
         Finishing         : constant Boolean := False;
         WritePos          : Integer :=  0;
         PendingSize       : Integer :=  0;
         --
         OPTS              : constant := 4096;
         EXTRA_SIZE_BEFORE : constant :=  OPTS;
         EXTRA_SIZE_AFTER  : constant :=  OPTS;

         KeepSizeBefore : constant Integer := EXTRA_SIZE_BEFORE + String_Buffer_Size;
         KeepSizeAfter  : constant Integer := EXTRA_SIZE_AFTER  + Look_Ahead;
         ReserveSize    : constant Integer :=
                            Integer'Min (
                                         String_Buffer_Size / 2 +
                                            256 * (2 ** 10),  --  256 KiB
                                         512 * (2 ** 20)   --  512 MiB
                                        );
         GetBufSize     : constant Integer := KeepSizeBefore + KeepSizeAfter + ReserveSize;

         type Int_Array is array (Natural range <>) of Integer;
         type P_Int_Array is access Int_Array;
         procedure Dispose is new Ada.Unchecked_Deallocation (Int_Array, P_Int_Array);

         procedure Normalize (Positions : in out Int_Array; NormalizationOffset : Integer) is
         begin
            for I in 0 .. Positions'Length - 1 loop
               if Positions (I) <= NormalizationOffset then
                  Positions (I) := 0;
               else
                  Positions (I) := Positions (I) - NormalizationOffset;
               end if;
            end loop;
         end Normalize;

         function Get_Available return Integer is
            pragma Inline (Get_Available);
         begin
            --  Compared to the Java version: - 1 shift for getting readPos
            --  in buf'Range upon: cur_literal := buf (readPos);
            return WritePos - ReadPos - 1;
         end Get_Available;

         function Move_Pos (RequiredForFlushing, RequiredForFinishing : Integer) return Integer is
            --  Java name: movePos.
            Avail : Integer;
         begin
            pragma Assert (RequiredForFlushing >= RequiredForFinishing);
            ReadPos := ReadPos + 1;
            Avail   := Get_Available;
            if Avail < RequiredForFlushing then
               if Avail < RequiredForFinishing or else not Finishing
               then
                  PendingSize := PendingSize + 1;
                  --  GdM: This causes cyclicPos and lzpos not being in sync with readPos.
                  --       The pendingSize value is there for catching up.
                  Avail := 0;
               end if;
            end if;
            return Avail;
         end Move_Pos;

         function GetHash4Size return Integer is
            H : Unsigned_32 := Unsigned_32 (String_Buffer_Size - 1);
         begin
            H := H or Shift_Right (H, 1);
            H := H or Shift_Right (H, 2);
            H := H or Shift_Right (H, 4);
            H := H or Shift_Right (H, 8);
            H := Shift_Right (H, 1);
            H := H or 16#FFFF#;  --  LzFind.c: "don't change it! It's required for Deflate"
            if H > 2 ** 24 then
               H := Shift_Right (H, 1);
            end if;
            return Integer (H + 1);
         end GetHash4Size;

         type P_Byte_Array is access Byte_Array;
         procedure Dispose is new Ada.Unchecked_Deallocation (Byte_Array, P_Byte_Array);

         package Hash234 is
            HASH_2_SIZE                        : constant := 2 ** 10;
            HASH_2_MASK                        : constant := HASH_2_SIZE - 1;
            HASH_3_SIZE                        : constant := 2 ** 16;
            HASH_3_MASK                        : constant := HASH_3_SIZE - 1;
            Hash_4_Size                        : constant Integer := GetHash4Size;
            Hash_4_Mask                        : constant Unsigned_32 := Unsigned_32 (Hash_4_Size) - 1;
            --
            Hash2Table                         : Int_Array (0 .. HASH_2_SIZE - 1) := (others => 0);  --  [Initialization added]
            Hash3Table                         : Int_Array (0 .. HASH_3_SIZE - 1) := (others => 0);  --  [Initialization added]
            Hash4Table                         : P_Int_Array;
            --
            Hash2Value, Hash3Value, Hash4Value : Unsigned_32 := 0;
            --
            procedure CalcHashes (Buf : Byte_Array; Off : Integer);
            procedure UpdateTables (Pos : Integer);
            procedure Normalize (NormalizeOffset : Integer);
         end Hash234;

         package body Hash234 is

            CrcTable   : array (Byte) of Unsigned_32;
            CRC32_POLY : constant := 16#EDB8_8320#;

            procedure CalcHashes (Buf : Byte_Array; Off : Integer) is
               Temp : Unsigned_32 := CrcTable (Buf (Off)) xor Unsigned_32 (Buf (Off + 1));
            begin
               Hash2Value := Temp and HASH_2_MASK;
               Temp := Temp xor Shift_Left (Unsigned_32 (Buf (Off + 2)), 8);
               Hash3Value := Temp and HASH_3_MASK;
               Temp := Temp xor Shift_Left (CrcTable (Buf (Off + 3)), 5);
               Hash4Value := Temp and Hash_4_Mask;
            end CalcHashes;

            procedure UpdateTables (Pos : Integer) is
            begin
               Hash2Table (Integer (Hash2Value)) := Pos;
               Hash3Table (Integer (Hash3Value)) := Pos;
               Hash4Table (Integer (Hash4Value)) := Pos;
            end UpdateTables;

            procedure Normalize (NormalizeOffset : Integer) is
            begin
               Normalize (Hash2Table, NormalizeOffset);
               Normalize (Hash3Table, NormalizeOffset);
               Normalize (Hash4Table.all, NormalizeOffset);
            end Normalize;

            R : Unsigned_32;
         begin
            --  NB: heap allocation used only for convenience because of
            --      small default stack sizes on some compilers.
            Hash4Table := new Int_Array (0 .. Hash_4_Size - 1);
            Hash4Table.all := (others => 0);  --  [Initialization added]
            for I in Byte loop
               R := Unsigned_32 (I);
               for J in 0 .. 7 loop
                  if (R and 1) = 0 then
                     R := Shift_Right (R, 1);
                  else
                     R := Shift_Right (R, 1) xor CRC32_POLY;
                  end if;
               end loop;
               CrcTable (I) := R;
            end loop;
         end Hash234;

         Nice_Length : constant Integer := Integer'Min (162, Look_Ahead);  --  const. was 64
         Depth_Limit : constant := 48;  --  Alternatively: 16 + Nice_Length / 2

         CyclicSize  : constant Integer := String_Buffer_Size;  --  Had: + 1;
         CyclicPos   : Integer := -1;
         LzPos       : Integer := CyclicSize;

         Max_Dist : constant Integer := CyclicSize - (Look_Ahead + 2);
         --  NB: 2020-11-04: added "- (Look_Ahead + 2)" to prevent corruption of
         --  the expansion buffer in LZMA.Encoding when DL codes are tested in front
         --  of the actual writes, before actual entropy compression (since rev. #850).

         package BT4_Algo is
            procedure Skip (Len : Natural);
            pragma Inline (Skip);
            procedure Read_One_And_Get_Matches (Matches : out Matches_Type);
         end BT4_Algo;

         Buf  : P_Byte_Array;
         Tree : P_Int_Array;

         package body BT4_Algo is

            function Move_Pos_In_BT4 return Integer is
               --  Java name: movePos.
               Avail               : constant Integer :=
                                       Move_Pos (RequiredForFlushing  => Nice_Length,
                                                 RequiredForFinishing => 4);
               NormalizationOffset : Integer;
            begin
               --  Put_Line ("BT4_Algo.Move_Pos_in_BT4");
               if Avail /= 0 then
                  LzPos := LzPos + 1;
                  if LzPos = Integer'Last then
                     NormalizationOffset := Integer'Last - CyclicSize;
                     Hash234.Normalize (NormalizationOffset);
                     Normalize (Tree.all, NormalizationOffset);
                     LzPos := LzPos - NormalizationOffset;
                  end if;
                  CyclicPos := CyclicPos + 1;
                  if CyclicPos = CyclicSize then
                     --  Put_Line("cyclicPos zeroed");
                     CyclicPos := 0;
                  end if;
               end if;
               return Avail;
            end Move_Pos_In_BT4;

            Null_Position : constant := -1;  --  LzFind.c: kEmptyHashValue, 0

            procedure Skip_And_Update_Tree (NiceLenLimit : Integer; CurrentMatch : in out Integer) is
               Delta0, Depth, Ptr0, Ptr1, Pair, Len, Len0, Len1 : Integer;
            begin
               --  Put("BT4_Algo.Skip_and_Update_Tree... ");
               Depth := Depth_Limit;
               Ptr0  := CyclicPos * 2 + 1;
               Ptr1  := CyclicPos * 2;
               Len0  := 0;
               Len1  := 0;
               loop
                  Delta0 := LzPos - CurrentMatch;
                  if Depth = 0 or else Delta0 >= Max_Dist then
                     Tree (Ptr0) := Null_Position;
                     Tree (Ptr1) := Null_Position;
                     return;
                  end if;
                  Depth := Depth - 1;
                  if CyclicPos - Delta0 < 0 then
                     Pair := CyclicSize;
                  else
                     Pair := 0;
                  end if;
                  Pair := (CyclicPos - Delta0 + Pair) * 2;
                  Len  := Integer'Min (Len0, Len1);
                  --  Match ?
                  if Buf (ReadPos + Len - Delta0) = Buf (ReadPos + Len) then
                     --  No need to look for longer matches than niceLenLimit
                     --  because we only are updating the tree, not returning
                     --  matches found to the caller.
                     loop
                        Len := Len + 1;
                        if Len = NiceLenLimit then
                           Tree (Ptr1) := Tree (Pair);
                           Tree (Ptr0) := Tree (Pair + 1);
                           return;
                        end if;
                        exit when Buf (ReadPos + Len - Delta0) /= Buf (ReadPos + Len);
                     end loop;
                  end if;
                  --  Bytes are no more matching. The past value is either smaller...
                  if Buf (ReadPos + Len - Delta0) < Buf (ReadPos + Len) then
                     Tree (Ptr1) := CurrentMatch;
                     Ptr1 := Pair + 1;
                     CurrentMatch := Tree (Ptr1);
                     Len1 := Len;
                  else  --  ... or larger
                     Tree (Ptr0) := CurrentMatch;
                     Ptr0 := Pair;
                     CurrentMatch := Tree (Ptr0);
                     Len0 := Len;
                  end if;
               end loop;
            end Skip_And_Update_Tree;

            procedure Skip (Len : Natural) is
               --
               procedure Skip_One is
                  pragma Inline (Skip_One);
                  NiceLenLimit, Avail, CurrentMatch : Integer;
               begin
                  NiceLenLimit := Nice_Length;
                  Avail := Move_Pos_In_BT4;
                  if Avail < NiceLenLimit then
                     if Avail = 0 then
                        return;
                     end if;
                     NiceLenLimit := Avail;
                  end if;
                  Hash234.CalcHashes (Buf.all, ReadPos);
                  CurrentMatch := Hash234.Hash4Table (Integer (Hash234.Hash4Value));
                  Hash234.UpdateTables (LzPos);
                  Skip_And_Update_Tree (NiceLenLimit, CurrentMatch);
               end Skip_One;
               --
            begin
               for Count in reverse 1 .. Len loop
                  Skip_One;
               end loop;
            end Skip;

            procedure Read_One_And_Get_Matches (Matches : out Matches_Type) is
               MatchLenLimit                                     : Integer := Look_Ahead;
               NiceLenLimit                                      : Integer := Nice_Length;
               Avail                                             : Integer;
               Delta0, Delta2, Delta3, CurrentMatch,
               LenBest, Depth, Ptr0, Ptr1, Pair, Len, Len0, Len1 : Integer;
            begin
               --  Put("BT4_Algo.Get_Matches... ");
               Matches.Count := 0;
               Avail := Move_Pos_In_BT4;
               if Avail < MatchLenLimit then
                  if Avail = 0 then
                     return;
                  end if;
                  MatchLenLimit := Avail;
                  if NiceLenLimit > Avail then
                     NiceLenLimit := Avail;
                  end if;
               end if;
               --
               Hash234.CalcHashes (Buf.all, ReadPos);
               Delta2 := LzPos - Hash234.Hash2Table (Integer (Hash234.Hash2Value));
               Delta3 := LzPos - Hash234.Hash3Table (Integer (Hash234.Hash3Value));
               CurrentMatch :=   Hash234.Hash4Table (Integer (Hash234.Hash4Value));
               Hash234.UpdateTables (LzPos);
               --
               LenBest := 0;
               --  See if the hash from the first two bytes found a match.
               --  The hashing algorithm guarantees that if the first byte
               --  matches, also the second byte does, so there's no need to
               --  test the second byte.
               if Delta2 < Max_Dist and then Buf (ReadPos - Delta2) = Buf (ReadPos) then
                  --  Match of length 2 found and checked.
                  LenBest := 2;
                  Matches.Count := 1;
                  Matches.Dl (Matches.Count).Length := 2;
                  Matches.Dl (Matches.Count).Distance := Delta2;
               end if;
               --  See if the hash from the first three bytes found a match that
               --  is different from the match possibly found by the two-byte hash.
               --  Also here the hashing algorithm guarantees that if the first byte
               --  matches, also the next two bytes do.
               if Delta2 /= Delta3 and then Delta3 < Max_Dist
                  and then Buf (ReadPos - Delta3) = Buf (ReadPos)
               then
                  --  Match of length 3 found and checked.
                  LenBest := 3;
                  Matches.Count := Matches.Count + 1;
                  Matches.Dl (Matches.Count).Distance := Delta3;
                  Delta2 := Delta3;
               end if;
               --  If a match was found, see how long it is.
               if Matches.Count > 0 then
                  while LenBest < MatchLenLimit and then Buf (ReadPos + LenBest - Delta2)
                     = Buf (ReadPos + LenBest)
                  loop
                     LenBest := LenBest + 1;
                  end loop;
                  Matches.Dl (Matches.Count).Length := LenBest;
                  --  Return if it is long enough (niceLen or reached the end of the dictionary).
                  if LenBest >= NiceLenLimit then
                     Skip_And_Update_Tree (NiceLenLimit, CurrentMatch);
                     return;
                  end if;
               end if;
               --  A long enough match wasn't found so easily.
               --  Look for better matches from the binary tree.
               if LenBest < 3 then
                  LenBest := 3;
               end if;
               Depth := Depth_Limit;
               Ptr0  := CyclicPos * 2 + 1;
               Ptr1  := CyclicPos * 2;
               Len0  := 0;
               Len1  := 0;
               --
               loop
                  Delta0 := LzPos - CurrentMatch;
                  --  Return if the search depth limit has been reached or
                  --  if the distance of the potential match exceeds the
                  --  dictionary size.
                  if Depth = 0 or else Delta0 >= Max_Dist then
                     Tree (Ptr0) := Null_Position;
                     Tree (Ptr1) := Null_Position;
                     return;
                  end if;
                  Depth := Depth - 1;
                  --
                  if CyclicPos - Delta0 < 0 then
                     Pair := CyclicSize;
                  else
                     Pair := 0;
                  end if;
                  Pair := (CyclicPos - Delta0 + Pair) * 2;
                  Len  := Integer'Min (Len0, Len1);
                  --  Match ?
                  if Buf (ReadPos + Len - Delta0) = Buf (ReadPos + Len) then
                     loop
                        Len := Len + 1;
                        exit when Len >= MatchLenLimit
                           or else Buf (ReadPos + Len - Delta0) /= Buf (ReadPos + Len);
                     end loop;
                     if Len > LenBest then
                        LenBest := Len;
                        Matches.Count := Matches.Count + 1;
                        Matches.Dl (Matches.Count).Length := Len;
                        Matches.Dl (Matches.Count).Distance := Delta0;
                        if Len >= NiceLenLimit then
                           Tree (Ptr1) := Tree (Pair);
                           Tree (Ptr0) := Tree (Pair + 1);
                           return;
                        end if;
                     end if;
                  end if;
                  --  Bytes are no more matching. The past value is either smaller...
                  if Buf (ReadPos + Len - Delta0) < Buf (ReadPos + Len) then
                     Tree (Ptr1) := CurrentMatch;
                     Ptr1 := Pair + 1;
                     CurrentMatch := Tree (Ptr1);
                     Len1 := Len;
                  else  --  ... or larger
                     Tree (Ptr0) := CurrentMatch;
                     Ptr0 := Pair;
                     CurrentMatch := Tree (Ptr0);
                     Len0 := Len;
                  end if;
               end loop;
            end Read_One_And_Get_Matches;

         begin
            --  NB: heap allocation used only for convenience because of
            --      small default stack sizes on some compilers.
            Tree := new Int_Array (0 .. CyclicSize * 2 - 1);
            for I in Tree'Range loop
               Tree (I) := Null_Position;
            end loop;
         end BT4_Algo;

         --  Moves data from the end of the buffer to the beginning, discarding
         --  old data and making space for new input.

         procedure Move_Window is
            --  Java name: moveWindow.
            --  Align the move to a multiple of 16 bytes (LZMA-friendly, see pos_bits)
            MoveOffset : constant Integer := ((ReadPos + 1 - KeepSizeBefore) / 16) * 16;
            MoveSize   : constant Integer := WritePos - MoveOffset;
         begin
            --  Put_Line("  Move window, size=" & moveSize'Img & " offset=" & moveOffset'Img);
            Buf (0 .. MoveSize - 1) := Buf (MoveOffset .. MoveOffset + MoveSize - 1);
            ReadPos   := ReadPos   - MoveOffset;
            ReadLimit := ReadLimit - MoveOffset;
            WritePos  := WritePos  - MoveOffset;
         end Move_Window;

         --  Copies new data into the buffer.
         function Fill_Window (Len_Initial : Integer) return Integer is
            --  Java name: fillWindow

            --  Process pending data that hasn't been ran through the match finder yet.
            --  Run it through the match finder now if there is enough new data
            --  available (readPos < readLimit) that the encoder may encode at
            --  least one more input byte.
            --
            procedure ProcessPendingBytes is
               OldPendingSize : Integer;
            begin
               if PendingSize > 0 and then ReadPos < ReadLimit then
                  ReadPos := ReadPos - PendingSize;
                  OldPendingSize := PendingSize;
                  PendingSize := 0;
                  BT4_Algo.Skip (OldPendingSize);
               end if;
            end ProcessPendingBytes;
            --
            Len : Integer := Len_Initial;
            Actual_Len : Integer := 0;
         begin
            --  Put_Line("Fill window - start");
            --  Move the sliding window if needed.
            if ReadPos >= Buf'Length - KeepSizeAfter then
               Move_Window;
            end if;

            --  Try to fill the dictionary buffer up to its boundary.
            if Len > Buf'Length - WritePos then
               Len := Buf'Length - WritePos;
            end if;

            while Len > 0 and then More_Bytes loop
               Buf (WritePos) := Read_Byte;
               WritePos := WritePos + 1;
               Len := Len - 1;
               Actual_Len := Actual_Len + 1;
            end loop;

            --  Set the new readLimit but only if there's enough data to allow
            --  encoding of at least one more byte.
            if WritePos >= KeepSizeAfter then
               ReadLimit := WritePos - KeepSizeAfter;
            end if;

            ProcessPendingBytes;

            --  Put_Line("Fill window, requested=" & len_initial'Img & " actual=" & actual_len'Img);
            --  Tell the caller how much input we actually copied into the dictionary.
            return Actual_Len;
         end Fill_Window;

         function Compute_Match_Length (Distance, Length_Limit : Integer) return Natural is
            pragma Inline (Compute_Match_Length);
            Back_Pos : constant Integer := ReadPos - Distance;
            Len      : Integer := 0;
         begin
            if Distance < 2 then
               return 0;
            end if;
            --  @ if readPos+len not in buf.all'Range then
            --  @   Put("**** readpos " & buf'Last'Img & readPos'Img);
            --  @ end if;
            --  @ if backPos+len not in buf.all'Range then
            --  @   Put("**** backpos " & buf'Last'Img & back_pos'Img);
            --  @ end if;
            while Len < Length_Limit and then Buf (ReadPos + Len) = Buf (Back_Pos + Len) loop
               Len := Len + 1;
            end loop;
            return Len;
         end Compute_Match_Length;

         ReadAhead    : Integer := -1;  --  LZMAEncoder.java
         --  Small stack of recent distances used for LZMA.
         subtype Repeat_Stack_Range is Integer range 0 .. 3;
         --  1-based distances.
         Rep_Dist     : array (Repeat_Stack_Range) of Distance_Type := (others => 1);
         Len_Rep_Dist : array (Repeat_Stack_Range) of Natural := (others => 0);

         function Has_Much_Smaller_Distance (SmallDist, BigDist : Distance_Type) return Boolean is
            pragma Inline (Has_Much_Smaller_Distance);
         begin
            return (SmallDist - 1) < (BigDist - 1) / 128;
         end Has_Much_Smaller_Distance;

         Best_Length_For_Rep_Dist, Best_Rep_Dist_Index : Integer;

         procedure Read_One_And_Get_Matches (Matches : out Matches_Type) is
            Avail, Len : Integer;
         begin
            ReadAhead := ReadAhead + 1;
            --
            BT4_Algo.Read_One_And_Get_Matches (Matches);
            --
            if LZMA_Friendly then
               Best_Length_For_Rep_Dist := 0;
               Avail := Integer'Min (Get_Available, Look_Ahead);
               if Avail >= MATCH_LEN_MIN then
                  for Rep in Repeat_Stack_Range loop
                     Len := Compute_Match_Length (Rep_Dist (Rep), Avail);
                     Len_Rep_Dist (Rep) := Len;
                     --  Remember the index and length of the best repeated match.
                     if Len > Best_Length_For_Rep_Dist then
                        Best_Rep_Dist_Index      := Rep;
                        Best_Length_For_Rep_Dist := Len;
                     end if;
                  end loop;
               else
                  for Rep in Repeat_Stack_Range loop
                     Len_Rep_Dist (Rep) := 0;  --  No match possible in any case.
                  end loop;
               end if;
            end if;
         end Read_One_And_Get_Matches;

         procedure Get_Supplemental_Matches_From_Repeat_Matches (Matches : in out Matches_Type) is
            Len, Ins : Integer;
         begin
            if Matches.Count = 0 then
               if Best_Length_For_Rep_Dist >= MATCH_LEN_MIN then
                  Matches.Dl (1).Distance := Rep_Dist (Best_Rep_Dist_Index);
                  Matches.Dl (1).Length   := Best_Length_For_Rep_Dist;
                  Matches.Count := 1;
               end if;
            end if;
            for Rep in Repeat_Stack_Range loop
               Len := Len_Rep_Dist (Rep);
               if Len >= MATCH_LEN_MIN then
                  Ins := 0;
                  for I in reverse 1 .. Matches.Count loop
                     if Len = Matches.Dl (I).Length then
                        if Rep_Dist (Rep) = Matches.Dl (I).Distance then
                           null;  --  Identical match
                        else
                           --  Tie: insert the repeat match of same length into the list.
                           --  If the longest match strategy is applied, the second item is preferred.
                           if Has_Much_Smaller_Distance (Matches.Dl (I).Distance, Rep_Dist (Rep)) then
                              Ins := I;      --  Insert before
                           else
                              Ins := I + 1;  --  Insert after
                           end if;
                           exit;
                           --  Ada.Text_IO.Put_Line ("Tie");
                        end if;
                     elsif I < Matches.Count then
                        if Len > Matches.Dl (I).Length and then Len < Matches.Dl (I + 1).Length then
                           --  Insert between existing lengths
                           Ins := I + 1;
                           exit;
                           --  We don't add len as the shortest length (worsens compression).
                           ------
                           --  elsif i = 1
                           --    and then len >= MATCH_LEN_MIN
                           --    and then len >= matches.dl (1).length - 1  --  Some reluctance...
                           --  then
                           --    ins := 1;
                        end if;
                     elsif Len > Matches.Dl (I).Length then
                        --  i = matches.count in this case: add as longest.
                        Ins := I + 1;
                        exit;
                     end if;
                  end loop;
                  --  We can insert this repeat match at position 'ins'.
                  if Ins > 0 then
                     for I in reverse Ins .. Matches.Count loop  --  Empty if ins > count.
                        Matches.Dl (I + 1) := Matches.Dl (I);
                     end loop;
                     Matches.Dl (Ins).Distance := Rep_Dist (Rep);
                     Matches.Dl (Ins).Length   := Len;
                     Matches.Count := Matches.Count + 1;
                     exit;
                  end if;
               end if;
            end loop;
            pragma Assert (Are_Matches_Sorted (Matches));
         end Get_Supplemental_Matches_From_Repeat_Matches;

         procedure Skip (Len : Natural) is
            pragma Inline (Skip);
         begin
            ReadAhead := ReadAhead + Len;
            BT4_Algo.Skip (Len);
         end Skip;

         procedure Reduce_Consecutive_Max_Lengths (M : in out Matches_Type) is
            --  Sometimes the BT4 algo returns a long list with consecutive lengths.
            --  We try to reduce it, if there is a clear advantage with distances.
         begin
            while M.Count > 1
               and then M.Dl (M.Count).Length = M.Dl (M.Count - 1).Length + 1
               and then Has_Much_Smaller_Distance (M.Dl (M.Count - 1).Distance, M.Dl (M.Count).Distance)
            loop
               M.Count := M.Count - 1;
            end loop;
         end Reduce_Consecutive_Max_Lengths;

         procedure Show_Matches (M : Matches_Type; Phase : String) is
         begin
            Ada.Text_IO.Put_Line (
                                  Phase & " --- Matches: " & Integer'Image (M.Count)
                                 );
            for I in 1 .. M.Count loop
               Ada.Text_IO.Put_Line (
                                     "  Distance:" & Integer'Image (M.Dl (I).Distance) &
                                        ";  Length:" & Integer'Image (M.Dl (I).Length)
                                    );
            end loop;
         end Show_Matches;
         pragma Unreferenced (Show_Matches);

         Matches             : Matches_Array (0 .. 1);
         Current_Match_Index : Prefetch_Index_Type := 0;
         Match_Trace         : DLP_Array (1 .. Max_Length_Any_Algo);

         procedure Get_Next_Symbol is
            New_Ld, Main : Distance_Length_Pair;

            --  This function is for debugging. The matches stored in the 'tree' array
            --  may be wrong if the variables cyclicPos, lzPos and readPos are not in sync.
            --  The issue seems to have been solved now (rev. 489).
            function Is_Match_Correct (Shift : Natural) return Boolean is
            begin
               for I in reverse -1 + Shift .. Main.Length - 2 + Shift loop
                  if Buf (ReadPos - (Main.Distance) + I) /= Buf (ReadPos + I) then
                     return False;  --  Should not occur.
                  end if;
               end loop;
               return True;
            end Is_Match_Correct;

            procedure Send_First_Literal_Of_Match is
            begin
               Write_Literal (Cur_Literal);
               ReadAhead := ReadAhead - 1;
            end Send_First_Literal_Of_Match;

            procedure Send_DL_Code (Distance, Length : Integer) is
               Found_Repeat : Integer := Rep_Dist'First - 1;
               Aux          : Integer;
            begin
               Write_DL_Code (Distance, Length);
               ReadAhead := ReadAhead - Length;
               if LZMA_Friendly then
                  --
                  --  Manage the stack of recent distances in the same way the "MA" part of LZMA does.
                  --
                  for I in Rep_Dist'Range loop
                     if Distance = Rep_Dist (I) then
                        Found_Repeat := I;
                        exit;
                     end if;
                  end loop;
                  if Found_Repeat >= Rep_Dist'First then
                     --  Roll the stack of recent distances up to the item with index found_repeat,
                     --  which becomes first. If found_repeat = rep_dist'First, no actual change occurs.
                     Aux := Rep_Dist (Found_Repeat);
                     for I in reverse Rep_Dist'First + 1 .. Found_Repeat loop
                        Rep_Dist (I) := Rep_Dist (I - 1);
                     end loop;
                     Rep_Dist (Rep_Dist'First) := Aux;
                  else
                     --  Shift the stack of recent distances; the new distance becomes the first item.
                     for I in reverse Rep_Dist'First + 1 .. Rep_Dist'Last loop
                        Rep_Dist (I) := Rep_Dist (I - 1);
                     end loop;
                     Rep_Dist (0) := Distance;
                  end if;
               end if;
            end Send_DL_Code;

            Avail, Limit    : Integer;
            Index_Max_Score : Positive;
            Set_Max_Score   : Prefetch_Index_Type;
            Hurdle          : constant := 40;
         begin
            --  Get the matches for the next byte unless readAhead indicates
            --  that we already got the new matches during the previous call
            --  to this procedure.
            if ReadAhead = -1 then
               Read_One_And_Get_Matches (Matches (Current_Match_Index));
            end if;
            --  @ if readPos not in buf.all'Range then
            --  @   Put("**** " & buf'Last'Img & keepSizeAfter'Img & readPos'Img & writePos'Img);
            --  @ end if;
            Cur_Literal := Buf (ReadPos);
            --  Get the number of bytes available in the dictionary, but
            --  not more than the maximum match length. If there aren't
            --  enough bytes remaining to encode a match at all, return
            --  immediately to encode this byte as a literal.
            Avail := Integer'Min (Get_Available, Look_Ahead);
            if Avail < MATCH_LEN_MIN then
               --  Put("[a]");
               Send_First_Literal_Of_Match;
               return;
            end if;

            if LZMA_Friendly and then Best_Length_For_Rep_Dist >= Nice_Length then
               Skip (Best_Length_For_Rep_Dist - 1);
               --  Put_Line("[DL RA]");
               Send_DL_Code (Rep_Dist (Best_Rep_Dist_Index), Best_Length_For_Rep_Dist);
               return;
            end if;

            Main := (Length => 1, Distance => 1);
            if Matches (Current_Match_Index).Count > 0 then
               Main := Matches (Current_Match_Index).Dl (Matches (Current_Match_Index).Count);
               if Main.Length >= Nice_Length then
                  pragma Assert (Is_Match_Correct (1));
                  Skip (Main.Length - 1);
                  --  Put_Line("[DL A]" & mainDist'Img & mainLen'Img);
                  Send_DL_Code (Main.Distance, Main.Length);
                  return;
               end if;
               Reduce_Consecutive_Max_Lengths (Matches (Current_Match_Index));
               if LZMA_Friendly then
                  Get_Supplemental_Matches_From_Repeat_Matches (Matches (Current_Match_Index));
               end if;
               Main := Matches (Current_Match_Index).Dl (Matches (Current_Match_Index).Count);
               --
               if Main.Length = MATCH_LEN_MIN and then Main.Distance > 128 then
                  Main.Length := 1;
               end if;
            end if;

            if LZMA_Friendly
               and then Best_Length_For_Rep_Dist > MATCH_LEN_MIN
               and then (Best_Length_For_Rep_Dist >= Main.Length
                           or else (Best_Length_For_Rep_Dist >= Main.Length - 2 and then Main.Distance > 2 ** 9)
                           or else (Best_Length_For_Rep_Dist >= Main.Length - 3 and then Main.Distance > 2 ** 15))
            then
               --  Shortcut: we choose the longest repeat match.
               Skip (Best_Length_For_Rep_Dist - 1);
               --  Put_Line("[DL RB]");
               Send_DL_Code (Rep_Dist (Best_Rep_Dist_Index), Best_Length_For_Rep_Dist);
               return;
            end if;

            if Main.Length < MATCH_LEN_MIN or else Avail <= MATCH_LEN_MIN then
               --  Put("[b]");
               Send_First_Literal_Of_Match;
               return;
            end if;

            -------------------------------------------------------------------------
            --  Get the next match. Test if it is better than the current match.   --
            --  If so, encode the current byte as a literal.                       --
            -------------------------------------------------------------------------
            Current_Match_Index := 1 - Current_Match_Index;
            Read_One_And_Get_Matches (Matches (Current_Match_Index));
            --
            --  Show_Matches (matches (1 - current_match_index), "------ Old");
            --  Show_Matches (matches (current_match_index),     "       New");
            --
            if Matches (Current_Match_Index).Count > 0 then
               New_Ld := Matches (Current_Match_Index).Dl (Matches (Current_Match_Index).Count);  --  Longest new match
               if        (New_Ld.Length >= Main.Length + Hurdle     and then New_Ld.Distance < Main.Distance)
                  or else
                     (New_Ld.Length =  Main.Length + Hurdle + 1
                        and then not Has_Much_Smaller_Distance (Main.Distance, New_Ld.Distance))
                     or else  New_Ld.Length >  Main.Length + Hurdle + 1
                     or else (New_Ld.Length >= Main.Length + Hurdle - 1
                                and then Main.Length >= MATCH_LEN_MIN + 1
                                and then Has_Much_Smaller_Distance (New_Ld.Distance, Main.Distance))
               then
                  --  We prefer literal, then the new match (or even better!)
                  Send_First_Literal_Of_Match;
                  return;
               end if;
               --
               --  Here we compare the scores of both match sets.
               --
               Reduce_Consecutive_Max_Lengths (Matches (Current_Match_Index));
               if LZMA_Friendly then
                  Get_Supplemental_Matches_From_Repeat_Matches (Matches (Current_Match_Index));
               end if;
               Estimate_DL_Codes (
                                  Matches, 1 - Current_Match_Index, (1 => Cur_Literal),
                                  Index_Max_Score, Set_Max_Score, Match_Trace
                                 );
               if Set_Max_Score = 1 - Current_Match_Index then
                  --  Old match is seems better.
                  Main :=  Matches (Set_Max_Score).Dl (Index_Max_Score);
               else
                  --  We prefer at least a literal, then a new, better match.
                  Send_First_Literal_Of_Match;
                  return;
               end if;
            end if;

            if LZMA_Friendly then
               Limit := Integer'Max (Main.Length - 1, MATCH_LEN_MIN);
               for Rep in Rep_Dist'Range loop
                  if Compute_Match_Length (Rep_Dist (Rep), Limit) = Limit then
                     --  A "literal then DL_Code (some distance, main.length - 1)" match
                     --  is verified and could use the stack of last distances -> got for it!
                     Send_First_Literal_Of_Match;
                     return;
                  end if;
               end loop;
            end if;

            pragma Assert (Is_Match_Correct (0));
            Skip (Main.Length - 2);
            --  Put_Line("[DL B]" & mainDist'Img & mainLen'Img);
            Send_DL_Code (Main.Distance, Main.Length);
         end Get_Next_Symbol;

         procedure Deallocation is
         begin
            Dispose (Buf);
            Dispose (Tree);
            Dispose (Hash234.Hash4Table);
         end Deallocation;

         Actual_Written, Avail : Integer;
      begin
         --  NB: heap allocation used only for convenience because of
         --      the small default stack sizes on some compilers.
         Buf := new Byte_Array (0 .. GetBufSize);
         --
         Actual_Written := Fill_Window (String_Buffer_Size);
         if Actual_Written > 0 then
            loop
               Get_Next_Symbol;
               Avail := Get_Available;
               if Avail = 0 then
                  Actual_Written := Fill_Window (String_Buffer_Size);
                  exit when Actual_Written = 0;
               end if;
            end loop;
         end if;
         Deallocation;
      exception
         when others =>
            Deallocation;
            raise;
      end LZ77_Using_BT4;

      procedure LZ77_By_Rich is
         --  * PROG2.C [lz77a.c]                                             *
         --  * Simple Hashing LZ77 Sliding Dictionary Compression Program    *
         --  * By Rich Geldreich, Jr. October, 1993                          *
         --  * Originally compiled with QuickC v2.5 in the small model.      *
         --  * This program uses more efficient code to delete strings from  *
         --  * the sliding dictionary compared to PROG1.C, at the expense of *
         --  * greater memory requirements. See the HashData and DeleteData  *
         --  * subroutines.                                                  *
         --
         --  Comments by GdM, 2019+ appear in square brackets: [...]

         --  Set this to True for a greedy encoder.
         GREEDY : constant Boolean := False;  --  [original: False]

         --  Ratio vs. speed constant [ Is it really a ratio? ].
         --  The larger this constant, the better the compression.
         MAXCOMPARES : constant := 4096;  --  [original: 75; good: 2400; from Info-Zip: 4096]

         --  Unused entry code.
         NIL : constant := 16#FFFF#;

         --  /* bits per symbol- normally 8 for general purpose compression */
         --  #define CHARBITS : constant := 8;  [ NB: dictionary uses char (byte) ]

         --  Minimum match length & maximum match length.
         THRESHOLD_Rich : constant := 2;
         MATCHBITS      : constant := 8;  --  [original: 4]
         --  [original: 2 ** MATCHBITS + THRESHOLD - 1]
         MAXMATCH  : constant := 2 ** MATCHBITS + THRESHOLD_Rich;  -- 258 is Deflate-friendly.

         --  Sliding dictionary size and hash table's size.
         --  Some combinations of HASHBITS and THRESHOLD values will not work
         --  correctly because of the way this program hashes strings.

         DICTBITS : constant := 15;  --  [original: 13]
         HASHBITS : constant := 13;  --  [original: 10]
         --
         DICTSIZE : constant := 2 ** DICTBITS;
         HASHSIZE : constant := 2 ** HASHBITS;

         --  # bits to shift after each XOR hash
         --  This constant must be high enough so that only THRESHOLD + 1
         --  characters are in the hash accumulator at one time.

         SHIFTBITS : constant := ((HASHBITS + THRESHOLD_Rich) / (THRESHOLD_Rich + 1));

         --  Sector size constants [the dictionary is partitoned in sectors].

         SECTORBIT : constant := 13;  --  [original: 10; OK: 13]
         SECTORLEN : constant := 2 ** SECTORBIT;

         HASH_MASK_1 : constant := 16#8000#;  --  [ was called HASHFLAG1 ]
         HASH_MASK_2 : constant := 16#7FFF#;  --  [ was called HASHFLAG2 ]

         --  Dictionary plus MAXMATCH extra chars for string comparisions.
         Dict        : array (Integer_M32'(0) .. DICTSIZE + MAXMATCH - 1) of Byte;

         subtype Unsigned_Int is Unsigned_16;

         --  Hash table & link list tables.

         --  [ So far we index the hash table with Integer (minimum 16 bit signed) ]
         Hash       : array (0 .. HASHSIZE - 1) of Unsigned_Int := (others => NIL);
         --  [ nextlink: in lz77a.c: only through DICTSIZE - 1,
         --    although Init has: nextlink[DICTSIZE] = NIL. In doubt we set the
         --    'Last to DICTSIZE and fill everything with NIL... ]
         Nextlink   : array (Integer_M32'(0) .. DICTSIZE)     of Unsigned_Int := (others => NIL);
         Lastlink   : array (Integer_M32'(0) .. DICTSIZE - 1) of Unsigned_Int := (others => NIL);

         --  Loads dictionary with characters from the input stream.
         --
         procedure Load_Dict (Dictpos : Integer_M32; Actually_Read : out Integer_M32) is
            I : Integer_M32 := 0;
         begin
            while More_Bytes loop
               Dict (Dictpos + I) := Read_Byte;
               I := I + 1;
               exit when I = SECTORLEN;
            end loop;

            --  Since the dictionary is a ring buffer, copy the characters at
            --  the very start of the dictionary to the end
            --  [this avoids having to use an "and" or a "mod" operator when searching].
            --
            if Dictpos = 0 then
               for J in Integer_M32'(0) .. MAXMATCH - 1 loop
                  Dict (J + DICTSIZE) := Dict (J);
               end loop;
            end if;

            Actually_Read := I;
         end Load_Dict;

         --  Deletes data from the dictionary search structures
         --  This is only done when the number of bytes to be
         --  compressed exceeds the dictionary's size.
         --
         procedure Delete_Data (Dictpos : Integer_M32) is
            J, K : Integer_M32;
         begin
            --  Delete all references to the sector being deleted.
            K := Dictpos + SECTORLEN;
            for I in Dictpos .. K - 1 loop
               J := Integer_M32 (Lastlink (I));
               if (Unsigned_Int (J) and HASH_MASK_1) /= 0 then
                  if J /= NIL then
                     Hash (Integer (Unsigned_Int (J) and HASH_MASK_2)) := NIL;
                  end if;
               else
                  Nextlink (J) := NIL;
               end if;
            end loop;
         end Delete_Data;

         --  Hash data just entered into dictionary.
         --  XOR hashing is used here, but practically any hash function will work.
         --
         procedure Hash_Data (Dictpos, Bytestodo : Integer_M32) is
            J : Integer;
            K : Integer_M32;
         begin
            if Bytestodo <= THRESHOLD_Rich then  -- Not enough bytes in sector for match?
               Nextlink (Dictpos .. Dictpos + Bytestodo - 1) := (others => NIL);
               Lastlink (Dictpos .. Dictpos + Bytestodo - 1) := (others => NIL);
            else
               --  Matches can't cross sector boundaries.
               Nextlink (Dictpos + Bytestodo - THRESHOLD_Rich .. Dictpos + Bytestodo - 1) := (others => NIL);
               Lastlink (Dictpos + Bytestodo - THRESHOLD_Rich .. Dictpos + Bytestodo - 1) := (others => NIL);

               J :=  Integer (
                              Shift_Left (Unsigned_Int (Dict (Dictpos)), SHIFTBITS)
                              xor
                                 Unsigned_Int (Dict (Dictpos + 1))
                             );

               K := Dictpos + Bytestodo - THRESHOLD_Rich;  --  Calculate end of sector.

               for I in Dictpos ..  K - 1 loop
                  J := Integer (
                                (Shift_Left (Unsigned_Int (J), SHIFTBITS) and (HASHSIZE - 1))
                                xor
                                   Unsigned_Int (Dict (I + THRESHOLD_Rich))
                               );
                  Lastlink (I) := Unsigned_Int (J) or HASH_MASK_1;
                  Nextlink (I) := Hash (J);
                  if Nextlink (I) /= NIL then
                     Lastlink (Integer_M32 (Nextlink (I))) := Unsigned_Int (I);
                  end if;
                  Hash (J) := Unsigned_Int (I);
               end loop;
            end if;
         end Hash_Data;

         Matchlength, Matchpos : Integer_M32;

         --  Finds match for string at position dictpos.
         --  This search code finds the longest AND closest
         --  match for the string at dictpos.
         --
         procedure Find_Match (Dictpos, Startlen : Integer_M32) is
            I, J       : Integer_M32;
            Match_Byte : Byte;
         begin
            I := Dictpos;
            Matchlength := Startlen;
            Match_Byte := Dict (Dictpos + Matchlength);
            --
            Chances :
            for Compare_Count in 1 .. MAXCOMPARES loop
               I := Integer_M32 (Nextlink (I));  --  Get next string in list.
               if I = NIL then
                  return;
               end if;
               --
               if Dict (I + Matchlength) = Match_Byte then  --  Possible larger match?
                  J := 0;
                  --  Compare strings.
                  loop
                     exit when Dict (Dictpos + J) /= Dict (I + J);
                     J := J + 1;
                     exit when J = MAXMATCH;
                  end loop;
                  --
                  if J > Matchlength then  --  Found larger match?
                     Matchlength := J;
                     Matchpos    := I;
                     if Matchlength = MAXMATCH then
                        return;  --  Exit if largest possible match.
                     end if;
                     Match_Byte := Dict (Dictpos + Matchlength);
                  end if;
               end if;
            end loop Chances;  --  Keep on trying until we run out of chances.
         end Find_Match;

         --  Finds dictionary matches for characters in current sector.
         --
         procedure Dict_Search (Dictpos, Bytestodo : Integer_M32) is
            I, J                 : Integer_M32;
            Matchlen1, Matchpos1 : Integer_M32;
            --
            procedure Write_Literal_Pos_I is
               pragma Inline (Write_Literal_Pos_I);
            begin
               Write_Literal (Dict (I));
               I := I + 1;
               J := J - 1;
            end Write_Literal_Pos_I;
         begin
            I := Dictpos;
            J := Bytestodo;

            if not GREEDY then  --  Non-greedy search loop (slow).

               while J /= 0 loop  --  Loop while there are still characters left to be compressed.
                  Find_Match (I, THRESHOLD_Rich);

                  if Matchlength > THRESHOLD_Rich then
                     Matchlen1 := Matchlength;
                     Matchpos1 := Matchpos;

                     loop
                        Find_Match (I + 1, Matchlen1);

                        if Matchlength > Matchlen1 then
                           Matchlen1 := Matchlength;
                           Matchpos1 := Matchpos;
                           Write_Literal_Pos_I;
                        else
                           if Matchlen1 > J then
                              Matchlen1 := J;
                              if Matchlen1 <= THRESHOLD_Rich then
                                 Write_Literal_Pos_I;
                                 exit;
                              end if;
                           end if;

                           Write_DL_Code (
                                          Length   => Integer (Matchlen1),
                                          --  [The subtraction happens modulo 2**n, needs to be cleaned modulo 2**DICTSIZE]
                                          Distance => Integer ((Unsigned_32 (I) - Unsigned_32 (Matchpos1)) and (DICTSIZE - 1))
                                         );
                           I := I + Matchlen1;
                           J := J - Matchlen1;
                           exit;
                        end if;
                     end loop;

                  else
                     Write_Literal_Pos_I;
                  end if;

               end loop;  --  while j /= 0

            else  --  Greedy search loop (fast).

               while J /= 0 loop  --  Loop while there are still characters left to be compressed.

                  Find_Match (I, THRESHOLD_Rich);

                  if Matchlength > J then
                     Matchlength := J;     --  Clamp matchlength.
                  end if;

                  if Matchlength > THRESHOLD_Rich then  --  Valid match?
                     Write_DL_Code (
                                    Length   => Integer (Matchlength),
                                    --  [The subtraction happens modulo 2**n, needs to be cleaned modulo 2**DICTSIZE]
                                    Distance => Integer ((Unsigned_32 (I) - Unsigned_32 (Matchpos)) and (DICTSIZE - 1))
                                   );
                     I := I + Matchlength;
                     J := J - Matchlength;
                  else
                     Write_Literal_Pos_I;
                  end if;
               end loop;

            end if;  --  Greedy or not.

         end Dict_Search;

         procedure Encode_Rich is
            Dictpos, Actual_Read : Integer_M32 :=  0;
            Deleteflag           : Boolean := False;
         begin
            loop
               --  Delete old data from dictionary.
               if Deleteflag then
                  Delete_Data (Dictpos);
               end if;

               --  Grab more data to compress.
               Load_Dict (Dictpos, Actual_Read);
               exit when Actual_Read = 0;

               --  Hash the data.
               Hash_Data (Dictpos, Actual_Read);

               --  Find dictionary matches.
               Dict_Search (Dictpos, Actual_Read);

               Dictpos := Dictpos + SECTORLEN;

               --  Wrap back to beginning of dictionary when it's full.
               if Dictpos = DICTSIZE then
                  Dictpos := 0;
                  Deleteflag := True;   --  Ok to delete now.
               end if;
            end loop;
         end Encode_Rich;

      begin
         Encode_Rich;
      end LZ77_By_Rich;

      --  The following is for research purposes: compare different LZ77
      --  algorithms applied to entropy encoders (Deflate, LZMA, ...).

      procedure LZ77_From_Dump_File is
         LZ77_Dump      : Ada.Text_IO.File_Type;
         Tag            : String (1 .. 3);
         Wrong_LZ77_Tag : exception;
         A, B           : Integer;
         Dummy          : Byte;
         use Ada.Integer_Text_IO;
      begin
         --  Pretend we compress the given stream.
         --  Entire stream is consumed here.
         while More_Bytes loop
            Dummy := Read_Byte;
         end loop;
         --  Now send dumped LZ77 data further.
         Ada.Text_IO.Open (LZ77_Dump, Ada.Text_IO.In_File, "dump.lz77");
         --  File from UnZip.Decompress, or LZMA.Decoding, some_trace = True mode
         while not Ada.Text_IO.End_Of_File (LZ77_Dump) loop
            Ada.Text_IO.Get (LZ77_Dump, Tag);
            if Tag = "Lit" then
               Get (LZ77_Dump, A);
               Write_Literal (Byte (A));
            elsif Tag = "DLE" then
               Get (LZ77_Dump, A);
               Get (LZ77_Dump, B);
               Write_DL_Code (A, B);
            else
               raise Wrong_LZ77_Tag;
            end if;
            Ada.Text_IO.Skip_Line (LZ77_Dump);
         end loop;
         Ada.Text_IO.Close (LZ77_Dump);
      end LZ77_From_Dump_File;

   begin
      case Method is
      when LZHuf =>
         LZ77_Using_LZHuf;
      when IZ_4 .. IZ_10 =>
         LZ77_Using_IZ (4 + Method_Type'Pos (Method) -  Method_Type'Pos (IZ_4));
      when BT4 =>
         LZ77_Using_BT4;
      when Rich =>
         LZ77_By_Rich;
      when No_LZ77 =>
         while More_Bytes loop
            Write_Literal (Read_Byte);
         end loop;
      when Read_LZ77_Codes =>
         LZ77_From_Dump_File;
      end case;
   end Encode;

end Z_Compression.LZ77;
