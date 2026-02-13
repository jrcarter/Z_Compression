# Z_Compression
Standalone version of the Zlib Deflate compression and Inflate decompression algorithms, derived from de Montmollin's Zip-Ada

Z_Compression.Compress applies Deflate to an input stream of bytes, defined by its generic formal subprograms Out_Of_Data and Next, producing an output stream of bytes that are passed to the generic formal procedure Put. The output may be in Zlib or raw Deflate formats.

Adler_32_Checksums is a package for calculating Adler-32 checksums. It is used by Z_Compression.Compress when outputing in Zlib format.

ZC_Test is a test/demo program for Z_Compression.Compress. Building it should also compile Z_Compression. It is invoked as

```
zc_test <file name>
```

No error checking or handling is done. It compresses the given file into a Zlib format file. The output file name is the input file name with ".zz" appended.

Z_Compression.Decompress applies Inflate to an input stream of bytes, defined by its generic formal subprograms Out_Of_Data and Next, producing an output stream of bytes that are passed to the generic formal procedure Put. The stream of bytes returned by Next must be in raw Deflate format.

It is reasonably straightforward to define Out_Of_Data and Next to decompress data in Zlib format. See program Z_Decomp for an example.

Z_Decomp is a test/demo program for Z_Compression.Decompress. It is invoked as

```
z_decomp <file name>
```

No error checking or handling is done. It decompresses the given file. The output file name is the input file name with ".zdc" appended. If the input file name ends in ".zz", the file is presumed to be in Zlib format; otherwise, it is treated as being in raw Deflate format. If the input is in Zlib format, the Zlib header is removed and checked that its first byte is 78<sub>16</sub>; if it is not, the program ends with an unhandled exception. The Adler-32 checksum of the output bytes is calculated and compared to the checksum in the file; if they are not the same, a message is output.
