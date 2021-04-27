(*  Create File3  *)
MODULE Merge;

FROM FileSystem IMPORT File, Lookup, Delete, ReadWord, WriteWord, Close,
  Response;

TYPE
  FileName = ARRAY [1..15] OF CHAR;

PROCEDURE Make(name: FileName; start, stop: INTEGER);
VAR
  outfile:  File;
  i:        INTEGER;

BEGIN
  (* open the file for output *)
  Lookup(outfile, name, TRUE);
  Delete(outfile);

  Lookup(outfile, name, TRUE);

  (* write values to the file *)
  FOR i := start TO stop DO
    WriteWord(outfile, i);
  END; (* for *)

  Close(outfile);
END Make;

VAR
  outfile: File;

PROCEDURE Append(name: FileName);
(*
  Add the input file to outfile

  Parameters:
    name - name of the file to add

  Global Variables:
    outfile - output file variable
*)
VAR
  i:      INTEGER;          (* file value *)
  infile: File;             (* file *)
BEGIN (* Merge *)
  Lookup(infile, name, FALSE);
  ReadWord(infile, i);

  WHILE NOT infile.eof DO
    WriteWord(outfile, i);
    ReadWord(infile, i);
  END; (* while *)

  Close(infile);
END Append;

(* Main Program *)

BEGIN
  Make('file1.dat', 1, 10);
  Make('file2.dat', 11, 20);

  (* open the file for output *)
  Lookup(outfile, 'file3.dat', TRUE);
  Delete(outfile);
  Lookup(outfile, 'file3.dat', TRUE);

  (* merge the two files *)
  Append('file1.dat');
  Append('file2.dat');

  Close(outfile);
END Merge.

