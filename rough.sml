open TextIO;
(* fun readList(infile) =
 if endOfStream(infile) then nil
 else input1(infile) :: readList(infile); *)

fun makeList1(infile, c) =
    if isSome(c) then
        valOf(c)::makeList1(infile,input1(infile))
    else nil;