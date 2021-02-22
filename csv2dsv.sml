open TextIO;
exception emptyInputFile;
exception UnevenFields of string;
fun rawconvertDelimeters(infilename, delim1, outfilename, delim2) = 
    let 
        (*infile takes input file, outfile gives output file, ncol: no. of column in line 1, nline: current line number.*)
        val infile = openIn(infilename);
        val outfile = openOut(outfilename);
        val ncol = ref 0;
        val flag = ref 0;
        val nline = ref 0;
    in 
        if (valOf(canInput(infile, 1)) = 0) then raise emptyInputFile
        else 
        while (valOf(canInput(infile, 1)) = 1) do (
            let 
                (*line stores current line, check is the number of delimeters detected, count refer to index of current character of the line.*)
                val line = valOf(inputLine(infile));
                val count = ref 0;
                val check = ref 0;
                val out = ref ("");
                val flag1 = ref 0;
                val del2 = ref 0;
                val mark = ref 0
            in
                nline := !nline +1;
                while(!count < String.size(line)) do (
                    let 
                        val c = String.sub(line, !count)
                    in
                        if(c = #"\"") then (
                            if(!flag1 = 0) then flag1 := 1
                            else flag1 := 0;
                            mark := 1;
                            out := (!out)^str(c)
                        )
                        else if(c = delim1) then (
                             if(!flag1 = 1) then out := (!out)^str(delim1)
                                else (
                                    if (!flag = 0) then  (
                                         ncol := !ncol + 1;
                                         check := !check + 1
                                    )
                                    else (
                                        check := !check + 1
                                    );
                                    if(!del2 = 0) then (
                                        mark := 0;
                                        out := !out^(str(delim2));
                                        output(outfile, (!out))
                                        )
                                    else (
                                        if(!mark = 1) then
                                            (mark := 0;
                                            out := !out^(str(delim2));
                                            output(outfile, !out) 
                                            )
                                        else (
                                            out := "\""^(!out)^"\""^(str(delim2));
                                            output(outfile, !out)
                                        );
                                        del2 := 0   
                                    );
                                    flag1 := 0;
                                    out := ""
                                )
                            )
                        else if (c = delim2) then (
                            out := (!out)^str(delim2);
                            del2 := 1)
                        else out := (!out)^substring(line, !count, 1);
                        count := !count +1
                    end
                );
                flag := 1;
                if (!ncol = !check) then (count := !count;
                                            flushOut(outfile))
                else raise UnevenFields("Expected: "^Int.toString(!ncol+1)^" fields, Present: "^Int.toString(!check+1)^" fields on Line "^Int.toString(!nline)^"\n")
            end
        );
        closeOut(outfile)
    end

fun rawconvertNewlines(infilename, newline1, outfilename, newline2:string) = 
    let 
    (*infile: input stream, outfile: output stream, size1: size of newline1*)
        val infile = openIn(infilename);
        val outfile = openOut(outfilename);
        val size1 = String.size(newline1)
    in 
        if (valOf(canInput(infile, 1)) = 0) then raise emptyInputFile
        else
        while (valOf(canInput(infile, 1)) = 1) do (
            let 
                val line = valOf(inputLine(infile));
                val count = ref 0
            in
                while(!count+size1 <= String.size(line)) do (
                    let 
                        val c = substring(line, !count, size1)
                    in
                        if(c = newline1) then (
                            output(outfile, newline2);
                            count := !count + size1)
                        else (
                            output(outfile, substring(line, !count, 1));
                            count := !count + 1
                            )
                    end
                )
            end
        );
        closeOut(outfile)
    end

fun convertDelimeters(infilename, delim1, outfilename, delim2) = rawconvertDelimeters(infilename, delim1, outfilename, delim2) handle
    UnevenFields(line) => print(line) |
    emptyInputFile => print("exception emptyInputFile\n");

fun convertNewlines(infilename, newline1, outfilename, newline2:string) = rawconvertNewlines(infilename, newline1, outfilename, newline2) handle
      emptyInputFile => print("exception emptyInputFile\n");

fun csv2tsv(infilename, outfilename) = convertDelimeters(infilename, #",", outfilename, #"\t");
fun tsv2csv(infilename, outfilename) = convertDelimeters(infilename, #"\t", outfilename, #",");
fun unix2dos(infilename, outfilename) = convertNewlines(infilename, "\n", outfilename, "\r\n");
fun dos2unix(infilename, outfilename) = convertNewlines(infilename, "\r\n", outfilename, "\n");