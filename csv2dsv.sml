open TextIO;
exception emptyInputFile;
exception LastFieldFollwedByDelimeter of string;
exception UnevenFields of string;
exception inputFormatIsIncorrect;
exception notEnclosed;
fun rawconvertDelimeters(infilename, delim1, outfilename, delim2) = 
    let 
        (*infile takes input file, outfile gives output file, ncol: no. of column in line 1, nline: current line number.*)
        val infile = openIn(infilename);
        val outfile = openOut(outfilename);
        val ncol = ref 0;
        val flag = ref 0;
        val nline = ref 0;
        val check = ref 0;
        val flag1 = ref 0;
        val del2 = ref 0;
        val mark = ref 0;
        val out = ref ("")
    in 
        if (valOf(canInput(infile, 1)) = 0) then raise emptyInputFile
        else 
        while (valOf(canInput(infile, 1)) = 1) do (
            let 
                (*line stores current line, check is the number of delimeters detected, count refer to index of current character of the line.*)
                val line = valOf(inputLine(infile));
                val count = ref 0;
               
            in
                while(!count < String.size(line)) do (
                    let 
                        val c = String.sub(line, !count)
                    in
                        if(c = #"\"") then (
                            if(!flag1 = 0) then flag1 := 1
                            else flag1 := 0;
                            if(!mark = 1) then (
                                if(!flag1 = 0) then (
                                    if((String.sub(line, !count+1) = #"\"") orelse (String.sub(line, !count+1) = delim1) orelse (String.sub(line, !count+1) = #"\n")) then out := (!out)^str(c)
                                    else raise inputFormatIsIncorrect
                                )
                                else(
                                    out := (!out)^str(c)
                                )
                            )
                            else (
                                if(!out = "") then 
                                    out := (!out)^str(c)
                                else raise notEnclosed;
                                 mark := 1
                            )    
                                (* if(String.sub(line, !count+1) = delim1 or String.sub(line, !count+1) = "\n") then 
                                    (
                                        if(!flag1 = 0) then out := (!out)^str(c) 
                                        else raise inputFormatIsIncorrect
                                    )
                                else (
                                    if(!flag1 = 0) then out := (!out)^str(c)
                                    else (
                                        if(String.sub(line, !count+1) = #"\"") then out := (!out)^str(c)
                                        else raise inputFormatIsIncorrect
                                    )
                                )          *)
                            
                        )
                        else if(c = #"\n") then(
                            if(!flag1 = 0) then (
                                if(String.sub(line, !count-1) = delim1) then raise LastFieldFollwedByDelimeter("Last Field is followed by Delimeter\n")
                                else(
                                    if (!flag = 0) then  (
                                            ncol := !ncol + 1;
                                            check := !check + 1
                                        )
                                    else (
                                            check := !check + 1
                                        );
                                    if(!del2 = 0) then (
                                            mark := 0;
                                            out := !out^(str(c));
                                            output(outfile, (!out))
                                            )
                                    else (
                                            if(!mark = 1) then
                                                (mark := 0;
                                                out := !out^(str(c));
                                                output(outfile, !out) 
                                                )
                                            else (
                                                out := "\""^(!out)^"\""^(str(c));
                                                output(outfile, !out)
                                            );
                                            del2 := 0   
                                        );
                                        out := ""
                                )
                            )
                            else  out := (!out)^substring(line, !count, 1)
                        )
                        else if(c = delim1) then (
                             if(!flag1 = 1) then (
                                if(!mark = 0) then raise inputFormatIsIncorrect  
                                else out := (!out)^str(delim1)
                                )
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
                if (!flag1 = 0) then (
                    nline := !nline +1;
                    if (!ncol = !check) then (count := !count;
                                                flushOut(outfile))
                    else raise UnevenFields("Expected: "^Int.toString(!ncol)^" fields, Present: "^Int.toString(!check)^" fields on Line "^Int.toString(!nline)^"\n");
                    check := 0
                )
                else flushOut(outfile)
            end
        );
        if(!flag1 = 1) then raise inputFormatIsIncorrect
        else closeOut(outfile)
        
    end

fun convertDelimeters(infilename, delim1, outfilename, delim2) = rawconvertDelimeters(infilename, delim1, outfilename, delim2) handle
    UnevenFields(line) => print(line) |
    emptyInputFile => print("exception emptyInputFile\n")
    | LastFieldFollwedByDelimeter(line) => print(line)
    | inputFormatIsIncorrect => print("Input Format Is Incorrect\n");

fun csv2tsv(infilename, outfilename) = convertDelimeters(infilename, #",", outfilename, #"\t");
fun tsv2csv(infilename, outfilename) = convertDelimeters(infilename, #"\t", outfilename, #",");
