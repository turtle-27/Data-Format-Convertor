open TextIO;
exception emptyInputFile; (*Exception raised when input file is empty*)
exception LastFieldFollwedByDelimeter of string; (*raised when last field of record is followed by delimeter*)
exception UnevenFields of string; (*When uneven fields are present in the file*)
exception inputFormatIsIncorrect; (*When input format is not as per specification*)
(*rawconverDelimeters convert delim1 to delim2*)
fun rawconvertDelimeters(infilename, delim1, outfilename, delim2) = 
    let 
       (*infile takes input file, outfile gives output file, ncol: no. of field in line 1, nline: current line number.*)
       (*flag is to check if current record is first record or not.*)
       (*check: no. of field in current record *)
       (*flag1: represents parity of double-quote*)
       (*del2: It checks if delim2 is in the field or not.*)
       (*mark: It checks if double-quote is in the field or not*)
       (*out: It stores current field *)
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
                            if(!flag1 = 0) then flag1 := 1  (*changing parity of double-quote*)
                            else flag1 := 0;
                            if(!mark = 1) then (   
                                if(!flag1 = 0) then (       (*checking the input and raise exception when input is not valid*)
                                    if((String.sub(line, !count+1) = delim1) orelse (String.sub(line, !count+1) = #"\n") orelse (String.sub(line, !count+1) = #"\"") ) then out := (!out)^str(c)
                                    else raise inputFormatIsIncorrect
                                )
                                else(
                                    out := (!out)^str(c)
                                )
                            )
                            else (
                                if(!out = "") then 
                                    out := (!out)^str(c)
                                else raise inputFormatIsIncorrect;
                                 mark := 1
                            )    
                            
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
                                                out := "\""^(!out)^"\""^(str(c));   (*enclosing field which contains delim2*)
                                                output(outfile, !out)
                                            );
                                            del2 := 0   
                                        );
                                        out := "";
                                        flag := 1

                                )
                            )
                            else  out := (!out)^substring(line, !count, 1)
                        )
                        else if(c = delim1) then (  (*converting delim1 to delim2 when delim1 is not inside field*)
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
                                            out := "\""^(!out)^"\""^(str(delim2));   (*enclosing field which contains double-quotes*)
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
                
                if (!flag1 = 0) then (   (*checking for uneven field*)
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
    emptyInputFile => print("Exception: emptyInputFile\n")
    | LastFieldFollwedByDelimeter(line) => print(line)
    | inputFormatIsIncorrect => print("Exception: Input format is incorrect\n");

fun csv2tsv(infilename, outfilename) = convertDelimeters(infilename, #",", outfilename, #"\t");
fun tsv2csv(infilename, outfilename) = convertDelimeters(infilename, #"\t", outfilename, #",");
