open TextIO;
fun convertDelimeters(infilename, delim1, outfilename, delim2) = 
    let 
        val infile = openIn(infilename);
        val outfile = openOut(outfilename)
    in 
        while (valOf(canInput(infile, 1)) = 1) do (
            let 
                val line = valOf(inputLine(infile));
                val count = ref 0
            in
                while(!count < String.size(line)) do (
                    let 
                        val c = String.sub(line, !count)
                    in
                        if(c = delim1) then output(outfile, str(delim2))
                        else if (c = delim2) then output(outfile, "\\"^str(delim2))
                        else if (c = #"\\") then
                            (
                            if(String.sub(line, !count+1) = delim1) then 
                            (output(outfile, str(delim1));
                            count := !count+1)
                            else if (String.sub(line, !count+1) = delim2) then (
                                output(outfile, "\\\\"^str(delim2));
                                count := !count + 1 )
                            else output(outfile, substring(line, !count, 1))
                            )
                        else output(outfile, substring(line, !count, 1));
                        count := !count +1
                    end
                )
            end
        );
        closeOut(outfile)
    end

fun convertNewlines(infilename, newline1, outfilename, newline2) = 
    let 
        val infile = openIn(infilename);
        val outfile = openOut(outfilename);
        val size1 = String.size(newline1)
    in 
        print(Int.toString(size1));
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
                            print(c^"a");
                            output(outfile, newline2);
                            count := !count + size1)
                        else (
                            print(c^"b");
                            output(outfile, substring(line, !count, 1));
                            count := !count + 1
                            )
                    end
                )
            end
        );
        closeOut(outfile)
    end