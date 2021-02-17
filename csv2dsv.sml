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
                            count := !count+1;
                            if(String.sub(line, !count) = delim1) then output(outfile, str(delim1))
                            else if (String.sub(line, !count) = delim2) then output(outfile, "\\"^str(delim2))
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