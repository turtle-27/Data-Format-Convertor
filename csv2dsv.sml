open TextIO;
fun convertDelimeters(infilename, delim1, outfilename, delim2) = 
    let 
        val infile = openIn(infilename);
        val outfile = openOut(outfilename);
        val n = ref 0
    in 
        while (valOf(canInput(infile, 1)) = 1) do (
            let 
                val line = valOf(inputLine(infile));
                val count = ref 0
            in
                while(!count < String.size(line)) do (
                    output(outfile, substring(line, !count, 1));
                    count := !count +1
                )
            end
        );
        closeOut(outfile)
    end