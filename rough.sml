fun makeList1() = 
    let
        val c = "\\c";
        val d = "\\d\n";
        val e = #"\\"
    in
        print(str(String.sub(c, 0)));
        print(str(String.sub(c,1)))
    end;