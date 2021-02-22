fun makeList1() = 
    let 
        val c = "\\c";
        val d = "\\d\n";
        val e = #"\""
        val out =  ref "";
        val flag  = ref 0;
        val count = ref 0
    in
        if (c= "\\c") then 
            (print(!out);
            print(str(e)^"\"");
            print(Int.toString(!flag)^"jj\n");
            out := "test\n";
            out := "\""^!out^"uu";
            print(!out^"KK")
            )
        else print("false");
        if (!count = 0) then count
        else count
    end

fun check() = 
    let 
        val t = makeList1()
    in 
      print(Int.toString(!t))
    end
