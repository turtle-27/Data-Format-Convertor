fun makeList1(x) = 
    let
        let 
            val t = "ss"
        in     
            case(x) of
            0 =>  t = "ss"
            | 1 =>  t = "tt"
        end;   
        val c = "\\c";
        val d = "\\d\n";
        val e = #"\\";
        val count = ref 0
    in
        if (c= "\\c") then print("true\n")
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
