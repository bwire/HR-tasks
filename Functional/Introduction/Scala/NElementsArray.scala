-- Array of N elements

def f(num:Int) : String = {
    val list = List.range(0, num)
    return list.mkString("[", ",", "]")
} 