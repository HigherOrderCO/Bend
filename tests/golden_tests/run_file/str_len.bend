String/len s = (String/len/go s 0)
String/len/go (String/Nil) x = x
String/len/go (String/Cons hd tl) x = (String/len/go tl (+ x 1))

main = (String/len "λx 🐼")
