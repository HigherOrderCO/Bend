String/len s = (String/len/go s 0)
String/len/go (String/nil) x = x
String/len/go (String/cons hd tl) x = (String/len/go tl (+ x 1))

main = (String/len "λx 🐼")
