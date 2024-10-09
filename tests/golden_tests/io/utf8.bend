to-and-back s = (String/decode_utf8 (String/encode_utf8 s))

v1 = (to-and-back "hi")
v2 = (to-and-back "(λf ((λx (f (x x))) (λx (f (x x)))))")
v3 = (to-and-back "🌟")
v4 = (to-and-back "Hello 🌎!")
v5 = (to-and-back "𓆈 𓆉 𓆊 𓆋 𓅯")
v6 = (String/decode_utf8 [])

main = [v1, v2, v3, v4, v5, v6]
