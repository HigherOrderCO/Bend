type Data/Bits = e | (o t) | (i t)

Data.Bits.dec Data/Bits/e = (Data/Bits/e)
Data.Bits.dec (Data/Bits/o (Data/Bits/e)) = (Data/Bits/e)
Data.Bits.dec (Data/Bits/i (Data/Bits/e)) = (Data/Bits/o (Data/Bits/e))
Data.Bits.dec (Data/Bits/i (Data/Bits/o b.pred)) = (Data/Bits/o b.pred)
Data.Bits.dec (Data/Bits/i (Data/Bits/i b.pred)) = (Data/Bits/o b.pred)
Data.Bits.dec (Data/Bits/o (Data/Bits/o b.pred)) = (Data/Bits/i (Data.Bits.dec b.pred))
Data.Bits.dec (Data/Bits/o (Data/Bits/i b.pred)) = (Data/Bits/i (Data.Bits.dec b.pred))
