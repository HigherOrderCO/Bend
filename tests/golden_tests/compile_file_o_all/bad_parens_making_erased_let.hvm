X = λx x

(main) = (
    let two = (λf1λx1 (f1 λf2λx2 (f2 λf0λx0 x0)));
    let qua = (λs1λz1 (s1 λs2λz2 (s2 λs3λz3 (s3 λs4λz4 (s4 λs0λz0 z0)))));
    X X (X two qua)
    // Because there is no parens around the return term, the lets are only in scope for the first X
    // This test is to make sure that the affine checking and duping infering marks them as erased as we expect.
    // We expect an unbound variable error on two and qua.
)
