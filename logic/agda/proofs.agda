data ℕ : Set where
  zero : ℕ
  suc  : ℕ → ℕ

{-# BUILTIN NATURAL ℕ #-}

pred : ℕ → ℕ
pred zero = zero
pred (suc n) = n

_+_ : ℕ → ℕ → ℕ
zero + n = n
suc m + n = suc (m + n)

t = 3 + 2

_-_ : ℕ → ℕ → ℕ
zero - n = zero
m - zero = m
suc m - suc n = m - n

t1 = 5 - 2

_*_ : ℕ → ℕ → ℕ
zero * _ = zero
suc m * n = (m * n) + n

t2 = 15 * 15
