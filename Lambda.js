// Basics

True  = t => f => t
False = t => f => f
console.assert(1 == True  (1) (2))
console.assert(2 == False (1) (2))

If = b => t => f => b (t) (f)
console.assert(1 == If (True)  (1) (2))
console.assert(2 == If (False) (1) (2))

Not = x => If (x) (False) (True)
And = x => y => If (x) (y) (False)
Or  = x => y => If (x) (True) (y)

Pair = x => y => (f => f (x) (y))
Fst  = p => p (True)
Snd  = p => p (False)

p = Pair (1) (2)
console.assert(1 == Fst (p))
console.assert(2 == Snd (p))

// Arithmetic

Zero  = s => z => z
One   = s => z => s (z)
Two   = s => z => s (s (z))
Three = s => z => s (s (s (z)))

Succ = n => (s => z => s (n (s) (z)))

toInt = n => n (x => x + 1) (0)
console.assert(0 == toInt (Zero))
console.assert(1 == toInt (One))
console.assert(2 == toInt (Two))

Add = n => m => m (Succ) (n)
Mul = n => m => m (Add (n)) (Zero)
Pow = n => p => p (Mul (n)) (One)
//⇈ = n => m => m (Pow (n)) (One)
console.assert(2 == toInt (Add (One) (One)))
console.assert(5 == toInt (Add (Two) (Three)))
console.assert(6 == toInt (Mul (Two) (Three)))
console.assert(8 == toInt (Pow (Two) (Three)))


Pred = n => Fst
    (n (p => Pair (Snd (p))
                  (Succ (Snd (p))))
       (Pair (Zero) (Zero)))
Sub  = n => m => m (Pred) (n)
console.assert(0 == toInt (Pred (Zero)))
console.assert(0 == toInt (Pred (One)))
console.assert(1 == toInt (Pred (Two)))
console.assert(1 == toInt (Sub (Three) (Two)))


IsZero = n => n (s => False) (True)
Lte    = n => m => IsZero (Sub (n) (m))
Lt     = n => m => Lte (Succ (n)) (m)
Eq     = n => m => And (Lte (n) (m)) (Lte (m) (n))

Max = n => m => If (Lte (n) (m)) (m) (n)
Min = n => m => If (Lte (n) (m)) (n) (m)

// Lists

Nil    = f => x => x
Append = a => l => (f => x => f (a) (l (f) (x)))

Head   = l => l (a => x => a) ()
console.assert(1 == Head (list))

Tail = l => Fst (
    l (a => p => Pair (Snd (p))
                      (Append (a) (Snd (p))))
      (Pair (Nil) (Nil))
)
list = Append (1) (Append (2) (Nil))
console.assert(2 == Head (Tail (list)))

Foldr   = f => z => l => l (f) (z)
Map     = m => l => (f => x => l (a => f (m (a))) (x))
Length  = l => Foldr (a => Succ) (Zero) (l)
IsEmpty = l => IsZero (Length (l))

function arraysEqual(a,b) { return !(a < b) && !(b < a); }
toList    = l => l (x => y => [x].concat(y)) ([])
toIntList = l => toList (Map (toInt) (l))
console.assert(0 == toInt (Length (Nil)))
console.assert(1 == toInt (Length (Append (1) (Nil))))
console.assert(arraysEqual([0, 1], toIntList (Map (Pred) (Append (One) (Append (Two) (Nil))))))

// Recursion

Z = f => (x => f (y => x (x) (y)))
         (x => f (y => x (x) (y)))

Rem = Z (rem => n => m => (
    If (Lt (n) (m)) (_ => n)
                    (_ => rem (Sub (n) (m)) (m))
) ())
console.assert(1 == toInt (Rem (Three) (Two)))
console.assert(0 == toInt (Rem (Three) (One)))

Gcd = Z (gcd => n => m => (
    If (IsZero (m)) (_ => n)
                    (_ => gcd (m) (Rem (n) (m)))
) ())

// Sequences

Seq = head => tail => Pair (head) (tail)

SeqHead = seq => Fst (seq)
SeqTail = seq => (Snd (seq)) ()

SeqTake = Z (take => n => seq => (
    If (IsZero (n)) (_ => Nil)
                    (_ => Append (SeqHead (seq))
                                 (take (Pred (n)) (SeqTail (seq))))
) ())

Nat = (Z (natFrom => n => Seq (n) (_ => natFrom (Succ (n))))) (Zero)
console.assert(arraysEqual([0, 1, 2], toIntList (SeqTake (Three) (Nat))))

SeqFilter = Z (filter => cond => seq => (
    If (cond (SeqHead (seq))) (_ => Seq (SeqHead (seq))
                                        (_ => filter (cond) (SeqTail (seq))))
                              (_ => filter (cond) (SeqTail (seq)))
) ())

Primes = (Z (sieve => nums =>
    Seq (SeqHead (nums))
        (_ => sieve (SeqFilter (p => Not (IsZero (Rem (p) (SeqHead (nums)))))
                               (SeqTail (nums)))
))) (SeqTail (SeqTail (Nat)))

Ten = Mul (Two) (Add (Two) (Three))
console.assert(arraysEqual([2, 3, 5, 7, 11, 13, 17, 19, 23, 29], toIntList (SeqTake (Ten) (Primes))))