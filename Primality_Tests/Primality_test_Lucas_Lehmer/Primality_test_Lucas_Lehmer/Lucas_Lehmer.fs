module Lucas_Lehmer

open System
open System.Collections
open System.Collections.Generic
open System.IO
open System.Numerics
open System.Linq
open System.Diagnostics
open System.Management
open System.Threading

let mutable p_start = -1
let mutable p_end = -1

printfn "Please enter p_start : "
p_start <- Convert.ToInt32 (Console.ReadLine())

printfn "Please enter p_end : "
p_end <- Convert.ToInt32 (Console.ReadLine())

let prime_set = new HashSet<int>()

let mersen_prime_set = new HashSet<BigInteger>()

let mersen_prime_exp = new HashSet<Int32>()

prime_set.Add(2) |> ignore

let prime_det (n : int) =
    if n%2 = 0 then 
        false
    elif n=1 then
        false
    else
        let prime_set_max = prime_set.Max()
        let mutable res = true
        if int32(sqrt(float n)) <= prime_set_max then
            for elem in prime_set do
                if n%elem = 0 then
                    res <- false
                else
                    res <- res
        else
            for elem in prime_set do
                if n%elem = 0 then
                    res <- false
                else
                    res <- res
            for j=prime_set_max+1 to int32(sqrt(float32 n))+1 do
                if n%j = 0 then
                    res <- false
                else
                    res <- res
        if res then prime_set.Add(n) |> ignore
        res

printfn "Generating prime number set from %d to %d" p_start p_end
let prime_num_gen_timespan = new Stopwatch()
prime_num_gen_timespan.Start()
for i=1 to p_start do
    prime_det i |> ignore
prime_num_gen_timespan.Stop()
let prime_num_gen_time = prime_num_gen_timespan.ElapsedMilliseconds
printfn "Generating prime number set done! - Elapsed Time : %d(ms)" prime_num_gen_time

let mersen_number_primality_timespan = new Stopwatch()
mersen_number_primality_timespan.Start()
for i=p_start to p_end do
    //printfn "Primality test for Mersen Number with p = %d" i
    let mutable res = false
    if prime_det i && i%2 <> 0 then
        //let mersen_number = (int64 (System.Math.Pow((float)2, (float)i))) - 1L
        let mutable mersen_number_big = 1I
        for j=0 to i-1 do
            mersen_number_big <- mersen_number_big * 2I
        mersen_number_big <- mersen_number_big - 1I
        let mutable s = 4I
        for j=0 to i-3 do
            s <- (s * s - 2I)%mersen_number_big
        if s%mersen_number_big = 0I then
            res <- true
        else
            res <- false
        if res then
            printfn "Primality test for Mersen Number with p = %d : TRUE" i
            mersen_prime_set.Add(mersen_number_big) |> ignore
            mersen_prime_exp.Add(i) |> ignore
        else
            printfn "Primality test for Mersen Number with p = %d : FALSE" i
    elif i=2 then
        let mersen_number_big = 3I
        printfn "Primality test for Mersen Number 3 (p = 2) : TRUE"
        mersen_prime_set.Add(mersen_number_big) |> ignore
        mersen_prime_exp.Add(2) |> ignore
    else
        printf ""

mersen_number_primality_timespan.Stop()
let mersen_number_primality_time = mersen_number_primality_timespan.ElapsedMilliseconds
printfn "Lucas-Lehmer Primality test for Mersen Numbers with p : %d to %d Done. - Elapsed Time : %d(ms)" p_start p_end mersen_number_primality_time
printf "\n====================\n\n"
printfn "Found Mersen Prime Numbers : "
printfn "\n===================\n"
for i=0 to mersen_prime_set.Count-1 do
    printfn "Mersen Prime Number : %s, Exponent : %d" (mersen_prime_set.ElementAt(i).ToString()) (mersen_prime_exp.ElementAt(i))
printfn ""