module AlphabetCipher =

    let table op col row =
        let alphabet = "abcdefghijklmnopqrstuvwxyz"
        let charToIdx (c:char) = int(c) - int(alphabet.[0])
        let size = alphabet.Length
        // `left |> op <| right` emulates the infix appilcaiton order like (-)
        // add size to avoid negative results post-modulus
        alphabet.[(size + charToIdx row |> op <| charToIdx col) % size]

    let translate op key message =
        // We repeate the key by treating it like a sequence rather than
        // creating a string of the exact length and then use the table
        // function to convert each character and then construct a string
        // from that sequence.
        let repeatedKey = seq {while true do yield! key}
        new string(Seq.map2 (table op) repeatedKey message |> Seq.toArray)

    let encode key message =
        translate (+) key message

    let decode key message =
        translate (-) key message

    let decipher cipher message =
        let rec findPrefix len (str:string) =
            let candidate = seq {for i in 0..str.Length -> str.[i % len]}
            if Seq.forall2 (=) candidate str then
                str.Substring(0, len)
            else
                findPrefix (len + 1) str
        translate (-) message cipher |> findPrefix 1

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =

    // verify encoding
    test <@ AlphabetCipher.encode "vigilance" "meetmeontuesdayeveningatseven" = "hmkbxebpxpmyllyrxiiqtoltfgzzv" @>
    test <@ AlphabetCipher.encode "scones" "meetmebythetree" = "egsgqwtahuiljgs" @>

    // verify decoding
    test <@ AlphabetCipher.decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv" = "meetmeontuesdayeveningatseven" @>
    test <@ AlphabetCipher.decode "scones" "egsgqwtahuiljgs" = "meetmebythetree" @>

    // verify decyphering
    test <@ AlphabetCipher.decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog" = "vigilance" @>
    test <@ AlphabetCipher.decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs" = "scones" @>

// run the tests
tests ()
