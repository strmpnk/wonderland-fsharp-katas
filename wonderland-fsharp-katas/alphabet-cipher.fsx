type Message = string
type Keyword = string

let alphabet = "abcdefghijklmnopqrstuvwxyz"
let charToIdx (c:char) = int(c) - int(alphabet.[0])

let tableEnc (cipherCol:char) (messageRow:char) =
    let size = alphabet.Length
    alphabet.[(charToIdx cipherCol + charToIdx messageRow) % size]

let tableDec (cipherCol:char) (messageRow:char) =
    let size = alphabet.Length
    alphabet.[(size + charToIdx messageRow - charToIdx cipherCol) % size]

let translate table (key:Keyword) (message:Message) =
    let repeatedKey = seq {while true do yield! key}
    new string(Seq.map2 table repeatedKey message |> Seq.toArray)

let encode (key:Keyword) (message:Message) : Message =
    translate tableEnc key message

let decode (key:Keyword) (message:Message) : Message =
    translate tableDec key message

let repeatedPrefix (str:string) =
    let rec findPrefix len =
        let candidate = seq {for i in 0 .. str.Length -> str.[i % len]}
        if Seq.forall2 (=) candidate str then
            str.Substring(0, len)
        else
            findPrefix (len + 1)
    findPrefix 1

let decipher (cipher:Message) (message:Message) =
    translate tableDec message cipher |> repeatedPrefix

#r @"../packages/Unquote/lib/net45/Unquote.dll"
open Swensen.Unquote

let tests () =

    // verify encoding
    test <@ encode "vigilance" "meetmeontuesdayeveningatseven" = "hmkbxebpxpmyllyrxiiqtoltfgzzv" @>
    test <@ encode "scones" "meetmebythetree" = "egsgqwtahuiljgs" @>

    // verify decoding
    test <@ decode "vigilance" "hmkbxebpxpmyllyrxiiqtoltfgzzv" = "meetmeontuesdayeveningatseven" @>
    test <@ decode "scones" "egsgqwtahuiljgs" = "meetmebythetree" @>

    // verify decyphering
    test <@ decipher "opkyfipmfmwcvqoklyhxywgeecpvhelzg" "thequickbrownfoxjumpsoveralazydog" = "vigilance" @>
    test <@ decipher "hcqxqqtqljmlzhwiivgbsapaiwcenmyu" "packmyboxwithfivedozenliquorjugs" = "scones" @>

// run the tests
tests ()
