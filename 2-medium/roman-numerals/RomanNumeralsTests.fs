module RomanNumeralsTests

open FsUnit.Xunit
open Xunit

open RomanNumerals

module Facts =
    [<Theory>]
    [<InlineData(0, "")>]
    [<InlineData(1, "I")>]
    [<InlineData(2, "II")>]
    [<InlineData(3, "III")>]
    [<InlineData(4, "IV")>]
    [<InlineData(5, "V")>]
    [<InlineData(6, "VI")>]
    [<InlineData(7, "VII")>]
    [<InlineData(8, "VIII")>]
    [<InlineData(9, "IX")>]
    let ``check single numbers`` number expected =
        roman number |> should equal expected

    [<Theory>]
    [<InlineData(10, "X")>]
    [<InlineData(20, "XX")>]
    [<InlineData(30, "XXX")>]
    [<InlineData(40, "XL")>]
    [<InlineData(50, "L")>]
    [<InlineData(60, "LX")>]
    [<InlineData(70, "LXX")>]
    [<InlineData(80, "LXXX")>]
    [<InlineData(90, "XC")>]
    let ``check tens`` number expected =
        roman number |> should equal expected

    [<Theory>]
    [<InlineData(100, "C")>]
    [<InlineData(200, "CC")>]
    [<InlineData(300, "CCC")>]
    [<InlineData(400, "CD")>]
    [<InlineData(500, "D")>]
    [<InlineData(600, "DC")>]
    [<InlineData(700, "DCC")>]
    [<InlineData(800, "DCCC")>]
    [<InlineData(900, "CM")>]
    let ``check hundreds`` number expected =
        roman number |> should equal expected

    [<Theory>]
    [<InlineData(1000, "M")>]
    [<InlineData(2000, "MM")>]
    [<InlineData(3000, "MMM")>]
    [<InlineData(4000, "MMMM")>]
    let ``check thousands`` number expected =
        roman number |> should equal expected

    [<Theory>]
    [<InlineData(48, "XL"+"VIII")>]
    [<InlineData(402, "CD"+"II")>]
    [<InlineData(577, "D"+"LXX"+"V"+"II")>]
    [<InlineData(911, "CM"+"X"+"I")>]
    [<InlineData(1024, "M"+"XX"+"IV")>]
    let ``check combinations`` number expected =
        roman number |> should equal expected


module Helpers =
    let numberAt number powerOfTen =
        let factor = pown 10 powerOfTen
        ((number / factor) % 10) * factor

    [<Theory>]
    [<InlineData(123, 0, 3)>]
    [<InlineData(123, 1, 20)>]
    [<InlineData(123, 2, 100)>]
    let ``check numberAt`` number powerOfTen expected =
        let result = numberAt number powerOfTen
        result |> should equal expected

    let romanToArabic (romanNumeral: string) =
      romanNumeral
        .Replace("CM","DCD")
        .Replace("CD","CCCC")
        .Replace("XC","LXL")
        .Replace("XL","XXXX")
        .Replace("IX","VIV")
        .Replace("IV","IIII")
        .Replace("M","DD")
        .Replace("D","CCCCC")
        .Replace("C","LL")
        .Replace("L","XXXXX")
        .Replace("X","VV")
        .Replace("V","IIIII")
        .Length

    [<Theory>]
    [<InlineData("I",        1)>]
    [<InlineData("IX",       9)>]
    [<InlineData("XXIV",     24)>]
    [<InlineData("CMXCIX",   999)>]
    [<InlineData("MCDXCIII", 1493)>]
    let ``check romanToArabic`` romanNumeral expected =
        let result = romanToArabic romanNumeral
        result |> should equal expected


// Property based testing
// See https://fsharpforfunandprofit.com/posts/property-based-testing-4/
module Properties =
    open FsCheck
    open FsCheck.Xunit

    type ArabicRomanNumerals =
        static member Values() =
            // More efficient than `Arb.Default.Int32() |> Arb.filter (fun i -> i >= 0 && i <= 4000)`
            // See https://blog.ploeh.dk/2015/02/23/a-simpler-arbitrary-for-the-diamond-kata/
            Gen.elements [0..4000] |> Arb.fromGen

    type RomanNumeralPropertyAttribute () =
        inherit PropertyAttribute(
            Arbitrary = [| typeof<ArabicRomanNumerals> |],
            EndSize = 4000,
            MaxTest = 1000 )

    [<RomanNumeralProperty>]
    let ``Roman numeral is the concatenation of the roman numerals of the digits`` (number: int) =
        let concatenation =
            [0..3]
            |> List.map (Helpers.numberAt number >> roman)
            |> List.rev
            |> String.concat ""

        let result = roman number
        concatenation = result

    [<RomanNumeralProperty>]
    let ``Reverse to arabic from roman get back to input number`` (number: int) =
        let result =
            number
            |> roman
            |> Helpers.romanToArabic
        number = result
