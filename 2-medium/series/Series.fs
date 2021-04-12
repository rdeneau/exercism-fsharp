module Series

let slices (str: string) length =
    let nbSlices = 1 + str.Length - length
    if nbSlices <= 0 || length <= 0 then
        None
    else
        Some [ for n in 1 .. nbSlices do
                str.Substring(n - 1, length) ]
