module String

let withCapital (s: string) =
    s.[0..0].ToUpper() + s.[1..]
