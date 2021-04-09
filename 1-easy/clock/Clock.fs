module Clock

type Clock = private Clock of hours: int * minutes: int

let create hours minutes =
    let rollOver =
        let rec loop offset range number =
            if number >= range then
                loop (offset + 1) range (number - range)
            elif number < 0 then
                loop (offset - 1) range (number + range)
            else
                (number, offset)
        loop 0

    let (minutes, offsetHours) = rollOver 60 minutes
    let (hours, _) = rollOver 24 (hours + offsetHours)

    Clock (hours, minutes)

let add minutes (Clock (h, m)) =
    create h (m + minutes)

let subtract minutes (Clock (h, m)) =
    create h (m - minutes)

let display (Clock (h, m)) =
    $"{h:d2}:{m:d2}"
