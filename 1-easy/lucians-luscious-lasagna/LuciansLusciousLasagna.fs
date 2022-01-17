module LuciansLusciousLasagna

let expectedMinutesInOven = 40

let remainingMinutesInOven duration =
    expectedMinutesInOven - duration

let preparationTimeInMinutes layerCount =
    2 * layerCount

let elapsedTimeInMinutes layerCount duration =
    duration + preparationTimeInMinutes layerCount
